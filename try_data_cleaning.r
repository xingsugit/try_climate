#Author:  Xing Su 2/24/2016.  Modified by N.Y. Kiang 2/25/2016.
## This code is a clean version for try data cleaning. It follows the steps, with the following fixes by Nancy:
## 1.  StdValue in the original TRY data is already converted to standard units.  It does not need to be converted again.
## 2.  Search for lower case Roman numerals i), ii), iii), etc. for different sections.

# i) Read the original data to build table for Ent
# To check: I used python code to process the data for some reason. It might because the
# length of lines are inconsistent. 
#Xing: orig_file = "TRYData"
#Xing: orig_data <- read.csv(orig_file,sep="\t")
#Nancy: switched to R code to retrieve from netcdf grd file by pixel instead of reading whole file.

datasourcesdir = "/Users/nkiang/NancyResearch/GISS/Models/Ent/Datasets/TRY/TRY_XingSu/20160211_XingTryscrub_sources/"

orig_data <- try
c_dataName = c("Leaf mass per area (LMA) of total leaf area",
               "Specific leaf area (SLA) of shaded leaves",
               "SLA of leaf lamina")
SLA <-  orig_data[ which(orig_data$TraitID==11), ]

SLA2 <- orig_data[ which(orig_data$TraitID==398 & orig_data$DataName %in%c_dataName ), ] ## note: inside 398, only pick DataName == 'SLA of leaf lamina' (6200 entries)

SLA = rbind(SLA,SLA2)
# ii) Extract and convert units for SLA.


###convert to standard value: mm2 mg-1 (mm2/mg-1)
###possible origValue to convert:
### X 1000: (m for 1000)
mUnitList = c("m2 g-1")
### 1/Orig * 1000
mOverUnitList = c("g m-2","g/m2")
### X 100:(h for 100)
hUnitList = c("cm2 mg-1")
### 1/Orig * 100
hOverUnitList = c("mg cm-2","mg/cm2")
### 1/Ori 10000: (t for 10000)
tOverUnitList = c("mg/dm2")
### same
sameUnitList = c("mm2 mg-1","m2 kg-1")
## X 1/10
over10UnitList = c("cm2 g-1")
## X 1/1000
over1000UnitList = c("mm2 g-1")

valueConvert = function(origValueList, origUnitName,origUnitStr)  {
  temp = c()
  for (i in 1:length(origValueList) )
  {
    if(origUnitName[i] %in% mUnitList)
    {
      convertValue = 1000 * origValueList[i]
    }
    else if(origUnitName[i] %in% mOverUnitList)
    {
      convertValue = 1000 * 1/origValueList[i]
    }
    else if(origUnitName[i] %in% hUnitList)
    {
      convertValue = 100 * origValueList[i]
    }
    else if(origUnitName[i] %in% hOverUnitList)
    {
      convertValue = 100 * 1/origValueList[i]
    }
    else if(origUnitName[i] %in% tOverUnitList)
    {
      convertValue = 10000 * 1/origValueList[i]
    }
    else if(origUnitName[i] %in% sameUnitList)
    {
      convertValue = origValueList[i]
    }
    else if(origUnitName[i] %in% over10UnitList)
    {
      convertValue = 0.1 * origValueList[i]
    }
    else 
    {
      if(origUnitStr[i] %in% sameUnitList)
      {
        convertValue = origValueList[i]
      }
      else if(origUnitStr[i] %in% mOverUnitList)
      {
        convertValue = 1000 * 1/origValueList[i]
      }
    }
    temp = c(temp,c(convertValue))
  }
  
  return(temp)
}

###convert OrigValue
insert_after_which = which(names(SLA) == "OrigValue")
newf <- data.frame(SLA[1:insert_after_which],ConvertedValue = rep(0,nrow(SLA)),SLA[(insert_after_which+1):ncol(SLA)])
newf$ConvertedValue <- valueConvert(SLA$OrigValue,SLA$Unit_UnitName,SLA$OrigUnitStr)
#Use only ConvertedValue where StdValue==NA
index = !is.na(newf$StdValue)
newf$ConvertedValue[index] = newf$StdValue[index]
#Comment out ConvertedStdValue.  StdValue is already in standard units, so should not be modified - NK
# There is one outlier value of SLA (mm2/mg) = 237.0777
#newf <- data.frame(newf[1:insert_after_which_2],ConvertedStdValue = rep(0,nrow(newf)),newf[(insert_after_which_2+1):ncol(newf)])
#newf$ConvertedStdValue <- valueConvert(newf$StdValue,newf$Unit_UnitName,newf$OrigUnitStr)

##write file for records backup. (Note: StdValue might be empty, but OrigValueStr is not empty)

# Summary of data
# current newf DataName:
# Specific leaf area (SLA) 
# 58690 
# SLA of leaf lamina 
# 6200 
# Specific leaf area (SLA) of shaded leaves 
# 649 
# Leaf mass per area (LMA) of total leaf area 
# 153
# 
# current newf TraitName:
# Leaf specific area (SLA)          Specific leaf area (SLA) of leaf lamina 
# 59492                              6200 

# iii) Extract geography (Latitude, Longitude, Altitude).  Correct known errors.
### 560.txt is the latest TRY data with geo information 
#newgeo = read.csv("/Users/Xing/Downloads/560.txt",header = TRUE,sep = '\t',stringsAsFactors = FALSE)
newgeo = read.csv("/Users/nkiang/NancyResearch/GISS/Models/Ent/Datasets/TRY/TRY_XingSu/20160211_XingTryscrub_sources/560.txt",header = TRUE,sep = '\t',stringsAsFactors = FALSE)
LatitudeData <- newgeo[which(newgeo$DataName == "Latitude"),] #DataID = 59
LongitudeData <- newgeo[which(newgeo$DataName == "Longitude"),] #DataID = 60
AltitudeData <- newgeo[which(newgeo$DataName == "Altitude"),] #DataID = 61


# ---------------------- fill the latitude/longitude/altitude by matching ObservationID ----------#
# 

newf_latitude = rep(NA,nrow(newf))   #Gets StdValue
newf_longitude = rep(NA,nrow(newf))
newf_altitude = rep(NA,nrow(newf))
newf_latitudeo = rep(NA,nrow(newf))  #"o" for OrigValueStr
newf_longitudeo = rep(NA,nrow(newf))
newf_altitudeo = rep(NA,nrow(newf))

# iiia) --- latitude ---
for (i in 1:nrow(newf))
{
  tempo = LatitudeData[which(LatitudeData$ObservationID == newf$ObservationID[i] ),]$OrigValueStr #yield:24767
  temp = LatitudeData[which(LatitudeData$ObservationID == newf$ObservationID[i] ),]$StdValue #yield:23802
  if(length(temp) == 1)
  {
    newf_latitudeo[i] = tempo
    newf_latitude[i] = temp
  }
  else if(length(temp) > 1)
  {
    print(temp)
    print("*****") 
  }
}
#Check OrigValueStr vs. StdValue
cbind(newf_latitudeo,newf_latitude)[!is.na(newf_latitudeo),] # & is.na(newf_latitude),] 
#Results: latitude 965 rows have OrigsValueStr="0" while StdValue=NA
#unique(newf_latitudeo)
#Conclude:  Use StdValue
index = NULL
newf_check = newf_latitudeo
for (i in 1:length(newf_check)) {
  if (is.na(as.numeric(newf_check[i])) & !is.na(newf_check[i])) {
	print(paste(i, newf_check[i]))
	index = c(index,i)
	}
}
#Result, new_latitude: rows 3666-36704 have OrigValueStr="34.5669, 150.6561" while StdValue=-34.5669.

# iiib) --- longitude ---
for (i in 1:nrow(newf))
{
  tempo = LongitudeData[which(LongitudeData$ObservationID == newf$ObservationID[i] ),]$OrigValueStr
  temp = LongitudeData[which(LongitudeData$ObservationID == newf$ObservationID[i] ),]$StdValue
  if(length(temp) == 1)
  {
    newf_longitudeo[i] = tempo
    newf_longitude[i] = temp
  } 
  else if(length(temp) > 1)
  {
    print(temp)
    print("*****") 
  }
}
#Check OrigValueStr vs. StdValue
cbind(newf_longitudeo,newf_longitude)[!is.na(newf_longitudeo) & is.na(newf_longitude),] 
#Results:
# newf_longitudeo: rows (31022 31023 31091 31092 31121 31122) have
# OrigValueStr="Dale M. P.(1992): The ecophysiology of  Veronica chamaedrys V. montana and V. officinalis. I. L ..."  
# 959 rows have OrigValueStr="0" while StdValue=NA
# Several OrigValueStr with problematic character encoding, e.g."44\xb0 49'" 
# A lot of OrigValueStr not in decimal degrees format.
# Conclude:  Use StdValue

# iiic) --- altitude ---

## unique(AltitudeData[,"OrigUnitStr"]):  The unit is m. 
## Some of the altitude data is not integer, but a range, (e.g. 50-100, 60m);  therefore, need to translate.
## Values that are in character:   
##   unique(AltitudeData$OrigValueStr[is.na(as.numeric(AltitudeData$OrigValueStr))])
## [1] "100-160"    "good sites" "50 - 100"   "0-50"       "950m"       "60 m"       "500-700"    "400-415"  
##      "320-350"    "100 - 500"  "500 - 1200"
## [12] "1200-1250"  "1250-1300"  "1150-1300" 

### 
for (i in 1:nrow(newf))
{
  index = which(AltitudeData$ObservationID == newf$ObservationID[i] )
  tempo = AltitudeData[index,]$OrigValueStr
  temp = AltitudeData[index,]$StdValue
  if(length(temp) == 1)
  {
    newf_altitudeo[i] = tempo
    newf_altitude[i] = temp
  } 
  else if(length(temp) > 1)
  {
    print(temp, temp2)
    print('**********')
  }
}
#Check OrigValueStr vs. StdValue - Nancy
if (FALSE) {
cbind(newf_altitudeo,newf_altitude)[!is.na(newf_altitudeo) & is.na(newf_altitude),] 
#Result:  40 rows with OrigValueStr "good sites" are assigned StdValue NA

##translate character altitude OrigValueStr - not needed, since StdValue already does this.
##altitude.char = unique(AltitudeData$OrigValueStr[is.na(as.numeric(AltitudeData$OrigValueStr))])
altitude.char.translate = as.data.frame(cbind(altitude.char,  #Average if range
	 altitude.num=c( 130, #"100-160"   
	    NA,  #"good sites" 
	    75,  #"50 - 100"
	    25,  #"0-50"
	    950, #"950m"
	    60,  #"60 m"
	    600, #"500-700"
	    407.5, #"400-415"  
       335,   # "320-350"
        300,   #"100 - 500" 
        1350,  #"500 - 1200"
		1225,  #"1200-1250"
		1275,  #"1250-1300"
		1225   #"1150-1300" 
		)))
altitude.char.translate[,"altitude.num"] = as.numeric(as.character(altitude.char.translate[,"altitude.num"] ))
	
#temp = as.numeric(newf_altitudeo)  #Convert to numeric;  altitude.char values will be NA
indexor = NULL
for (i in 1:nrow(altitude.char.translate)) {
	index = which(newf_altitudeo==altitude.char.translate[i,1])
	#temp[index] = altitude.char.translate[i,2]
	indexor = c(indexor, index)
}
##Check:  
cbind(newf_altitudeo, newf_altitude)[indexor,]
table(newf_altitudeo[indexor])
table(newf_altitude[indexor])
#Result:  
#OrigValueStr            0-50    100-160   50 - 100 good sites 
#StdValue				25			130		75			NA
#Number of rows	        30         30         37         40 
#Conclude:  Nancy's translation is same as TRY's.  Keep StdValue.
} #end check altitude


newf$Latitude = newf_latitude #23802 points
newf$Longitude = newf_longitude #23780 points
newf$Altitude.m = newf_altitude  #10246 points 

#---- Xing's python elevation data that were averaged to 0.5 degrees - WRONG -----------------------------
if (FALSE) {
height_data = read.csv('/Users/Xing/Dropbox/NASAData/output/elevationIndex_Value',sep = ',',header = F)
names(height_data) = c('orig.row','Latitude','Longitude','ObservationID','Altitude')


newf[which(!is.na(newf$Latitude) & !is.na(newf$Longitude)),c("ObservationID")] == height_data$ObservationID
}



#===========================================================================================================
# --------------------- Xing's Deal with the problem caused by different encoding format in the txt---------# 
if (FALSE) {  #StdValue translates these 
bad_encode_la = c("23<b0> 21'","23<b0> 20'" ,"3<b0>1'E" , "3\xb01'E","49 30 N" , "62 47 N","50 17 N", "48 42 N","51 08 N","52 28 N","34.5669, 150.6561","64 07 N","55 31 N","51 10 N")
bad_encode_lo = c("30 58 E", "44\xb0 49'","44\xb0 51'","45\xb043'N","19 27 E","2 09 E","18 32 E","13 18 E","13 38 E","11 32 E","00 50 W","5 55 E","18 32 E","3 12 W","4 24 E")
# name:Louault
geo_error_sla = newf[which(newf$Latitude %in% bad_encode_la | newf$Longitude %in% bad_encode_lo),]
geo_error_sla[which(geo_error_sla$Latitude == "34.5669, 150.6561"),c("Latitude")] = "34.5669"
#"45°43'00.0 N+3°01'00.0 E" -> 45.7166704,3.0166667
geo_error_sla[which(geo_error_sla$LastName == 'Louault'),c('Latitude')] = "45.716667"
geo_error_sla[which(geo_error_sla$LastName == 'Louault'),c('Longitude')] = "3.016667"
# 48 42 N 2 09 E -> la = 48.7, lo = 2.15
geo_error_sla[which(geo_error_sla$Latitude == '48 42 N'),c('Latitude')] = "48.7"
geo_error_sla[which(geo_error_sla$Longitude == '2 09 E'),c('Longitude')] = "2.15"
#64 07 N 19 27 E->la = 64.116667,lo = 19.45
geo_error_sla[which(geo_error_sla$Latitude == '64 07 N'),c('Latitude')] = "64.116667"
geo_error_sla[which(geo_error_sla$Longitude == '19 27 E'),c('Longitude')] = "19.45"
#42 22 N  11 32 E -> 42.366667, 1.533333
geo_error_sla[which(geo_error_sla$Latitude == '42 22 N'),c('Latitude')] = "42.366667"
geo_error_sla[which(geo_error_sla$Longitude == '11 32 E'),c('Longitude')] = "11.533333"
#41 52 N 13 38 E -> 41.866667,13.63333
geo_error_sla[which(geo_error_sla$Latitude == '41 52 N'),c('Latitude')] = "41.866667"
geo_error_sla[which(geo_error_sla$Longitude == '13 38 E'),c('Longitude')] = "13.63333"
#52 28 N 13 18 E -> 52.466667, 13.3
geo_error_sla[which(geo_error_sla$Latitude == '52 28 N'),c('Latitude')] = "52.4666667" #Xing had repeat 41.86667
geo_error_sla[which(geo_error_sla$Longitude == '13 18 E'),c('Longitude')] = "13.3" #Xing had repeat 13.63333
#50 17 N 5 55 E -> 50.283333,5.9166667
geo_error_sla[which(geo_error_sla$Latitude == '50 17 N'),c('Latitude')] = "50.283333"
geo_error_sla[which(geo_error_sla$Longitude == '5 55 E'),c('Longitude')] = "5.9166667"

#51 08 N 00 50 W ->51.13333,-0.83333
geo_error_sla[which(geo_error_sla$Latitude == '51 08 N'),c('Latitude')] = "51.13333"
geo_error_sla[which(geo_error_sla$Longitude == '00 50 W'),c('Longitude')] = "-0.83333"

#62 47 N 30 58 E ->62.78333,30.96667
geo_error_sla[which(geo_error_sla$Latitude == '62 47 N'),c('Latitude')] = "62.78333"
geo_error_sla[which(geo_error_sla$Longitude == '30 58 E'),c('Longitude')] = "30.96667"

#49 30 N 18 32 E -> 49.5,18.53333
geo_error_sla[which(geo_error_sla$Latitude == '49 30 N'),c('Latitude')] = "49.5"
geo_error_sla[which(geo_error_sla$Longitude == '18 32 E'),c('Longitude')] = "18.53333"

#23<b0> 20' 44<b0> 49' ->23.3333,44.81667
geo_error_sla[which(geo_error_sla$Altitude.cm == '50 - 100'),c('Latitude')] = "23.3333"
geo_error_sla[which(geo_error_sla$Altitude.cm == '50 - 100'),c('Longitude')] = "44.81667"

#23<b0> 21' 44<b0> 51'-> 23.35,44.85
geo_error_sla[which(geo_error_sla$Altitude.cm == '0-50'),c('Latitude')] = "23.35"
geo_error_sla[which(geo_error_sla$Altitude.cm == '0-50'),c('Longitude')] = "44.85"



#### write back to the original data
j = 1
for (i in row.names(geo_error_sla))
{
  which = which(row.names(newf) == i)
  
  if(newf[which,c("ObservationID")] != geo_error_sla[j,c("ObservationID")])
  {
    print(j)
  }
  else
  {
    newf[which,] = geo_error_sla[j,]
  }
  j = j+1
}

#### check whether all the weird characters are correct.

# ### find new errors for Longitude
which = which(is.na(newf$Longitude) & !is.na(copy_newf$Longitude))

#          Latitude Longitude
# 2892231  55 31 N    3 12 W  --- 55.51667, -3.2
# 2892261  51 10 N    4 24 E  --- 51.16667, 4.4

#correction:
newf[which(newf$Latitude == '55 31 N'),]$Latitude = 55.516667
newf[which(newf$Latitude == '51 10 N'),]$Latitude = 51.166667
newf[which(newf$Longitude == '3 12 W'),]$Longitude = -3.2
newf[which(newf$Longitude == '4 24 E'),]$Longitude = 4.4

#Kleyer data (0,0) to (NA,NA)
which = (!is.na(newf$Latitude) & newf$Latitude == 0 & !is.na(newf$Longitude) &newf$Longitude == 0)
newf[which,c('Latitude')] = NA
newf[which,c('Longitude')] = NA

#### --------------- Errors in Altitude -----------------####
#           Latitude Longitude Altitude.cm
# 614316     43.85      3.93     100-160
# 1191795    32.58     -88.8  good sites
# 3030087  23.3333  44.81667    50 - 100
# 3030845    23.35     44.85        0-50

# ------------- Some of the geo records are in UTM format, but the regular format is saved in 'StdValue' ---- # 
#e.g. ObservationID == 206909

newgeo_la = newgeo[which(newgeo$DataID == 59),c('ObservationID',"OrigValueStr","StdValue")] ##for fast access
newgeo_lo = newgeo[which(newgeo$DataID == 60),c('ObservationID',"OrigValueStr","StdValue")]

UTM_format_index = which(newf$Latitude > 90)
for (i in UTM_format_index)
{
  la = newgeo_la[which(newgeo_la$ObservationID == newf[i,c("ObservationID")]),c("StdValue")]
  lo = newgeo_lo[which(newgeo_lo$ObservationID == newf[i,c("ObservationID")]),c("StdValue")]
  newf[i,c("Latitude")] = la
  newf[i,c("Longitude")] = lo
#   print(la)
#   print(lo)
}

# ------------------------- Corrections to the GEO data ------------------------- #
#convert to numeric type for plotting
newf$Latitude =as.numeric(newf$Latitude)
newf$Longitude =as.numeric(newf$Longitude)

}# End Xing if(FALSE) corrections of OrigValueStr
#==========================================================================================


# --------------------- The Function to Plot Points (lo,la) on world map ------------------- # 
mapPoints <- function(Longitude,Latitude,pchVal=16, cexVal = 0.4)
{
  ####maplibrary
  library(fields) 
  library(spam) 
  library(maps) 
  library(maptools) 
  library(rworldmap) 
  library(SDMTools) 
  library(plotrix)
  library(rworldmap)
  
  data(coastsCoarse)
  
  plot.grid2 = function(mapz, res="1x1",colors=terrain.colors(40), legend.lab=NULL, xlab="longitude", ylab="latitude", zlim=NULL, ADD=FALSE, if.fill=TRUE) {
    #Grid centers (x,y)
    if (res=="1x1") {
      i = (-180:179) + 0.5
      j = (-90:89) + 0.5
    } else if (res=="2x2.5") {
      i = (-72:71)*2.5 + 1.25
      j = (-45:44)*2 + 1
    }
    mapzlim = mapz
    if (if.fill & !is.null(zlim)) {  #Color extreme values with the zlim colors
      mapzlim[mapzlim<zlim[1]] = zlim[1]
      mapzlim[mapzlim>zlim[2]] = zlim[2]
    }
    image.plot(x=i,y=j,t(mapzlim), xlab=xlab, ylab=ylab, zlim=zlim, col=colors, add=ADD)
  }
  plot(coastsCoarse)
  
  points(Longitude,Latitude, col="red", pch=pchVal, cex = cexVal)
}



mapPoints(newf$Longitude,newf$Latitude)

#----------------------- Corrections of the points at wrong locations --------------------------------#

#1. Some points on the ocean near japan, should be in Australia (flip the Latitude)
japan_error = newf[which(abs(newf$Latitude - 30) < 5 & abs(newf$Longitude - 145) < 20),]
#mapPoints(japan_error$Longitude,japan_error$Latitude)

japan_error$Latitude = -japan_error$Latitude
#mapPoints(japan_error$Longitude,japan_error$Latitude)
newf[row.names(japan_error),] = japan_error
#mapPoints(newf$Longitude,newf$Latitude)


# 2. 'longitude 6.497358 and latitude 74.80795'  to 'E8.02°, N 47.85°'.

index = which(abs(newf$Longitude - 6.497358)<0.1 & abs(newf$Latitude - 74.80) < 0.1)
newf[index,c('Latitude')] = 48.41667
newf[index,c('Longitude')] = 8.03333

#3. Somali: (long = 49.72586, la = 4.910540) to (lon = 11.96436,la=49.17044)
newf[which(abs(newf$Longitude - 49.7) < 1),c('Latitude')] = 49.17044
newf[which(abs(newf$Longitude - 49.7) < 1),c('Longitude')] =11.96436

## 4. Bengal Bay: (84,10.43) to (-84,10.43) 
newf[which(abs(newf$Longitude -84) < 0.1),]$Longitude = -84
#mapPoints(newf$Longitude,newf$Latitude)

## 5. LastName: Shipley
# two points: -45.4, -71.8, and -37.5, 75.5
# 5.1:update -45.4,-71.8 to 45.4,71.8
indian_ocean_err = newf[which(abs(newf$Latitude + 38) < 10 & abs(newf$Longitude - 75) < 10),]
#mapPoints(indian_ocean_err$Longitude,indian_ocean_err$Latitude)

indian_ocean_err$Latitude = -indian_ocean_err$Latitude
indian_ocean_err$Longitude = -indian_ocean_err$Longitude
#mapPoints(indian_ocean_err$Longitude,indian_ocean_err$Latitude)

newf[row.names(indian_ocean_err),] = indian_ocean_err
#mapPoints(newf$Longitude,newf$Latitude)


#5.2: ???? ??? -37.5,75.5 to 37.5,75.5  (suspicious Argentina data)

#----- Nancy's elevation matching to 15 arcsec data -------------------------------------------------------

#index = !is.na(newf$Latitude) & !is.na(newf$Longitude) & is.na(newf$Altitude.m) #14154 points
#write.table(newf[index, c( "ObservationID", "Latitude", "Longitude" )],
#file="try_data_clean_NY_intermediate_noelev.csv", sep=',', row.names=FALSE)

nc = open.nc(con="/Users/nkiang/NancyResearch/GISS/Models/Ent/Datasets/SRTM15/topo15.grd" )
varidz = var.inq.nc(nc, "z")
#index = !is.na(newf$Latitude) & !is.na(newf$Longitude) & is.na(newf$Altitude.m) #14154 points
index = !is.na(newf$Latitude) & !is.na(newf$Longitude) #23780 points
srtmz = NULL
ngeo = length(index)
for (n in 1:ngeo) {
	if (index[n]) {
		trylon = newf[n, "Longitude"]
		trylat = newf[n, "Latitude"]
		i = floor((trylon + 180)*60*60/15 + 1)
		j = floor((trylat + 90)*60*60/15 + 1)	
		lon = var.get.nc(nc, "lon", start=i,count=2)
		lat = var.get.nc(nc, "lat", start=j,count=2)
		zval = var.get.nc(nc, "z", start=c(i,j),count=c(2,2)) #matrix:  rows i, columns j
	
		X = matrix(cbind(c(1,1,1,1), 
					c(lon[1],lon[1],lon[2],lon[2]),
					c(lat[1],lat[2],lat[1],lat[2]), 
					c(lon[1]*lat[1],lon[1]*lat[2],lon[2]*lat[1],lon[2]*lat[2])), 
					4,4)
		b = t(solve(X))%*%c(1,trylon,trylat,trylon*trylat)	
		zavg = t(b) %*% as.vector(zval)
	} else {
		zavg = newf$Altitude.m[n]
	}
	
	srtmz = c(srtmz, zavg)
}

if (FALSE) { #Nancy's checks of elevation data - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #Check results for srtmz
junk = cbind(newf$Longitude, newf$Latitude, newf$Altitude, srtmz)
junk[!is.na(srtmz) & srtmz<0,]
junk[!is.na(newf$Altitude) & !is.na(srtmz),]
quartz(width=9,height=6)
plot(newf$Longitude, newf$Latitude, pch=16, col=2, cex=0.6)
plot(coastsCoarse, add=TRUE)
index = is.na(newf$Altitude.m) & !is.na(srtmz) #SRTM15-only points
points(newf$Longitude[index], newf$Latitude[index], pch=16, col=3, cex=0.6)
index = !is.na(srtmz) & srtmz<0 #SRTM15<0 points
points(newf$Longitude[index], newf$Latitude[index], pch=16, col=4, cex=0.6)
index = !is.na(srtmz) & srtmz<(-1000) #Lowest point
points(newf$Longitude[index], newf$Latitude[index], pch=16, col=5, cex=0.6)
legend(-150,-10, legend=c("All lon/lat","SRTM fill Alt NA", "SRTM<0", "SRTM< -1000 m"), col=c(2:5), pch=16, cex=0.8, bty="n")
text()

quartz(width=6,height=8) #Plot regions -----
par(mfrow=c(2,2))
#index=!is.na(Altitude.m)
#plot(newf$Altitude.m[index], srtmz[index], pch=16, cex=0.4) #TRY vs. SRTM -----
plot(newf$Altitude.m, srtmz, pch=16, cex=0.4) #TRY vs. SRTM -----
index = is.na(newf$Altitude) & !is.na(srtmz)
junknoalt = sum(index)
points(srtmz[index], srtmz[index], col=2, pch=16, cex=0.5)
negelev = -5
index = srtmz< negelev & !is.na(srtmz)
junknegz = sum(index)
points(srtmz[index], srtmz[index], col=3, pch=16, cex=0.5)
legend(1500,-500, legend=c("Alt vs. srtmz",paste("SRTM fill Alt NA (",junknoalt,")"), 
					paste("SRTM<(", negelev,") (",junknegz,")")), col=1:3, pch=16, cex=0.8, bty="n")



index = srtmz<negelev & !is.na(srtmz)
plot(newf$Longitude, newf$Latitude, pch=16, col=2, cex=0.6, xlim=c(-20,50), ylim=c(25,75)) #N. Europe
points(newf$Longitude[index], newf$Latitude[index], col=3, pch=16, cex=0.5)
title("N. Europe")
plot(coastsCoarse, add=TRUE)
plot(newf$Longitude, newf$Latitude, pch=16, col=2, cex=0.6, xlim=c(-90,-70), ylim=c(20,35)) #Florida
points(newf$Longitude[index], newf$Latitude[index], col=3, pch=16, cex=0.5)
title("Florida")
plot(coastsCoarse, add=TRUE)
plot(newf$Longitude, newf$Latitude, pch=16, col=2, cex=0.6, xlim=c(145,160), ylim=c(-40,-20)) #Australia
points(newf$Longitude[index], newf$Latitude[index], col=3, pch=16, cex=0.5)
title("Australia")
plot(coastsCoarse, add=TRUE)
mtext(outer=TRUE, "TRY Lon/Lat and SRTM15 elevation check 3/16/2016", line=-1.5)

junk = cbind(newf[,c("LastName","FileName","ObservationID", "ObsDataID")], Orig_lon=newf_longitudeo, Orig_lat=newf_latitudeo, newf[, c("Longitude", "Latitude","Altitude.m")], srtmz)[!is.na(srtmz) & srtmz<negelev,]  #717 points negative srtmz
write.table(junk, file="TRY_SRTM15_elevltneg5.csv", sep=',', row.names=FALSE)
junk = cbind(Orig_lon=newf_longitudeo, Orig_lat=newf_latitudeo, newf[,c("LastName", "Longitude", "Latitude","Altitude.m")], srtmz)[!is.na(srtmz) & srtmz<0,]  #717 points negative srtmz

#Conclude:
#Points -5 to 0 m are all > ~-3.5 m, on land in The Netherlands, and one in Florida.
#(62.97032 N, 8.465521 E) in Norway on a tiny peninsula
#(62.97032 N, 8.465521 E) in Norway on land in the middle of a lot of fjords, elev < -10 m, as low as <-30 m.
#(56.28000 N, 16.330000  E) off Norway in a strait.
#All others over ocean (North Sea, Eastern Australia)
#(57.69306 N, 6.765338 E) off Norway over North Sea
#(-34.77780 S, 151.336400 E) off E. Australia, Wright StdValue Altitude.m is 315 m.
#And more...
#Final:  Exclude all points < -5 m elevation.
} #end if(FALSE) Nancy's elevation checks - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

#Assign Altitude.m.translation, merging TRY Altitude.m and SRTM15.
Altitude.m.translation = newf$Altitude.m
index = is.na(newf$Altitude.m)
Altitude.m.translation[index] = srtmz[index]
Altitude.m.translation[Altitude.m.translation< (-5)] = NA
newf = cbind(newf, Altitude.SRTM15.m=srtmz, Altitude.m.translation)


if (FALSE) { #Xing's 
######################################## Altitude Update #########################
#elevation = read.csv('/Users/Xing/Dropbox/NASAData/output/elevation_half_r',header = FALSE)
elevation = read.csv('/Volumes/EVERGREEN/SRTM30_plus/elevation_half_r',header = FALSE)

#convert the matrix to match the map region
## Elevation data encoding:
##    [0:720]
##  90|0 -----------180 | -180 ----------- -1|
##    |  Euro           |   North American   |
##    |        Asia     |        *           |
##  0 | -----------------------------------  |
##    | Africa          |                    |
##    |                 |      Australia     |
## -90| ------------------------------------ |

## Given: la = 37.4220009, lo = -122.0846221 (California, the above star place)
## translate: lo_index: if lo > 0, then lo_index = int(lo * 2) + (((lo*2) - int(lo * 2)) >=0.5)
##                      if lo < 0, then lo_index = 360 + int(180 - abs(lo)) + ((180 - abs(lo) - int(180 - abs(lo)))>=0.5)
## translate: la_index: if la > 0, then la_index = int((90 - la)*2) + (((90 - la)*2 - int((90 - la)*2)) > 0.5)
##                      if la < 0, then la_index = 180 + int(abs(la)*2) + ((abs(la)*2-int(abs(la)*2))>0.5)


#### --------------- Errors in Altitude -----------------####
#           Latitude Longitude Altitude.cm
# 614316     43.85      3.93     100-160
# 1191795    32.58     -88.8  good sites
# 3030087  23.3333  44.81667    50 - 100
# 3030845    23.35     44.85        0-50


err_alt = c('100-160','good sites','50 - 100','0-50')
which = newf$Altitude.m %in% err_alt
newf[which,c("Altitude.m")] = NA
newf$Altitude.m = as.numeric(as.character(newf$Altitude.m))



## Elevation data encoding:
##    [0:720]
##  90|0 -----------180 | -180 ----------- -1|
##    |  Euro           |   North American   |
##    |        Asia     |        *           |
##  0 | -----------------------------------  |
##    | Africa          |                    |
##    |                 |      Australia     |
## -90| ------------------------------------ |

ele_index_translation <- function(la,lo)
{
  if (lo > 0)
  {
    lo_index = as.integer(lo * 2) + (((lo*2) - as.integer(lo * 2)) >=0.5)
  }
  else
  {
    dist = 180 - abs(lo)
    lo_index = 360 + as.integer(dist*2) + ((dist*2 - as.integer(dist*2))>=0.5)
  }
  if(la > 0)
  {
    la_index = as.integer((90 - la)*2) + (((90 - la)*2 - as.integer((90 - la)*2)) > 0.5)
  }
  else
  {
    la_index = 180 + as.integer(abs(la)*2) + ((abs(la)*2-as.integer(abs(la)*2))>0.5)
  }
  c(la_index,lo_index)
}


lat_list = newf$Latitude
lon_list = newf$Longitude
alt_list = newf$Altitude.m

for (i in 1:nrow(newf))
{
  if (is.na(alt_list[i]) && !is.na(lat_list[i]) && !is.na(lon_list[i]))
  {
    index = ele_index_translation(lat_list[i],lon_list[i])
    la_index = index[1]
    lo_index = index[2]
    alt_list[i] = elevation[la_index,lo_index]
  }
}

newf$Altitude.m = alt_list
### how to deal altitude < 0??


### -------------------------------Finish GEO correction###########################################
} #Xing's if(FALSE)
 
# iv) Extract PFT categories (Photosynthesis.pathway,Plant.growth.form, Leaf.type, Phenology.vegetative). 
# -- Add Genus column
CategoryTraits <- read.csv(paste(datasourcesdir,"CategoryTraits.csv", sep=""),header=TRUE,stringsAsFactors = F)
insertIndex = which(names(newf) == "AccSpeciesName")
newf <- data.frame(newf[1:insertIndex],Genus = rep(NA,nrow(newf)),newf[(insertIndex+1):ncol(newf)])
#CategoryTraits <- data.frame(lapply(CategoryTraits, as.character), stringsAsFactors=FALSE)

### There're cases which the match is more than one column

for (i in 1:nrow(newf))
{
  temp = CategoryTraits[which(CategoryTraits$TRY_AccSpeciesName == newf$AccSpeciesName[i]),]
  
  if (nrow(temp) == 1)
  {
    newf$Genus[i] = temp$Genus
  }
  else if (nrow(temp) == 2)
  {
    newf$Genus[i] = temp[1,]$Genus
  }
}



# - Get Data ready:
#pathway
Photosynthesis.pathway <- orig_data[which(orig_data$TraitID == 22),]
Photosynthesis.pathway <- data.frame(lapply(Photosynthesis.pathway, as.character), stringsAsFactors=FALSE)
#growthform
Plant.growth.form <- orig_data[which(orig_data$TraitID == 42),]
Plant.growth.form <- Plant.growth.form[which(Plant.growth.form$DataName == "Plant growth form"),]
Plant.growth.form <- data.frame(lapply(Plant.growth.form, as.character), stringsAsFactors=FALSE)
#phenology
Phenology.vegetative <- orig_data[which(orig_data$TraitID == 37),]
Phenology.vegetative <- Phenology.vegetative[which(Phenology.vegetative$DataName == "Leaf phenology type"),]
Phenology.vegetative <- data.frame(lapply(Phenology.vegetative, as.character), stringsAsFactors=FALSE)
#leaftype
Leaf.type <- orig_data[which(orig_data$TraitID == 43),]
Leaf.type <- Leaf.type[which(Leaf.type$DataName == "Leaf type"),]
Leaf.type <- data.frame(lapply(Leaf.type, as.character), stringsAsFactors=FALSE)

# - Translate to Ent categories
# 1. Add the columns
newf$Photosynthesis.pathway <- 'NA'
newf$Plant.growth.form <- 'NA'
newf$Phenology.vegetative <- 'NA'
newf$Leaf.type <- 'NA'

insertIndex = which(names(newf) == "Plant.growth.form")
newf <- data.frame(newf[1:insertIndex],Plant.growth.form.translation = rep(0,nrow(newf)),newf[(insertIndex+1):(ncol(newf))])

insertIndex = which(names(newf) == "Photosynthesis.pathway")
newf <- data.frame(newf[1:insertIndex],Photosynthesis.pathway.translation = rep(0,nrow(newf)),newf[(insertIndex+1):(ncol(newf))])

insertIndex = which(names(newf) == "Phenology.vegetative")
newf <- data.frame(newf[1:insertIndex],Phenology.vegetative.translation = rep(0,nrow(newf)),newf[(insertIndex+1):(ncol(newf))])

#last column:
newf$Leaf.type.translation = NA

# iv-a) ------------------------------- photosynthesis pathway ---------------------------------------------#
# Fill the data:
# for photosynthesis pathway:
noMatchPhotoSynthesis = c() ## for inconsistency records
moreMatchPhotoSynthesis = c()


for (i in 1:nrow(newf))
{
  temp = Photosynthesis.pathway[which(Photosynthesis.pathway$AccSpeciesName == newf$AccSpeciesName[i]),]
  temp = temp[which(temp$ObservationID == newf$ObservationID[i]),]
  
  if (nrow(temp) == 1)
  {
    newf$Photosynthesis.pathway[i] = temp$OrigValueStr
  }
  else if (nrow(temp) == 0)
  {
    #print(toString(newf$AccSpeciesName[i]))
    noMatchPhotoSynthesis <- c(noMatchPhotoSynthesis,toString(newf$AccSpeciesName[i]))
  }
  else if(nrow(temp) > 1)
  {
    
    #print("More than one result found:")
    moreMatchPhotoSynthesis = c(moreMatchPhotoSynthesis, toString(newf$AccSpeciesName[i]))
  }
}
# iv-b) ------------------------------- Plant.Growth.Form ---------------------------------------------#
#Plant.growth.form
noMatchGrowthForm = c()
moreMatchGrowthForm = c()
for (i in 1:nrow(newf))
{
  temp = Plant.growth.form[which(Plant.growth.form$AccSpeciesName == newf$AccSpeciesName[i]),]
  temp = temp[which(temp$ObservationID == newf$ObservationID[i]),]
  
  if (nrow(temp) == 1)
  {
    newf$Plant.growth.form[i] = temp$OrigValueStr
  }
  else if (nrow(temp) == 0)
  {
    #print(toString(newf$AccSpeciesName[i]))
    noMatchGrowthForm <- c(noMatchGrowthForm,toString(newf$AccSpeciesName[i]))
  }
  else if(nrow(temp) > 1)
  {
    
    #print("More than one result found:")
    moreMatchGrowthForm = c(moreMatchGrowthForm, toString(newf$AccSpeciesName[i]))
  }
}
# iv-c) --------------------------------- Phenology ---------------------------------------------#
noMatchPhenology = c()
moreMatchPhenology = c()

for (i in 1:nrow(newf))
{
  temp = Phenology.vegetative[which(Phenology.vegetative$AccSpeciesName == newf$AccSpeciesName[i]),]
  temp = temp[which(temp$ObservationID == newf$ObservationID[i]),]
  
  if (nrow(temp) == 1)
  {
    newf$Phenology.vegetative[i] = temp$OrigValueStr
  }
  else if (nrow(temp) == 0)
  {
    #print(toString(newf$AccSpeciesName[i]))
    noMatchPhenology <- c(noMatchPhenology,toString(newf$AccSpeciesName[i]))
  }
  else if(nrow(temp) > 1)
  {
    
    #print("More than one result found:")
    moreMatchPhenology = c(moreMatchPhenology, toString(newf$AccSpeciesName[i]))
  }
}
# iv-d) ------------------------------- Leaf Type ---------------------------------------------#
##Leaf.type
noMatchLeafType = c()
moreMatchLeafType = c()

for (i in 1:nrow(newf))
{
  temp = Leaf.type[which(Leaf.type$AccSpeciesName == newf$AccSpeciesName[i]),]
  temp = temp[which(temp$ObservationID == newf$ObservationID[i]),]
  
  if (nrow(temp) == 1)
  {
    newf$Leaf.type[i] = temp$OrigValueStr
  }
  else if (nrow(temp) == 0)
  {
    #print(toString(newf$AccSpeciesName[i]))
    noMatchLeafType <- c(noMatchLeafType,toString(newf$AccSpeciesName[i]))
  }
  else if(nrow(temp) > 1)
  {
    
    #print("More than one result found:")
    moreMatchLeafType = c(moreMatchLeafType, toString(newf$AccSpeciesName[i]))
  }
}

# Check Table.
checkTable = read.csv(paste(datasourcesdir,"translation.csv",sep=""),stringsAsFactors =F)


########### -------------------------- deal with nectar data --------------------------------###

#SLA_transpose = newf ## copy it to SLA_transpose first, to make sure everything work correctly, and then make the change on real newf
nectar_file =  paste(datasourcesdir,"pheno_NECTAR091614_Eva_033115EI.csv", sep="")
nectar_data = read.table(nectar_file,sep = ',',header = TRUE)

nectar_data$Count = 1
nectar_species = aggregate(Count~Species,nectar_data,"sum")
##
'- Search in the TRY database for the same species name, and see 
if the TRY data have the missing categorical data.'

try_species = as.character(newf$SpeciesName)
## add combination of species+genus
nectar_data$speciesgenus = paste(nectar_data$Genus,nectar_data$Species)
nectar_species = nectar_data$speciesgenus
intersection_species = intersect(try_species,nectar_species)

Nectar_species_overlap_index = match(intersection_species , nectar_species)
nectar_data_for_dictionary = nectar_data[Nectar_species_overlap_index,]

# Note: the aggregate doesn't work because it skip the rows that has NAs
# nectar_dictionary = aggregate(Count~speciesgenus + Form + LeafType	+ Phenology	+ PhotoPath,nectar_data_for_dictionary,'sum',na.action = NULL)
nectar_dictionary = unique(nectar_data_for_dictionary[,c("speciesgenus","Form" ,"LeafType","Phenology","PhotoPath")])
### This lines convert the factor of data.frame column into character type
nectar_dictionary[] <- lapply(nectar_dictionary, as.character)

### For fast process (and safety...), make each column a list, and then put back. 
### Pay attention here that, some of the value are 'NA' as string, some are null, some are NA (is.na(**) = TRUE)
try_growth_form = newf$Plant.growth.form
#sum(try_growth_form== 'NA') = 41741
try_phenology = newf$Phenology.vegetative
#sum(is.na(try_phenology) | !is.na(try_phenology)&try_phenology == 'NA') = 56133
try_leaf_type = newf$Leaf.type
#sum(is.na(try_leaf_type) | (!is.na(try_leaf_type) & try_leaf_type == 'NA') | try_leaf_type == '*') = 60140
try_photosynethesis = newf$Photosynthesis.pathway
#sum(!is.na(try_photosynethesis) & try_photosynethesis == 'NA' | try_photosynethesis == '?') = 60215


for(i in 1:length(try_species))
{
  if (try_species[i] %in% intersection_species)
  {
    which = which(nectar_dictionary$speciesgenus == try_species[i])
    print(which)
    if(try_growth_form[i] == 'NA')
    {
      try_growth_form[i] = nectar_dictionary[which,c("Form")]
    }
    if(is.na(try_phenology[i]) | !is.na(try_phenology[i])&try_phenology[i] == 'NA')
    {
      try_phenology[i] = nectar_dictionary[which,c("Phenology")]
    }
    if(is.na(try_leaf_type[i]) | (!is.na(try_leaf_type[i]) & try_leaf_type[i] == 'NA') | try_leaf_type[i] == '*')
    {
      try_leaf_type[i] = nectar_dictionary[which,c("LeafType")]
    }
    if(is.na(try_photosynethesis[i]) | (!is.na(try_photosynethesis[i]) & try_photosynethesis[i] == 'NA') | try_photosynethesis[i] == '?')
    {
      try_photosynethesis[i] = nectar_dictionary[which,c("PhotoPath")]
    }
  }
}


### put it back:
newf$Plant.growth.form = try_growth_form

newf$Phenology.vegetative = try_phenology

newf$Leaf.type = try_leaf_type

newf$Photosynthesis.pathway = try_photosynethesis

####### -------------- End of nectar data processing--------------------------------------#####


####### -------------- Translate the growth form -----------------------------------------#### 
verbose = c("Shrub / tree","Tree / shrub","Shrub - tree","Tree-shrub", "Tree - shrub","Shrub/tree")
orig_growthform = newf$Plant.growth.form
lastname = newf$LastName
growthform_translation = rep(NA,nrow(newf))

for (i in 1:nrow(newf))
{
  
  if(is.na(orig_growthform[i]))
  {
    key = NA
  }
  else if (orig_growthform[i] %in% checkTable$Short)
  {
    key = checkTable[which(checkTable$Short == orig_growthform[i]
                           & checkTable$LastName == lastname[i]),c("Meaning")]
  }
  
  else if (toupper(orig_growthform[i]) %in% checkTable$Short)
  {
    key = checkTable[which(checkTable$Short == toupper(orig_growthform[i])
                           & checkTable$LastName == lastname[i]),c("Meaning")]
    
  } 
  
  else  if(orig_growthform[i] %in% verbose)
  {
    key = "Tree/Shrub"
  }
  
  else
  {
    
    key = as.character(orig_growthform[i])
  }
  
  growthform_translation[i] = key
  
}
#Get a lot of warnings(), several of below:
#Warning messages:
#1: In growthform_translation[i] = key :
#  number of items to replace is not a multiple of replacement length

growthform_translation = lapply(growthform_translation,tolower)
growthform_translation = unlist(growthform_translation)

Grass = c("vine", "liana", 'climber', 'scrambler', 'sedge', 'grass&sedge','graminoid - geophyte','graminoid',
          'grass (poaceae only)')

Herb = c('forb', 'agaves&cacti  resp.  agaves&cacti', 'leaf succulent', 'bromeliad',
         'bromeliad-like rosette','leguminous forb','forb (herbaceous, with or without woody base)','aquatic forb',
         'fern','fernally','fern  resp.  fern','fern ally','herbaceous monocotyl','herbaceous dicotyl','herbaceous')

Shrub = c('club moss','tree - shrub','tree-shrub','leaf succulent','dwarf shrub','vine/shrub','shrub-subshrub',
          'subshrub','forb - subshrub','shrub -subshrub','shrub (woody 1-4m)',"vine/shrub")

Tree = c('horsetail','palm',"tree/shrub","tree / shrub",'tree (woody >4m)','conifer')

Crop = c('carnivorous plant  resp.  carnivorous plant','crops')

Shrub_tree = c("shrub / tree","shrub - tree","shrub/tree",'hemiparasite', 'parasite', 'stem parasite', 'root parasite', 
               'hemiepiphyte', 'hemiepiphyte', 'epiphyte (mistletoe)')

Woody = c('woody','subshrub (woody <1m)','shrub (woody 1-4m)',
         'forb (herbaceous, with or without woody base)','woody deciduous','woody evergreen')


for(i in 1: length(growthform_translation))
{
  if (growthform_translation[i] %in% Grass)
  {
    growthform_translation[i] = 'grass'
  }
  else if (growthform_translation[i] %in% Herb)
  {
    growthform_translation[i] = 'herb'
  }
  else if (growthform_translation[i] %in% Shrub)
  {
    growthform_translation[i] = 'shrub'
  }
  else if (growthform_translation[i] %in% Tree)
  {
    growthform_translation[i] = 'tree'
  }
  else if (growthform_translation[i] %in% Crop)
  {
    growthform_translation[i] = 'crop'
  }
  else if (growthform_translation[i] %in% Shrub_tree)
  {
    growthform_translation[i] = 'shrub-tree'
  }
  else if (growthform_translation[i] %in% Woody)
  {
    growthform_translation[i] = 'woody'
  }
  else if (growthform_translation[i] %in% c('na','NA'))
  {
    growthform_translation[i] = NA
  }
}
# ------- the latest plant growth form translation so far ------- ###
# [1] "shrub"           "grass"           "tree"            "herb"            "epiphytes"       NA     "woody"                           
# [8] "shrub-tree"      "epiphyte"        "crop"            "rushes"          "twiner/climber." "mistletoe"
# ----------------------------------------------------------------------

newf$Plant.growth.form.translation = growthform_translation

#write.csv(newf,"sla_geo_adding_genus_adding_original_pf_growthform_updated_20160210.csv",row.names = F)

# ---------------- Convert Pathway -------------------------------- ###

### unique(newf$Photosynthesis.pathway) = "C3"  "C4"  "NA"  "c3"  "c4"  "CAM" "Y"   "?"

c3 = c('c3','C3','C3/CAM')
c4 = c('c4','C4','C4/CAM','Y')
CAM = c('CAM')
C3_C4 = c('C3/C4','c3/c4')
other = c('?','NA')


convert_pathway<-function(pathwayCol)
{
  for(i in 1:length(pathwayCol))
  {
    if(pathwayCol[i] %in% c3)
    {
      pathwayCol[i] = 'c3'
    }
    else if(pathwayCol[i] %in% c4)
    {
      pathwayCol[i] = 'c4'
    }
    else if(pathwayCol[i] %in% CAM)
    {
      pathwayCol[i] = 'CAM'
    }
    else if(pathwayCol[i] %in% C3_C4)
    {
      pathwayCol[i] = 'c3/c4'
    }
    else if(pathwayCol[i] %in% other)
    {
      pathwayCol[i] = NA
    }
  }
  pathwayCol
}

test_pathway = newf$Photosynthesis.pathway
pathway_trans = convert_pathway(test_pathway)

## put back: 
newf$Photosynthesis.pathway.translation = pathway_trans

# ------------------------- Convert Leaftype--------- ------------------ #
convert_leaftype <-function(leaftypeCol)
{
  for(i in 1:length(leaftypeCol))
  {
    if (leaftypeCol[i] %in% c('B','broadleaved') )
    {
      leaftypeCol[i] = 'broadleaf'
    }
    else if (leaftypeCol[i] %in% c('N','needle-leaved','needleleaved') ) ## wrong? 
    {
      leaftypeCol[i] = 'needleleaf'
    }
    else if (leaftypeCol[i] %in% c('*') ) ## wrong? 
    {
      leaftypeCol[i] = NA
    }
  }
  leaftypeCol
}

test_leaftype = newf$Leaf.type
leaftype_tran = convert_leaftype(test_leaftype) 
## put it back:
newf$Leaf.type.translation = leaftype_tran

### -------------------------------- convert Phenology -----------------------# 
### The logical field always answers the name of the column. If it is "evergreen" then Y means evergreen. Y/N is 
### semi-deciduous, although I would recommend to classify as winter-deciduous, drought-deciduous and evergreen. Y/N is 
### (drought)-semi-deciduous of course. Just evergreen-deciduous is too simplistic in these days.

evergreen = c('E','evergreen','Evergreen','Y')
deciduous = c('D','deciduous','DC',"N")
semi_deciduous = c('S','SD','SEMI','semi-deciduous','Y/N','deciduous/evergreen')
perennial = c('perennial')
annual_perennial = c("annual/perennial")
annual= c('annual')
                                                                         
convert_phenology <- function(phenologyCol)
{
  ##Herbs and grasses can be annual or perennial. 
  ##Shrubs and trees can be deciduous or evergreen
  
  for(i in 1:length(phenologyCol))
  {
    if (phenologyCol[i] %in% deciduous )
    {
      phenologyCol[i] = 'deciduous'
    }
    else if (phenologyCol[i] %in% evergreen )
    {
      phenologyCol[i] = 'evergreen'
    }
    else if (phenologyCol[i] %in% semi_deciduous)
    {
      phenologyCol[i] = 'semi-deciduous'
    }  
    else if (phenologyCol[i] %in% perennial)
    {
      phenologyCol[i] = "perennial"
    }
    else if(phenologyCol[i] %in% annual)
    {
      phenologyCol[i] = "annual"
    }
    else if(phenologyCol[i] %in% annual_perennial)
    {
      phenologyCol[i] = "annual/perennial"
    }
    else if(phenologyCol[i] %in% c('NA'))
    {
      phenologyCol[i] = NA
    }
  }
  phenologyCol 
}
test_phenology = newf$Phenology.vegetative
phenology_tran = convert_phenology(test_phenology)

### put it back 
newf$Phenology.vegetative.translation = phenology_tran

#### -------------------------------------------- Finish phenology ---------------------- ####

### ---------------------------------------------- correct some errors/empty field --------###

##if plant.growth.form.translation is grass/herb and the leaf.type.translation is not specified:
## assign the leaf.type.translation: herbaceous
which = newf$Plant.growth.form.translation %in% c('grass','herb') 
which_2 = is.na(newf$Leaf.type.translation)

View(newf[which&which_2,])
newf[which&which_2,'Leaf.type.translation'] = 'herbaceous'

#### From Nancy's code in helping checking the translation:
Genus = newf$Genus
index = !is.na(Genus) & (Genus=="Picea" | Genus == "Pinus" | Genus == "Abies" | Genus == "Podocarpus" |
                           Genus=="Sciadopitys" | Genus=="Araucaria" | Genus=="Cupressaceae" | Genus=="Cephalotaxaceae" | Genus=="Taxaceae"
                         | Genus=="Larix")
## sum(index) = 1974

newf[index,'Leaf.type.translation'] = 'needleleaf' ##needled Leaf.

## More notes from Nancy's notes:

index = index & Genus!='Larix'
newf[index, 'Phenology.vegetative.translation'] = 'evergreen'

#Assign needleleaf deciduous

index2 = (!is.na(Genus) &Genus=='Larix')
newf[index2, 'Phenology.vegetative.translation'] = 'deciduous'

x1 = (!is.na(Genus)&Genus== 'Quercus')

quercus_form_nas = x1 & is.na(newf$Plant.growth.form.translation)
newf[quercus_form_nas,'Plant.growth.form.translation'] = 'tree' 
quercus_form_herbs = x1 & newf$Plant.growth.form.translation == 'herb'
newf[quercus_form_herbs,'Plant.growth.form.translation'] = 'tree'

##of those entries that are "tree and perennial", these should be the following:

#Fraxinus --> tree and deciduous
newf[which(newf$Genus == 'Fraxinus'),'Plant.growth.form.translation'] = 'tree'
newf[which(newf$Genus == 'Fraxinus'),"Phenology.vegetative.translation"] = 'deciduous'
#Salix --> tree and deciduous
newf[which(newf$Genus == 'Salix'),'Plant.growth.form.translation'] = 'tree'
newf[which(newf$Genus == 'Salix'),"Phenology.vegetative.translation"] = 'deciduous'

#Solanum dulcamara --> shrub and perennial  (fix ONLY Solanum dulcamara;  there are many other Solanum species that are not the same)
newf[which(newf$AccSpeciesName == 'Solanum dulcamara'),"Plant.growth.form.translation"] = 'shrub'
newf[which(newf$AccSpeciesName == 'Solanum dulcamara'),"Phenology.vegetative.translation"] = 'perennial'

#Solidago canadensis --> should be herb and perennial  (there are many other Solidago species, so ONLY fix Solidago canadensis)
newf[which(newf$AccSpeciesName == 'Solidago canadensis'),"Plant.growth.form.translation"] = 'herb'
newf[which(newf$AccSpeciesName == 'Solidago canadensis'),"Phenology.vegetative.translation"] = 'perennial'

# Fix broadleaf & herb --> herbaceous and herb
index = !is.na(newf$Plant.growth.form.translation) & newf$Plant.growth.form.translation =='herb' & !is.na(newf$Leaf.type.translation) & newf$Leaf.type.translation=='broadleaf'
newf[index,"Leaf.type.translation"] = 'herbaceous'

# ---------------------------- Finish working with PFT ----------------------------------------------#### 



# ---------------------------- Begin Working On Climate Data ----------------------------------------####


#temperature_file = '/Users/Xing/Downloads/CRU_TS3.22_HXH/cru_ts3.22_TS_means_1951-1980_HXH.nc'
#perc_file = '/Users/Xing/Downloads/GPCC_v6_HXH/GPCC_v6_PREC_means_1951-1980_HXH.nc'
temperature_file = paste(datasourcesdir, 'cru_ts3.22_TS_means_1951-1980_HXH.nc', sep="") 
perc_file = paste(datasourcesdir, 'GPCC_v6_PREC_means_1951-1980_HXH.nc', sep="")

library('ncdf')
nc = open.ncdf(temperature_file)
nc2 = open.ncdf(perc_file)


temperature_data = get.var.ncdf(nc,verbose = TRUE)
perc_data = get.var.ncdf(nc2)

close.ncdf(nc)
close.ncdf(nc2)


temperatures <- paste0('temp_',1:12)
precipitations <- paste0('precip_',1:12)

temp_51_80 = paste0(temperatures,'_51_80')
prep_51_80 <- paste0(precipitations,'_51_80')

newf[,temp_51_80] <- NA
newf[,prep_51_80]<- NA



#### To optimize the searching and updating speed
num = nrow(newf)
temp1 = rep(NA, num)
temp2 = rep(NA, num)
temp3 = rep(NA, num)
temp4 = rep(NA, num)
temp5 = rep(NA, num)
temp6 = rep(NA, num)
temp7 = rep(NA, num)
temp8 = rep(NA, num)
temp9 = rep(NA, num)
temp10= rep(NA, num)
temp11= rep(NA, num)
temp12= rep(NA, num)

prec1 = rep(NA, num)
prec2 = rep(NA, num)
prec3 = rep(NA, num)
prec4 = rep(NA, num)
prec5 = rep(NA, num)
prec6 = rep(NA, num)
prec7 = rep(NA, num)
prec8 = rep(NA, num)
prec9 = rep(NA, num)
prec10 = rep(NA, num)
prec11 = rep(NA, num)
prec12 = rep(NA, num)

latitude = newf$Latitude
longitude = newf$Longitude
#######
count = 0
for (i in 1:num) 
{
  #   print (i) ## maximum is 66749
  la = latitude[i] 
  lo = longitude[i]
  la = (90+la) *2
  lat_index = as.integer(la) + (abs((la - as.integer(la) )) > 0.5) + 1 ##+1 because the index begin from 1
  lo = (lo + 180)*2
  lon_index=as.integer(lo)  + (abs(lo - as.integer(lo)) > 0.5) + 1
  
  if(!is.na(lon_index) & !is.na(lat_index))
  {
    count = count + 1
    temp1[i] = temperature_data[lon_index,lat_index,1]
    temp2[i] = temperature_data[lon_index,lat_index,2]
    temp3[i] = temperature_data[lon_index,lat_index,3]
    temp4[i] = temperature_data[lon_index,lat_index,4]
    temp5[i] = temperature_data[lon_index,lat_index,5]
    temp6[i] = temperature_data[lon_index,lat_index,6]
    temp7[i] = temperature_data[lon_index,lat_index,7]
    temp8[i] = temperature_data[lon_index,lat_index,8]
    temp9[i] = temperature_data[lon_index,lat_index,9]
    temp10[i] = temperature_data[lon_index,lat_index,10]
    temp11[i] = temperature_data[lon_index,lat_index,11]
    temp12[i] = temperature_data[lon_index,lat_index,12]
    
    prec1[i] = perc_data[lon_index,lat_index,1]
    prec2[i] = perc_data[lon_index,lat_index,2]
    prec3[i] = perc_data[lon_index,lat_index,3]
    prec4[i] = perc_data[lon_index,lat_index,4]
    prec5[i] = perc_data[lon_index,lat_index,5]
    prec6[i] = perc_data[lon_index,lat_index,6]
    prec7[i] = perc_data[lon_index,lat_index,7]
    prec8[i] = perc_data[lon_index,lat_index,8]
    prec9[i] = perc_data[lon_index,lat_index,9]
    prec10[i] = perc_data[lon_index,lat_index,10]
    prec11[i] = perc_data[lon_index,lat_index,11]
    prec12[i] = perc_data[lon_index,lat_index,12]
    
    
  }
  
}


newf$temp_1_51_80 = temp1
newf$temp_2_51_80 = temp2
newf$temp_3_51_80 = temp3
newf$temp_4_51_80 = temp4
newf$temp_5_51_80 = temp5
newf$temp_6_51_80 = temp6
newf$temp_7_51_80 = temp7
newf$temp_8_51_80 = temp8
newf$temp_9_51_80 = temp9
newf$temp_10_51_80 = temp10
newf$temp_11_51_80 = temp11
newf$temp_12_51_80 = temp12

newf$precip_1_51_80 = prec1
newf$precip_2_51_80 = prec2
newf$precip_3_51_80 = prec3
newf$precip_4_51_80 = prec4
newf$precip_5_51_80 = prec5
newf$precip_6_51_80 = prec6
newf$precip_7_51_80 = prec7
newf$precip_8_51_80 = prec8
newf$precip_9_51_80 = prec9
newf$precip_10_51_80 = prec10
newf$precip_11_51_80 = prec11
newf$precip_12_51_80 = prec12

#### adding the new climate data (copied code from above, except change the climate source into new period data)
#temperature_file = '/Users/Xing/Downloads/CRU_TS3.22_HXH/cru_ts3.22_TS_means_1981-2010_HXH.nc'
#perc_file = '/Users/Xing/Downloads/GPCC_v6_HXH/GPCC_v6_PREC_means_1981-2010_HXH.nc'
temperature_file = paste(datasourcesdir, 'cru_ts3.22_TS_means_1981-2010_HXH.nc', sep="")
perc_file = paste(datasourcesdir, 'GPCC_v6_PREC_means_1981-2010_HXH.nc', sep="")

library('ncdf')
nc = open.ncdf(temperature_file)
nc2 = open.ncdf(perc_file)


temperature_data = get.var.ncdf(nc,verbose = TRUE)
perc_data = get.var.ncdf(nc2)

temp_81_10 = paste0(temperatures,'_81_10')
prep_81_10 <- paste0(precipitations,'_81_10')
newf[,temp_81_10] <- NA
newf[,prep_81_10]<- NA


#### To optimize the searching and updating speed
num = nrow(newf)
temp1 = rep(NA, num)
temp2 = rep(NA, num)
temp3 = rep(NA, num)
temp4 = rep(NA, num)
temp5 = rep(NA, num)
temp6 = rep(NA, num)
temp7 = rep(NA, num)
temp8 = rep(NA, num)
temp9 = rep(NA, num)
temp10= rep(NA, num)
temp11= rep(NA, num)
temp12= rep(NA, num)

prec1 = rep(NA, num)
prec2 = rep(NA, num)
prec3 = rep(NA, num)
prec4 = rep(NA, num)
prec5 = rep(NA, num)
prec6 = rep(NA, num)
prec7 = rep(NA, num)
prec8 = rep(NA, num)
prec9 = rep(NA, num)
prec10 = rep(NA, num)
prec11 = rep(NA, num)
prec12 = rep(NA, num)

latitude = newf$Latitude
longitude = newf$Longitude
#######
count = 0
for (i in 1:num) 
{
  #   print (i) ## maximum is 66749
  la = latitude[i] 
  lo = longitude[i]
  la = (90+la) *2
  lat_index = as.integer(la) + (abs((la - as.integer(la) )) > 0.5) + 1 ##+1 because the index begin from 1
  lo = (lo + 180)*2
  lon_index=as.integer(lo)  + (abs(lo - as.integer(lo)) > 0.5) + 1
  
  if(!is.na(lon_index) & !is.na(lat_index))
  {
    count = count + 1
    temp1[i] = temperature_data[lon_index,lat_index,1]
    temp2[i] = temperature_data[lon_index,lat_index,2]
    temp3[i] = temperature_data[lon_index,lat_index,3]
    temp4[i] = temperature_data[lon_index,lat_index,4]
    temp5[i] = temperature_data[lon_index,lat_index,5]
    temp6[i] = temperature_data[lon_index,lat_index,6]
    temp7[i] = temperature_data[lon_index,lat_index,7]
    temp8[i] = temperature_data[lon_index,lat_index,8]
    temp9[i] = temperature_data[lon_index,lat_index,9]
    temp10[i] = temperature_data[lon_index,lat_index,10]
    temp11[i] = temperature_data[lon_index,lat_index,11]
    temp12[i] = temperature_data[lon_index,lat_index,12]
    
    prec1[i] = perc_data[lon_index,lat_index,1]
    prec2[i] = perc_data[lon_index,lat_index,2]
    prec3[i] = perc_data[lon_index,lat_index,3]
    prec4[i] = perc_data[lon_index,lat_index,4]
    prec5[i] = perc_data[lon_index,lat_index,5]
    prec6[i] = perc_data[lon_index,lat_index,6]
    prec7[i] = perc_data[lon_index,lat_index,7]
    prec8[i] = perc_data[lon_index,lat_index,8]
    prec9[i] = perc_data[lon_index,lat_index,9]
    prec10[i] = perc_data[lon_index,lat_index,10]
    prec11[i] = perc_data[lon_index,lat_index,11]
    prec12[i] = perc_data[lon_index,lat_index,12]
    
    
  }
  
}


newf$temp_1_81_10 = temp1
newf$temp_2_81_10 = temp2
newf$temp_3_81_10 = temp3
newf$temp_4_81_10 = temp4
newf$temp_5_81_10 = temp5
newf$temp_6_81_10 = temp6
newf$temp_7_81_10 = temp7
newf$temp_8_81_10 = temp8
newf$temp_9_81_10 = temp9
newf$temp_10_81_10 = temp10
newf$temp_11_81_10 = temp11
newf$temp_12_81_10 = temp12

newf$precip_1_81_10 = prec1
newf$precip_2_81_10 = prec2
newf$precip_3_81_10 = prec3
newf$precip_4_81_10 = prec4
newf$precip_5_81_10 = prec5
newf$precip_6_81_10 = prec6
newf$precip_7_81_10 = prec7
newf$precip_8_81_10 = prec8
newf$precip_9_81_10 = prec9
newf$precip_10_81_10 = prec10
newf$precip_11_81_10 = prec11
newf$precip_12_81_10 = prec12



#----------------------------------- Radiation Data ----------------------------------#


# v)  Attach radiation data (1951 - 1980)
library(ncdf)
radiation_file = paste(datasourcesdir, 'PGMF_M.A1951-1980.SWdown.1x1.nc', sep="")

nc = open.ncdf(radiation_file)

dswrf = get.var.ncdf(nc)
## dim(dswrf) = 360 180  12
### -180 - 180 (east - west)
### -90 - 90 (north - sourth)


latitude = newf$Latitude
longitude = newf$Longitude 

unit = 'rad.Wm2_'
period = '_51_80'
Rad_names = paste0(unit,1:12,period)

newf[,Rad_names] <- NA

###get index for 
num = nrow(newf)
rad_1 = rep(NA, num)
rad_2 = rep(NA, num)
rad_3 = rep(NA, num)
rad_4 = rep(NA, num)
rad_5 = rep(NA, num)
rad_6 = rep(NA, num)
rad_7 = rep(NA, num)
rad_8 = rep(NA, num)
rad_9 = rep(NA, num)
rad_10= rep(NA, num)
rad_11= rep(NA, num)
rad_12= rep(NA, num)


#######
count = 0

for (i in 1:num) 
{
  #   print (i) ## maximum is 66749
  la = latitude[i] 
  lo = longitude[i]
  
  lat_index = as.integer(la) + 90  ##+1 because the index begin from 1
  
  lon_index = as.integer(lo) + 180
  
  if(!is.na(lon_index) & !is.na(lat_index))
  {
    count = count + 1
    rad_1[i] = dswrf[lon_index,lat_index,1]
    rad_2[i] = dswrf[lon_index,lat_index,2]
    rad_3[i] = dswrf[lon_index,lat_index,3]
    rad_4[i] = dswrf[lon_index,lat_index,4]
    rad_5[i] = dswrf[lon_index,lat_index,5]
    rad_6[i] = dswrf[lon_index,lat_index,6]
    rad_7[i] = dswrf[lon_index,lat_index,7]
    rad_8[i] = dswrf[lon_index,lat_index,8]
    rad_9[i] = dswrf[lon_index,lat_index,9]
    rad_10[i] = dswrf[lon_index,lat_index,10]
    rad_11[i] = dswrf[lon_index,lat_index,11]
    rad_12[i] = dswrf[lon_index,lat_index,12]
  }
  
}
newf$rad.Wm2_1_51_80 = rad_1
newf$rad.Wm2_2_51_80 = rad_2
newf$rad.Wm2_3_51_80 = rad_3
newf$rad.Wm2_4_51_80 = rad_4
newf$rad.Wm2_5_51_80 = rad_5
newf$rad.Wm2_6_51_80 = rad_6
newf$rad.Wm2_7_51_80 = rad_7
newf$rad.Wm2_8_51_80 = rad_8
newf$rad.Wm2_9_51_80 = rad_9
newf$rad.Wm2_10_51_80 = rad_10
newf$rad.Wm2_11_51_80 = rad_11
newf$rad.Wm2_12_51_80 = rad_12


################# copy the code from above, except change the source data into the 81_2008 period 

#radiation_file = '/Users/Xing/Downloads/PGMF_M.A1981-2008.SWdown.1x1.nc'
radiation_file = paste(datasourcesdir, 'PGMF_M.A1981-2008.SWdown.1x1.nc', sep="")
nc = open.ncdf(radiation_file)

dswrf = get.var.ncdf(nc)
## dim(dswrf) = 360 180  12
### -180 - 180 (east - west)
### -90 - 90 (north - sourth)


latitude = newf$Latitude
longitude = newf$Longitude 

unit = 'rad.Wm2_'
period = '_80_08'
Rad_names = paste0(unit,1:12,period)

newf[,Rad_names] <- NA

###get index for 
num = nrow(newf)
rad_1 = rep(NA, num)
rad_2 = rep(NA, num)
rad_3 = rep(NA, num)
rad_4 = rep(NA, num)
rad_5 = rep(NA, num)
rad_6 = rep(NA, num)
rad_7 = rep(NA, num)
rad_8 = rep(NA, num)
rad_9 = rep(NA, num)
rad_10= rep(NA, num)
rad_11= rep(NA, num)
rad_12= rep(NA, num)


#######
count = 0

for (i in 1:num) 
{
  #   print (i) ## maximum is 66749
  la = latitude[i] 
  lo = longitude[i]
  
  lat_index = as.integer(la) + 90  ##+1 because the index begin from 1
  
  lon_index = as.integer(lo) + 180
  
  if(!is.na(lon_index) & !is.na(lat_index))
  {
    count = count + 1
    rad_1[i] = dswrf[lon_index,lat_index,1]
    rad_2[i] = dswrf[lon_index,lat_index,2]
    rad_3[i] = dswrf[lon_index,lat_index,3]
    rad_4[i] = dswrf[lon_index,lat_index,4]
    rad_5[i] = dswrf[lon_index,lat_index,5]
    rad_6[i] = dswrf[lon_index,lat_index,6]
    rad_7[i] = dswrf[lon_index,lat_index,7]
    rad_8[i] = dswrf[lon_index,lat_index,8]
    rad_9[i] = dswrf[lon_index,lat_index,9]
    rad_10[i] = dswrf[lon_index,lat_index,10]
    rad_11[i] = dswrf[lon_index,lat_index,11]
    rad_12[i] = dswrf[lon_index,lat_index,12]
  }
  
}

newf$rad.Wm2_1_80_08 = rad_1
newf$rad.Wm2_2_80_08 = rad_2
newf$rad.Wm2_3_80_08 = rad_3
newf$rad.Wm2_4_80_08 = rad_4
newf$rad.Wm2_5_80_08 = rad_5
newf$rad.Wm2_6_80_08 = rad_6
newf$rad.Wm2_7_80_08 = rad_7
newf$rad.Wm2_8_80_08 = rad_8
newf$rad.Wm2_9_80_08 = rad_9
newf$rad.Wm2_10_80_08 = rad_10
newf$rad.Wm2_11_80_08 = rad_11
newf$rad.Wm2_12_80_08 = rad_12

#write.csv(newf,'sla_geo_climate_pft_radiation_20160211.csv',row.names = F)

# -------------------------------- End of the radiation -------------------------### 




write.csv(newf,'TRY_sla_scrub_climate_rad_geo_pft_20160316.csv',row.names = F)

############# -------------------------- ABOVE is TRY Data Srubb -----------------------------------------------------#########


## ------------------------------ Nancy's inspection code ---------------------###

#try.inspect.fn.R
#2016-01-12 by Nancy Kiang

try.inspect = function(mydata, datasetname="TRY scrub MM/DD/YY", if.genus=FALSE, if.newwindow=TRUE) {
  #Run this function on a scrubbed TRY data set to check frequencies and find any errors.
  
  detach()
  attach(mydata)
  
  #Inspect frequencies
  print("PFT CATEGORY FREQUENCY")
  #Growth form
  print("Plant.growth.form.translation")
  indexg = !is.na(Plant.growth.form.translation)
  print(sort(table(Plant.growth.form.translation[indexg])))
  
  #Leaf type
  print("Leaf.type.translation")
  indexl = !is.na(Leaf.type.translation)
  print(sort(table(Leaf.type.translation[indexl])))
  
  #Phenology
  print("Phenology.vegetative.translation")
  indexp = !is.na(Phenology.vegetative.translation)
  print(sort(table(Phenology.vegetative.translation[indexp])))
  
  #Photosynthetic pathway
  print("Photosynthesis.pathway")
  indexps = !is.na(Photosynthesis.pathway.translation)
  print(sort(table(Photosynthesis.pathway.translation)))
  
  #Check for errors
  print("")
  print("LEAF TYPE BY GROWTH FORM")
  for (g in c("herb", "grass", "shrub", "tree", "woody")) {
    print(paste(g))
    print(table(Leaf.type.translation[indexg & indexl & Plant.growth.form.translation==g]))
  }
  
  print("")
  print("PHENOLOGY BY GROWTH FORM")
  for (g in c("herb", "grass", "shrub", "tree", "woody")) {
    print(paste(g))
    print(table(Phenology.vegetative.translation[indexg & indexl & Plant.growth.form.translation==g]))
  }
  
  #Check entries that have odd category pairs
  print("")
  print("CHECK ODD CATEGORY PAIRS GENUS")
  print("--grass and leaf type--------------------------------------")
  for (lt in c("broadleaf", "needleleaf")) {
    print(paste("grass and", lt))
    index = indexg & indexl & Plant.growth.form.translation=="grass" & Leaf.type.translation==lt
    temp=table(Genus[index])
    print(temp[temp>0])
  }
  print("--grass and phenology--------------------------------------") 
  for (ph in c("deciduous", "evergreen")) {
    print(paste("grass and", ph))
    index = indexg & indexl & Plant.growth.form.translation=="grass" & Phenology.vegetative.translation==ph
    temp=table(Genus[index])
    print(temp[temp>0])
    #temp=table(AccSpeciesName)
    #write.table(temp,paste("try.scrub.grass.and.",ph, ".txt",sep=""))	
  }
  print("--tree and phenology---------------------------------------")
  for (ph in c("perennial")) {
    print(paste("tree and ", ph))
    index = indexg & indexp & Plant.growth.form.translation=="tree" & 	Phenology.vegetative.translation==ph
    temp=table(Genus[index])
    print(temp[temp>0])
    #temp=table(AccSpeciesName)
    #write.table(temp,paste("try.scrub.tree.and.",ph, ".txt",sep=""))
  }
  
  # - genus
  if (if.genus) {
    print("------------------------------------------------------------")
    print("GENUS BY GROWTH FORM")
    for (g in c("herb", "grass", "shrub", "tree", "woody")) {
      index2 = indexg & Plant.growth.form.translation==g
      print(paste(g))
      temp = sort(table(Genus[index2]))
      print(temp[temp>0])
    }
  }
  
  # - summarize geography (lon, lat, elevation)
  summary(Longitude)
  summary(Latitude)
  summary(Altitude.m.translation)
  
  #Map the points
  library(sp)
  library(fields) 
  library(spam) 
  library(maps) 
  library(maptools) 
  library(rworldmap) 
  library(SDMTools) 
  library(plotrix)
  if (if.newwindow) {
    quartz(width=9,height=6)
  }
  plot(Longitude, Latitude, pch=16, col=2, cex=0.5, xlim=c(-180,180), ylim=c(-90,90))
  plot(coastsCoarse, add=TRUE)
  n = sum(!is.na(Longitude) & !is.na(Latitude))
  text(-180,-10, paste("n total =", sum(!is.na(ConvertedValue))), adj=0)
  text(-180,-25, paste("n with lat/lon =", n), adj=0)
  #Points with climate data
  index = !is.na(temp_1_51_80)
  points(Longitude[index], Latitude[index], pch=16, col=3, cex=0.5)
  text(-180, -40, paste("n with climate =",sum(index)), adj=0)
  index = !is.na(SLA.mm2.mg) & !is.na(temp_1) & !is.na(Latitude) & !is.na(Longitude)
  text(-180, -20, paste("n with SLA & climate =",sum(index)), adj=0)
  title(paste("Green points with climate data. ", datasetname))
  
  
  if (if.newwindow) {
    quartz(width=9,height=6)
  }
  plot(Longitude, Latitude, pch=16, col=2, cex=0.6, xlim=c(-20,50), ylim=c(25,75))
  plot(coastsCoarse, add=TRUE)
  n = sum(!is.na(Longitude) & !is.na(Latitude))
  #Points with climate data
  index = !is.na(temp_1)
  points(Longitude[index], Latitude[index], pch=16, col=3, cex=0.6)
  title(paste("Green points with climate data. ", datasetname))
  
  detach()
}

try.inspect(newf,"TRY scrub 03/16/16")
title("TRY-geo scrub 03/16/2016")

par(mfrow=c(2,2))
plot(newf$ConvertedValue, pch=16, cex=0.3, col=2)
points(newf$StdValue, pch=16, col=1, cex=0.3)
mtext(paste("n =", sum(!is.na(newf$ConvertedValue)),",  no StdValue = ",sum(!is.na(newf$ConvertedValue) & is.na(newf$StdValue)), sep=""))
plot(newf$StdValue, newf$ConvertedValue, pch=16, cex=0.4)
plot(newf$Altitude.m, srtmz, pch=16, cex=0.4) #TRY vs. SRTM -----
index = is.na(newf$Altitude) & !is.na(srtmz)
junknoalt = sum(index)
points(srtmz[index], srtmz[index], col=2, pch=16, cex=0.5)
negelev = -5
index = srtmz< negelev & !is.na(srtmz)
junknegz = sum(index)
points(srtmz[index], srtmz[index], col=3, pch=16, cex=0.5)
legend(1500,-50, legend=c("Alt vs. srtmz",paste("SRTM fill Alt NA (",junknoalt,")"), 
					paste("SRTM<(", negelev,") (",junknegz,")")), col=1:3, pch=16, cex=0.8, bty="n")
mtext(outer=TRUE, "TRY-geo-climate scrub 03/16/2016", line=-1.5)


## ------------------------------ End of Inspection ---------------------------###



