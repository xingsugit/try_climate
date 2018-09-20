### This function is to generate the config string e.g. '001x10' from nc file name 
### for the convenience of generating plotting titles
get_configuration <-function(file_name,lmc = TRUE,year_pattern = '51_80_')
{
  
  begin_index = gregexpr(pattern =year_pattern,file_name)[[1]][1] + nchar(year_pattern)
  end_index = gregexpr(pattern = "_lmclus",file_name)[[1]][1]-1
  
  config_string = substr(file_name,begin_index,end_index)
  print('Extracted the config_string:')
  print(config_string)
  
  ## next step: split the config string into sampling_factor and the min_cluster_number
  sampling_factor = substring(config_string,1,nchar(config_string)-2)
  sampling_factor = as.integer(sampling_factor)/(10**(nchar(sampling_factor)-1)) 
  min_cluster_number = substring(config_string,nchar(config_string)-2+1,nchar(config_string))
  
  if(lmc)
  {
    configuration = paste0('lmclus: ',sampling_factor,' x ',min_cluster_number)
  }
  else
  {
    configuration = paste0('kmean, cluster number = ',min_cluster_number)
  }
  configuration
}


### This function is to generate the config string
### for the convenience of generating file names to save results
get_configuration_for_save_file <-function(file_name,lmc = TRUE,year_pattern = '51_80_')
{
  begin_index = gregexpr(pattern =year_pattern,file_name)[[1]][1] + nchar(year_pattern)
  end_index = gregexpr(pattern = "_lmclus",file_name)[[1]][1]-1
  
  config_string = substr(file_name,begin_index,end_index)
  config_string
}

## @write_clustering_result: merge the clustering results with the data (including the PFT and GEO info)
## param:
## @path the path of the nc result files
## @year_range the years' range e.g. '51_80'
## The function write results into a csv file, and return the dataframe
write_clustering_result <- function(path,result_write_to_path,orig_data_file,year_range)
{
  
  lmclus_prefix = paste0(result_write_to_path,'TRY-sla-seasonalclimate-altitude-lmclus-')
  lmclus_suffix = paste0(year_range,'.csv')
  
  #   kmean_file_name = paste0(kmean_prefix,cluster_number,kmean_suffix)
  #   lmclus_file_name = paste0(lmclus_prefix,sampling_factor,'-',cluster_number,lmclus_suffix)
  
  print('loading original data from the following file.....')
  # print(kmean_file_name)
  print(orig_data_file)
  orig_data = read.csv(orig_data_file)
  
  ## process the nc files:
  ## 1. Get the nc file list
  ## 2. For each nc file in the files list: extract the clustering label and put it in the dest csv file
  
  library("RNetCDF")
  nc_file_list = list.files(path, pattern = '*.nc', all.files = FALSE,
                            full.names = FALSE, recursive = FALSE,
                            ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  
  for(file in nc_file_list)
  {
    lmclus_file_name = paste0(path,file)
    print("The file being process is....")
    print(lmclus_file_name)
    nc_lmclus = open.nc(lmclus_file_name)
    lmclus_labels = var.get.nc(nc_lmclus,"labels")
    
    print("lmclus_labels length:")
    print(length(lmclus_labels))
    year_pattern = paste0(year_range,'_')
    config_str = get_configuration_for_save_file(file)
    col_config = paste0('lmclust.',config_str)
    orig_data[,col_config] = lmclus_labels
    
  }
  dest_file = paste0(lmclus_prefix,lmclus_suffix)
  
  print("writing results to following file:")
  print(dest_file)
  write.csv(orig_data,dest_file,row.names=F)
  print("Finish writing")
  orig_data
}
### util function: get nc file name from config string 
### input: config_str
### output: nc file name
### example config_str = '00110'
### output: nc_file_name = 'TRY-climate_51_80_00110_lmclus-20160404-164939.nc'
get_file_name<-function(config_str,path,year_range="51_80")
{
  nc_file_list = list.files(path, pattern = '*.nc', all.files = FALSE,
                            full.names = FALSE, recursive = FALSE,
                            ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  
  prefix = paste0('TRY-seasonalClimate_altitude_',year_range,'_',config_str,"_")
  file_index = grep(prefix, nc_file_list, ignore.case = FALSE, perl = FALSE, value = FALSE,
                    fixed = FALSE, useBytes = FALSE, invert = FALSE)
  
  nc_file_list[file_index]
}


### end of get_file_name
### function to plot the parallel temperature/precipitation distribution along 
### 12 months (colored with SLA)
### rainbow is set to from blue to red (begin from 0.5 hue)
### clust_result_file is the csv file that contain clustering information and the original data.
climate_parallel_SLA_plot <-function(clust_result_file,path,save_to_path,lmc = TRUE,year_range = '51_80',normalized = T)
{
  
  print(clust_result_file)
  nc_result =  read.csv(clust_result_file)
  print(names(nc_result))
  print(paste("nrow(nc_result) = ",nrow(nc_result)))
  #### 
  n = length(unique(nc_result$SLA.mm2.mg.translation))
  ### add new data to config sla range:
  ## added <sla in range>
  ranged_sla = nc_result[which(nc_result$SLA.mm2.mg.translation <=40),c("SLA.mm2.mg.translation")]
  n = length(unique(ranged_sla))
  sorted_ranged_sla = sort(unique(ranged_sla))
  ## finish <sla in range>
  
  ## prepare the color plate
  colPlate = rainbow(n, s = 1, v = 1, start = 0.5, end = max(1, n - 1)/n, alpha = 1)
  #nc_result$SLA.mm2.mg.translation = nc_result$SLA.mm2.mg.translation/scale_sla + mmin_sla
  nc_result$SLA.mm2.mg.translation = round(nc_result$SLA.mm2.mg.translation,3)
  
  sorted_sla = sort(unique(nc_result$SLA.mm2.mg.translation))
  col_max = max(sorted_sla)
  col_min = min(sorted_sla)
  ## begin <sla in range>
  col_max = 40
  col_min = 0.5
  ## end <sla in range>s
  nc_result[which(nc_result$SLA.mm2.mg.translation > 40),c("SLA.mm2.mg.translation")] = 40
  nc_result$SLA.mm2.mg.translation = as.factor(nc_result$SLA.mm2.mg.translation)
  ##
  
  ##
  clus_result_col_names = names(nc_result)[123:134]
  
  print(clus_result_col_names)
  
  ## now should do loop with col_name: clus_result_col_names
  #col_name = lmclust.00110
  
  for(col_name in clus_result_col_names)
  {
    print("Now: deal with col:")
    print(col_name)
    
    config_index = which(clus_result_col_names==col_name)
    config_str = substr(col_name,9,13)
    
    if (lmc)
    {
      cluster_number = length(unique(nc_result[[col_name]]))
      print(paste("Total cluster number is:",cluster_number))
      
      file_save_prefix = paste0('try_lmc_seasonalclimate_altitude_',config_str,'_plot')
    }
    else
    {
      cluster_number = length(unique(nc_result$kmeans.clusters))
      file_save_prefix = paste0('try_kmean_climate_altitude_',cluster_number,'_')
    }
    
    print("Get the corresponding nc file name:")
    
    nc_file_name = get_file_name(config_str,path,year_range)
    print(nc_file_name)
    ncf = nc_open(paste0(path,nc_file_name))
    
    
    # layout(matrix(c(1,1,1,1,1,1,2:37),6,6,byrow = TRUE),heights=c(1,2,2,2,2,2))
    title2 = substr(config_str,nchar(config_str)-1,nchar(config_str))
    title1 = substr(config_str,1,nchar(config_str)-2)
    main_title = paste("Configuration of clustering:",title1,'x',title2)
    
    print(paste("main tile of the plot page is:",main_title))
    
    count = 1
    ## loop in each cluster
    for(i in 1:cluster_number)
    {
      print(paste("Current Cluster #:",i))
      
      if(i%%10 == 1) ## legend
      {
        ## add pdf setting
        save_to_file = paste0(file_save_prefix,'_',year_range,'_',count,'.png')
        save_to_file = paste0(save_to_path,save_to_file)
        print(save_to_file)
        png(file=save_to_file, width = 895, height = 855, units = "px")
        #         #par(mfrow=c(5,6), oma=c(0,0,2,0), mar=c(0,0,3,0))
        layout(matrix(c(1,1,1,1,1,1,2:37),6,6,byrow = TRUE),heights=c(1,2,2,2,2,2))
        ## 
        print("set the legend and title....")
        print("now check the window first:")
        print(par("mar"))
        par(mar=c(2.7,2.7,3.3,2.7))
        plot.new()
        plot.window(xlim=c(col_min,col_max), ylim=0:1)
        rect(seq(col_min, col_max, length=n)[-n],0,
             seq(col_min, col_max, length=n)[-1],1,
             col=rainbow(n, s = 1, v = 1, start = 0.5, end = max(1, n - 1)/n, alpha = 1), angle = -30,border=NA)
        
        box()
        axis(1, at=c(0,col_max*0.2,col_max*0.4,col_max*0.6,col_max*0.8,col_max),label = quantile(sorted_ranged_sla,probs = seq(0, 1, 0.2)), las=1,cex.lab=1.3)
        
        title(main_title,cex = 3)
      }
      if(year_range == '51_80')
      {
        precip_cols = precip_51_80
        temp_cols = temp_51_80
        rad_cols = rad_51_80
      }
      else
      {
        precip_cols = precip_81_10
        temp_cols = temp_81_10
        rad_cols = rad_80_08
      }
      if(lmc)
      {
        
        precip_data = nc_result[nc_result[[col_name]] == i,c(sla,precip_cols)]
        size = nrow(precip_data)
        temperature_data = nc_result[nc_result[[col_name]] == i,c(sla,temp_cols)]
        title_prefix = 'Cl:'
        rad_data = nc_result[nc_result[[col_name]] == i, c(sla,rad_cols)]
      }
      else
      {
        precip_data = nc_result[nc_result$kmeans.clusters== i,c(sla,precip_cols)]
        temperature_data = nc_result[nc_result$kmeans.clusters == i,c(sla,temp_cols)]
        rad_data = nc_result[nc_result$kmeans.clusters == i,c(sla,rad_cols)]
        title_prefix = 'Cl:'
      }
      ##
      ##get depth, discriminability, and Theta of current cluster###
      
      ## ?? need some function to get 1)corresponding nc file, 2) from nc file get ncf
      var_id = paste0('M',i)
      depth = ncatt_get(ncf,varid = var_id)$depth
      depth = round(depth,3)
      dscrm = ncatt_get(ncf,varid = var_id)$discriminability
      dscrm = round(dscrm,3)
      theta = ncatt_get(ncf,varid = var_id)$θ
      theta = round(theta,3)
      dimension = ncf[['dim']][[var_id]][['len']]
      ##
      first_title = paste0('Cl:',i,', Size:',size)
      print(first_title)
      second_title = paste0('Dim: ',dimension,', DSR:',dscrm)
      print(second_title)
      third_title = paste0("θ:",theta,", DPT:",depth)
      print(third_title)
      
      ## reset margin for the cubs
      par(mar=c(2.3,2.5,2.0,2.0))
      ## limit
      temp_lim = c(-10,30) 
      precip_lim =  c(0,1000)
      rad_lim = c(0, 600)
      
      ## precipitation
      names(precip_data) = c('SLA.mm2.mg',precip_cols)
      precip_data$record = 1:nrow(precip_data)
      precip_data = precip_data[complete.cases(precip_data),]
      precip_long = reshape(precip_data, direction='long',idvar = 'record',varying = precip_cols,v.names = "precip",timevar='month')
      ## recover the data value from normalized value
      if(normalized)
      {
        precip_long$precip = precip_long$precip/scale_precip + mmin_precip
      }
      
      plot(x = precip_long$month, y = precip_long$precip,col = colPlate[precip_long$SLA.mm2.mg],pch = 20, cex = 0.7,main = first_title,xlab = "",ylab = 'Precipitation',ylim = precip_lim , cex.lab=1.3,cex.axis=0.9,mgp = c(1.5,0.5,0))
      
      ## temperature
      names(temperature_data) = c('SLA.mm2.mg',temp_cols)
      temperature_data$record = 1:nrow(temperature_data)
      # temperature_data = temperature_data[complete.cases(temperature_data),]
      temp_long = reshape(temperature_data, direction='long',idvar = 'record',varying = temp_cols,v.names = 'temperature',timevar = 'month')
      ## revcover the data from normalized value
      if(normalized)
      {
        temp_long$temperature = temp_long$temperature/scale_temp + mmin_temp
      }
      plot(x = temp_long$month, y = temp_long$temperature,col = colPlate[temp_long$SLA.mm2.mg],pch = 20, cex = 0.7,main = second_title,xlab = "",ylab = 'Temperature',ylim = temp_lim,cex.lab=1.3,cex.axis=0.9,mgp=c(1.5, 0.5, 0)) 
      
      ### radiation:
      names(rad_data) = c('SLA.mm2.mg',rad_cols)
      rad_data$record = 1:nrow(rad_data)
      rad_long = reshape(rad_data,direction = 'long',idvar = 'record',varying = rad_cols,v.names = 'Radiation',timevar = 'month')      
      if(normalized)
      {
        rad_long$Radiation = rad_long$Radiation/scale_rad + mmin_rad
      }
      #anything out of range will plot with the upper bound color
      rad_long$Radiation
      
      plot(x = rad_long$month, y = rad_long$Radiation,col = colPlate[rad_long$SLA.mm2.mg],pch = 20, cex = 0.7,main = third_title,xlab = "",ylab = 'Radiation',ylim = rad_lim,cex.lab=1.3,cex.axis=0.9,mgp=c(1.5, 0.5, 0)) 
      
      if(i %% 10 == 0 || i == cluster_number) ## the 10th plot, last plot of current page
      { 
        count = count+1
        dev.off()
      }
    }
    print("finished plot of current nc result")
  }
  
  
}

### end of get_file_name
### function to plot the parallel temperature/precipitation distribution along 
### 12 months (colored with altitude)
### rainbow is set to from blue to red (begin from 0.5 hue)
### clust_result_file is the csv file that contain clustering information and the original data.
climate_parallel_altitude_plot <-function(clust_result_file,path,save_to_path,lmc = TRUE,year_range = '51_80',normalized = T)
{
  nc_result =  read.csv(clust_result_file)
  #### 
  n = length(unique(nc_result$Altitude.SRTM15.m))
  ## prepare the color plate
  colPlate = rainbow(n, s = 1, v = 1, start = 0.5, end = max(1, n - 1)/n, alpha = 1)
  if(normalized)
  {
    nc_result$Altitude.SRTM15.m = nc_result$Altitude.SRTM15.m/scale_alt + mmin_alt
  }
  
  nc_result$Altitude.SRTM15.m = round(nc_result$Altitude.SRTM15.m,3)
  
  sorted_alt = sort(unique(nc_result$Altitude.SRTM15.m))
  col_max = max(sorted_alt)
  col_min = min(sorted_alt)
  mmax_alt = max(sorted_alt)
  nc_result$Altitude.SRTM15.m = as.factor(nc_result$Altitude.SRTM15.m)
  ##
  
  ##
  clus_result_col_names = names(nc_result)[123:ncol(nc_result)]
  
  
  
  ## now should do loop with col_name: clus_result_col_names
  #col_name = lmclust.00110
  
  for(col_name in clus_result_col_names)
  {
    print("Now: deal with col:")
    print(col_name)
    
    config_index = which(clus_result_col_names==col_name)
    config_str = substr(col_name,9,13)
    
    if (lmc)
    {
      cluster_number = length(unique(nc_result[[col_name]]))
      print(paste("Total cluster number is:",cluster_number))
      
      file_save_prefix = paste0('try_lmc_climate_altitude_',config_str,'_plot')
    }
    else
    {
      cluster_number = length(unique(nc_result$kmeans.clusters))
      file_save_prefix = paste0('try_kmean_climate_altitude_',cluster_number,'_')
    }
    
    print("Get the corresponding nc file name:")
    
    nc_file_name = get_file_name(config_str,path,year_range)
    print(nc_file_name)
    ncf = nc_open(paste0(path,nc_file_name))
    
    
    # layout(matrix(c(1,1,1,1,1,1,2:37),6,6,byrow = TRUE),heights=c(1,2,2,2,2,2))
    title2 = substr(config_str,2,nchar(config_str))
    title1 = substr(config_str,1,1)
    main_title = paste("Configuration of clustering:",title1,'x',title2)
    
    print(paste("main tile of the plot page is:",main_title))
    
    count = 1
    ## loop in each cluster
    for(i in 1:cluster_number)
    {
      print(paste("Current Cluster #:",i))
      
      if(i%%10 == 1) ## legend
      {
        ## add pdf setting
        save_to_file = paste0(file_save_prefix,'_',year_range,'_',count,'.png')
        save_to_file = paste0(save_to_path,save_to_file)
        print(save_to_file)
        png(file=save_to_file, width = 895, height = 855, units = "px")
        #         #par(mfrow=c(5,6), oma=c(0,0,2,0), mar=c(0,0,3,0))
        layout(matrix(c(1,1,1,1,1,1,2:37),6,6,byrow = TRUE),heights=c(1,2,2,2,2,2))
        ## 
        print("set the legend and title....")
        print("now check the window first:")
        print(par("mar"))
        par(mar=c(2.7,2.7,3.3,2.7))
        plot.new()
        plot.window(xlim=c(col_min,col_max), ylim=0:1)
        rect(seq(col_min, col_max, length=n)[-n],0,
             seq(col_min, col_max, length=n)[-1],1,
             col=rainbow(n, s = 1, v = 1, start = 0.5, end = max(1, n - 1)/n, alpha = 1), angle = -30,border=NA)
        
        box()
        axis(1, at=c(0,mmax_alt*0.2,mmax_alt*0.4,mmax_alt*0.6,mmax_alt*0.8,mmax_alt),label = quantile(sorted_alt,probs = seq(0, 1, 0.2)), las=1,cex.lab=1.3)
        
        title(main_title,cex = 3)
      }
      
      if(lmc)
      {
        
        precip_data = nc_result[nc_result[[col_name]] == i,c('Altitude.SRTM15.m',precip_51_80)]
        size = nrow(precip_data)
        temperature_data = nc_result[nc_result[[col_name]] == i,c('Altitude.SRTM15.m',temp_51_80)]
        title_prefix = 'Cl:'
        rad_data = nc_result[nc_result[[col_name]] == i, c('Altitude.SRTM15.m',rad_51_80)]
      }
      else
      {
        precip_data = nc_result[nc_result$kmeans.clusters== i,precip_51_80]
        temperature_data = nc_result[nc_result$kmeans.clusters == i,temp_51_80]
        rad_data = nc_result[nc_result$kmeans.clusters  == i, rad_51_80]
        title_prefix = 'Cl:'
      }
      ##
      ##get depth, discriminability, and Theta of current cluster###
      
      ## ?? need some function to get 1)corresponding nc file, 2) from nc file get ncf
      var_id = paste0('M',i)
      depth = ncatt_get(ncf,varid = var_id)$depth
      depth = round(depth,3)
      dscrm = ncatt_get(ncf,varid = var_id)$discriminability
      dscrm = round(dscrm,3)
      theta = ncatt_get(ncf,varid = var_id)$θ
      theta = round(theta,3)
      dimension = ncf[['dim']][[var_id]][['len']]
      ##
      first_title = paste0('Cl:',i,', Size:',size)
      print(first_title)
      second_title = paste0('Dim: ',dimension,', DSR:',dscrm)
      print(second_title)
      third_title = paste0("θ:",theta,", DPT:",depth)
      print(third_title)
      
      ## reset margin for the cubs
      par(mar=c(2.3,2.5,2.0,2.0))
      if(year_range == '51_80')
      {
        precip_cols = precip_51_80
        temp_cols = temp_51_80
        rad_cols = rad_51_80
      }
      else
      {
        precip_cols = precip_81_10
        temp_cols = temp_81_10
        rad_cols = rad_80_08
      }
      ## precipitation
      names(precip_data) = c('Altitude.SRTM15.m',precip_cols)
      precip_data$record = 1:nrow(precip_data)
      precip_data = precip_data[complete.cases(precip_data),]
      precip_long = reshape(precip_data, direction='long',idvar = 'record',varying = precip_cols,v.names = "precip",timevar='month')
      ## recover the data value from normalized value
      if(normalized)
      {
        precip_long$precip = precip_long$precip/scale_precip + mmin_precip
      }
      
      precip_long$Altitude.SRTM15.m = as.factor(precip_long$Altitude.SRTM15.m)
      plot(x = precip_long$month, y = precip_long$precip,col = colPlate[precip_long$Altitude.SRTM15.m],pch = 20, cex = 0.7,main = first_title,xlab = "",ylab = 'Precipitation',ylim =c(0,610) , cex.lab=1.3,cex.axis=0.9,mgp = c(1.5,0.5,0))
      
      ## temperature
      names(temperature_data) = c('Altitude.SRTM15.m',temp_cols)
      temperature_data$record = 1:nrow(temperature_data)
      # temperature_data = temperature_data[complete.cases(temperature_data),]
      temp_long = reshape(temperature_data, direction='long',idvar = 'record',varying = temp_cols,v.names = 'temperature',timevar = 'month')
      ## revcover the data from normalized value
      if(normalized)
      {
        temp_long$temperature = temp_long$temperature/scale_temp + mmin_temp
      }
      
      temp_long$Altitude.SRTM15.m = as.factor(temp_long$Altitude.SRTM15.m)
      plot(x = temp_long$month, y = temp_long$temperature,col = colPlate[temp_long$Altitude.SRTM15.m],pch = 20, cex = 0.7,main = second_title,xlab = "",ylab = 'Temperature',ylim = c(-17,30),cex.lab=1.3,cex.axis=0.9,mgp=c(1.5, 0.5, 0)) 
      
      ### radiation:
      names(rad_data) = c('Altitude.SRTM15.m',rad_cols)
      rad_data$record = 1:nrow(rad_data)
      rad_long = reshape(rad_data,direction = 'long',idvar = 'record',varying = rad_cols,v.names = 'Radiation',timevar = 'month')      
      if(normalized)
      {
        rad_long$Radiation = rad_long$Radiation/scale_rad + mmin_rad
      }
      rad_long$Altitude.SRTM15.m = as.factor(rad_long$Altitude.SRTM15.m)
      mmax_rad = max(nc_result[,rad_51_80])
      plot(x = rad_long$month, y = rad_long$Radiation,col = colPlate[rad_long$Altitude.SRTM15.m],pch = 20, cex = 0.7,main = third_title,xlab = "",ylab = 'Radiation',ylim = c(0,mmax_rad),cex.lab=1.3,cex.axis=0.9,mgp=c(1.5, 0.5, 0)) 
      
      #image.plot(zlim=c(col_min,col_max), col = rainbow(n, s = 1, v = 1, start = 0, end = max(1, n - 1)/n, alpha = 1),legend.shrink = 1,legend.only=TRUE, horizontal =FALSE)
      print(i)
      if(i %% 10 == 0 || i == cluster_number) ## the 10th plot, last plot of current page
      { 
        count = count+1
        dev.off()
      }
    }
    print("finished plot of current nc result")
  }
  
  
}

### function to plot the manifold distribution (spread and distance from the center)
## e.g. config_col_name_suffix = '0.01.x.10' 
distribution_in_manifold <-function(path,data_source,nc_file,M=10,config_col_name_suffix,save_to_path,exp_cols,isnormalized = T)
{
  
  col_name = paste0("lmclust.",config_col_name_suffix)
  
  library('ncdf4')
  nc = nc_open(paste0(path,nc_file),suppress_dimvals = T)
  cluster_number = nc$dim$m$len
  
  mu = ncvar_get(nc,varid = 'mu')
  count = 0
  ## for each cluster:
  
  for(i in 1:cluster_number)
  {
    if(count %% 16 == 0)
    {
      if(!is.null(dev.list()))
      {
        dev.off()
      }
      save_to_file = paste0(save_to_path,config_col_name_suffix,'_distribution_spread',count%/%16,'.png')
      print(paste("Create new file to plot:",save_to_file))
      png(file=save_to_file, width = 895, height = 855, units = "px")
      par(oma=c(0,0,3,0),mfrow=c(4,4),mar=c(2.7,3.0,3.0,2.7))
    }
    print(paste0("In cluster:",i))
    var_id = paste0('M',i)
    dim_number = nc[['dim']][[var_id]][['len']]
    
    sub_space = data_source[data_source[[col_name]] == i,]
    ## x is the data that is being clustered on
    
    x = sub_space[,exp_cols]
    
    cluster_base = as.matrix(ncvar_get(nc,varid = var_id))
    cluster_center = mu[,i]
    subDistance_array = rep(0,nrow(sub_space))
    ##for each dimension:
    for(j in 1:dim_number)
    {
      #distance_array = rep(0,nrow(sub_space))
      print(paste0("In dimension:",j))
      
      lambda_array = c(numeric(0))
      
      cluster_base_onedimension = cluster_base[,j]
      if(abs(mean(cluster_base_onedimension)) < 2e10)
      {
        #print("cluster_base_onedimension:")
        #print(cluster_base_onedimension)
        #print("*******")
        for(e in 1:nrow(sub_space))
        {
          if(isnormalized)
          {
            normx = as.vector(x[e,]) ## x is already normalized
          }
          else
          {
            mmin_vec = c(0.506121736, -30.9676993646546, -11.7688887914022, -11.7688887914022, 1.68111111720403, 1.68111111720403, 18.1993363847335, 18.1993363847335)
            mmax_vec = c(237.077683958, 4944, 28.7794446945191, 28.7794446945191, 418.333887736003, 418.333887736003, 315.436620076497, 315.436620076497)
            scaler = 1.0/(mmax_vec-mmin_vec)
            #(x - min) * scaler = normalized_x
            orig_x = as.vector(x[e,])
            normx = (orig_x-mmin_vec)/(mmax_vec-mmin_vec)
          }
          
          lambda = cluster_base_onedimension %*% t(normx-cluster_center)
          
          lambda_array = c(lambda_array,lambda)
          distance = (t(cluster_base_onedimension) %*% t(normx-cluster_center)) ** 2
          #print(paste("distance:",distance))
          #print("distance:")
          #print(distance)
          #distance_array[i] = distance_array[i] + distance
          #ubDistance = (as.matrix(lambda * (normx-cluster_center)) %*% t(  lambda * (normx-cluster_center))) ** 2
          #print(paste("subdistance:",subDistance))
          subDistance_array[e] =  subDistance_array[e] + distance
          #print(paste0("subDistance_array[",i,']:'))
          #print(subDistance_array[e])
        }
        ##divide the axis volumn into M piece: (M = 10)
        lambda_max = max(lambda_array)
        
        lambda_min = min(lambda_array)
        
        if(lambda_max == lambda_min)
        {
          lambda_max = lambda_min + 1
        }
        interval_array = seq(lambda_min, lambda_max, l=M)
        
        main_title = paste0('Distribution and spread of x inside a manifold,  Total Cluster Number = ',cluster_number)
        #par(mfrow=c(1,1))
        #print("distribution array:")
        #print(interval_array)
        print('plot')
        hist(lambda_array,breaks = interval_array,xlab = interval_array,main = paste0('Distr of Cl:',i,' Dim: ',j),cex.axis = 1.5,cex.main = 1.5)
        
        count = count + 1
        
        if(count %% 16 == 0)
        {
          
          sub = (count%%16) + 1
          mtext(main_title, side = 3, outer = TRUE,cex = 2)
          #save_to_file = paste0(config_col_name_suffix,'_distribution_spread',count%/%16,'.png')
          #dev.copy(png,paste0(path,save_to_file),width=dev.size("px")[1], height=dev.size("px")[2])
          if(!is.null(dev.list()))
          {
            dev.off()
          }
          save_to_file = paste0(save_to_path,config_col_name_suffix,'_distribution_spread',count%/%16,'.png')
          print(paste("Create new file to plot:",save_to_file))
          png(save_to_file, width = 895, height = 855, units = "px")
          par(oma=c(0,0,3,0),mfrow=c(4,4),mar=c(2.7,3.0,3.0,2.7))
        }
      }
      
    }
    spread = sqrt(subDistance_array)
    #print("spread that is smaller than 0:")
    #print(sum(spread<0))
    dist_interval_array = seq(min(spread), max(spread), l=M)
    #print(dist_interval_array)
    print('plot')
    hist(spread,breaks = dist_interval_array,xlab = dist_interval_array,main = paste0('Spread, size = ',nrow(sub_space)),cex.axis = 1.5,cex.main = 1.5)
    count = count+1
    if(count %% 16 == 0)
    {
      sub = (count%%16) + 1
      mtext(main_title, side = 3, outer = TRUE,cex = 2)
      #save_to_file = paste0(config_col_name_suffix,'_distribution_spread',count%/%16,'.png')
      #dev.copy(png,paste0(path,save_to_file),width=dev.size("px")[1], height=dev.size("px")[2])
      if(!is.null(dev.list()))
      {
        dev.off()
      }
      save_to_file = paste0(save_to_path,config_col_name_suffix,'_distribution_spread',count%/%16,'.png')
      print(paste("Create new file to plot:",save_to_file))
      png(save_to_file, width = 895, height = 855, units = "px")
      par(oma=c(0,0,3,0),mfrow=c(4,4),mar=c(2.7,3.0,3.0,2.7))
    }
  }
  nc_close(nc)
}

## path and data_file are the path and the file that all the data and cluster results
## It is the file generated by function write_clustering_result().
batch_plot_distribution_spread <-function(path, data_file,save_path,exp_cols)
{
  common_prefix = 'lmclust.'
  data_source = read.csv(data_file) 
  name_list = names(data_source)[123:134] ## clustering results columns indexes
  config_col_names =substr(name_list,9,nchar(name_list))
  nc_files = list.files(path, pattern = '*.nc', all.files = FALSE,
                        full.names = FALSE, recursive = FALSE,
                        ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  
  for(i in 1:length(nc_files))
  {
    
    nc_file = nc_files[i]
    config_col_name_suffix = config_col_names[i]
    distribution_in_manifold(path,data_source,nc_file,M=10,config_col_name_suffix,save_path,exp_cols,isnormalized = F)
    if(!is.null(dev.list()))
    {
      dev.off()## Note: this line close the last plot page
    }
    
  }
}


# Set 2) one color each for  Plant.growth.form.translation:  shrub, tree, grass, herb, shrubtree, woody, NA(Grey)
plot_PF <-function(path, data_file, pf_name, save_to_path, NATona = T , lmc=T,normalized  =T)
{
  ## data_file: get data, for each configuration, do plot 
  data_source = read.csv(data_file,stringsAsFactors = FALSE)
  
  clus_result_col_names = names(data_source)[123:ncol(data_source)]
  for(col_name in clus_result_col_names)
  {
    ### begin loop of configurations
    ### within each configuration: 
    ### 1. get the cluster numbers
    cluster_number = length(unique(data_source[[col_name]]))
    
    ### 2. set the NA values into 'other'
    if(NATona)
    {
      which = is.na(data_source[[pf_name]])
      data_source[which,c(pf_name)] = 'NA'
    }
    else
    {
      data_source = data_source[complete.cases(pf_name),]
    }
    data_source[,c(pf_name)] = as.factor(data_source[,c(pf_name)])
    ### 3. get the nc file:
    config_index = which(clus_result_col_names==col_name)
    config_str = substr(col_name,9,13)
    nc_file_name = get_file_name(config_str,path)
    print(nc_file_name)
    ncf = nc_open(paste0(path,nc_file_name))
    
    ### 4. set the colors to plot (each class within current PF use one color)
    #col_plate = rainbow(length(unique(data_source[[pf_name]])))
    col_plate = c("blue","red", "green", "orange", "purple","tan","grey")
    ### 5. get the name of file prefix to save the plot
    file_save_prefix = paste0('try_lmc_climate_',pf_name,'_',config_str,'_plot')
    ### 6. get the main title for each page (same with each configuration)
    title2 = substr(config_str,2,nchar(config_str))
    title1 = substr(config_str,1,1)
    main_title = paste("Configuration of clustering:",title1,'x',title2,", Cluster Number:",cluster_number)
    
    ### 7. turn off any device
    ### 8. Initialize the plot count
    count = 0
    for (i in 1:cluster_number)
    {
      ### begin iterate each cluster
      
      ### 1. get each cluster's attribute
      size = sum(data_source[,c(col_name)] == i)
      var_id = paste0('M',i)
      depth = ncatt_get(ncf,varid = var_id)$depth
      depth = round(depth,3)
      dscrm = ncatt_get(ncf,varid = var_id)$discriminability
      dscrm = round(dscrm,3)
      theta = ncatt_get(ncf,varid = var_id)$θ
      theta = round(theta,3)
      dimension = ncf[['dim']][[var_id]][['len']]
      ##
      first_title = paste0('Cl:',i,', Size:',size)
      print(first_title)
      second_title = paste0('Dim: ',dimension,', DSR:',dscrm)
      print(second_title)
      third_title = paste0("θ:",theta,", DPT:",depth)
      print(third_title)
      
      # within each cluster, plot 3 paralleled, if reach 30 plots, save current and open new file to plot 
      if(count%%30 == 0)
      {
        if(!is.null(dev.list()))
        {
          dev.off()
        }
        ## 1. create file to save
        file_save_suffix = (count%/%30) + 1
        save_to_file = paste0(save_to_path,file_save_prefix,'_',file_save_suffix,'.png')
        png(file=save_to_file, width = 895, height = 855, units = "px")
        ## 2. set the layout
        layout(matrix(c(1,1,1,1,1,1,2:37),6,6,byrow = TRUE),heights=c(1,2,2,2,2,2),widths = rep.int(1,6))
        ## 3. plot the common legend, set the page title
        par(mar=c(1.2,1.5,2.3,1.5))
        plot(1, type = "n", axes=FALSE, xlab="", ylab="")
        legend(x = "top",inset = 0,
               legend = unique(data_source[,c(pf_name)]), 
               col=col_plate, lwd=5, cex=1.5, horiz = TRUE,bty = "n")
        
        title(main_title,cex.main = 1.5)
      }
      
      par(mar=c(2.7,2.5,1.3,2.6))
      ## limit
      temp_lim = c(-10,30) 
      precip_lim =  c(0,1000)
      rad_lim = c(0, 600)
      ### get the data and plot
      precip_data = data_source[data_source[[col_name]] == i,c(pf_name,precip_51_80)]
      temperature_data = data_source[data_source[[col_name]] == i,c(pf_name,temp_51_80)]
      rad_data = data_source[data_source[[col_name]] == i, c(pf_name,rad_51_80)]
      
      ## convert temp, precip, rad data into long shape:
      precip_data$record = 1:nrow(precip_data)
      precip_long = reshape(precip_data, direction='long',idvar = 'record',varying = precip_51_80,v.names = "precip",timevar='month')
      ## recover the data value from normalized value
      if(normalized)
      {
        precip_long$precip = precip_long$precip/scale_precip + mmin_precip
      }
      
      plot(x = precip_long$month, y = precip_long$precip,col = col_plate[precip_long[[pf_name]]],pch = 20, cex = 0.7,main = first_title,xlab = "",ylab = 'Precipitation',ylim =precip_lim , cex.lab=1.3,cex.axis=0.9,mgp = c(1.5,0.5,0))
      
      ## temperature
      temperature_data$record = 1:nrow(temperature_data)
      # temperature_data = temperature_data[complete.cases(temperature_data),]
      temp_long = reshape(temperature_data, direction='long',idvar = 'record',varying = temp_51_80,v.names = 'temperature',timevar = 'month')
      ## revcover the data from normalized value
      if(normalized)
      {
        temp_long$temperature = temp_long$temperature/scale_temp + mmin_temp
      }
      
      plot(x = temp_long$month, y = temp_long$temperature,col = col_plate[temp_long[[pf_name]]],pch = 20, cex = 0.7,main = second_title,xlab = "",ylab = 'Temperature',ylim = temp_lim,cex.lab=1.3,cex.axis=0.9,mgp=c(1.5, 0.5, 0)) 
      
      ### radiation:
      rad_data$record = 1:nrow(rad_data)
      rad_long = reshape(rad_data,direction = 'long',idvar = 'record',varying = rad_51_80,v.names = 'radiation',timevar = 'month')      
      if(normalized)
      {
        rad_long$radiation = rad_long$radiation/scale_rad + mmin_rad
      }
      
      plot(x = rad_long$month, y = rad_long$radiation,col = col_plate[rad_long[[pf_name]]],pch = 20, cex = 0.7,main = second_title,xlab = "",ylab = 'Radiation',ylim = rad_lim,cex.lab=1.3,cex.axis=0.9,mgp=c(1.5, 0.5, 0)) 
      
      ### end iterate each cluster
      count = count + 3
    }
    ### end loop of configurations
    if( i == cluster_number)
    {
      dev.off()
    }
  }
  
}

prepare_map_environment <-function()
{
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
}

plot_on_map<-function(path,save_to_path,file_name,lmc = T)
{
  data_source =  read.csv(file_name,stringsAsFactors = FALSE)
  namelist = names(data_source)
  
  
  clus_result_col_names = names(data_source)[123:150]
  for(col_name in clus_result_col_names)
  {
    data_source[,c(col_name)] = as.factor(data_source[,c(col_name)])
    cluster_number = length(unique(data_source[[col_name]]))
    config_index = which(clus_result_col_names==col_name)
    config_str = substr(col_name,9,13)
    nc_file_name = get_file_name(config_str,path)
    print(nc_file_name)
    ncf = nc_open(paste0(path,nc_file_name))
    
    ### 4. set the colors to plot (each class within current PF use one color)
    
    
    col_plate = colorRampPalette(c("blue", "red"))(cluster_number) 
    ### 5. get the name of file prefix to save the plot
    file_save_prefix = paste0('try_lmc_map_',config_str,'_plot')
    ### 6. get the main title for each page (same with each configuration)
    title2 = substr(config_str,nchar(config_str)-1,nchar(config_str))
    title1 = substr(config_str,1,nchar(config_str)-2)
    main_title = paste("Configuration of clustering:",title1,'x',title2,", Cluster Number:",cluster_number)
    count = 0
    for (i in 1:cluster_number)
    {
      size = sum(data_source[,c(col_name)] == i)
      var_id = paste0('M',i)
      depth = ncatt_get(ncf,varid = var_id)$depth
      depth = round(depth,3)
      dscrm = ncatt_get(ncf,varid = var_id)$discriminability
      dscrm = round(dscrm,3)
      theta = ncatt_get(ncf,varid = var_id)$θ
      theta = round(theta,3)
      dimension = ncf[['dim']][[var_id]][['len']]
      
      first_title = paste('Cl:',i,", Size:",size,", Dim: ",dimension)
      print(first_title) 
      second_title = paste('DSR:',dscrm,", θ:",theta,", DPT:",depth)
      print(second_title)
      title = paste(first_title," ", second_title)
      
      if(count %% 4 == 0)
      {
        if(!is.null(dev.list()))
        {
          dev.off()
        }
        file_save_suffix = (count%/%4) + 1
        save_to_file = paste0(save_to_path,file_save_prefix,'_',file_save_suffix,'.png')
        png(file=save_to_file, width = 755, height = 755, units = "px")
        ## 2. set the layout
        layout(matrix(c(1,1,2:5),3,2,byrow = TRUE),heights=c(1,3,3))
        par(mar=c(2.7,2.5,2.9,2.6))
        plot(1, type = "n", axes=FALSE, xlab="", ylab="")
        #legend(x = "top",inset = 0,
        #    legend = levels(data_source[,c(col_name)]), 
        #     col=col_plate, lwd=5, cex=1.5, horiz = TRUE,bty = "n")
        
        title(main_title,cex.main = 2)
      }
      
      location_data = data_source[data_source[[col_name]] == i,c("Latitude","Longitude")]
      par(mar=c(2.0,2.0,2.0,2.0))
      prepare_map_environment()
      plot(coastsCoarse,main = title,cex.main = 1.4)
      points(location_data$Longitude,location_data$Latitude, col="red", pch=16, cex = 0.6)
      count = count + 1
      
      if (i == cluster_number)
      {
        dev.off()
      }
    }
  }
}

### test the write_clustering_result with the following code ### 
set_global_variables<-function()
{
  ## library
  library(ncdf4)
  ## names
  setwd('~/nasa-climate/')
  sla <<- 'SLA.mm2.mg.translation'
  temp_51_80 <<- paste0('temp.C_',seq(1,12),'_51_80')
  temp_81_10 <<- paste0('temp.C_',seq(1,12),'_81_10')
  precip_51_80 <<- paste0('precip.mm.mon_',seq(1,12),'_51_80')
  precip_81_10 <<- paste0('precip.mm.mon_',seq(1,12),'_81_10')
  rad_51_80 <<- paste0('rad.Wm2_',seq(1,12),'_51_80')
  rad_80_08 <<- paste0('rad.Wm2_',seq(1,12),'_80_08')
  altitude <<- 'Altitude.SRTM15.m'
  geo <<- c( "Latitude","Longitude","Altitude.SRTM15.m")
  pft <<- c("Plant.growth.form.translation","Leaf.type.translation","Photosynthesis.pathway.translation","Phenology.vegetative.translation")
  
  path <<- '/Users/Xing/nasa-climate/results/results5180/'
  normalized_file <<- '/Users/Xing/Dropbox/NASAData/output/SLA_climate_altitude_pft_5180_normalized_0512.csv'
  orig_file <<- "/Users/Xing/nasa-climate/data/try_SLA_altitude_seasonalClimate_pft_geo_original_20160713.csv"
  clust_result_file <<- "./results/results5180/TRY-sla-seasonalclimate-altitude-lmclus-51_80.csv"
  year_range <<- '51_80'
  #scaler for experiment #1
  mmin_vec <<- c(0.506121736, -30.9676993646546, -11.7688887914022, -11.7688887914022, 1.68111111720403, 1.68111111720403, 18.1993363847335, 18.1993363847335)
  mmax_vec <<- c(237.077683958, 4944, 28.7794446945191, 28.7794446945191, 418.333887736003, 418.333887736003, 315.436620076497, 315.436620076497)
  scaler <<- 1.0/(mmin_vec-mmax_vec)
  #path = '/Users/Xing/nasa-climate/results/results8110/'
  #orig_file = '/Users/Xing/Dropbox/NASAData/output/SLA_climate_pft_8110_normalized_0409.csv'
  #year_range = '81_10'
  
  ## results(original data + cluster results) will write to this folder:
  result_write_to_path <<- './results/results5180/'
  
  ## climate parallel plot with rainbow colored by sla value
  climate_sla_parallel_save_to_path <<- "results/results5180/climate_sla_parallel_plot/"
  ## climate parallel plot with rainbow colored by altitude value
  climate_altitude_parallel_save_to_path <<- 'results/results5180/climate_altitude_parallel_plot/'
  
  
  ## distribution and spread plot for experiment config #1:
  distribution_spread_write_to_path <<- './results/results5180/distribution_spread_plots/'
  exp_1_cols <<- c(sla,altitude,'T.C.summer','T.C.winter',"P.mm.summer", 'P.mm.winter', 'R.Wm2.summer', 'R.Wm2.winter')
  
  ## plots of each cluster on the map
  maps_plot_folder <<- './results/results5180/clusters_on_map_plots/'
  
  
  ## PF clustering plot
  growth_form_plots <<- './results/results5180/growth_form_plots/'
  
  leaf_type_plots <<- './results/results5180/leaf_type_plots/'
  
  photosynthesis_plots <<- './results/results5180/photosynthesis_plots/'
  
  phenology_plots <<- './results/results5180/phenology_plots/'
}


set_global_variables()

## write cluster results to file and generate result plots
cluster_result = write_clustering_result(path,result_write_to_path,orig_file,year_range)

climate_parallel_SLA_plot(clust_result_file,path,climate_sla_parallel_save_to_path,year_range = '51_80',normalized = F)

climate_parallel_altitude_plot(clust_result_file,path,climate_altitude_parallel_save_to_path,year_range = '51_80',normalized = F)

batch_plot_distribution_spread(path, clust_result_file,distribution_spread_write_to_path,exp_cols = exp_1_cols)


plot_on_map(path,maps_plot_folder,clust_result_file,lmc = T)

plot_PF(path,clust_result_file,pf_name = 'Plant.growth.form.translation',growth_form_plots,normalized=F)

plot_PF(path,clust_result_file,pf_name = 'Leaf.type.translation',leaf_type_plots,normalized=F)

plot_PF(path,clust_result_file,pf_name = 'Photosynthesis.pathway.translation',photosynthesis_plots,normalized=F)

plot_PF(path,clust_result_file,pf_name = 'Phenology.vegetative.translation',phenology_plots,normalized=F)

#/Users/Xing/Dropbox/NASAData/nasa-climate/results/results5180
data_source = read.csv("/Users/Xing/Dropbox/NASAData/nasa-climate/results/results5180/TRY-sla-seasonalclimate-altitude-lmclus-51_80.csv",stringsAsFactors = FALSE)
location_data = data_source[,c("Latitude","Longitude","lmclust.110")]
par(mar=c(2.0,2.0,2.0,2.0))
prepare_map_environment()
plot(coastsCoarse,main = 'Clusters on Map, Configuration 1 x 20',cex.main = 1.4)
points(location_data$Longitude,location_data$Latitude, col=data_source$lmclust.120, pch=16, cex = 0.4)

