###############################################################################################################################
### Calculation of Population Weighted Exposure (PWE) according to the UN SDG Indicator 11.6.2
### Steps of this script:
### 1. Set input variables
### 2. Annual averaged Surface Concentration Field of PM10 and PM1.5 from EPISODE-CityChem is read.
### 3. Total European Population Density Map in 100x100 m2 resolution is read and projected to Concentration Field Extent.
### 4. Population Weighted Exposure (PWE) is calculated and written to selected output format.
###
### v1.01, Martin O.P. Ramacher, Helmholtz-Zentrum Geesthacht (Feb 11 2019)
###############################################################################################################################
###

### INPUT section
### path of concentraton and population density input files directory
input <- "../input/"
### name of concentration input file
statrecp <- "statrecpyearmean.nc"
### name of European population density tif file
#pop_dens <- "popu01clcv5.tif"
### path of static output
output <- "../output/"
### format of output (csv, tif, nc or asc)
format <- "tif"
#format <- "csv"
###
### end of input section
###############################################################################################################################
###
###libraries
library(raster)
library(ncdf4)
###
### checks before program starts
if (dir.exists(input)) print("Input Directory exists ...") else stop("Input Directory does not exist! Please change Input Directory Path!")
if (dir.exists(output)) print("Output Directory exists ...") else stop("Output Directory does not exist! Please change Input Directory Path!")
setwd(input)
if (file.exists(statrecp)) print("Concentration File exists ...") else stop("Concentration File does not exist!")
#if (file.exists(pop_dens)) print("Population Density File exists ...") else stop("Population Density File does not exist!")
if (format == "nc" || format == "tif" || format == "asc" || format == "csv") print(paste("Output Format for PWE: ", format, sep ="")) else 
  stop("Wrong Output Format selected! Choose from: nc, tif, asc or csv!")
###
### extract projection from statrecp file
proj4_string <- as.character(ncatt_get(nc_open(statrecp), 0, "proj4_string")[2])
###
###
### read annual mean concentration for pm2.5 and pm10 from statrecp
print("Read Annual Mean Concentrations of PM10 and PM2.5 ...")
pm10 <- raster(statrecp, var = "PM10", crs = proj4_string)
pm25 <- raster(statrecp, var = "PM25", crs = proj4_string)
###
### read and project population density grid to projection and extent of concentration input file (statrecphour) from citychem
### if file "popu01clcv5.tif" is not in input folder it will be downloaded from EEA Website.
### attention: values in popu01clcv5.tif represent population/km2 but the population density grid is in 100x100 m2 grid resolution
if (file.exists("popu01clcv5.tif"))
{
  print("Read European Population Density File, Crop and Project it to Concentration File Extent and Projection ...")
  pop_dens <- "popu01clcv5.tif"  
} else
{
  print("Download European Population Density File - http://ftp.eea.europa.eu/www/eea-data/popu01clcv5.tif")
  download.file(url = "http://ftp.eea.europa.eu/www/eea-data/popu01clcv5.tif", destfile = "popu01clcv5.tif", mode = "wb")
  print("Read European Population Density File, Crop and Project it to Concentration File Extent and Projection ...")
  pop_dens <- "popu01clcv5.tif"
}
pop <- crop(raster(pop_dens), projectRaster(pm10, crs = crs(raster(pop_dens))))/100
### option to delete file after downloaded and processed
#file.remove("popu01clcv5.tif")
###
pop_proj <- projectRaster(from = pop, to = pm10)
pop_sum_proj <- sum(getValues(pop_proj), na.rm = T)
projection_correction <- sum(getValues(pop), na.rm = T)/pop_sum_proj
pop_proj <- pop_proj*projection_correction
###
pop_sum_proj <- sum(getValues(pop_proj), na.rm = T)
print(paste("Total Population in Urban Research Domain: ", as.integer(pop_sum_proj), sep=""))
###
### calculate static exposure by multiplying statrecp with population density
### following https://unstats.un.org/wiki/display/SDGeHandbook/Indicator+11.6.2#Indicator11.6.2-MethodofComputationandOtherMethodologicalConsiderations
### population weighted exposure in each grid cell [ug m-3] = ( annual mean concentration in grid cell * population in grid cell ) / sum of population in all grid cells
print("Calculate Population Weighted Exposure (PWE) according to UN SDG Indicator 11.6.2 ...")
#pwe_pm10 <- ((pm10*pop_proj)/pop_sum_proj)
#pwe_pm25 <- ((pm25*pop_proj)/pop_sum_proj)
#pwe_pm10 <- ((pm10*pop_proj)/pop_proj)
#pwe_pm25 <- ((pm25*pop_proj)/pop_proj)
pwe_pm10 <- ((pm10*pop_proj)/(pop_sum_proj/ncell(pop_proj[pop_proj>0])))
pwe_pm25 <- ((pm25*pop_proj)/(pop_sum_proj/ncell(pop_proj[pop_proj>0])))
###
### write population weighted exposure to output directory in selected format
crs(pwe_pm10) <- proj4_string
crs(pwe_pm25) <- proj4_string
names(pwe_pm10) <- "PWE_PM10"
names(pwe_pm25) <- "PWE_PM25"
###
print(paste("Write Population Weighted Exposure (PWE) for PM10 and PM2.5 to Output Directory in selected Format: ",  output, "pwe_pm10.", format, sep=""))
print(paste("Write Population Weighted Exposure (PWE) for PM10 and PM2.5 to Output Directory in selected Format: ",  output, "pwe_pm2.5.", format, sep=""))
setwd("../output")

#inputpath<-getwd()
#lenin<-nchar(inputpath)
#homepath<-substr(inputpath,1,lenin-5)
#outpath<-paste(homepath,"output/",sep="")
#print( outpath   )
#setwd(outpath)

###
if (format == "nc" || format == "tif" || format == "asc")
{
  writeRaster(pwe_pm10, paste("pwe_pm10.", format, sep=""), overwrite = T)
  writeRaster(pwe_pm25, paste("pwe_pm2.5.", format, sep=""), overwrite = T)
} else if (format == "csv")
{
  pwe_pm10_csv <- as.data.frame(cbind(coordinates(pwe_pm10), getValues(pwe_pm10)))
  pwe_pm25_csv <- as.data.frame(cbind(coordinates(pwe_pm25), getValues(pwe_pm25)))
  colnames(pwe_pm10_csv) <- c("x_coord", "y_coord", "pwe_pm10")
  colnames(pwe_pm25_csv) <- c("x_coord", "y_coord", "pwe_pm25")
  write.csv(pwe_pm10_csv, paste("pwe_pm10.", format, sep=""), quote = F, row.names=F)
  write.csv(pwe_pm25_csv, paste("pwe_pm2.5.", format, sep=""), quote = F, row.names=F)
} 
###
### save images
print ("Plot and Save PWE Plots in Output ...")
png(filename = "pwe_pm10.png")
plot(pwe_pm10, col = colorRampPalette(c("seashell","red"))(100))
title(main= paste("Population Weighted Exposure PM10 (Total Pop. = ", as.integer(pop_sum_proj),")", sep =""),
      sub = paste("CRS: ", proj4_string, sep = ""))
#dev.off()
invisible(dev.off())
###
png(filename = "pwe_pm25.png")
plot(pwe_pm25, col = colorRampPalette(c("seashell","red"))(100))
title(main= paste("Population Weighted Exposure PM2.5 (Total Pop. = ", as.integer(pop_sum_proj),")", sep =""),
      sub = paste("CRS: ", proj4_string, sep = ""))
#dev.off()
invisible(dev.off())
###
### clear workspace & free memory
rm(list=ls())
invisible(gc())
print ("Static PWE Calculation & Writing done!")
