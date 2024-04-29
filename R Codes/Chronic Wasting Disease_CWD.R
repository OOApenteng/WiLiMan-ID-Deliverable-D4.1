# These lines of code were written to analyse ASF, WNF, and CWD from different public domain databases 
#as described in this report for WiLiMan_ID. The R code is intended to ensure the reproducibility of the results or, 
#with slight modification, to be applied to other diseases in the databases used. 
#R Code written by Ofosuhene O. Apenteng. 

remove (list = objects() )
library(lattice)
library(tmap)
library(readxl)
library(sp)
library(sf)
library(terra)
library(tidyverse)
library(dplyr)

setwd("Your directory")
### READ IN newest OIE DATA ###
##link to where files are - the below code will pick the newest file in the folder"
df <- file.info(list.files("./data", full.names = T,pattern = "infur" )) # modded

# READ IN SHAPEFILE OF THE STUDY REGION ###
europeanCountries = st_read('./data/GIS/Europe_shapefiles.shp')

# CLEAN AND PREPARE DATA---------------------------------
# I create this dataframe based the link above
ADMIN <- c("Finland", "Finland", "Finland")
#Cases <- c(1, 1, 1)
Species <- c("Moose", "Moose", "Moose")
Outbreak_start_date <- c("2018-03-12", "2020-06-21", "2022-07-22")
Longitude <- c(64.13333, 62.41407, 63.03916651)
Latitude <- c(29.51667, 25.95194, 24.5583311)

Finland_dat_CWD <- data.frame(ADMIN,Species,Outbreak_start_date,Longitude,Latitude)
Finland_dat_CWD$Species[Finland_dat_CWD$Species == "Moose"] <- 'Deer'
Finland_dat_CWD_deers <- Finland_dat_CWD

Finland_dat_CWD_sf_deers <- st_as_sf(Finland_dat_CWD_deers, coords=c("Latitude","Longitude"),crs="EPSG:4326")

#------------------------------------------------
# I create this dataframe based the link above
ADMIN <- c("Norway", "Norway", "Norway","Norway", "Norway", "Norway",
           "Norway", "Norway", "Norway","Norway", "Norway", "Norway",
           "Norway", "Norway", "Norway","Norway", "Norway", "Norway")
#Cases <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
Species <- c("Deer","Deer","Deer","Deer","Deer","Deer","Moose", 
             "Moose", "Moose", "Moose","Deer","Moose", "Moose", 
             "Deer","Moose","Moose","Deer","Deer")
Outbreak_start_date <- c("2018-01-05","2018-01-17","2018-02-01","2018-02-13",
                         "2018-02-20","2018-03-09","2018-10-27",
                         "2019-09-29","2019-06-21","2020-04-21","2020-09-03",
                         "2021-06-29","2021-12-26","2022-01-01","2022-01-23",
                         "2022-02-22","2022-09-27","2023-09-29")
Latitude <- c(11.0342, 9.0342, 10.0342,12.0342,11.0342,7.4166,
              9.4822,11.039,9.4256,11.4953,14.4049,5.9403,5.9372,
              17.888,11.6374,4.9774,7.4166,10.0355)
Longitude <- c(64.5469, 62.5469, 61.5469, 61.0469,59.5469,60.049,
               59.847,63.2177,60.1308,64.0150,67.2803,58.2596,59.6653,
               64.5783,60.4238,61.8446,60.0499,58.9988)

Norway_dat_CWD <- data.frame(ADMIN,Species,Outbreak_start_date,
                             Longitude,Latitude)
#Renaming
Norway_dat_CWD$Species[Norway_dat_CWD$Species == "Moose"] <- 'Deer'
Norway_dat_CWD_deers <- Norway_dat_CWD

Norway_dat_CWD_sf_deers <- st_as_sf(Norway_dat_CWD_deers, coords=c("Latitude","Longitude"),crs="EPSG:4326")

#---------------------------------------------------------
ADMIN <- c("Sweden", "Sweden", "Sweden","Sweden")
#CasesS <- c(1, 1, 1, 1)
Species <- c("Moose", "Moose", "Moose", "Moose")
Outbreak_start_date <- c("2019-03-14","2019-05-10", "2019-09-12","2020-09-16")
Longitude <- c(66.0333332, 67.3869, 66.7642, 64.344665)
Latitude <- c(17.9499962, 18.3210, 20.3344, 18.314209)

Sweden_dat_CWD <- data.frame(ADMIN,Species,Outbreak_start_date,Longitude,Latitude)

# Renaming Data Frame 
Sweden_dat_CWD$Species[Sweden_dat_CWD$Species == "Moose"] <- 'Deer'
Sweden_dat_CWD_deers <- Sweden_dat_CWD

Sweden_dat_CWD_sf_deers <- st_as_sf(Sweden_dat_CWD_deers, coords=c("Latitude","Longitude"),crs="EPSG:4326")

#-----------------------------------------
#Read in landcover data
prec<- rast("./data/bio_12.tif") # Precipitation
temp<- rast("./data/bio_1.tif") # Temperature 
cor <- rast("./data/corine_rat_LEVEL2.tif") # Land cover 

#------------------------------------------------------
#--Plots
tm_shape(prec/100) +
  tm_raster(title="Annual precipitation (CWD_Deers)",palette="Mako") +
  tm_shape(Finland_dat_CWD_sf_deers)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="blue",shape = 9,size = 0.3)+
  tm_add_legend('symbol',
                col = "blue",
                shape = 9,size = 0.3,
                labels="RUOKAVIRASTO")+
tmap_options(max.categories = 44)+
  tm_shape(Norway_dat_CWD_sf_deers)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="deeppink",shape = 14,size = 0.3)+
  tm_add_legend('symbol',
                col = "deeppink",
                shape = 14,size = 0.3,
                labels="VETINST")+
  tmap_options(max.categories = 44)+
  tm_shape(Sweden_dat_CWD_sf_deers)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="black",shape = 10,size = 0.3)+
  tm_add_legend('symbol',
                col = "black",
                shape = 10,size = 0.3,
                labels="SVA")
  
tm_shape(temp/10) +
  tm_raster(title="Annual mean temperature (CWD_Deers)", palette=rev(hcl.colors(7, "PinkYl"))) +
  tmap_options(max.categories = 44)+
  tm_shape(Finland_dat_CWD_sf_deers)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="blue",shape = 9,size = 0.3)+
  tm_add_legend('symbol',
                col = "blue",
                shape = 9,size = 0.3,
                labels="RUOKAVIRASTO")+
tmap_options(max.categories = 44)+
  tm_shape(Norway_dat_CWD_sf_deers)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="deeppink",shape = 14,size = 0.3)+
  tm_add_legend('symbol',
                col = "deeppink",
                shape = 14,size = 0.3,
                labels="VETINST")+
  tmap_options(max.categories = 44)+
  tm_shape(Sweden_dat_CWD_sf_deers)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="black",shape = 10,size = 0.3)+
  tm_add_legend('symbol',
                col = "black",
                shape = 10,size = 0.3,
                labels="SVA")


tm_shape(cor) +
  tm_raster(title="Land cover (CWD_Deers)")+
  tmap_options(max.categories = 44)+
  tm_shape(Finland_dat_CWD_sf_deers)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="blue",shape = 9,size = 0.3)+
  tm_add_legend('symbol',
                col = "blue",
                shape = 9,size = 0.3,
                labels="RUOKAVIRASTO")+
  tmap_options(max.categories = 44)+
  tm_shape(Norway_dat_CWD_sf_deers)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="deeppink",shape = 14,size = 0.3)+
  tm_add_legend('symbol',
                col = "deeppink",
                shape = 14,size = 0.3,
                labels="VETINST")+
  tmap_options(max.categories = 44)+
  tm_shape(Sweden_dat_CWD_sf_deers)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="black",shape = 10,size = 0.3)+
  tm_add_legend('symbol',
                col = "black",
                shape = 10,size = 0.3,
                labels="SVA")

#-------------------------------------------------------

