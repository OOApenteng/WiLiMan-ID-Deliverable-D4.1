
remove (list = objects() )
library(lattice)
library(tmap)
library(readxl)
library(sp)
library(sf)
library(terra)
library(tidyverse)
library(dplyr)

setwd("/Users/xqv795/Desktop/APENTENG_LENE")
### READ IN newest OIE DATA ###
##link to where files are - the below code will pick the newest file in the folder"
df <- file.info(list.files("./data", full.names = T,pattern = "infur" )) # modded

# READ IN SHAPEFILE OF THE STUDY REGION ###
europeanCountries = st_read('./data/GIS/Europe_shapefiles.shp')

# CLEAN AND PREPARE DATA---------------------------------

#https://www.ruokavirasto.fi/en/animals/animal-health-and-diseases/animal-diseases/wildlife/chronic-wasting-disease-cwd-in-cervids/
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
#https://www.ruokavirasto.fi/en/animals/animal-health-and-diseases/animal-diseases/wildlife/chronic-wasting-disease-cwd-in-cervids/
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

################################################################
#Read in landcover data
prec<- rast("bio_12.tif")
temp<- rast("bio_1.tif")
corine <-rast("g1k_06wgs.tif")
corine_attributes <- read.csv("clc_legend.csv")

#convert rgb colors to hex:
# Function to apply
rgb2hex <- function(x) rgb(substr(x, 1, 3), substr(x, 5, 7), substr(x, 9, 11), maxColorValue = 255)
corine_attributes$Color<-rgb2hex(corine_attributes$RGB)
levels(corine) <-corine_attributes[c(1,4)]#corine_attributes[c(1,5)] # levels of the factors
coltab(corine)<-corine_attributes[c(1,7)]

#writeRaster(corine, 'corine_rat_LEVEL2.tif', datatype='INT1U', overwrite = TRUE)
cor <- rast("corine_rat_LEVEL2.tif")

#------------------------------------------------------
#https://search.r-project.org/CRAN/refmans/tmap/html/tm_symbols.html
#https://www.rdocumentation.org/packages/tmap/versions/3.3-4/topics/tm_symbols
#https://finnstats.com/2021/06/13/r-plot-pch-symbols-different-point-shapes-in-r/
#--Plots
tm_shape(prec/100) +
  tm_raster(title="Annual precipitation (CWD_Deers)",palette="Blues") +
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
  tm_raster(title="Annual mean temperature (CWD_Deers)", palette="Reds") +
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
#extract raster values to point locations using the terra package
#prec
precip <-terra::extract(prec, vect(Finland_dat_CWD_sf_deers))
Finland_dat_CWD_sf_deers$prec <- precip$bio_12/100
#temperature
tempe <-terra::extract(temp, vect(Finland_dat_CWD_sf_deers))
Finland_dat_CWD_sf_deers$temp <- tempe$bio_1/10
#landcover
cover <-terra::extract(cor, vect(Finland_dat_CWD_sf_deers))
Finland_dat_CWD_sf_deers$cover <- cover$LABEL2

#prec
precip <-terra::extract(prec, vect(Sweden_dat_CWD_sf_deers))
Sweden_dat_CWD_sf_deers$prec <- precip$bio_12/100
#temperature
tempe <-terra::extract(temp, vect(Sweden_dat_CWD_sf_deers))
Sweden_dat_CWD_sf_deers$temp <- tempe$bio_1/10
#landcover
cover <-terra::extract(cor, vect(Sweden_dat_CWD_sf_deers))
Sweden_dat_CWD_sf_deers$cover <- cover$LABEL2

#prec
precip <-terra::extract(prec, vect(Norway_dat_CWD_sf_deers))
Norway_dat_CWD_sf_deers$prec <- precip$bio_12/100
#temperature
tempe <-terra::extract(temp, vect(Norway_dat_CWD_sf_deers))
Norway_dat_CWD_sf_deers$temp <- tempe$bio_1/10
#landcover
cover <-terra::extract(cor, vect(Norway_dat_CWD_sf_deers))
Norway_dat_CWD_sf_deers$cover <- cover$LABEL2
#---------------------------------------------

library(glmmTMB)
library(DHARMa)
library(MuMIn)

#check your data - to see if it is overdispersed, if the ratio between variances and means is >1 your data is overdispersed
dispersionstats <- Norway_dat_CWD_sf_deers %>%
  summarise(
    means = mean(cases, na.rm=T),
    variances = var(cases,na.rm=T),
    ratio = variances/means)
dispersionstats











#--------------------------------------------
#60.0499998 7.416665 ha
#61.839329976 4.959829494 bre
#60.423831638 11.63749745 North
#64.5783089 17.888237 Thi
#60.83416333 10.071833046 eth
#43.259622, 2.340387 vine
#60.270277777778 , 9.4080555555556 Sigdal
#63.2177 11.03938 selbu
#  Location FLESBERG    Latitude  59.83283000    Longitude  9.58308000  
#64.546988 11.034265 Nor
#67.28, 14.40501 Bum


