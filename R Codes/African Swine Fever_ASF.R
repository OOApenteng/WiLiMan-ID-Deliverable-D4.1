# These lines of code were written to analyse ASF, WNF, and CWD from different public domain databases as described
#in this report for WiLiMan_ID. The R code is intended to ensure the reproducibility of the results or, 
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

setwd("Your Directory")
### Read the the various the diseases from the WOAH database ###
##link to where files are - the below code will pick the newest file in the folder"
df <- file.info(list.files("./data", full.names = T,pattern = "infur" )) # modded

europe_data<-read_excel("infur_20231124.xlsx")

#Read in the shapefile of the study region ###
europeanCountries = st_read('./data/GIS/Europe_shapefiles.shp')

# CLEAN AND PREPARE DATA---------------------------------

#make sure that outbreak start date is in date format 
europe_data$Outbreak_start_date <- as.Date(europe_data$Outbreak_start_date, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))

#filter out other diseases than African swine fever virus (ASF)
europe_data_ASF <- europe_data[europe_data$disease_eng %in% c("African swine fever virus (Inf. with)"),]

#filter out other species than birds using the pig_names_20231203.csv 
#file where it contains both pigs and wild boar
pigFam <- as.list(read.csv("./data/pig_names_20231203.csv", sep=";", header=FALSE))
europe_data_ASF$Species <- gsub("\\s*\\([^\\)]+\\)","",as.character(europe_data_ASF$Species))
europe_data_ASF_pigs <- europe_data_ASF[europe_data_ASF$Species %in% pigFam$V1[2],] #V1[3] #V1[2]Swine
europe_data_ASF_WildBoar <- europe_data_ASF[europe_data_ASF$Species %in% pigFam$V1[3],] #V1[3]Wildboar 

#we only want data from Europe (so filter using the europe shapefile) and data between 2018 and 2023
#where the period we are using five years interval
# For pigs only 
europe_data_ASF_pigs <-europe_data_ASF_pigs %>%
  filter(iso_code %in% unique(europeanCountries$ADM0_A3),Outbreak_start_date >"2017-12-31") %>% 
  dplyr::filter(!is.na(cases)) %>% 
  dplyr::filter(!is.na(Longitude)) %>% 
  dplyr::filter(!is.na(Latitude)) 

#we only want data from Europe (so filter using the europe shapefile) and data between 2018 and 2023
#where the period we are using five years interval
#For wild boar only
europe_data_ASF_WildBoar <-europe_data_ASF_WildBoar %>%
  filter(iso_code %in% unique(europeanCountries$ADM0_A3),Outbreak_start_date >"2017-12-31") %>% 
  dplyr::filter(!is.na(cases)) %>% 
  dplyr::filter(!is.na(Longitude)) %>% 
  dplyr::filter(!is.na(Latitude)) 

WOAH_pigs=europe_data_ASF_pigs
WOAH_boar=europe_data_ASF_WildBoar

#We needed to represent spatial vector data to include points (coordinates), 
#in view of that we added the their latitude and longitude
# For pigs only
endDate <- max(europe_data_ASF_pigs$Outbreak_start_date)
europe_dat_ASF_sf_pigs <- st_as_sf(europe_data_ASF_pigs, coords=c("Longitude","Latitude"),crs="EPSG:4326")

#We needed to represent spatial vector data to include points (coordinates), 
#in view of that we added the their latitude and longitude
# For wild boar only
endDate <- max(europe_data_ASF_WildBoar$Outbreak_start_date)
europe_dat_ASF_sf_WildBoar <- st_as_sf(europe_data_ASF_WildBoar, coords=c("Longitude","Latitude"),crs="EPSG:4326")

#We remove these countries because we noticed that it was not part of corine land cover 
# For pigs only
RemoveCountries <- c("Ukraine","Russia", "Belarus")
europe_dat_ASF_sf_pigs<- europe_dat_ASF_sf_pigs[!europe_dat_ASF_sf_pigs$country %in% RemoveCountries, ]

#We remove these countries because we noticed that it was not part of corine land cover 
#For wild boar only
RemoveCountries <- c("Ukraine","Russia", "Belarus")
europe_dat_ASF_sf_WildBoar<- europe_dat_ASF_sf_WildBoar[!europe_dat_ASF_sf_WildBoar$country %in% RemoveCountries, ]


#---Empres_I--from different database ---------------------------------###############
Empres_data_ASF <- read_csv("Africa_Swine_Fever_Pigs_WildBoar.csv", skip=11)
Empres_data_ASF <- Empres_data_ASF %>% select_if(~ !any(is.na(.)))

#Renaming
Empres_data_ASF <- Empres_data_ASF %>% 
  rename(
    Outbreak_start_date = Observation.date..dd.mm.yyyy.,
    Reporting_date = Report.date..dd.mm.yyyy.)

Empres_data_ASF <- Empres_data_ASF %>% 
  rename(country = Country)

Empres_data_ASF[Empres_data_ASF == "Wild Boar:sus Scrofa(Suidae)"] <- 'Sus scrofa'

pigFam <- as.list(read.csv("./data/pig_names_20231203.csv", sep=";", header=FALSE))
Empres_data_ASF$Species <- gsub("\\s*\\([^\\)]+\\)","",as.character(Empres_data_ASF$Species))
Empres_data_ASF_pigs <- Empres_data_ASF[Empres_data_ASF$Species %in% pigFam$V1[2],] #V1[3]Wildboar #V1[2]Swine
Empres_data_ASF_WildBoar <- Empres_data_ASF[Empres_data_ASF$Species %in% pigFam$V1[3],] #V1[3]Wildboar #V1[2]Swine

#we only want data from Europe (so filter using the europe shapefile) and data from 2018 and onwards
Empres_data_ASF_pigs <-Empres_data_ASF_pigs %>%
  filter(country %in% unique(europeanCountries$ADMIN),Outbreak_start_date >"31-12-2017")

Empres_data_ASF_WildBoar <-Empres_data_ASF_WildBoar %>%
  filter(country %in% unique(europeanCountries$ADMIN),Outbreak_start_date >"31-12-2017") 

Empres_pigs=Empres_data_ASF_pigs
Empres_boar=Empres_data_ASF_WildBoar

# pass them in to the function as the `coords` parameter
endDate <- max(Empres_data_ASF_pigs$Outbreak_start_date)
Empres_dat_ASF_sf_pigs <- st_as_sf(Empres_data_ASF_pigs, coords=c("Longitude","Latitude"),crs="EPSG:4326")

endDate <- max(Empres_data_ASF_WildBoar$Outbreak_start_date)
Empres_dat_ASF_sf_WildBoar <- st_as_sf(Empres_data_ASF_WildBoar, coords=c("Longitude","Latitude"),crs="EPSG:4326")

#-----We remove these countries because we noticed that it was not part of corine land cover 
RemoveCountries <- c("Ukraine", "Russian Federation", "Belarus")
Empres_dat_ASF_sf_pigs<- Empres_dat_ASF_sf_pigs[!Empres_dat_ASF_sf_pigs$country %in% RemoveCountries, ]

RemoveCountries <- c("Ukraine", "Russian Federation", "Belarus")
Empres_dat_ASF_sf_WildBoar<- Empres_dat_ASF_sf_WildBoar[!Empres_dat_ASF_sf_WildBoar$country %in% RemoveCountries, ]

#-----Merging these two database
WOEM_pigs = merge(WOAH_pigs, Empres_pigs, by = "country")
WOEM_boar = merge(WOAH_boar, Empres_boar, by = "country")

#Remove the NA's 
WOEM_pigs=WOEM_pigs[!is.na(WOEM_pigs$Outbreak_end_date),]
WOEM_boar=WOEM_boar[!is.na(WOEM_boar$Outbreak_end_date),]

WOEM_pigs <- WOEM_pigs %>% 
  rename(Longitude = Longitude.y,
         Latitude = Latitude.y)

WOEM_boar <- WOEM_boar %>% 
  rename(Longitude = Longitude.y,
         Latitude = Latitude.y)


#We needed to represent spatial vector data to include points (coordinates), 
#in view of that we added the their latitude and longitude
# For pigs only
endDate <- max(WOEM_pigs$Outbreak_start_date)
WOEM_pigs <- st_as_sf(WOEM_pigs, coords=c("Longitude","Latitude"),crs="EPSG:4326")

#We needed to represent spatial vector data to include points (coordinates), 
#in view of that we added the their latitude and longitude
# For wild boar only
endDate <- max(WOEM_boar$Outbreak_start_date)
WOEM_boar <- st_as_sf(WOEM_boar, coords=c("Longitude","Latitude"),crs="EPSG:4326")

#We remove these countries because we noticed that it was not part of corine land cover 
# For pigs only
RemoveCountries <- c("Ukraine","Russia", "Belarus")
WOEM_pigs <- WOEM_pigs[!WOEM_pigs$country %in% RemoveCountries, ]

#We remove these countries because we noticed that it was not part of corine land cover 
#For wild boar only
RemoveCountries <- c("Ukraine","Russia", "Belarus")
WOEM_boar<- WOEM_boar[!WOEM_boar$country %in% RemoveCountries, ]

#------------------------------------------------------
#Read in the bio climate and land cover data
prec<- rast("bio_12.tif") #Precipitation
temp<- rast("bio_1.tif") # Temperature
stock <-rast("Pg_density.tif") #Pig density
wild <-rast("wildboar_density.tif") # Wild boar density
cor <- rast("corine_rat_LEVEL2.tif") # Land cover
#-----------------------------------------------------------
#Plots 
tm_shape(prec/100) +
  tm_raster(title="Annual precipitation (ASF_Pigs)",palette="Mako") +
  tm_layout(legend.outside = TRUE) +
  tm_shape(europe_dat_ASF_sf_pigs)+ 
  tm_dots(col="black",shape = 2,size = 0.1)+
  tm_add_legend('symbol',
                col = "black",
                shape = 2,size = 0.3,
                labels ="WOAH")+
  tm_shape(Empres_dat_ASF_sf_pigs)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="deeppink",shape = 8,size = 0.1)+
  tm_add_legend('symbol',
                col = "deeppink",
                shape = 8,size = 0.3,
                labels="Empres_i")+
  tm_shape(WOEM_pigs)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="red",shape = 9,size = 0.1)+
  tm_add_legend('symbol',
                col = "red",
                shape = 9,size = 0.3,
                labels="Merged(WOAH&Empres_i)")

tm_shape(prec/100) +
  tm_raster(title="Annual precipitation (ASF_Wild Boar)",palette="Mako") +
  tm_shape(europe_dat_ASF_sf_WildBoar)+ 
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="black",shape = 2,size = 0.1)+
  tm_add_legend('symbol',
                col = "black",
                shape = 2,size = 0.3,
                labels="WOAH")+
  tm_shape(Empres_dat_ASF_sf_WildBoar)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="deeppink",shape = 8,size = 0.1)+
  tm_add_legend('symbol',
                col = "deeppink",
                shape = 8,size = 0.3,
                labels="Empres_i")+
  tm_shape(WOEM_boar)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="red",shape = 9,size = 0.1)+
  tm_add_legend('symbol',
                col = "red",
                shape = 9,size = 0.3,
                labels="Merged(WOAH&Empres_i)")

#-----------------------------------------------
tm_shape(stock) +
  tm_raster(title="Pigs density (ASF_Pigs)",palette = rev(hcl.colors(7, "ag_GrnYl"))) +
  tm_layout(legend.outside = TRUE) +
  tm_shape(europe_dat_ASF_sf_pigs)+ 
  tm_dots(col="black",shape = 2,size = 0.3)+
  tm_add_legend('symbol',
                col = "black",
                shape = 2,size = 0.3,
                labels="WOAH")+
  tm_shape(Empres_dat_ASF_sf_pigs)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="deeppink",shape = 8,size = 0.3)+
  tm_add_legend('symbol',
                col = "deeppink",
                shape = 8,size = 0.3,
                labels="Empres_i")+
  tm_shape(WOEM_pigs)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="red",shape = 9,size = 0.3)+
  tm_add_legend('symbol',
                col = "red",
                shape = 9,size = 0.3,
                labels="Merged(WOAH&Empres_i)")


tm_shape(wild) +
  tm_raster(title="Wild boar density (ASF_Wild Boar)",palette = rev(hcl.colors(7, "ag_GrnYl"))) +
  tm_layout(legend.outside = TRUE) +
  tm_shape(europe_dat_ASF_sf_WildBoar)+ 
  tm_dots(col="black",shape = 2,size = 0.3)+
  tm_add_legend('symbol',
                col = "black",
                shape = 2,size = 0.3,
                labels="WOAH")+
  tm_shape(Empres_dat_ASF_sf_WildBoar)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="deeppink",shape = 8,size = 0.3)+
  tm_add_legend('symbol',
                col = "deeppink",
                shape = 8,size = 0.3,
                labels="Empres_i")+
  tm_shape(WOEM_boar)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="red",shape = 9,size = 0.3)+
  tm_add_legend('symbol',
                col = "red",
                shape = 9,size = 0.3,
                labels="Merged(WOAH&Empres_i)")

#----------------------------------------------
tm_shape(temp/10) +
  tm_raster(title="Annual mean temperature (ASF_Pigs)", palette=rev(hcl.colors(7, "PinkYl"))) +
  tmap_options(max.categories = 44)+
  tm_shape(europe_dat_ASF_sf_pigs)+ 
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="black",shape = 2,size = 0.3)+
  tm_add_legend('symbol',
                col = "black",
                shape = 2,size = 0.3,
                labels="WOAH")+
  tm_shape(Empres_dat_ASF_sf_pigs)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="deeppink",shape = 8,size = 0.3)+
  tm_add_legend('symbol',
                col = "deeppink",
                shape = 8,size = 0.3,
                labels="Empres_i")+
  tm_shape(WOEM_pigs)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="red",shape = 9,size = 0.3)+
  tm_add_legend('symbol',
                col = "red",
                shape = 9,size = 0.3,
                labels="Merged(WOAH&Empres_i)")


tm_shape(temp/10) +
  tm_raster(title="Annual mean temperature (ASF_Wild Boar)", palette=rev(hcl.colors(7, "PinkYl"))) +
  tmap_options(max.categories = 44)+
  tm_shape(europe_dat_ASF_sf_WildBoar)+ 
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="black",shape = 2,size = 0.3)+
  tm_add_legend('symbol',
                col = "black",
                shape = 2,size = 0.3,
                labels="WOAH")+
  tm_shape(Empres_dat_ASF_sf_WildBoar)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="deeppink",shape = 8,size = 0.3)+
  tm_add_legend('symbol',
                col = "deeppink",
                shape = 8,size = 0.3,
                labels="Empres_i")+
  tm_shape(WOEM_boar)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="red",shape = 9,size = 0.3)+
  tm_add_legend('symbol',
                col = "red",
                shape = 9,size = 0.3,
                labels="Merged(WOAH&Empres_i)")

#----------------------------------------------
tm_shape(cor) +
  tm_raster(title="Land cover (ASF_Pigs)")+
  tmap_options(max.categories = 44)+
  tm_layout(legend.outside = TRUE) +
  tm_shape(europe_dat_ASF_sf_pigs)+ 
  tm_dots(col="black",shape = 2,size = 0.3)+
  tm_add_legend('symbol',
                col = "black",
                shape = 2,size = 0.3,
                labels="WOAH")+
  tm_shape(Empres_dat_ASF_sf_pigs)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="deeppink",shape = 8,size = 0.3)+
  tm_add_legend('symbol',
                col = "deeppink",
                shape = 8,size = 0.3,
                labels ="Empres_i")+
  tm_shape(WOEM_pigs)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="red",shape = 9,size = 0.3)+
  tm_add_legend('symbol',
                col = "red",
                shape = 9,size = 0.3,
                labels="Merged(WOAH&Empres_i)")


tm_shape(cor) +
  tm_raster(title="Land cover (ASF_Wild boar)")+
  tmap_options(max.categories = 44)+
  tm_layout(legend.outside = TRUE) +
  tm_shape(europe_dat_ASF_sf_WildBoar)+ 
  tm_dots(col="black",shape = 2,size = 0.3)+
  tm_add_legend('symbol',
                col = "black",
                shape = 2,size = 0.3,
                labels="WOAH")+
  tm_shape(Empres_dat_ASF_sf_WildBoar)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="deeppink",shape = 8,size = 0.3)+
  tm_add_legend('symbol',
                col = "deeppink",
                shape = 8,size = 0.3,
                labels="Empres_i")+
  tm_shape(WOEM_boar)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="red",shape = 9,size = 0.3)+
  tm_add_legend('symbol',
                col = "red",
                shape = 9,size = 0.3,
                labels="Merged(WOAH&Empres_i)")


#---------------------------------------------
#extract raster values to point locations where the diseases were found
#using the terra package
#WOAH
#to extract the livestock for wild boar density
stock <-terra::extract(stock, vect(europe_dat_ASF_sf_pigs))
europe_dat_ASF_sf_pigs$stock <- stock$Pg_density
#to extract the precipitation
precip <-terra::extract(prec, vect(europe_dat_ASF_sf_pigs))
europe_dat_ASF_sf_pigs$prec <- precip$bio_12/100
#to extract the temperature
tempe <-terra::extract(temp, vect(europe_dat_ASF_sf_pigs))
europe_dat_ASF_sf_pigs$temp <- tempe$bio_1/10
#to extract the of land cover (14 factors)
cover <-terra::extract(cor, vect(europe_dat_ASF_sf_pigs))
europe_dat_ASF_sf_pigs$cover <- cover$LABEL2

#WOAH
#to extract the wild boar density
wild <-terra::extract(wild, vect(europe_dat_ASF_sf_WildBoar))
europe_dat_ASF_sf_WildBoar$wild <- wild$wildboar_density
#to extract the precipitation
precip <-terra::extract(prec, vect(europe_dat_ASF_sf_WildBoar))
europe_dat_ASF_sf_WildBoar$prec <- precip$bio_12/100
#to extract the temperature
tempe <-terra::extract(temp, vect(europe_dat_ASF_sf_WildBoar))
europe_dat_ASF_sf_WildBoar$temp <- tempe$bio_1/10
#to extract the land cover (14 factors)
cover <-terra::extract(cor, vect(europe_dat_ASF_sf_WildBoar))
europe_dat_ASF_sf_WildBoar$cover <- cover$LABEL2


#Merged
#Pig density
stock <-rast("Pg_density.tif")
stocks <-terra::extract(stock, vect(WOEM_pigs))
WOEM_pigs$stock <- stocks$Pg_density
#precipitation
precip <-terra::extract(prec, vect(WOEM_pigs))
WOEM_pigs$prec <- precip$bio_12/100
#temperature
tempe <-terra::extract(temp, vect(WOEM_pigs))
WOEM_pigs$temp <- tempe$bio_1/10
#landcover
cover <-terra::extract(cor, vect(WOEM_pigs))
WOEM_pigs$cover <- cover$LABEL2

#Merged
#wild boar density
wild <-rast("wildboar_density.tif")
wilde <-terra::extract(wild, vect(WOEM_boar))
WOEM_boar$wild <- wilde$wildboar_density
#precipitation
precip <-terra::extract(prec, vect(WOEM_boar))
WOEM_boar$prec <- precip$bio_12/100
#temperature
tempe <-terra::extract(temp, vect(WOEM_boar))
WOEM_boar$temp <- tempe$bio_1/10
#landcover
cover <-terra::extract(cor, vect(WOEM_boar))
WOEM_boar$cover <- cover$LABEL2


#Empres-i
#Pig density
stock <-rast("Pg_density.tif")
stocks <-terra::extract(stock, vect(Empres_dat_ASF_sf_pigs))
Empres_dat_ASF_sf_pigs$stock <- stocks$Pg_density
#precipitation
precip <-terra::extract(prec, vect(Empres_dat_ASF_sf_pigs))
Empres_dat_ASF_sf_pigs$prec <- precip$bio_12/100
#temperature
tempe <-terra::extract(temp, vect(Empres_dat_ASF_sf_pigs))
Empres_dat_ASF_sf_pigs$temp <- tempe$bio_1/10
#landcover
cover <-terra::extract(cor, vect(Empres_dat_ASF_sf_pigs))
Empres_dat_ASF_sf_pigs$cover <- cover$LABEL2

#Empres-i
#wildl boar density
#wild <-rast("wildboar_density.tif")
wilde <-terra::extract(wild, vect(Empres_dat_ASF_sf_WildBoar))
Empres_dat_ASF_sf_WildBoar$wild <- wilde$wildboar_density
#precipitation
precip <-terra::extract(prec, vect(Empres_dat_ASF_sf_WildBoar))
Empres_dat_ASF_sf_WildBoar$prec <- precip$bio_12/100
#temperature
tempe <-terra::extract(temp, vect(Empres_dat_ASF_sf_WildBoar))
Empres_dat_ASF_sf_WildBoar$temp <- tempe$bio_1/10
#landcover
cover <-terra::extract(cor, vect(Empres_dat_ASF_sf_WildBoar))
Empres_dat_ASF_sf_WildBoar$cover <- cover$LABEL2

#------------------------------------------------------
#Checking for autocorrelation
fak=europe_dat_ASF_sf_pigs %>% 
  select(c(cases,prec,temp,stock))
faky=sf::st_drop_geometry(fak)
faki=faky
fa=na.omit(faki)
correlation <- cor(fa$cases, fa$stock, method = 'pearson') # Remember to change (a)biotic factor based what you want
correlation

fak1=europe_dat_ASF_sf_WildBoar %>% 
  select(c(cases,prec,temp,wild))
faky1=sf::st_drop_geometry(fak1)
faki1=faky1
fa1=na.omit(faki1)
correlation <- cor(fa1$cases, fa1$prec, method = 'pearson') # Remember to change (a)biotic factor based what you want
correlation

fak11=WOEM_boar %>% 
  select(c(cases,prec,temp,wild))
faky11=sf::st_drop_geometry(fak11)
faki11=faky11
fa11=na.omit(faki11)
correlation <- cor(fa11$cases, fa11$wild, method = 'pearson') # Remember to change (a)biotic factor based what you want
correlation

fak12=WOEM_pigs %>% 
  select(c(cases,prec,temp,stock))
faky12=sf::st_drop_geometry(fak12)
faki12=faky12
fa12=na.omit(faki12)
correlation <- cor(fa12$cases,fa12$temp,method = "pearson")# Remember to change (a)biotic factor based what you want
correlation
#------------------------------------------------------
#Univariate regression analysis for pigs
summary(lm(cases ~ stock, data = europe_dat_ASF_sf_pigs,family=nbinom1))
summary(lm(cases ~ temp, data = europe_dat_ASF_sf_pigs,family=nbinom1))
summary(lm(cases ~ prec, data = europe_dat_ASF_sf_pigs,family=nbinom1)) #only this is significant
#------------------------------------------------------
#Univariate regression analysis for wild boars
summary(lm(cases ~ wild, data = europe_dat_ASF_sf_WildBoar,family=nbinom1)) #only this is significant
summary(lm(cases ~ temp, data = europe_dat_ASF_sf_WildBoar,family=nbinom1)) #only this is significant
summary(lm(cases ~ prec, data = europe_dat_ASF_sf_WildBoar,family=nbinom1)) #only this is significant
#------------------------------------------------------
#Univariate regression analysis for merged pigs
summary(lm(cases ~ stock, data = WOEM_pigs,family=nbinom1))
summary(lm(cases ~ temp, data = WOEM_pigs,family=nbinom1)) #only this is significant
summary(lm(cases ~ prec, data = WOEM_pigs,family=nbinom1)) 
#------------------------------------------------------
#Univariate regression analysis for wild boars
summary(lm(cases ~ wild, data = WOEM_boar,family=nbinom1)) #only this is significant
summary(lm(cases ~ temp, data = WOEM_boar,family=nbinom1)) #only this is significant
summary(lm(cases ~ prec, data = WOEM_boar,family=nbinom1)) #only this is significant
#------------------------------------------------------

#Multivariate regression analysis for wild boars
library(glmmTMB)
library(DHARMa)
library(MuMIn)
#---Pigs---
#After check univariate regression analysis now proceed to do multivariate regression analysis:
europe_dat_ASF_sf_pigs1  <- europe_dat_ASF_sf_pigs %>% 
  dplyr::mutate(country = as.factor(country) ) %>% 
  dplyr::mutate(cover = as.factor(cover) ) %>% 
  dplyr::select(prec, cases, cover, country) %>% 
  na.omit()
  #as.data.frame()
#str(europe_dat_ASF_sf_pigs1)
m1<- glmmTMB(cases~prec+cover, ziformula=~ 0, data=europe_dat_ASF_sf_pigs1, family=nbinom1)
summary(m1)
m2<- glmmTMB(cases~prec+cover+(1|country), ziformula=~ 0, data=europe_dat_ASF_sf_pigs1, family=nbinom1)
summary(m2)

###########################
#retrieve coordinates in matrix form
europe_dat_ASF_sf_pigs2 = europe_dat_ASF_sf_pigs1
dat <- st_coordinates(europe_dat_ASF_sf_pigs2$geometry)
#Renaming
#dat <- dat %>% 
colnames(dat)[c(1, 2)] <- c("lon", "lat") 
residuals=resid(m2)
dat1 = cbind.data.frame(dat,residuals)
coordinates(dat1)<-c('lon','lat')
bubble(dat1,zcol='residuals')


europe_dat_ASF_sf_WildBoar2 = europe_dat_ASF_sf_WildBoar1
datw <- st_coordinates(europe_dat_ASF_sf_WildBoar2$geometry)
#Renaming
#dat <- dat %>% 
colnames(datw)[c(1, 2)] <- c("lon", "lat") 
residuals=resid(m4)
datw1 = cbind.data.frame(datw,residuals)
coordinates(datw1)<-c('lon','lat')
bubble(datw1,zcol='residuals')


WOEM_pigs2 = WOEM_pigs1
dat2 <- st_coordinates(WOEM_pigs2$geometry)
#Renaming
#dat <- dat %>% 
colnames(dat2)[c(1, 2)] <- c("lon", "lat") 
residuals=resid(m6)
dat3 = cbind.data.frame(dat2,residuals)
coordinates(dat3)<-c('lon','lat')
bubble(dat3,zcol='residuals')


WOEM_boar2 = WOEM_boar1
datw1 <- st_coordinates(WOEM_boar2$geometry)
#Renaming
#dat <- dat %>% 
colnames(datw1)[c(1, 2)] <- c("lon", "lat") 
residuals=resid(m8)
datw11 = cbind.data.frame(datw1,residuals)
coordinates(datw11)<-c('lon','lat')
bubble(datw11,zcol='residuals')

#------------------------------------
#Code for checking for spatial autocorrelation:

##check for autocorrelation
# extract residuals
Residuals<- resid(m1)
Residuals = data.frame(Residuals)
Residuals$ID <- 1:nrow(Residuals)
new <- europe_dat_ASF_sf_WildBoar
new$ID <- 1:nrow(europe_dat_ASF_sf_WildBoar)
#merge with original data

europe_dat_ASF_sf_WildBoar_new <- merge(new, Residuals, by ="ID")  #assuming you have an ID for each row in your dataset – else run YOURDATA$ID <- 1:nrow(YOURDATA)

#now plot to see if there are any spatial patterns in the residuals
tmap_mode("plot")
tm_shape(prec/100) +
  tm_polygons()+
  tm_shape(europe_dat_ASF_sf_WildBoar_new) + 
  tm_dots (col="Residuals",palette="Blues")
#------------------------------------------------------
#ASF_pigs
library(surveillance)
za_ASF_pigs = europe_data_ASF_pigs

#set all cases to 1, so we are counting number of outbreaks instead of number of birds affected
za_ASF_pigs$cases<- 1

## Dates and week numbers are tricky as they differ from year to year. Here we use iso 8601 to create the new variables - isoweek aned isoyear
za_ASF_pigs <-za_ASF_pigs %>% 
  mutate(isoweek=lubridate::isoweek(Outbreak_start_date)) %>% 
  mutate(isoyear=lubridate::isoyear(Outbreak_start_date))

# check if any observations with week 53 ?
za_ASF_pigs[which(za_ASF_pigs$isoweek == 53), ]

# In this data set, we have 140 observations within week 53 one from  January 2018. As the surveillance package needs a matrix of countries and week numbers, we cannot have week 53 in some years and not others. We also cannot create a week 53 for the remaining years and set it to zero observations as this week does not exist for those years. Instead, we force dates in week 53 to be either week 52 in 2020 (if in December 2020), week 1 in 2021 (if in January 2021)and so or week 1 in 2018.
for (i in 1:nrow(za_ASF_pigs)){
  if(za_ASF_pigs$isoweek[i]==53 & lubridate::month(za_ASF_pigs$Outbreak_start_date[i]) == 12){
    za_ASF_pigs$isoweek[i]<-52
    if(lubridate::year(za_ASF_pigs$Outbreak_start_date[i]) != za_ASF_pigs$isoyear[i]){
      za_ASF_pigs$isoyear[i] <- lubridate::year(za_ASF_pigs$Outbreak_start_date[i])
    }
  }
  else if(za_ASF_pigs$isoweek[i]==53 & lubridate::month(za_ASF_pigs$Outbreak_start_date[i]) == 1){
    za_ASF_pigs$isoweek[i]<-1
    if(lubridate::year(za_ASF_pigs$Outbreak_start_date[i]) != za_ASF_pigs$isoyear[i]){
      za_ASF_pigs$isoyear[i] <- lubridate::year(za_ASF_pigs$Outbreak_start_date[i])
    }
  }
}

## now aggregate per week per year per country
za_ASF_pigs_weekly <- za_ASF_pigs  %>% 
  group_by(iso_code, isoweek,isoyear) %>% summarise(no_outbreaks = sum(cases)) 


colnames(za_ASF_pigs_weekly)<- c("ADM0_A3", "Week", "Year","no_outbreaks")

#this methods fill in missing years for each country, by adding week 1 of the missing year (zero number of detections)
za_ASF_pigs_weekly <-za_ASF_pigs_weekly %>% 
  group_by(ADM0_A3) %>% 
  complete(Year = 2018:2023, fill = list(Week=1, no_outbreaks = 0))

#this method then add missing weeks to the data set and set number of outbreaks for the missing weeks to zero
za_ASF_pigs_weekly <-za_ASF_pigs_weekly %>% 
  group_by(ADM0_A3, Year) %>% 
  complete(Week = 1:52, fill = list(no_outbreaks = 0))

# Create data, neighborhood and covariate matrices ###
#first read in shapefile where water is added as polygons to account for countries with water between them still being connected in a bird's perspective
europeanCountries_water = st_read('./data/GIS/Europe_shapefiles_water.shp')

#now calculate neighborhood (matrix) and set column and row names
library(spdep)
za_ASF_pigs_adjmat <-nbOrder(poly2adjmat(europeanCountries_water,zero.policy = TRUE), maxlag = Inf)
colnames(za_ASF_pigs_adjmat) <- europeanCountries_water$ADM0_A3  # column names
rownames(za_ASF_pigs_adjmat) <- europeanCountries_water$ADM0_A3 # row names

#Now only keep countries in the neighborhood matrix where we have outbreak data and remove the newly created water connections as well
za_ASF_pigs_adjmat<-za_ASF_pigs_adjmat[rownames(za_ASF_pigs_adjmat)%in% unique(za_ASF_pigs_weekly$ADM0_A3),colnames(za_ASF_pigs_adjmat)%in% unique(za_ASF_pigs_weekly$ADM0_A3)]

#create shape file with only countries where we have data - this is needed for the model
europeanCountries.sub<-europeanCountries_water[europeanCountries_water$ADM0_A3 %in% unique(za_ASF_pigs_weekly$ADM0_A3),]
row.names(europeanCountries.sub)<- europeanCountries.sub$ADM0_A3

#convert sf object europeanCountries.sub to spatialpolygon dataframe as the sts class needs this format
europeanCountries.sub <- as(europeanCountries.sub, 'Spatial')

# Make sure that the country order in the data matrix we will create is the same order as in the neighborhood matrix 
adjmat_order <- colnames(za_ASF_pigs_adjmat)
keyDF <- data.frame(key=adjmat_order,weight=1:length(adjmat_order))
merged_ASF_pigs <- merge(za_ASF_pigs_weekly,keyDF,by.x='ADM0_A3',by.y='key',all.x=T,all.y=F)
res_ASF_pigs <- merged_ASF_pigs[order(merged_ASF_pigs$weight, merged_ASF_pigs$Year,merged_ASF_pigs$Week),c('ADM0_A3','Year', "Week", "no_outbreaks")]

#create data matrix to be used in hhh4 model in HPAI_hhh4_master.R
AI_ASF_pigs_weekly <-res_ASF_pigs %>%  pivot_wider(
  names_from = ADM0_A3,
  values_from = no_outbreaks,
  id_cols = c(Year, Week)) %>% 
  select(-Year,-Week) %>% 
  as.matrix()

#za_ASF_pigs_weekly 
### CONSTRUCTION OF CLASS STS USED IN ASF MODELS ###
#data_name <- za_ASF_pigs_weekly
subset_start <- 1
subset_end <- 312
AI_ASF_pigs_weekly1 <-  AI_ASF_pigs_weekly[subset_start:subset_end,,drop=FALSE]
start_w <- 1
start_Y <- 2018 


AI_ASF_pigs_sts <- sts(observed = AI_ASF_pigs_weekly1, start = c(as.numeric(start_Y), as.numeric(start_w)), 
              frequency = 52,neighbourhood = za_ASF_pigs_adjmat, map = europeanCountries.sub) 
AI_ASF_pigs_sts_twoWeeks <- aggregate(AI_ASF_pigs_sts, nfreq=26)

plot(AI_ASF_pigs_sts, type = observed ~ time, ylab="No. of reported cases",
     main = "Number of ASF_pigs cases in Europe")

plot(AI_ASF_pigs_sts_twoWeeks, type = observed ~ time, ylab="No. of reported cases")

#------------------------------------------------------
#ASF_Wild Boar
library(surveillance)
za_ASF_WildBoar = europe_data_ASF_WildBoar

#set all cases to 1, so we are counting number of outbreaks instead of number of birds affected
za_ASF_WildBoar$cases<- 1

## Dates and week numbers are tricky as they differ from year to year. Here we use iso 8601 to create the new variables - isoweek aned isoyear
za_ASF_WildBoar <-za_ASF_WildBoar %>% 
  mutate(isoweek=lubridate::isoweek(Outbreak_start_date)) %>% 
  mutate(isoyear=lubridate::isoyear(Outbreak_start_date))

# check if any observations with week 53 ?
za_ASF_WildBoar[which(za_ASF_WildBoar$isoweek == 53), ]

# In this data set, we have 140 observations within week 53 one from  January 2018. As the surveillance package needs a matrix of countries and week numbers, we cannot have week 53 in some years and not others. We also cannot create a week 53 for the remaining years and set it to zero observations as this week does not exist for those years. Instead, we force dates in week 53 to be either week 52 in 2020 (if in December 2020), week 1 in 2021 (if in January 2021)and so or week 1 in 2018.
for (i in 1:nrow(za_ASF_WildBoar)){
  if(za_ASF_WildBoar$isoweek[i]==53 & lubridate::month(za_ASF_WildBoar$Outbreak_start_date[i]) == 12){
    za_ASF_WildBoar$isoweek[i]<-52
    if(lubridate::year(za_ASF_WildBoar$Outbreak_start_date[i]) != za_ASF_WildBoar$isoyear[i]){
      za_ASF_WildBoar$isoyear[i] <- lubridate::year(za_ASF_WildBoar$Outbreak_start_date[i])
    }
  }
  else if(za_ASF_WildBoar$isoweek[i]==53 & lubridate::month(za_ASF_WildBoar$Outbreak_start_date[i]) == 1){
    za_ASF_WildBoar$isoweek[i]<-1
    if(lubridate::year(za_ASF_WildBoar$Outbreak_start_date[i]) != za_ASF_WildBoar$isoyear[i]){
      za_ASF_WildBoar$isoyear[i] <- lubridate::year(za_ASF_WildBoar$Outbreak_start_date[i])
    }
  }
}

## now aggregate per week per year per country
za_ASF_WildBoar_weekly <- za_ASF_WildBoar  %>% 
  group_by(iso_code, isoweek,isoyear) %>% summarise(no_outbreaks = sum(cases)) 

colnames(za_ASF_WildBoar_weekly)<- c("ADM0_A3", "Week", "Year","no_outbreaks")

#this methods fill in missing years for each country, by adding week 1 of the missing year (zero number of detections)
za_ASF_WildBoar_weekly <-za_ASF_WildBoar_weekly %>% 
  group_by(ADM0_A3) %>% 
  complete(Year = 2018:2023, fill = list(Week=1, no_outbreaks = 0))

#this method then add missing weeks to the data set and set number of outbreaks for the missing weeks to zero
za_ASF_WildBoar_weekly <-za_ASF_WildBoar_weekly %>% 
  group_by(ADM0_A3, Year) %>% 
  complete(Week = 1:52, fill = list(no_outbreaks = 0))


# Create data, neighborhood and covariate matrices ###
#first read in shapefile where water is added as polygons to account for countries with water between them still being connected in a bird's perspective
europeanCountries_water = st_read('./data/GIS/Europe_shapefiles_water.shp')

#now calculate neighborhood (matrix) and set column and row names
library(spdep)
za_ASF_WildBoar_adjmat <-nbOrder(poly2adjmat(europeanCountries_water,zero.policy = TRUE), maxlag = Inf)
colnames(za_ASF_WildBoar_adjmat) <- europeanCountries_water$ADM0_A3  # column names
rownames(za_ASF_WildBoar_adjmat) <- europeanCountries_water$ADM0_A3 # row names

#Now only keep countries in the neighborhood matrix where we have outbreak data and remove the newly created water connections as well
za_ASF_WildBoar_adjmat<-za_ASF_WildBoar_adjmat[rownames(za_ASF_WildBoar_adjmat)%in% unique(za_ASF_WildBoar_weekly$ADM0_A3),colnames(za_ASF_WildBoar_adjmat)%in% unique(za_ASF_WildBoar_weekly$ADM0_A3)]

#create shape file with only countries where we have data - this is needed for the model
europeanCountries.sub<-europeanCountries_water[europeanCountries_water$ADM0_A3 %in% unique(za_ASF_WildBoar_weekly$ADM0_A3),]
row.names(europeanCountries.sub)<- europeanCountries.sub$ADM0_A3

#convert sf object europeanCountries.sub to spatialpolygon dataframe as the sts class needs this format
europeanCountries.sub <- as(europeanCountries.sub, 'Spatial')

# Make sure that the country order in the data matrix we will create is the same order as in the neighborhood matrix 
adjmat_order <- colnames(za_ASF_WildBoar_adjmat)
keyDF <- data.frame(key=adjmat_order,weight=1:length(adjmat_order))
merged_ASF_WildBoar <- merge(za_ASF_WildBoar_weekly,keyDF,by.x='ADM0_A3',by.y='key',all.x=T,all.y=F)
res_ASF_WildBoar <- merged_ASF_WildBoar[order(merged_ASF_WildBoar$weight, merged_ASF_WildBoar$Year,merged_ASF_WildBoar$Week),c('ADM0_A3','Year', "Week", "no_outbreaks")]

#create data matrix to be used in hhh4 model in HPAI_hhh4_master.R
AI_ASF_WildBoar_weekly <-res_ASF_WildBoar %>%  pivot_wider(
  names_from = ADM0_A3,
  values_from = no_outbreaks,
  id_cols = c(Year, Week)) %>% 
  select(-Year,-Week) %>% 
  as.matrix()

### CONSTRUCTION OF CLASS STS USED IN ASF MODELS ###
#data_name <- za_ASF_WildBoar_weekly
subset_start <- 1
subset_end <- 312
AI_ASF_WildBoar_weekly1 <-  AI_ASF_WildBoar_weekly[subset_start:subset_end,,drop=FALSE]
start_w <- 1
start_Y <- 2018 


AI_ASF_WildBoar_sts <- sts(observed = AI_ASF_WildBoar_weekly1, start = c(as.numeric(start_Y), as.numeric(start_w)), 
                       frequency = 52,neighbourhood = za_ASF_WildBoar_adjmat, map = europeanCountries.sub) 
AI_ASF_WildBoar_sts_twoWeeks <- aggregate(AI_ASF_WildBoar_sts, nfreq=26)

plot(AI_ASF_WildBoar_sts, type = observed ~ time, ylab="No. of reported cases",
     main = "Number of ASF_Wild Boar cases in Europe")

plot(AI_ASF_WildBoar_sts_twoWeeks, type = observed ~ time, ylab="No. of reported cases")


#------------------------------------------------------
#ASF_pigs (WOAH&Empres_i)
library(surveillance)
za_ASF_WOEM_pigs = WOEM_pigs

za_ASF_WOEM_pigs <- za_ASF_WOEM_pigs %>% 
  rename(Outbreak_start_date = Outbreak_start_date.x,
         Species = Species.x,
         Reporting_date = Reporting_date.x,
         Longitude = Longitude.x,
         Latitude = Latitude.x)


#Remove the NA's 
za_ASF_WOEM_pigs=za_ASF_WOEM_pigs[!is.na(za_ASF_WOEM_pigs$Outbreak_start_date),]

#set all cases to 1, so we are counting number of outbreaks instead of number of birds affected
za_ASF_WOEM_pigs$cases<- 1
zad=za_ASF_WOEM_pigs
zaad = zad
za_ASF_WOEM_pigs=subset( zaad, select = -c(Species.y,Reporting_date.y, Outbreak_start_date.y) ) # WILL

#library(tidyr)
#zaad_newdf <- zaad %>% drop_na()

## Dates and week numbers are tricky as they differ from year to year. Here we use iso 8601 to create the new variables - isoweek aned isoyear
za_ASF_WOEM_pigs <-za_ASF_WOEM_pigs %>% 
  mutate(isoweek=lubridate::isoweek(Outbreak_start_date)) %>% 
  mutate(isoyear=lubridate::isoyear(Outbreak_start_date))

# check if any observations with week 53 ?
za_ASF_WOEM_pigs[which(za_ASF_WOEM_pigs$isoweek == 53), ]

# In this data set, we have 140 observations within week 53 one from  January 2018. As the surveillance package needs a matrix of countries and week numbers, we cannot have week 53 in some years and not others. We also cannot create a week 53 for the remaining years and set it to zero observations as this week does not exist for those years. Instead, we force dates in week 53 to be either week 52 in 2020 (if in December 2020), week 1 in 2021 (if in January 2021)and so or week 1 in 2018.

for (i in 1:nrow(za_ASF_WOEM_pigs)){
  if(za_ASF_WOEM_pigs$isoweek[i]==53 & lubridate::month(za_ASF_WOEM_pigs$Outbreak_start_date[i]) == 12){
    za_ASF_WOEM_pigs$isoweek[i]<-52
    if(lubridate::year(za_ASF_WOEM_pigs$Outbreak_start_date[i]) != za_ASF_WOEM_pigs$isoyear[i]){
      za_ASF_WOEM_pigs$isoyear[i] <- lubridate::year(za_ASF_WOEM_pigs$Outbreak_start_date[i])
    }
  }
  else if(za_ASF_WOEM_pigs$isoweek[i]==53 & lubridate::month(za_ASF_WOEM_pigs$Outbreak_start_date[i]) == 1){
    za_ASF_WOEM_pigs$isoweek[i]<-1
    if(lubridate::year(za_ASF_WOEM_pigs$Outbreak_start_date[i]) != za_ASF_WOEM_pigs$isoyear[i]){
      za_ASF_WOEM_pigs$isoyear[i] <- lubridate::year(za_ASF_WOEM_pigs$Outbreak_start_date[i])
    }
  }
}

## now aggregate per week per year per country
za_ASF_WOEM_pigs_weekly <- za_ASF_WOEM_pigs  %>% 
  group_by(iso_code, isoweek,isoyear) %>% summarise(no_outbreaks = sum(cases)) 

colnames(za_ASF_WOEM_pigs_weekly)<- c("ADM0_A3", "Week", "Year","no_outbreaks")

za_ASF_WOEM_pigs_weekly=za_ASF_WOEM_pigs_weekly %>%
  select("ADM0_A3", "Week", "Year","no_outbreaks")

#this methods fill in missing years for each country, by adding week 1 of the missing year (zero number of detections)
za_ASF_WOEM_pigs_weekly <-za_ASF_WOEM_pigs_weekly %>% 
  group_by(ADM0_A3) %>% 
  complete(Year = 2018:2023, fill = list(Week=1, no_outbreaks = 0))

#this method then add missing weeks to the data set and set number of outbreaks for the missing weeks to zero
za_ASF_WOEM_pigs_weekly <-za_ASF_WOEM_pigs_weekly %>% 
  group_by(ADM0_A3, Year) %>% 
  complete(Week = 1:52, fill = list(no_outbreaks = 0))


# Create data, neighborhood and covariate matrices ###
#first read in shapefile where water is added as polygons to account for countries with water between them still being connected in a bird's perspective
europeanCountries_water = st_read('./data/GIS/Europe_shapefiles_water.shp')

#now calculate neighborhood (matrix) and set column and row names
library(spdep)
za_ASF_WOEM_pigs_adjmat <-nbOrder(poly2adjmat(europeanCountries_water,zero.policy = TRUE), maxlag = Inf)
colnames(za_ASF_WOEM_pigs_adjmat) <- europeanCountries_water$ADM0_A3  # column names
rownames(za_ASF_WOEM_pigs_adjmat) <- europeanCountries_water$ADM0_A3 # row names

#Now only keep countries in the neighborhood matrix where we have outbreak data and remove the newly created water connections as well
za_ASF_WOEM_pigs_adjmat<-za_ASF_WOEM_pigs_adjmat[rownames(za_ASF_WOEM_pigs_adjmat)%in% unique(za_ASF_WOEM_pigs_weekly$ADM0_A3),colnames(za_ASF_WOEM_pigs_adjmat)%in% unique(za_ASF_WOEM_pigs_weekly$ADM0_A3)]

#create shape file with only countries where we have data - this is needed for the model
europeanCountries.sub<-europeanCountries_water[europeanCountries_water$ADM0_A3 %in% unique(za_ASF_WOEM_pigs_weekly$ADM0_A3),]
row.names(europeanCountries.sub)<- europeanCountries.sub$ADM0_A3

#convert sf object europeanCountries.sub to spatialpolygon dataframe as the sts class needs this format
europeanCountries.sub <- as(europeanCountries.sub, 'Spatial')

# Make sure that the country order in the data matrix we will create is the same order as in the neighborhood matrix 
adjmat_order <- colnames(za_ASF_WOEM_pigs_adjmat)
keyDF <- data.frame(key=adjmat_order,weight=1:length(adjmat_order))
merged_ASF_WOEM_pigs <- merge(za_ASF_WOEM_pigs_weekly,keyDF,by.x='ADM0_A3',by.y='key',all.x=T,all.y=F)
res_ASF_WOEM_pigs <- merged_ASF_WOEM_pigs[order(merged_ASF_WOEM_pigs$weight, merged_ASF_WOEM_pigs$Year,merged_ASF_WOEM_pigs$Week),c('ADM0_A3','Year', "Week", "no_outbreaks")]

#create data matrix to be used in hhh4 model in HPAI_hhh4_master.R
AI_ASF_WOEM_pigs_weekly <-res_ASF_WOEM_pigs %>%  pivot_wider(
  names_from = ADM0_A3,
  values_from = no_outbreaks,
  id_cols = c(Year, Week)) %>% 
  select(-Year,-Week) %>% 
  as.matrix()

### CONSTRUCTION OF CLASS STS USED IN ASF MODELS ###
#data_name <- za_ASF_WOEM_pigs_weekly
subset_start <- 1
subset_end <- 312
AI_ASF_WOEM_pigs_weekly1 <-  AI_ASF_WOEM_pigs_weekly[subset_start:subset_end,,drop=FALSE]
start_w <- 1
start_Y <- 2018 


AI_ASF_WOEM_pigs_sts <- sts(observed = AI_ASF_WOEM_pigs_weekly1, start = c(as.numeric(start_Y), as.numeric(start_w)), 
                       frequency = 52,neighbourhood = za_ASF_WOEM_pigs_adjmat, map = europeanCountries.sub) 
AI_ASF_WOEM_pigs_sts_twoWeeks <- aggregate(AI_ASF_WOEM_pigs_sts, nfreq=26)

plot(AI_ASF_WOEM_pigs_sts, type = observed ~ time, ylab="No. of reported cases",
     main = "Number of ASF_pigs cases within WOAH&Empres_i in Europe")

plot(AI_ASF_WOEM_pigs_sts_twoWeeks, type = observed ~ time, ylab="No. of reported cases")


#------------------------------------------------------
#ASF_pigs (WOAH&Empres_i)
library(surveillance)
za_ASF_WOEM_boar = WOEM_boar

za_ASF_WOEM_boar <- za_ASF_WOEM_boar %>% 
  rename(Outbreak_start_date = Outbreak_start_date.x,
         Species = Species.x,
         Reporting_date = Reporting_date.x,
         Longitude = Longitude.x,
         Latitude = Latitude.x)


#Remove the NA's 
za_ASF_WOEM_boar=za_ASF_WOEM_boar[!is.na(za_ASF_WOEM_boar$Outbreak_start_date),]

#set all cases to 1, so we are counting number of outbreaks instead of number of birds affected
za_ASF_WOEM_boar$cases<- 1
zad=za_ASF_WOEM_boar
zaad = zad
za_ASF_WOEM_boar=subset( zaad, select = -c(Species.y,Reporting_date.y, Outbreak_start_date.y) ) # WILL

#library(tidyr)
#zaad_newdf <- zaad %>% drop_na()

## Dates and week numbers are tricky as they differ from year to year. Here we use iso 8601 to create the new variables - isoweek aned isoyear
za_ASF_WOEM_boar <-za_ASF_WOEM_boar %>% 
  mutate(isoweek=lubridate::isoweek(Outbreak_start_date)) %>% 
  mutate(isoyear=lubridate::isoyear(Outbreak_start_date))

# check if any observations with week 53 ?
za_ASF_WOEM_boar[which(za_ASF_WOEM_boar$isoweek == 53), ]

# In this data set, we have 140 observations within week 53 one from  January 2018. As the surveillance package needs a matrix of countries and week numbers, we cannot have week 53 in some years and not others. We also cannot create a week 53 for the remaining years and set it to zero observations as this week does not exist for those years. Instead, we force dates in week 53 to be either week 52 in 2020 (if in December 2020), week 1 in 2021 (if in January 2021)and so or week 1 in 2018.

for (i in 1:nrow(za_ASF_WOEM_boar)){
  if(za_ASF_WOEM_boar$isoweek[i]==53 & lubridate::month(za_ASF_WOEM_boar$Outbreak_start_date[i]) == 12){
    za_ASF_WOEM_boar$isoweek[i]<-52
    if(lubridate::year(za_ASF_WOEM_boar$Outbreak_start_date[i]) != za_ASF_WOEM_boar$isoyear[i]){
      za_ASF_WOEM_boar$isoyear[i] <- lubridate::year(za_ASF_WOEM_boar$Outbreak_start_date[i])
    }
  }
  else if(za_ASF_WOEM_boar$isoweek[i]==53 & lubridate::month(za_ASF_WOEM_boar$Outbreak_start_date[i]) == 1){
    za_ASF_WOEM_boar$isoweek[i]<-1
    if(lubridate::year(za_ASF_WOEM_boar$Outbreak_start_date[i]) != za_ASF_WOEM_boar$isoyear[i]){
      za_ASF_WOEM_boar$isoyear[i] <- lubridate::year(za_ASF_WOEM_boar$Outbreak_start_date[i])
    }
  }
}

## now aggregate per week per year per country
za_ASF_WOEM_boar_weekly <- za_ASF_WOEM_boar  %>% 
  group_by(iso_code, isoweek,isoyear) %>% summarise(no_outbreaks = sum(cases)) 

colnames(za_ASF_WOEM_boar_weekly)<- c("ADM0_A3", "Week", "Year","no_outbreaks")

za_ASF_WOEM_boar_weekly=za_ASF_WOEM_boar_weekly %>%
  select("ADM0_A3", "Week", "Year","no_outbreaks")

#this methods fill in missing years for each country, by adding week 1 of the missing year (zero number of detections)
za_ASF_WOEM_boar_weekly <-za_ASF_WOEM_boar_weekly %>% 
  group_by(ADM0_A3) %>% 
  complete(Year = 2018:2023, fill = list(Week=1, no_outbreaks = 0))

#this method then add missing weeks to the data set and set number of outbreaks for the missing weeks to zero
za_ASF_WOEM_boar_weekly <-za_ASF_WOEM_boar_weekly %>% 
  group_by(ADM0_A3, Year) %>% 
  complete(Week = 1:52, fill = list(no_outbreaks = 0))


# Create data, neighborhood and covariate matrices ###
#first read in shapefile where water is added as polygons to account for countries with water between them still being connected in a bird's perspective
europeanCountries_water = st_read('./data/GIS/Europe_shapefiles_water.shp')

#now calculate neighborhood (matrix) and set column and row names
library(spdep)
za_ASF_WOEM_boar_adjmat <-nbOrder(poly2adjmat(europeanCountries_water,zero.policy = TRUE), maxlag = Inf)
colnames(za_ASF_WOEM_boar_adjmat) <- europeanCountries_water$ADM0_A3  # column names
rownames(za_ASF_WOEM_boar_adjmat) <- europeanCountries_water$ADM0_A3 # row names

#Now only keep countries in the neighborhood matrix where we have outbreak data and remove the newly created water connections as well
za_ASF_WOEM_boar_adjmat<-za_ASF_WOEM_boar_adjmat[rownames(za_ASF_WOEM_boar_adjmat)%in% unique(za_ASF_WOEM_boar_weekly$ADM0_A3),colnames(za_ASF_WOEM_boar_adjmat)%in% unique(za_ASF_WOEM_boar_weekly$ADM0_A3)]

#create shape file with only countries where we have data - this is needed for the model
europeanCountries.sub<-europeanCountries_water[europeanCountries_water$ADM0_A3 %in% unique(za_ASF_WOEM_boar_weekly$ADM0_A3),]
row.names(europeanCountries.sub)<- europeanCountries.sub$ADM0_A3

#convert sf object europeanCountries.sub to spatialpolygon dataframe as the sts class needs this format
europeanCountries.sub <- as(europeanCountries.sub, 'Spatial')

# Make sure that the country order in the data matrix we will create is the same order as in the neighborhood matrix 
adjmat_order <- colnames(za_ASF_WOEM_boar_adjmat)
keyDF <- data.frame(key=adjmat_order,weight=1:length(adjmat_order))
merged_ASF_WOEM_boar <- merge(za_ASF_WOEM_boar_weekly,keyDF,by.x='ADM0_A3',by.y='key',all.x=T,all.y=F)
res_ASF_WOEM_boar <- merged_ASF_WOEM_boar[order(merged_ASF_WOEM_boar$weight, merged_ASF_WOEM_boar$Year,merged_ASF_WOEM_boar$Week),c('ADM0_A3','Year', "Week", "no_outbreaks")]


#create a data matrix to be used in model 
AI_ASF_WOEM_boar_weekly <-res_ASF_WOEM_boar %>%  pivot_wider(
  names_from = ADM0_A3,
  values_from = no_outbreaks,
  id_cols = c(Year, Week)) %>% 
  select(-Year,-Week) %>% 
  as.matrix()

### CONSTRUCTION OF CLASS STS USED IN ASF MODELS ###
subset_start <- 1
subset_end <- 312
AI_ASF_WOEM_boar_weekly1 <-  AI_ASF_WOEM_boar_weekly[subset_start:subset_end,,drop=FALSE]
start_w <- 1
start_Y <- 2018 


AI_ASF_WOEM_boar_sts <- sts(observed = AI_ASF_WOEM_boar_weekly1, start = c(as.numeric(start_Y), as.numeric(start_w)), 
                            frequency = 52,neighbourhood = za_ASF_WOEM_boar_adjmat, map = europeanCountries.sub) 
AI_ASF_WOEM_boar_sts_twoWeeks <- aggregate(AI_ASF_WOEM_boar_sts, nfreq=26)

plot(AI_ASF_WOEM_boar_sts, type = observed ~ time, ylab="No. of reported cases",
     main = "Number of ASF_Wild Boar cases within WOAH&Empres_i in Europe")

plot(AI_ASF_WOEM_boar_sts_twoWeeks, type = observed ~ time, ylab="No. of reported cases")

