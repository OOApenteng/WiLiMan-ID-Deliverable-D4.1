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

setwd("Your Directory")
### READ IN newest OIE DATA ###
##link to where files are - the below code will pick the newest file in the folder"
df <- file.info(list.files("./data", full.names = T,pattern = "infur" )) # modded

europe_data<-read_excel("infur_20231124.xlsx")

#europe_data <-read_excel(rownames(df)[which.max(df$mtime)],sheet=2)
# READ IN SHAPEFILE OF THE STUDY REGION ###
europeanCountries = st_read('./data/GIS/Europe_shapefiles.shp')

# CLEAN AND PREPARE DATA---------------------------------

#make sure that outbreak start date is in date format
europe_data$Outbreak_start_date <- as.Date(europe_data$Outbreak_start_date, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))

#filter out other diseases than WNF
europe_data_WNF <- europe_data[europe_data$disease_eng %in% c("West Nile Fever"),]

#filter out other species than birds using the bird_names_20220517.csv file
birdFam <- as.list(read.csv("./data/bird_names_20220517.csv", sep=";", header=FALSE))
europe_data_WNF$Species <- gsub("\\s*\\([^\\)]+\\)","",as.character(europe_data_WNF$Species))
europe_data_WNF_birds <- europe_data_WNF[europe_data_WNF$Species %in% birdFam$V1[320:323],] #[320:323]#onlybirds
europe_data_WNF_horse <- europe_data_WNF[europe_data_WNF$Species %in% birdFam$V1[326],]#onlyhorse

#we only want data from Europe (so filter using the europe shapefile) and data between 2018 and 2023
#where the period we are using five years interval
# For birds only
europe_data_WNF_birds <-europe_data_WNF_birds %>%
  filter(iso_code %in% unique(europeanCountries$ADM0_A3),Outbreak_start_date >"2017-12-31")%>% 
  dplyr::filter(!is.na(cases)) %>% 
  dplyr::filter(!is.na(Longitude)) %>% 
  dplyr::filter(!is.na(Latitude)) 

#we only want data from Europe (so filter using the europe shapefile) and data between 2018 and 2023
#where the period we are using five years interval
# For horses only
europe_data_WNF_horse <-europe_data_WNF_horse %>%
  filter(iso_code %in% unique(europeanCountries$ADM0_A3),Outbreak_start_date >"2017-12-31")%>% 
  dplyr::filter(!is.na(cases)) %>% 
  dplyr::filter(!is.na(Longitude)) %>% 
  dplyr::filter(!is.na(Latitude)) 

WOAH_birds=europe_data_WNF_birds
WOAH_horse=europe_data_WNF_horse

#We needed to represent spatial vector data to include points (coordinates),
endDate <- max(europe_data_WNF_birds$Outbreak_start_date)
europe_dat_WNF_sf_birds <- st_as_sf(europe_data_WNF_birds, coords=c("Longitude","Latitude"),crs="EPSG:4326")

endDate <- max(europe_data_WNF_horse$Outbreak_start_date)
europe_dat_WNF_sf_horse <- st_as_sf(europe_data_WNF_horse, coords=c("Longitude","Latitude"),crs="EPSG:4326")

#-----We remove these countries because we noticed that it was not part of corine land cover 
RemoveCountries <- c("Ukraine","Russia", "Belarus")
europe_dat_WNF_sf_birds<- europe_dat_WNF_sf_birds[!europe_dat_WNF_sf_birds$country %in% RemoveCountries, ]

RemoveCountries <- c("Ukraine","Russia", "Belarus")
europe_dat_WNF_sf_horse<- europe_dat_WNF_sf_horse[!europe_dat_WNF_sf_horse$country %in% RemoveCountries, ]


#---Empres_I--from different database ---------------------------------###############
Empres_data_WNF <- read_csv("West_Nile_Virus_Birds.csv", skip=11)
Empres_data_WNF <- Empres_data_WNF %>% select_if(~ !any(is.na(.)))

#Renaming
Empres_data_WNF <- Empres_data_WNF %>% 
  rename(
    Outbreak_start_date = Observation.date..dd.mm.yyyy.,
    Reporting_date = Report.date..dd.mm.yyyy.)

Empres_data_WNF <- Empres_data_WNF %>% 
  rename(country = Country)

Empres_data_WNF[Empres_data_WNF == "Unspecified Bird"] <- 'Bird'
Empres_data_WNF[Empres_data_WNF == "Horse"] <- 'Equidae'

birdFam <- as.list(read.csv("./data/bird_names_20220517.csv", sep=";", header=FALSE))
Empres_data_WNF$Species <- gsub("\\s*\\([^\\)]+\\)","",as.character(Empres_data_WNF$Species))
Empres_data_WNF_birds <- Empres_data_WNF[Empres_data_WNF$Species %in% birdFam$V1[321],] #V1[321] Bird
Empres_data_WNF_horse <- Empres_data_WNF[Empres_data_WNF$Species %in% birdFam$V1[326],]#onlyhorse

#we only want data from Europe (so filter using the europe shapefile) and data from 2018 and onwards
Empres_data_WNF_birds <-Empres_data_WNF_birds %>%
  filter(country %in% unique(europeanCountries$ADMIN),Outbreak_start_date >"31-12-2017")

Empres_data_WNF_horse <-Empres_data_WNF_horse %>%
  filter(country %in% unique(europeanCountries$ADMIN),Outbreak_start_date >"31-12-2017")

Empres_birds=Empres_data_WNF_birds
Empres_horse=Empres_data_WNF_horse

# pass them in to the function as the `coords` parameter
endDate <- max(Empres_data_WNF_birds$Outbreak_start_date)
Empres_dat_WNF_sf_birds <- st_as_sf(Empres_data_WNF_birds, coords=c("Longitude","Latitude"),crs="EPSG:4326")

endDate <- max(Empres_data_WNF_horse$Outbreak_start_date)
Empres_dat_WNF_sf_horse <- st_as_sf(Empres_data_WNF_horse, coords=c("Longitude","Latitude"),crs="EPSG:4326")

#-----We remove these countries because we noticed that it was not part of corine land cover 
RemoveCountries <- c("Ukraine", "Russian Federation", "Belarus")
Empres_dat_WNF_sf_birds<- Empres_dat_WNF_sf_birds[!Empres_dat_WNF_sf_birds$country %in% RemoveCountries, ]

RemoveCountries <- c("Ukraine", "Russian Federation", "Belarus")
Empres_dat_WNF_sf_horse<- Empres_dat_WNF_sf_horse[!Empres_dat_WNF_sf_horse$country %in% RemoveCountries, ]

#-----Merging these two database
WOEM_birds = merge(WOAH_birds, Empres_birds, by = "country")
WOEM_horse = merge(WOAH_horse, Empres_horse, by = "country")

#Remove the NA's 
WOEM_birds=WOEM_birds[!is.na(WOEM_birds$Outbreak_end_date),]
WOEM_horse=WOEM_horse[!is.na(WOEM_horse$Outbreak_end_date),]


WOEM_birds <- WOEM_birds %>% 
  rename(Longitude = Longitude.y,
         Latitude = Latitude.y)

WOEM_horse <- WOEM_horse %>% 
  rename(Longitude = Longitude.y,
         Latitude = Latitude.y)

#We needed to represent spatial vector data to include points (coordinates), 
#in view of that we added the their latitude and longitude
# For birds only
endDate <- max(WOEM_birds$Outbreak_start_date)
WOEM_birds <- st_as_sf(WOEM_birds, coords=c("Longitude","Latitude"),crs="EPSG:4326")

#We needed to represent spatial vector data to include points (coordinates), 
#in view of that we added the their latitude and longitude
# For wild horse only
endDate <- max(WOEM_horse$Outbreak_start_date)
WOEM_horse <- st_as_sf(WOEM_horse, coords=c("Longitude","Latitude"),crs="EPSG:4326")

#We remove these countries because we noticed that it was not part of corine land cover 
# For pigs only
RemoveCountries <- c("Ukraine","Russia", "Belarus")
WOEM_birds <- WOEM_birds[!WOEM_birds$country %in% RemoveCountries, ]

#We remove these countries because we noticed that it was not part of corine land cover 
#For wild boar only
RemoveCountries <- c("Ukraine","Russia", "Belarus")
WOEM_horse <- WOEM_horse[!WOEM_horse$country %in% RemoveCountries, ]

################################################################
#Read in the (a)biotic feactors
prec<- rast("./data/bio_12.tif") #Precipitation
temp<- rast("./data/bio_1.tif") # Temperature
horses <-rast("./data/Ho_density.tif") # Horse density
cor <- rast("./data/corine_rat_LEVEL2.tif") # Land cover

#-------------------------------------------------------
#--Plots
tm_shape(prec/100) +
  tm_raster(title="Annual precipitation (WNF_birds)",palette="Mako") +
  tm_shape(europe_dat_WNF_sf_birds)+ 
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="black",shape = 2,size = 0.3)+
  tm_add_legend('symbol',
                col = "black",
                shape = 2,size = 0.3,
                labels="WOAH")+
  tm_shape(Empres_dat_WNF_sf_birds)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="deeppink",shape = 8,size = 0.3)+
  tm_add_legend('symbol',
                col = "deeppink",
                shape = 8,size = 0.3,
                labels="Empres_i")+
  tm_shape(WOEM_birds)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="red",shape = 9,size = 0.3)+
  tm_add_legend('symbol',
                col = "red",
                shape = 9,size = 0.3,
                labels="Merged(WOAH&Empres_i)")
  

tm_shape(prec/100) +
  tm_raster(title="Annual precipitation (WNF_Horse)",palette="Mako") +
  tm_shape(europe_dat_WNF_sf_horse)+ 
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="black",shape = 2,size = 0.3)+
  tm_add_legend('symbol',
                col = "black",
                shape = 2,size = 0.3,
                labels="WOAH")+
  tm_shape(Empres_dat_WNF_sf_horse)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="deeppink",shape = 8,size = 0.3)+
  tm_add_legend('symbol',
                col = "deeppink",
                shape = 8,size = 0.3,
                labels="Empres_i")+
  tm_shape(WOEM_horse)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="red",shape = 9,size = 0.3)+
  tm_add_legend('symbol',
                col = "red",
                shape = 9,size = 0.3,
                labels="Merged(WOAH&Empres_i)")
#-----------------------------------------------
tm_shape(temp/10) +
  tm_raster(title="Annual mean temperature (WNF_birds)", palette=rev(hcl.colors(7, "PinkYl"))) +
  tmap_options(max.categories = 44)+
  tm_shape(europe_dat_WNF_sf_birds)+ 
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="black",shape = 2,size = 0.3)+
  tm_add_legend('symbol',
                col = "black",
                shape = 2,size = 0.3,
                labels="WOAH")+
  tm_shape(Empres_dat_WNF_sf_birds)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="deeppink",shape = 8,size = 0.3)+
  tm_add_legend('symbol',
                col = "deeppink",
                shape = 8,size = 0.3,
                labels="Empres_i")+
  tm_shape(WOEM_birds)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="red",shape = 9,size = 0.3)+
  tm_add_legend('symbol',
                col = "red",
                shape = 9,size = 0.3,
                labels="Merged(WOAH&Empres_i)")

tm_shape(temp/10) +
  tm_raster(title="Annual mean temperature (WNF_Horse)", palette=rev(hcl.colors(7, "PinkYl"))) +
  tmap_options(max.categories = 44)+
  tm_shape(europe_dat_WNF_sf_horse)+ 
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="black",shape = 2,size = 0.3)+
  tm_add_legend('symbol',
                col = "black",
                shape = 2,size = 0.5,
                labels="WOAH")+
  tm_shape(Empres_dat_WNF_sf_horse)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="deeppink",shape = 8,size = 0.3)+
  tm_add_legend('symbol',
                col = "deeppink",
                shape = 8,size = 0.3,
                labels="Empres_i")+
  tm_shape(WOEM_horse)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="red",shape = 9,size = 0.3)+
  tm_add_legend('symbol',
                col = "red",
                shape = 9,size = 0.3,
                labels="Merged(WOAH&Empres_i)")
#----------------------------------------------
tm_shape(horses) +
  tm_raster(title="Horses density (WNF_Horse)",palette = rev(hcl.colors(7, "ag_GrnYl"))) +
  tm_layout(legend.outside = TRUE) +
  tm_shape(europe_dat_WNF_sf_horse)+ 
  tm_dots(col="black",shape = 2,size = 0.3)+
  tm_add_legend('symbol',
                col = "black",
                shape = 2,size = 0.3,
                labels="WOAH")+
  tm_shape(Empres_dat_WNF_sf_horse)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="deeppink",shape = 8,size = 0.3)+
  tm_add_legend('symbol',
                col = "deeppink",
                shape = 8,size = 0.3,
                labels="Empres_i")+
  tm_shape(WOEM_horse)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="red",shape = 9,size = 0.3)+
  tm_add_legend('symbol',
                col = "red",
                shape = 9,size = 0.3,
                labels="Merged(WOAH&Empres_i)")
#----------------------------------------------
tm_shape(cor) +
  tm_raster(title="Land cover (WNF_birds)")+
  tmap_options(max.categories = 44)+
  tm_shape(europe_dat_WNF_sf_birds)+ 
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="black",shape = 2,size = 0.3)+
  tm_add_legend('symbol',
                col = "black",
                shape = 2,size = 0.3,
                labels="WOAH")+
  tm_shape(Empres_dat_WNF_sf_birds)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="deeppink",shape = 8,size = 0.3)+
  tm_add_legend('symbol',
                col = "deeppink",
                shape = 8,size = 0.3,
                labels="Empres_i")+
  tm_shape(WOEM_birds)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="red",shape = 9,size = 0.3)+
  tm_add_legend('symbol',
                col = "red",
                shape = 9,size = 0.3,
                labels="Merged(WOAH&Empres_i)")

tm_shape(cor) +
  tm_raster(title="Land cover (WNF_Horse)")+
  tmap_options(max.categories = 44)+
  tm_layout(legend.outside = TRUE) +
  tm_shape(europe_dat_WNF_sf_horse)+ 
  tm_dots(col="black",shape = 2,size = 0.09)+
  tm_add_legend('symbol',
                col = "black",
                shape = 2,size = 0.3,
                labels="WOAH")+
  tm_shape(Empres_dat_WNF_sf_horse)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="deeppink",shape = 8,size = 0.3)+
  tm_add_legend('symbol',
                col = "deeppink",
                shape = 8,size = 0.3,
                labels="Empres_i")+
  tm_shape(WOEM_horse)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="red",shape = 9,size = 0.3)+
  tm_add_legend('symbol',
                col = "red",
                shape = 9,size = 0.3,
                labels="Merged(WOAH&Empres_i)")
#---------------------------------------------
#extract raster values to point locations using the terra package
#WOAH
#precipitation
precip <-terra::extract(prec, vect(europe_dat_WNF_sf_birds))
europe_dat_WNF_sf_birds$prec <- precip$bio_12/100
#temperature
tempe <-terra::extract(temp, vect(europe_dat_WNF_sf_birds))
europe_dat_WNF_sf_birds$temp <- tempe$bio_1/10
#landcover
cover <-terra::extract(cor, vect(europe_dat_WNF_sf_birds))
europe_dat_WNF_sf_birds$cover <- cover$LABEL2

#WOAH
#to extract the information for horse density
horses <-terra::extract(horses, vect(europe_dat_WNF_sf_horse))
europe_dat_WNF_sf_horse$horses <- horses$Ho_density
#precipitation
precip <-terra::extract(prec, vect(europe_dat_WNF_sf_horse))
europe_dat_WNF_sf_horse$prec <- precip$bio_12/100
#temperature
tempe <-terra::extract(temp, vect(europe_dat_WNF_sf_horse))
europe_dat_WNF_sf_horse$temp <- tempe$bio_1/10
#landcover
cover <-terra::extract(cor, vect(europe_dat_WNF_sf_horse))
europe_dat_WNF_sf_horse$cover <- cover$LABEL2

#Merged
#precipitation
precip <-terra::extract(prec, vect(WOEM_birds))
WOEM_birds$prec <- precip$bio_12/100
#temperature
tempe <-terra::extract(temp, vect(WOEM_birds))
WOEM_birds$temp <- tempe$bio_1/10
#landcover
cover <-terra::extract(cor, vect(WOEM_birds))
WOEM_birds$cover <- cover$LABEL2


#Merged 
#to extract the information for horse density
horses <-rast("Ho_density.tif")
horses <-terra::extract(horses, vect(WOEM_horse))
WOEM_horse$horses <- horses$Ho_density
#precipitation
precip <-terra::extract(prec, vect(WOEM_horse))
WOEM_horse$prec <- precip$bio_12/100
#temperature
tempe <-terra::extract(temp, vect(WOEM_horse))
WOEM_horse$temp <- tempe$bio_1/10
#landcover
cover <-terra::extract(cor, vect(WOEM_horse))
WOEM_horse$cover <- cover$LABEL2

#Empres-i
#precipitation
precip <-terra::extract(prec, vect(Empres_dat_WNF_sf_birds))
Empres_dat_WNF_sf_birds$prec <- precip$bio_12/100
#temperature
tempe <-terra::extract(temp, vect(Empres_dat_WNF_sf_birds))
Empres_dat_WNF_sf_birds$temp <- tempe$bio_1/10
#landcover
cover <-terra::extract(cor, vect(Empres_dat_WNF_sf_birds))
Empres_dat_WNF_sf_birds$cover <- cover$LABEL2

#Empres-i
#to extract the information for horse density
horses <-rast("Ho_density.tif")
horses <-terra::extract(horses, vect(Empres_dat_WNF_sf_horse))
Empres_dat_WNF_sf_horse$horses <- horses$Ho_density
#precipitation
precip <-terra::extract(prec, vect(Empres_dat_WNF_sf_horse))
Empres_dat_WNF_sf_horse$prec <- precip$bio_12/100
#temperature
tempe <-terra::extract(temp, vect(Empres_dat_WNF_sf_horse))
Empres_dat_WNF_sf_horse$temp <- tempe$bio_1/10
#landcover
cover <-terra::extract(cor, vect(Empres_dat_WNF_sf_horse))
Empres_dat_WNF_sf_horse$cover <- cover$LABEL2
#------------------------------------------------------
#Checking for autocorrelation
fakb=europe_dat_WNF_sf_birds %>% 
  select(c(cases,prec,temp))
fakyb=sf::st_drop_geometry(fakb)
fakib=fakyb
fab=na.omit(fakib)
correlation <- cor(fab$cases, fab$temp, method = 'pearson') #Change here based on the (a)biotic factors you want to check 
correlation

fakw=europe_dat_WNF_sf_horse %>% 
  select(c(cases,prec,temp,horses))
fakyw=sf::st_drop_geometry(fakw)
fakiw=fakyw
faw=na.omit(fakiw)
correlation <- cor(faw$cases, faw$temp, method = 'pearson') #Change here based on the (a)biotic factors you want to check 
correlation

fake=WOEM_birds %>% 
  select(c(cases,prec,temp))
fakye=sf::st_drop_geometry(fake)
fakie=fakye
fae=na.omit(fakie)
correlation <- cor(fae$cases, fae$prec, method = 'pearson') #Change here based on the (a)biotic factors you want to check 
correlation

fake=WOEM_horse %>% 
  select(c(cases,prec,temp,horses))
fakye=sf::st_drop_geometry(fake)
fakie=fakye
fae=na.omit(fakie)
correlation <- cor(fae$cases, fae$horses, method = 'pearson') #Change here based on the (a)biotic factors you want to check 
correlation
#------------------------------------------------------
#Univariate regression analysis for birds
summary(lm(cases ~ temp, data = europe_dat_WNF_sf_birds,family=nbinom1))
summary(lm(cases ~ prec, data = europe_dat_WNF_sf_birds,family=nbinom1)) 
#------------------------------------------------------
#Univariate regression analysis for horses
summary(lm(cases ~ horses, data = europe_dat_WNF_sf_horse,family=nbinom1))
summary(lm(cases ~ temp, data = europe_dat_WNF_sf_horse,family=nbinom1)) #only this is significant
summary(lm(cases ~ prec, data = europe_dat_WNF_sf_horse,family=nbinom1)) 
#------------------------------------------------------
#Univariate regression analysis for merged pigs
summary(lm(cases ~ horses, data = WOEM_horse,family=nbinom1)) #only this is significant
summary(lm(cases ~ temp, data = WOEM_horse,family=nbinom1)) #only this is significant
summary(lm(cases ~ prec, data = WOEM_horse,family=nbinom1)) #only this is significant
#------------------------------------------------------
#Multivariate regression analysis for wild boars
library(glmmTMB)
library(DHARMa)
library(MuMIn)
#---Wild boar---
#After check univariate regression analysis now proceed to do multivariate regression analysis:
WOEM_horse1  <- WOEM_horse %>% 
  dplyr::mutate(country = as.factor(country) ) %>% 
  dplyr::mutate(cover = as.factor(cover) ) %>% 
  dplyr::select(prec, horses, cases, cover, country) %>% 
  na.omit()

m9<- glmmTMB(cases~prec+horses+cover, ziformula=~ 0, data=WOEM_horse1, family=nbinom1)
summary(m9)
m10<- glmmTMB(cases~prec+horses+ cover+(1|country), ziformula=~ 0, data=WOEM_horse1, family=nbinom1)
summary(m10)
#-----------------------------------------------------
#retrieve coordinates in matrix form
WOEM_horse2 = WOEM_horse1
datw1 <- st_coordinates(WOEM_horse2$geometry)
#Renaming
#dat <- dat %>% 
colnames(datw1)[c(1, 2)] <- c("lon", "lat") 
residuals=resid(m10)
datw21 = cbind.data.frame(datw1,residuals)
coordinates(datw21)<-c('lon','lat')
bubble(datw21,zcol='residuals')
#------------------------------------------------------
#WNF_horses
library(surveillance)
za_WNF_horse = europe_data_WNF_horse

#set all cases to 1, so we are counting number of outbreaks instead of number of birds affected
za_WNF_horse$cases<- 1

## Dates and week numbers are tricky as they differ from year to year. Here we use iso 8601 to create the new variables - isoweek aned isoyear
za_WNF_horse <-za_WNF_horse %>% 
  mutate(isoweek=lubridate::isoweek(Outbreak_start_date)) %>% 
  mutate(isoyear=lubridate::isoyear(Outbreak_start_date))

# check if any observations with week 53 ?
za_WNF_horse[which(za_WNF_horse$isoweek == 53), ]

# In this data set, we have 140 observations within week 53 one from  January 2018. As the surveillance package needs a matrix of countries and week numbers, we cannot have week 53 in some years and not others. We also cannot create a week 53 for the remaining years and set it to zero observations as this week does not exist for those years. Instead, we force dates in week 53 to be either week 52 in 2020 (if in December 2020), week 1 in 2021 (if in January 2021) and so or week 1 in 2018.

for (i in 1:nrow(za_WNF_horse)){
  if(za_WNF_horse$isoweek[i]==53 & lubridate::month(za_WNF_horse$Outbreak_start_date[i]) == 12){
    za_WNF_horse$isoweek[i]<-52
    if(lubridate::year(za_WNF_horse$Outbreak_start_date[i]) != za_WNF_horse$isoyear[i]){
      za_WNF_horse$isoyear[i] <- lubridate::year(za_WNF_horse$Outbreak_start_date[i])
    }
  }
  else if(za_WNF_horse$isoweek[i]==53 & lubridate::month(za_WNF_horse$Outbreak_start_date[i]) == 1){
    za_WNF_horse$isoweek[i]<-1
    if(lubridate::year(za_WNF_horse$Outbreak_start_date[i]) != za_WNF_horse$isoyear[i]){
      za_WNF_horse$isoyear[i] <- lubridate::year(za_WNF_horse$Outbreak_start_date[i])
    }
  }
}

## now aggregate per week per year per country
za_WNF_horse_weekly <- za_WNF_horse  %>% 
  group_by(iso_code, isoweek,isoyear) %>% summarise(no_outbreaks = sum(cases)) 

colnames(za_WNF_horse_weekly)<- c("ADM0_A3", "Week", "Year","no_outbreaks")

#this methods fill in missing years for each country, by adding week 1 of the missing year (zero number of detections)
za_WNF_horse_weekly <-za_WNF_horse_weekly %>% 
  group_by(ADM0_A3) %>% 
  complete(Year = 2018:2023, fill = list(Week=1, no_outbreaks = 0))

#this method then add missing weeks to the data set and set number of outbreaks for the missing weeks to zero
za_WNF_horse_weekly <-za_WNF_horse_weekly %>% 
  group_by(ADM0_A3, Year) %>% 
  complete(Week = 1:52, fill = list(no_outbreaks = 0))


# Create data, neighborhood and covariate matrices ###
#first read in shapefile where water is added as polygons to account for countries with water between them still being connected in a bird's perspective
europeanCountries_water = st_read('./data/GIS/Europe_shapefiles_water.shp')

#now calculate neighborhood (matrix) and set column and row names
library(spdep)
za_WNF_horse_adjmat <-nbOrder(poly2adjmat(europeanCountries_water,zero.policy = TRUE), maxlag = Inf)
colnames(za_WNF_horse_adjmat) <- europeanCountries_water$ADM0_A3  # column names
rownames(za_WNF_horse_adjmat) <- europeanCountries_water$ADM0_A3 # row names

#Now only keep countries in the neighborhood matrix where we have outbreak data and remove the newly created water connections as well
za_WNF_horse_adjmat<-za_WNF_horse_adjmat[rownames(za_WNF_horse_adjmat)%in% unique(za_WNF_horse_weekly$ADM0_A3),colnames(za_WNF_horse_adjmat)%in% unique(za_WNF_horse_weekly$ADM0_A3)]

#create shape file with only countries where we have data - this is needed for the model
europeanCountries.sub<-europeanCountries_water[europeanCountries_water$ADM0_A3 %in% unique(za_WNF_horse_weekly$ADM0_A3),]
row.names(europeanCountries.sub)<- europeanCountries.sub$ADM0_A3

#convert sf object europeanCountries.sub to spatialpolygon dataframe as the sts class needs this format
europeanCountries.sub <- as(europeanCountries.sub, 'Spatial')

# Make sure that the country order in the data matrix we will create is the same order as in the neighborhood matrix 
adjmat_order <- colnames(za_WNF_horse_adjmat)
keyDF <- data.frame(key=adjmat_order,weight=1:length(adjmat_order))
merged_WNF_horse <- merge(za_WNF_horse_weekly,keyDF,by.x='ADM0_A3',by.y='key',all.x=T,all.y=F)
res_WNF_horse <- merged_WNF_horse[order(merged_WNF_horse$weight, merged_WNF_horse$Year,merged_WNF_horse$Week),c('ADM0_A3','Year', "Week", "no_outbreaks")]


#create a data matrix to be used in model 
AI_WNF_horse_weekly <-res_WNF_horse %>%  pivot_wider(
  names_from = ADM0_A3,
  values_from = no_outbreaks,
  id_cols = c(Year, Week)) %>% 
  select(-Year,-Week) %>% 
  as.matrix()

#za_WNF_horse_weekly 
### CONSTRUCTION OF CLASS STS USED IN ASF MODELS ###
subset_start <- 1
subset_end <- 312
AI_WNF_horse_weekly1 <-  AI_WNF_horse_weekly[subset_start:subset_end,,drop=FALSE]
start_w <- 1
start_Y <- 2018 


AI_WNF_horse_sts <- sts(observed = AI_WNF_horse_weekly1, start = c(as.numeric(start_Y), as.numeric(start_w)), 
                       frequency = 52,neighbourhood = za_WNF_horse_adjmat, map = europeanCountries.sub) 
AI_WNF_horse_sts_twoWeeks <- aggregate(AI_WNF_horse_sts, nfreq=26)

plot(AI_WNF_horse_sts, type = observed ~ time, ylab="No. of reported cases",
     main = "Number of WNF_Horse cases in Europe")

plot(AI_WNF_horse_sts_twoWeeks, type = observed ~ time, ylab="No. of reported cases")

#------------------------------------------------------
#WNF_Bird
library(surveillance)
za_WNF_birds = europe_data_WNF_birds

#set all cases to 1, so we are counting number of outbreaks instead of number of birds affected
za_WNF_birds$cases<- 1

## Dates and week numbers are tricky as they differ from year to year. Here we use iso 8601 to create the new variables - isoweek aned isoyear
za_WNF_birds <-za_WNF_birds %>% 
  mutate(isoweek=lubridate::isoweek(Outbreak_start_date)) %>% 
  mutate(isoyear=lubridate::isoyear(Outbreak_start_date))

# check if any observations with week 53 ?
za_WNF_birds[which(za_WNF_birds$isoweek == 53), ]

# In this data set, we have 140 observations within week 53 from 2020/2021 and one from  January 2016. As the surveillance package needs a matrix of countries and week numbers, we cannot have week 53 in some years and not others. We also cannot create a week 53 for the remaining years and set it to zero observations as this week does not exist for those years. Instead, we force dates in week 53 to be either week 52 in 2020 (if in December 2020), week 1 in 2021 (if in January 2021) or week 1 in 2016.

for (i in 1:nrow(za_WNF_birds)){
  if(za_WNF_birds$isoweek[i]==53 & lubridate::month(za_WNF_birds$Outbreak_start_date[i]) == 12){
    za_WNF_birds$isoweek[i]<-52
    if(lubridate::year(za_WNF_birds$Outbreak_start_date[i]) != za_WNF_birds$isoyear[i]){
      za_WNF_birds$isoyear[i] <- lubridate::year(za_WNF_birds$Outbreak_start_date[i])
    }
  }
  else if(za_WNF_birds$isoweek[i]==53 & lubridate::month(za_WNF_birds$Outbreak_start_date[i]) == 1){
    za_WNF_birds$isoweek[i]<-1
    if(lubridate::year(za_WNF_birds$Outbreak_start_date[i]) != za_WNF_birds$isoyear[i]){
      za_WNF_birds$isoyear[i] <- lubridate::year(za_WNF_birds$Outbreak_start_date[i])
    }
  }
}

## now aggregate per week per year per country
za_WNF_birds_weekly <- za_WNF_birds  %>% 
  group_by(iso_code, isoweek,isoyear) %>% summarise(no_outbreaks = sum(cases)) 


colnames(za_WNF_birds_weekly)<- c("ADM0_A3", "Week", "Year","no_outbreaks")

#this methods fill in missing years for each country, by adding week 1 of the missing year (zero number of detections)
za_WNF_birds_weekly <-za_WNF_birds_weekly %>% 
  group_by(ADM0_A3) %>% 
  complete(Year = 2018:2023, fill = list(Week=1, no_outbreaks = 0))

#this method then add missing weeks to the data set and set number of outbreaks for the missing weeks to zero
za_WNF_birds_weekly <-za_WNF_birds_weekly %>% 
  group_by(ADM0_A3, Year) %>% 
  complete(Week = 1:52, fill = list(no_outbreaks = 0))


# Create data, neighborhood and covariate matrices ###
#first read in shapefile where water is added as polygons to account for countries with water between them still being connected in a bird's perspective
europeanCountries_water = st_read('./data/GIS/Europe_shapefiles_water.shp')

#now calculate neighborhood (matrix) and set column and row names
library(spdep)
za_WNF_birds_adjmat <-nbOrder(poly2adjmat(europeanCountries_water,zero.policy = TRUE), maxlag = Inf)
colnames(za_WNF_birds_adjmat) <- europeanCountries_water$ADM0_A3  # column names
rownames(za_WNF_birds_adjmat) <- europeanCountries_water$ADM0_A3 # row names

#Now only keep countries in the neighborhood matrix where we have outbreak data and remove the newly created water connections as well
za_WNF_birds_adjmat<-za_WNF_birds_adjmat[rownames(za_WNF_birds_adjmat)%in% unique(za_WNF_birds_weekly$ADM0_A3),colnames(za_WNF_birds_adjmat)%in% unique(za_WNF_birds_weekly$ADM0_A3)]

#create shape file with only countries where we have data - this is needed for the model
europeanCountries.sub<-europeanCountries_water[europeanCountries_water$ADM0_A3 %in% unique(za_WNF_birds_weekly$ADM0_A3),]
row.names(europeanCountries.sub)<- europeanCountries.sub$ADM0_A3

#convert sf object europeanCountries.sub to spatialpolygon dataframe as the sts class needs this format
europeanCountries.sub <- as(europeanCountries.sub, 'Spatial')

# Make sure that the country order in the data matrix we will create is the same order as in the neighborhood matrix 
adjmat_order <- colnames(za_WNF_birds_adjmat)
keyDF <- data.frame(key=adjmat_order,weight=1:length(adjmat_order))
merged_WNF_birds <- merge(za_WNF_birds_weekly,keyDF,by.x='ADM0_A3',by.y='key',all.x=T,all.y=F)
res_WNF_birds <- merged_WNF_birds[order(merged_WNF_birds$weight, merged_WNF_birds$Year,merged_WNF_birds$Week),c('ADM0_A3','Year', "Week", "no_outbreaks")]


#create a data matrix to be used in the model 
AI_WNF_birds_weekly <-res_WNF_birds %>%  pivot_wider(
  names_from = ADM0_A3,
  values_from = no_outbreaks,
  id_cols = c(Year, Week)) %>% 
  select(-Year,-Week) %>% 
  as.matrix()

### CONSTRUCTION OF CLASS STS USED IN ASF MODELS ###
subset_start <- 1
subset_end <- 312
AI_WNF_birds_weekly1 <-  AI_WNF_birds_weekly[subset_start:subset_end,,drop=FALSE]
start_w <- 1
start_Y <- 2018 


AI_WNF_birds_sts <- sts(observed = AI_WNF_birds_weekly1, start = c(as.numeric(start_Y), as.numeric(start_w)), 
                        frequency = 52,neighbourhood = za_WNF_birds_adjmat, map = europeanCountries.sub) 
AI_WNF_birds_sts_twoWeeks <- aggregate(AI_WNF_birds_sts, nfreq=26)

plot(AI_WNF_birds_sts, type = observed ~ time, ylab="No. of reported cases",
     main = "Number of WNF_Birds cases in Europe")

plot(AI_WNF_birds_sts_twoWeeks, type = observed ~ time, ylab="No. of reported cases")

#------------------------------------------------------
#WNF_horse (WOAH&Empres_i)
library(surveillance)
za_WNF_WOEM_horse = WOEM_horse

za_WNF_WOEM_horse <- za_WNF_WOEM_horse %>% 
  rename(Outbreak_start_date = Outbreak_start_date.x,
         Species = Species.x,
         Reporting_date = Reporting_date.x,
         Longitude = Longitude.x,
         Latitude = Latitude.x)


#Remove the NA's 
za_WNF_WOEM_horse=za_WNF_WOEM_horse[!is.na(za_WNF_WOEM_horse$Outbreak_start_date),]

#set all cases to 1, so we are counting number of outbreaks instead of number of birds affected
za_WNF_WOEM_horse$cases<- 1
zad=za_WNF_WOEM_horse
zaad = zad
za_WNF_WOEM_horse=subset( zaad, select = -c(Species.y,Reporting_date.y, Outbreak_start_date.y) ) # WILL


## Dates and week numbers are tricky as they differ from year to year. Here we use iso 8601 to create the new variables - isoweek aned isoyear
za_WNF_WOEM_horse <-za_WNF_WOEM_horse %>% 
  mutate(isoweek=lubridate::isoweek(Outbreak_start_date)) %>% 
  mutate(isoyear=lubridate::isoyear(Outbreak_start_date))

# check if any observations with week 53 ?
za_WNF_WOEM_horse[which(za_WNF_WOEM_horse$isoweek == 53), ]

# In this data set, we have 140 observations within week 53 from 2020/2021 and one from  January 2016. As the surveillance package needs a matrix of countries and week numbers, we cannot have week 53 in some years and not others. We also cannot create a week 53 for the remaining years and set it to zero observations as this week does not exist for those years. Instead, we force dates in week 53 to be either week 52 in 2020 (if in December 2020), week 1 in 2021 (if in January 2021) or week 1 in 2016.

for (i in 1:nrow(za_WNF_WOEM_horse)){
  if(za_WNF_WOEM_horse$isoweek[i]==53 & lubridate::month(za_WNF_WOEM_horse$Outbreak_start_date[i]) == 12){
    za_WNF_WOEM_horse$isoweek[i]<-52
    if(lubridate::year(za_WNF_WOEM_horse$Outbreak_start_date[i]) != za_WNF_WOEM_horse$isoyear[i]){
      za_WNF_WOEM_horse$isoyear[i] <- lubridate::year(za_WNF_WOEM_horse$Outbreak_start_date[i])
    }
  }
  else if(za_WNF_WOEM_horse$isoweek[i]==53 & lubridate::month(za_WNF_WOEM_horse$Outbreak_start_date[i]) == 1){
    za_WNF_WOEM_horse$isoweek[i]<-1
    if(lubridate::year(za_WNF_WOEM_horse$Outbreak_start_date[i]) != za_WNF_WOEM_horse$isoyear[i]){
      za_WNF_WOEM_horse$isoyear[i] <- lubridate::year(za_WNF_WOEM_horse$Outbreak_start_date[i])
    }
  }
}

## now aggregate per week per year per country
za_WNF_WOEM_horse_weekly <- za_WNF_WOEM_horse  %>% 
  group_by(iso_code, isoweek,isoyear) %>% summarise(no_outbreaks = sum(cases)) 

colnames(za_WNF_WOEM_horse_weekly)<- c("ADM0_A3", "Week", "Year","no_outbreaks")

za_WNF_WOEM_horse_weekly=za_WNF_WOEM_horse_weekly %>%
  select("ADM0_A3", "Week", "Year","no_outbreaks")

#this methods fill in missing years for each country, by adding week 1 of the missing year (zero number of detections)
za_WNF_WOEM_horse_weekly <-za_WNF_WOEM_horse_weekly %>% 
  group_by(ADM0_A3) %>% 
  complete(Year = 2018:2023, fill = list(Week=1, no_outbreaks = 0))

#this method then add missing weeks to the data set and set number of outbreaks for the missing weeks to zero
za_WNF_WOEM_horse_weekly <-za_WNF_WOEM_horse_weekly %>% 
  group_by(ADM0_A3, Year) %>% 
  complete(Week = 1:52, fill = list(no_outbreaks = 0))


# Create data, neighborhood and covariate matrices ###
#first read in shapefile where water is added as polygons to account for countries with water between them still being connected in a bird's perspective
europeanCountries_water = st_read('./data/GIS/Europe_shapefiles_water.shp')

#now calculate neighborhood (matrix) and set column and row names
library(spdep)
za_WNF_WOEM_horse_adjmat <-nbOrder(poly2adjmat(europeanCountries_water,zero.policy = TRUE), maxlag = Inf)
colnames(za_WNF_WOEM_horse_adjmat) <- europeanCountries_water$ADM0_A3  # column names
rownames(za_WNF_WOEM_horse_adjmat) <- europeanCountries_water$ADM0_A3 # row names

#Now only keep countries in the neighborhood matrix where we have outbreak data and remove the newly created water connections as well
za_WNF_WOEM_horse_adjmat<-za_WNF_WOEM_horse_adjmat[rownames(za_WNF_WOEM_horse_adjmat)%in% unique(za_WNF_WOEM_horse_weekly$ADM0_A3),colnames(za_WNF_WOEM_horse_adjmat)%in% unique(za_WNF_WOEM_horse_weekly$ADM0_A3)]

#create shape file with only countries where we have data - this is needed for the model
europeanCountries.sub<-europeanCountries_water[europeanCountries_water$ADM0_A3 %in% unique(za_WNF_WOEM_horse_weekly$ADM0_A3),]
row.names(europeanCountries.sub)<- europeanCountries.sub$ADM0_A3

#convert sf object europeanCountries.sub to spatialpolygon dataframe as the sts class needs this format
europeanCountries.sub <- as(europeanCountries.sub, 'Spatial')

# Make sure that the country order in the data matrix we will create is the same order as in the neighborhood matrix 
adjmat_order <- colnames(za_WNF_WOEM_horse_adjmat)
keyDF <- data.frame(key=adjmat_order,weight=1:length(adjmat_order))
merged_WNF_WOEM_horse <- merge(za_WNF_WOEM_horse_weekly,keyDF,by.x='ADM0_A3',by.y='key',all.x=T,all.y=F)
res_WNF_WOEM_horse <- merged_WNF_WOEM_horse[order(merged_WNF_WOEM_horse$weight, merged_WNF_WOEM_horse$Year,merged_WNF_WOEM_horse$Week),c('ADM0_A3','Year', "Week", "no_outbreaks")]

#create data matrix to be used in hhh4 model in HPAI_hhh4_master.R
AI_WNF_WOEM_horse_weekly <-res_WNF_WOEM_horse %>%  pivot_wider(
  names_from = ADM0_A3,
  values_from = no_outbreaks,
  id_cols = c(Year, Week)) %>% 
  select(-Year,-Week) %>% 
  as.matrix()

### CONSTRUCTION OF CLASS STS USED IN WNF MODELS ###
subset_start <- 1
subset_end <- 312
AI_WNF_WOEM_horse_weekly1 <-  AI_WNF_WOEM_horse_weekly[subset_start:subset_end,,drop=FALSE]
start_w <- 1
start_Y <- 2018 


AI_WNF_WOEM_horse_sts <- sts(observed = AI_WNF_WOEM_horse_weekly1, start = c(as.numeric(start_Y), as.numeric(start_w)), 
                            frequency = 52,neighbourhood = za_WNF_WOEM_horse_adjmat, map = europeanCountries.sub) 
AI_WNF_WOEM_horse_sts_twoWeeks <- aggregate(AI_WNF_WOEM_horse_sts, nfreq=26)

plot(AI_WNF_WOEM_horse_sts, type = observed ~ time, ylab="No. of reported cases",
     main = "Number of WNF_Horse cases within WOAH&Empres_i in Europe")

plot(AI_WNF_WOEM_horse_sts_twoWeeks, type = observed ~ time, ylab="No. of reported cases")

#------------------------------------------------------------------------
#---Empres_I--from different database for humans 
Empres_dataH_WNF <- read_csv("West_Nile_Virus_Human Being_ECDC.csv")#, skip=11
Empres_dataH_WNF <- Empres_dataH_WNF %>% select_if(~ !any(is.na(.)))

#Renaming
Empres_dataH_WNF <- Empres_dataH_WNF %>% 
  rename(Outbreak_start_date = Time,
         ADMIN = RegionName,
         Species = Indicator)

Empres_dataH_WNF[Empres_dataH_WNF == "Reported cases"] <- 'Human'

library(zoo)
Empres_dataH_WNF$Outbreak_start_date = as.Date(as.yearmon(Empres_dataH_WNF$Outbreak_start_date))
Empres_dataH_WNF$Outbreak_start_date=as.character.Date(Empres_dataH_WNF$Outbreak_start_date)

#make sure that outbreak start date is in date format
Empres_dataH_WNF$Outbreak_start_date <- as.Date(Empres_dataH_WNF$Outbreak_start_date, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))

#filter out other species than birds using the human_names_20231203.csv file
birdFam <- as.list(read.csv("./data/bird_names_20220517.csv", sep=";", header=FALSE))
Empres_dataH_WNF$Species <- gsub("\\s*\\([^\\)]+\\)","",as.character(Empres_dataH_WNF$Species))
Empres_dataH_WNF_humans <- Empres_dataH_WNF[Empres_dataH_WNF$Species %in% birdFam$V1[327],]#onlyhuman

#we only want data from Europe (so filter using the europe shapefile) and data from 2018 and onwards
Empres_dataH_WNF_humans <-Empres_dataH_WNF_humans %>%
  filter(ADMIN %in% unique(europeanCountries$ADMIN),Outbreak_start_date >"2017-12-01")#"31-12-2017"


ava = Empres_dataH_WNF_humans
data.merged <- merge(x = europeanCountries,
                     y = ava, by = "ADMIN",
                     all = TRUE)
# pass them in to the function as the `coords` parameter
endDate <- max(data.merged$Outbreak_start_date)
Empres_dataH_WNF_sf_humans <- st_as_sf(data.merged, coords=c("Longitude","Latitude"),crs="EPSG:4326")


#-----We remove these countries because we noticed that it was not part of corine land cover 
RemoveCountries <- c("Ukraine", "Russian Federation", "Belarus")#,"EU/EEA"
Empres_dat_WNF_sf_humans<- Empres_dataH_WNF_sf_humans[!Empres_dataH_WNF_sf_humans$ADMIN %in% RemoveCountries, ]

#------------------------------------------------------

#--Plots
tm_shape(prec/100) +
  tm_raster(title="Annual precipitation (WNF_Human Beings)",palette="Mako") +
  tm_shape(Empres_dat_WNF_sf_humans)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="deeppink",shape = 8,size = 0.3)+
  tm_add_legend('symbol',
                col = "deeppink",
                shape = 8,size = 0.3,
                labels ="Empres-i")

tm_shape(temp/10) +
  tm_raster(title="Annual mean temperature (WNF_Human Beings)", palette=rev(hcl.colors(7, "PinkYl"))) +
  tmap_options(max.categories = 44)+
  tm_shape(Empres_dat_WNF_sf_humans)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="deeppink",shape = 8,size = 0.3)+
  tm_add_legend('symbol',
                col = "deeppink",
                shape = 8,size = 0.3,
                labels="Empres-i")

tm_shape(cor) +
  tm_raster(title="Land cover (WNF_Human Beings)")+
  tmap_options(max.categories = 44)+
  tm_shape(Empres_dat_WNF_sf_humans)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="deeppink",shape = 8,size = 0.3)+
  tm_add_legend('symbol',
                col = "deeppink",
                shape = 8,size = 0.3,
                labels="Empres-i")
