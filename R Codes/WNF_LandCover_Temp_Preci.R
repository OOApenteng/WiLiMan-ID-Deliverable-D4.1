
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

#https://stackoverflow.com/questions/63517414/how-to-replace-all-values-using-dplyrs-across-function
Empres_data_WNF[Empres_data_WNF == "Unspecified Bird"] <- 'Bird'
Empres_data_WNF[Empres_data_WNF == "Horse"] <- 'Equidae'

#df3$Outbreak_start_date <- as.Date(df3$Outbreak_start_date, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))
#df3$Reporting_date <- as.Date(df3$Reporting_date, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))

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

#Read in landcover data
prec <- rast("bio_12.tif")
temp <- rast("bio_1.tif")
horses <-rast("Ho_density.tif")
corine <-rast("g1k_06wgs.tif")
corine_attributes <- read.csv("clc_legend.csv")


#convert rgb colors to hex:
# Function to apply
rgb2hex <- function(x) rgb(substr(x, 1, 3), substr(x, 5, 7), substr(x, 9, 11), maxColorValue = 255)
corine_attributes$Color<-rgb2hex(corine_attributes$RGB)
levels(corine) <-corine_attributes[c(1,4)]#corine_attributes[c(1,5)] # levels of the factors
coltab(corine)<-corine_attributes[c(1,7)]

#writeRaster(corine, '~/Desktop/APENTENG_LENE/corine_rat.tif', datatype='INT1U', overwrite = TRUE)
cor <- rast("corine_rat_LEVEL2.tif")

#-------------------------------------------------------
#--Plots
tm_shape(prec/100) +
  tm_raster(title="Annual precipitation (WNF_birds)",palette="Blues") +
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
  tm_raster(title="Annual precipitation (WNF_Horse)",palette="Blues") +
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
  tm_raster(title="Annual mean temperature (WNF_birds)", palette="Reds") +
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
  tm_raster(title="Annual mean temperature (WNF_Horse)", palette="Reds") +
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
#prec
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
#prec
precip <-terra::extract(prec, vect(europe_dat_WNF_sf_horse))
europe_dat_WNF_sf_horse$prec <- precip$bio_12/100
#temperature
tempe <-terra::extract(temp, vect(europe_dat_WNF_sf_horse))
europe_dat_WNF_sf_horse$temp <- tempe$bio_1/10
#landcover
cover <-terra::extract(cor, vect(europe_dat_WNF_sf_horse))
europe_dat_WNF_sf_horse$cover <- cover$LABEL2

#Merged
#prec
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
#prec
precip <-terra::extract(prec, vect(WOEM_horse))
WOEM_horse$prec <- precip$bio_12/100
#temperature
tempe <-terra::extract(temp, vect(WOEM_horse))
WOEM_horse$temp <- tempe$bio_1/10
#landcover
cover <-terra::extract(cor, vect(WOEM_horse))
WOEM_horse$cover <- cover$LABEL2

#Empres-i
#prec
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
#prec
precip <-terra::extract(prec, vect(Empres_dat_WNF_sf_horse))
Empres_dat_WNF_sf_horse$prec <- precip$bio_12/100
#temperature
tempe <-terra::extract(temp, vect(Empres_dat_WNF_sf_horse))
Empres_dat_WNF_sf_horse$temp <- tempe$bio_1/10
#landcover
cover <-terra::extract(cor, vect(Empres_dat_WNF_sf_horse))
Empres_dat_WNF_sf_horse$cover <- cover$LABEL2
#------------------------------------------------------
#Univariate regression analysis for birds
summary(lm(cases ~ temp, data = europe_dat_WNF_sf_birds))
summary(lm(cases ~ prec, data = europe_dat_WNF_sf_birds)) 
#------------------------------------------------------
#Univariate regression analysis for horses
summary(lm(cases ~ horses, data = europe_dat_WNF_sf_horse))
summary(lm(cases ~ temp, data = europe_dat_WNF_sf_horse)) #only this is significant
summary(lm(cases ~ prec, data = europe_dat_WNF_sf_horse)) 
#------------------------------------------------------
#Univariate regression analysis for merged pigs
summary(lm(cases ~ horses, data = WOEM_horse)) #only this is significant
summary(lm(cases ~ temp, data = WOEM_horse)) #only this is significant
summary(lm(cases ~ prec, data = WOEM_horse)) #only this is significant
#------------------------------------------------------
#Multivariate regression analysis for wild boars
library(glmmTMB)
library(DHARMa)
library(MuMIn)
#---Pigs---
#After check univariate regression analysis now proceed to do multivariate regression analysis:
WOEM_horse1  <- WOEM_horse %>% 
  dplyr::mutate(country = as.factor(country) ) %>% 
  dplyr::mutate(cover = as.factor(cover) ) %>% 
  dplyr::select(prec, horses, cases, cover, country) %>% 
  na.omit()
#as.data.frame()
#str(europe_dat_ASF_sf_pigs1)
m9<- glmmTMB(cases~prec+horses+cover, ziformula=~ 0, data=WOEM_horse1, family=nbinom1)
summary(m9)
m10<- glmmTMB(cases~prec+horses+ cover+(1|country), ziformula=~ 0, data=WOEM_horse1, family=nbinom1)
summary(m10)

#-----------------------------------------------------
#Prediction
library(caTools)
datae = WOEM_horse1
set.seed(1)
#use 80% of dataset as training set and 20% as test set
datae = as.data.frame(datae)
num_obs = nrow(datae)
train_index = sample(num_obs, size = trunc(0.80 * num_obs))
train  <- datae[train_index, ]
test   <- datae[-train_index, ]
#https://cran.r-project.org/web/packages/caret/vignettes/caret.html
#https://daviddalpiaz.github.io/r4sl/linear-models.html
#Testing the accuracy
library(caret)
rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

library(Metrics) 
# train RMSE
Metrics::rmse(actual = train$cases, predicted = predict(m10, train))
# test RMSE
Metrics::rmse(actual = test$cases, predicted = predict(m10, test))

# train R-square
R2(predict(m10, train), train$cases)
R2(predict(m10, test), test$cases)

plot(train$cases,predict(m10, train),
     ylab="Predicted cases (precipitation & horse density)",xlab="Observed cases",
     main="Merged (WOAH&Empres_i) for WNF-Horses",pch=16, col="deeppink")

#-----------------------------------------------------
#retrieve coordinates in matrix form
#https://beckmw.wordpress.com/2013/01/07/breaking-the-rules-with-spatial-correlation/

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

# In this data set, we have 140 observations within week 53 from 2020/2021 and one from  January 2016. As the surveillance package needs a matrix of countries and week numbers, we cannot have week 53 in some years and not others. We also cannot create a week 53 for the remaining years and set it to zero observations as this week does not exist for those years. Instead, we force dates in week 53 to be either week 52 in 2020 (if in December 2020), week 1 in 2021 (if in January 2021) or week 1 in 2016.

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


# CREATE DATA, NEIGHBORHOOD AND COVARIATE MATRICES ###
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

#create data matrix to be used in hhh4 model in HPAI_hhh4_master.R
AI_WNF_horse_weekly <-res_WNF_horse %>%  pivot_wider(
  names_from = ADM0_A3,
  values_from = no_outbreaks,
  id_cols = c(Year, Week)) %>% 
  select(-Year,-Week) %>% 
  as.matrix()

#za_WNF_horse_weekly 
### CONSTRUCTION OF CLASS STS USED IN ASF MODELS ###
#data_name <- za_WNF_horse_weekly
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


# CREATE DATA, NEIGHBORHOOD AND COVARIATE MATRICES ###
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

#create data matrix to be used in hhh4 model in HPAI_hhh4_master.R
AI_WNF_birds_weekly <-res_WNF_birds %>%  pivot_wider(
  names_from = ADM0_A3,
  values_from = no_outbreaks,
  id_cols = c(Year, Week)) %>% 
  select(-Year,-Week) %>% 
  as.matrix()

#za_WNF_birds_weekly 
### CONSTRUCTION OF CLASS STS USED IN ASF MODELS ###
#data_name <- za_WNF_birds_weekly
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

#library(tidyr)
#zaad_newdf <- zaad %>% drop_na()

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


# CREATE DATA, NEIGHBORHOOD AND COVARIATE MATRICES ###
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
#data_name <- za_WNF_WOEM_horse_weekly
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






#-------------------------------
library(glmmTMB)
library(DHARMa)
library(MuMIn)

#check your data - to see if it is overdispersed, if the ratio between variances and means is >1 your data is overdispersed
dispersionstats <- europe_dat_WNF_sf_birds %>%
  summarise(
    means = mean(cases, na.rm=T),
    variances = var(cases,na.rm=T),
    ratio = variances/means)
dispersionstats


# if it is overdispersed - you should probably use a negative binomial distribution
#Here I use the packages glmmTMB, which is a generalised linear model, that can incorporate spatial- and temporal autocorrelation and zero-inflation

#you can also test, if a poisson or a negative binomial model is better:
poismodel <-glmmTMB(cases~1, ziformula=~ 0,data = europe_dat_WNF_sf_birds, family = "poisson") 
nbmodel <- glmmTMB(cases~1, ziformula=~ 0,data = europe_dat_WNF_sf_birds,family=nbinom1)
model.sel(poismodel, nbmodel)# which one is better


#univariable tests
m0<- glmmTMB(cases~temp, ziformula=~ 0, data=europe_dat_WNF_sf_birds, family=nbinom1)
summary(m0)
#THEN IN A MULTIVARIABLE TESTS, YOU CAN DO THIS:
m1<- glmmTMB(cases~cover, ziformula=~ 0, data=europe_dat_WNF_sf_birds, family="poisson")
summary(m1)


cases_pred_birds <-NULL

#create fake id for each row, to keep track of rows
europe_dat_WNF_sf_birds$ID <- 1:nrow(europe_dat_WNF_sf_birds)

###Change 880 by the number of observation you have###
for (i in 1:nrow(europe_dat_WNF_sf_birds)) #i in 1:length(europe_dat_sf)
{
  ##Data that will be predicted
  DataC1=europe_dat_WNF_sf_birds[europe_dat_WNF_sf_birds$ID==i,]
  ###To train the model
  DataCV=europe_dat_WNF_sf_birds[europe_dat_WNF_sf_birds$ID!=i,]
  #testformula4<- update(testformula1, . ~ .- (ou(times + 0 | Site)))
  M1 <- glmmTMB(cases~prec+cover, ziformula=~ 0, data= DataCV, family=nbinom1)# here I assume that the negative binomial was the better model
  # is.unsorted(DataC1$times)
  P1=predict(M1, DataC1,allow.new.levels=TRUE, type="response")
  names(P1)=NULL
  P1
  cases_pred_birds =c(cases_pred_birds, P1)
  print(i)
}

#bind the new predicted values to your original dataset
europe_dat_sf<- cbind(europe_dat_sf, cases_pred)
plot(europe_dat_sf$cases,europe_dat_sf$cases_pred)


#To prediction
europe_dat_WNF_sf_birds_pred <- read_excel("europe_dat_WNF_sf_birds_pred.xlsx")
plot(europe_dat_WNF_sf_birds_pred$cases,europe_dat_WNF_sf_birds_pred$cases_pred_pigs,
     ylab="Predicted cases",xlab="Observed cases",
     main="WOAH for ASF-Pigs",pch=16, col="deeppink")

aw <- europe_dat_WNF_sf_birds_pred

dfi <- as.data.frame(aw[,c(55,66)])

row.has.na <- apply(dfi, 1, function(x){any(is.na(x))})

final <- dfi[!row.has.na,]

Rsquared <- cor(final$cases,final$cases_pred_pigs)^2
rmse <- sqrt(mean((final$cases - final$cases_pred)^2))
rmse
nrmse <- rmse/mean (final$cases)
#-------------------------------------
#Code for checking for spatial autocorrelation:

##check for autocorrelation
# extract residuals
Residuals<- resid(m1)
Residuals = data.frame(Residuals)
Residuals$ID <- 1:nrow(Residuals)
new <- europe_dat_WNF_sf_birds
new$ID <- 1:nrow(europe_dat_WNF_sf_birds)
#merge with original data

europe_dat_WNF_sf_birds_new <- merge(new, Residuals, by ="ID")  #assuming you have an ID for each row in your dataset – else run YOURDATA$ID <- 1:nrow(YOURDATA)

#now plot to see if there are any spatial patterns in the residuals
tmap_mode("plot")

# tm_shape(europe_dat_WNF_sf_birds_new) +
#   tm_polygons(title="Residuals",col="res", palette="Reds")+
#   tm_layout(legend.outside = TRUE) +
#   tm_scale_bar(position=c("RIGHT","BOTTOM"))+
#   tm_compass(position=c("RIGHT","top"))
# 
# tm_shape(europe_dat_sf_new) +
#   tm_polygons()+
#   tm_dots(title="Residuals",col="res", palette="Reds")

tm_shape(prec/100) +
  tm_polygons()+
  tm_shape(europe_dat_WNF_sf_birds_new) + 
  tm_dots (col="Residuals",palette="Blues")

#---------------------------------------------------

#check your data - to see if it is overdispersed, if the ratio between variances and means is >1 your data is overdispersed
dispersionstats <- europe_dat_WNF_sf_horse %>%
  summarise(
    means = mean(cases, na.rm=T),
    variances = var(cases,na.rm=T),
    ratio = variances/means)
dispersionstats


# if it is overdispersed - you should probably use a negative binomial distribution
#Here I use the packages glmmTMB, which is a generalised linear model, that can incorporate spatial- and temporal autocorrelation and zero-inflation

#you can also test, if a poisson or a negative binomial model is better:
poismodel <-glmmTMB(cases~1, ziformula=~ 0,data = europe_dat_WNF_sf_horse, family = "poisson") 
nbmodel <- glmmTMB(cases~1, ziformula=~ 0,data = europe_dat_WNF_sf_horse,family=nbinom1)
model.sel(poismodel, nbmodel)# which one is better


#univariable tests
m0<- glmmTMB(cases~prec, ziformula=~ 0, data=europe_dat_WNF_sf_horse, family="poisson")
summary(m0)
#THEN IN A MULTIVARIABLE TESTS, YOU CAN DO THIS:
m1<- glmmTMB(cases~prec+temp+cover, ziformula=~ 0, data=europe_dat_WNF_sf_horse, family="poisson")
summary(m1)


cases_pred_horse <-NULL

#create fake id for each row, to keep track of rows
europe_dat_WNF_sf_horse$ID <- 1:nrow(europe_dat_WNF_sf_horse)

###Change 880 by the number of observation you have###
for (i in 1:nrow(europe_dat_WNF_sf_horse)) #i in 1:length(europe_dat_sf)
{
  ##Data that will be predicted
  DataC1=europe_dat_WNF_sf_horse[europe_dat_WNF_sf_horse$ID==i,]
  ###To train the model
  DataCV=europe_dat_WNF_sf_horse[europe_dat_WNF_sf_horse$ID!=i,]
  #testformula4<- update(testformula1, . ~ .- (ou(times + 0 | Site)))
  M1 <- glmmTMB(cases~prec+temp+cover, ziformula=~ 0, data= DataCV, family="poisson")# here I assume that the negative binomial was the better model
  # is.unsorted(DataC1$times)
  P1=predict(M1, DataC1,allow.new.levels=TRUE, type="response")
  names(P1)=NULL
  P1
  cases_pred_horse =c(cases_pred_horse, P1)
  print(i)
}

#bind the new predicted values to your original dataset
europe_dat_sf<- cbind(europe_dat_sf, cases_pred)
plot(europe_dat_sf$cases,europe_dat_sf$cases_pred)


#To prediction
europe_data_WNF_horse_pred <- read_excel("europe_data_WNF_horse_pred.xlsx")
plot(europe_data_WNF_horse_pred$cases,europe_data_WNF_horse_pred$cases_pred_pigs,
     ylab="Predicted cases",xlab="Observed cases",
     main="WOAH for ASF-Pigs",pch=16, col="deeppink")

aw <- europe_data_WNF_horse_pred

dfi <- as.data.frame(aw[,c(55,66)])

row.has.na <- apply(dfi, 1, function(x){any(is.na(x))})

final <- dfi[!row.has.na,]

Rsquared <- cor(final$cases,final$cases_pred_pigs)^2
rmse <- sqrt(mean((final$cases - final$cases_pred)^2))
rmse
nrmse <- rmse/mean (final$cases)
#-------------------------------------
#Code for checking for spatial autocorrelation:

##check for autocorrelation
# extract residuals
Residuals<- resid(m1)
Residuals = data.frame(Residuals)
Residuals$ID <- 1:nrow(Residuals)
new <- europe_data_WNF_horse
new$ID <- 1:nrow(europe_data_WNF_horse)
#merge with original data

europe_data_WNF_horse_new <- merge(new, Residuals, by ="ID")  #assuming you have an ID for each row in your dataset – else run YOURDATA$ID <- 1:nrow(YOURDATA)

#now plot to see if there are any spatial patterns in the residuals
tmap_mode("plot")

# tm_shape(europe_data_WNF_horse_new) +
#   tm_polygons(title="Residuals",col="res", palette="Reds")+
#   tm_layout(legend.outside = TRUE) +
#   tm_scale_bar(position=c("RIGHT","BOTTOM"))+
#   tm_compass(position=c("RIGHT","top"))
# 
# tm_shape(europe_dat_sf_new) +
#   tm_polygons()+
#   tm_dots(title="Residuals",col="res", palette="Reds")

tm_shape(prec/100) +
  tm_polygons()+
  tm_shape(europe_data_WNF_horse_new) + 
  tm_dots (col="Residuals",palette="Blues")






##############################################################
# Regression analysis
model1  <- lm(cases ~ temp, data = europe_dat_WNF_sf_birds)
summary(model1)
model2  <- lm(cases ~ prec, data = europe_dat_WNF_sf_birds)
summary(model2)
model3  <- lm(cases ~ temp + prec, data = europe_dat_WNF_sf_birds)
summary(model3)

# We can set reference point 
selection1 <- c( "Olive groves", "Pastures", "Fruit trees and berry plantations" )
europe_dat_sf_i <- europe_dat_WNF_sf_horse  %>%
  dplyr::filter(cover %in% selection1)

model14  <- lm(cases ~ temp + prec + cover, data = europe_dat_WNF_sf_birds)
summary(model14)

model15  <- lm(cases ~ temp + prec + cover, data = europe_dat_WNF_sf_horse)
summary(model15)

#--------------------------
add=as.data.frame(rep(1,dim(Empres_dat_WNF_sf_horse)[1]))
add <- add %>% 
  rename(cases = rep(1, dim(Empres_dat_WNF_sf_horse)[1]))
add <- add %>% 
  rename(cases = cases11)
Empres_dat_WNF_sf_horse_Emp = Empres_dat_WNF_sf_horse
Empres_dat_WNF_sf_horse_Emp$cases = add$cases 
model16  <- lm(cases ~ temp + prec + cover, data = Empres_dat_WNF_sf_horse_Emp)
summary(model16)

#y = bo + b_1*temp + . . . . . . .
#The confidence interval of the model coefficient can be extracted as follow:
confint(model1)
confint(model2)
confint(model3)



