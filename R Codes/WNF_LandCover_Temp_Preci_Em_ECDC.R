# These lines of code were written to analyse ASF, WNF, and CWD from different public domain databases 
#as described in this report for WiLiMan_ID. The R code is intended to ensure the reproducibility of the results or, 
#with slight modification, to be applied to other diseases in the databases used. 
#R Code written by Ofosuhene O. Apenteng and help from Lene Jung Kjær. 

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

# READ IN SHAPEFILE OF THE STUDY REGION ###
europeanCountries = st_read('./data/GIS/Europe_shapefiles.shp')

# CLEAN AND PREPARE DATA---------------------------------

#---Empres_I--from different database ---------------------------------###############
Empres_dataH_WNF <- read_csv("West_Nile_Virus_Human Being_ECDC.csv")#, skip=11
Empres_dataH_WNF <- Empres_dataH_WNF %>% select_if(~ !any(is.na(.)))

#Renaming
 Empres_dataH_WNF <- Empres_dataH_WNF %>% 
   rename(Outbreak_start_date = Time,
          ADMIN = RegionName,
          Species = Indicator)

 Empres_dataH_WNF[Empres_dataH_WNF == "Reported cases"] <- 'Human'
 #Empres_dataH_WNF_human = Empres_dataH_WNF
 #Since dates correspond to a numeric value and a starting date, 
 #we indeed need the day. If we really need our data to be in Date format, 
 #we can just fix the day to the first of each month manually by pasting it to the date:
 #https://stackoverflow.com/questions/6242955/converting-year-and-month-yyyy-mm-format-to-a-date
 
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

#Please kindly check here for me, there is no column called 'cases' 
#so I created one for it meaning that all cases in this dataset represent on case
#Empres_dat_WNF_sf_humans$cases <- rep(1,nrow(Empres_dat_WNF_sf_humans))

################################################################
#Read in landcover data
prec <- rast("bio_12.tif")
temp <- rast("bio_1.tif")
corine <-rast("g1k_06wgs.tif")
cor <- rast("corine_rat_LEVEL2.tif")
#------------------------------------------------------

#--Plots
tm_shape(prec/100) +
  tm_raster(title="Annual precipitation (WNF_Human Beings)",palette="Blues") +
  tm_shape(Empres_dat_WNF_sf_humans)+
  tm_layout(legend.outside = TRUE) +
  tm_dots(col="deeppink",shape = 8,size = 0.3)+
  tm_add_legend('symbol',
                col = "deeppink",
                shape = 8,size = 0.3,
                labels ="Empres-i")
  
tm_shape(temp/10) +
  tm_raster(title="Annual mean temperature (WNF_Human Beings)", palette="Reds") +
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

