# Project Title

Dataflow

# Description

These lines of code were written to analyse ASF, WNF, and CWD from different public domain databases as described in this report for WiLiMan_ID. The R code is intended to ensure the reproducibility of the results or, with slight modification, to be applied to other diseases in the databases used. R Code written by Ofosuhene O. Apenteng.

## Getting Started

# Dependencies
The following libraries need to be installed to run the code
library(lattice)
library(tmap)
library(readxl)
library(sp)
library(sf)
library(terra)
library(tidyverse)
library(dplyr)
library(glmmTMB)
library(DHARMa)
library(MuMIn)
library(caTools) 
library(caret)
library(surveillance)
library(spdep)
library(zoo)

# Installing

The following are the links where the data were downloaded in CSV files based on the description of the data between 2018 and 2023. You can, however, go directly to the websites provided below to choose the range you want. 

European Centre for Disease Prevention and Control: http://atlas.ecdc.europa.eu/public/index.aspx
Norwegian Veterinary Institute: http://apps.vetinst.no/skrantesykestatistikk/NO/#kasus   
Swedish Veterinary Agency: https://www.sva.se/en/wildlife/wildlife-health-and-disease-surveillance-in-sweden/map-of-chronic-wasting-disease-cwd/
Finnish Food Authority: https://www.ruokavirasto.fi/en/animals/animal-health-and-diseases/animal-diseases/wildlife/chronic-wasting-disease-cwd-in-cervids/. 
Global Animal Disease Information System (EMPRES-i): https://empres-i.apps.fao.org/diseases
WOAH: https://www.woah.org/en/home/ (SharePoint)
WorldClim: https://www.worldclim.org/data/worldclim21.html#google_vignette


# Executing program

#Read of data 

#link to where files are - the below code will pick the newest file in the folder"

#Clear and prepare data

#make sure that the outbreak start date is in date format
#filter out other diseases than WNF, ASF, and CWD

#filter out other species than birds using the bird_names_20220517.csv file

#we only want data from Europe (so filter using the europe shapefile) and data between 2018 and 2023 where the period we are using is five years interval

#For birds only

#We needed to represent spatial vector data to include points (coordinates),

#-Remove these countries because we noticed that it was not part of Corine land cover

#-Empres_I--from different database

#Renaming

#we only want data from Europe (so filter using the europe shapefile) and data from 2018 and onwards

#pass them into the function as the `coords` parameter

#Remove these countries because we noticed that it was not part of Corine land cover (eg. "Ukraine", "Russian Federation", "Belarus")
#-Merging these two database

#Remove the NA's

#We needed to represent spatial vector data to include points (coordinates), in view of that we added their latitude and longitude
#For birds only
#For wild horses only
#For pigs only

#Read in the (a)biotic factors (download)

#-Plots

#Extract raster values to point locations of the (a)biotic factors using the terra package 

#Univariate regression analysis to check for significance by negative binomial regression 
#Multivariate regression analysis for negative binomial regression 

#Time series analysis

#set all cases to 1, so we are counting the number of outbreaks instead of the number of birds affected

#Dates and week numbers are tricky as they differ from year to year. Here we use iso 8601 to create the new variables - isoweek and isoyear

#check if any observations with week 53?

#The data was converted into weekly bases. Take note here based on you want one can convert it to want eh or she want. 

#Now aggregate per week per year per country

#These methods fill in missing years for each country, by adding week 1 of the missing year (zero number of detections)

#Create data, neighborhood and covariate matrices

#first read in shapefile where water is added as polygons to account for countries with water between them still being connected in a (species you want to use) perspective

#now calculate the neighborhood (matrix) and set column and row names

#Now only keep countries in the neighborhood matrix where we have outbreak data and remove the newly created water connections as well

#create a shape file with only countries where we have data - this is needed for the model

#convert sf object europeanCountries.sub to spatialpolygon dataframe as the sts class needs this format

#Make sure that the country order in the data matrix we will create is the same as in the neighborhood matrix

#create a data matrix to be used in the model 

#Plots

# Acknowledgments
Lene Jung Kjær and Ana Rita Pinheiro Marques
