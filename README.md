# Project Title

Dataflow (Wiliman-ID)

# Description

These lines of code were written to analyse the following animal diseases African Swine Fever (ASF), West-Nile Fever (WNF), and Cronical Waste Disease (CWD) from different public domain databases. The R code will ensure the reproducibility of the results of the Wiliman-ID project or, with slight modification, be applied to other diseases than described above. 

R Code was written by Ofosuhene O. Apenteng.

## Getting Started

# Dependencies

This is very important, the following packages must be installed for this code to work correctly. 

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

The following described the data source links where the disease occurrence data and were downloaded as CSV files for the period 2018-2023 and just for Europe. The code provided here is an example of processing the diseases ASF, WNF, and CWD but the user can use the data source links to select other diseases of interest and periods. 

Extract European Centre for Disease Prevention and Control data: http://atlas.ecdc.europa.eu/public/index.aspx

Follow these steps to process the data from the European Centre for Disease Prevention and Control by hand tasks are required:

Comma-separated values (CSV) format were downloaded. Between the period 2018 and 2023. The following steps need to be follow: 1) Select the Disease, 2) Population (All cases), 3) Indicator (Reported cases), 4) Unit (North, East, West, and South), 5) Time, 6) RegionCode (Only Europe), 7) NumValue, and then finally extract or download in the format you prefer. 

Extract CWD data from Norwegian Veterinary Institute: http://apps.vetinst.no/skrantesykestatistikk/NO/#kasus  
In Norway there were only few cases which are provided them in the R code for the user by hand tasks are required if the user wants to update. 

Extract CWD data from Swedish Veterinary Agency: https://www.sva.se/en/wildlife/wildlife-health-and-disease-surveillance-in-sweden/map-of-chronic-wasting-disease-cwd/
In Sweden there were only four cases which are provided them in the R code for the user by hand tasks are required if the user wants to update.

Extract CWD data from Finnish Food Authority: https://www.ruokavirasto.fi/en/animals/animal-health-and-diseases/animal-diseases/wildlife/chronic-wasting-disease-cwd-in-cervids/ 
In Finland there were only three cases which are provided them in the R code for the user by hand tasks are required if the user wants to update.

Extract Global Animal Disease Information System (EMPRES-i) data: https://empres-i.apps.fao.org/diseases 

Follow these steps to process the data from the Global Animal Disease Information System (EMPRES-i) by hand tasks are required:

Comma-separated values (CSV) format were downloaded. Between the period 2018 and 2023. The following steps need to be follow: 1) Select the Disease, 2) Region (Europe), 3) Diagnosis status (Confirmed and Denied), 4) Animal type (Captive, Domestic, and Wild), 5) Observation date range, 6) Species (if the user want to use), and then finally extract or download in the format you prefer. 

Extract WOAH data: https://www.woah.org/en/home/ (SharePoint). This is easy to access it by contacting them through the link provided here and they provide or give you permission to access the data. 

Extract climate data(s): https://www.worldclim.org/data/worldclim21.html#google_vignette in order to obtain this data by hand tasks are required. 

Extract CORINE Land Cover 2018 data (vector/raster 100m), Europe, 6-yearly: https://land.copernicus.eu/en/products/corine-land-cover/clc2018#download

Follow these steps to process the tif (image) for the CORINE Land Cover by hand tasks are required:

This is how the CORINE data was obtained. From this https://land.copernicus.eu/en/products/corine-land-cover/clc2018#download you need to register and Corine Land Cover 2018 datastes in their full coverage can be downloaded in raster (100 m resolution) by selecting the following: u2018_clc2018_v2020_20u1_raster100m, Area of interest(Europe),	Version(v2020_20u1), Resolution(100m), Type(Raster), and Format(Geotiff). After adding to your cart and selecting "Download," a request will be sent to the email address you have on file, from which you can download the file. I've included the aforementioned CORINE land cover in the data file to make things easier. 

# Executing program

Read in the (a)biotic fectors and land cover data, all these data are provided in the 'data'

These are images, and the meta-data that extracts data from images was chosen using this link: https://www.worldclim.org/data/worldclim21.html#google_vignette. Each of these (a)biotic factors were downloaded as a “zip” file containing 12 GeoTiff (.tif) files. The code to extract data from European portion or part can be found in the R code. 

prec<- rast("bio_12.tif") #Precipitation

temp<- rast("bio_1.tif") # Temperature

stock <-rast("Pg_density.tif") #Pig density 

wild <-rast("wildboar_density.tif") # Wild boar density

horses <-rast("Ho_density.tif") # Horse density

cor <- rast("corine_rat_LEVEL2.tif") # Land cover for level 2

Clear and prepare data

Make sure that the outbreak start date is in date format

Filter out other diseases than WNF, ASF, and CWD

Filter out other species than birds using the bird_names_20220517.csv file

Only data were dowloaded for  Europe (so filter using the europe shapefile) and data between 2018 and 2023 where the period we are using is five years interval

For birds only

Needed to represent spatial vector data to include points (coordinates),

Remove these countries because we noticed that it was not part of Corine land cover

Empres_I--from different database

Only data were dowloaded for Europe (so filter using the europe shapefile) and data from 2018 and onwards

The data was pass into the function as the `coords` parameter

Remove these countries because we noticed that it was not part of Corine land cover (eg. "Ukraine", "Russian Federation", "Belarus")

Merging these two database

Remove the NA's

The spatial vector data includes points (coordinates), in view of this, their latitude and longitude were added by selecting the following species:

1. For birds only

2. For wild horses only

3. For pigs only

Extract raster values to point locations of the (a)biotic factors using the terra package and the codes are provided in R codes

Univariate negative binomial regression analysis to check for the significance of independent variables (e.g., precipitation, temperature, densities (pigs, horse, and wild boar)) on the dependent variable (i.e. disease occurrence data) by negative binomial regression

Multivariate negative binomial regression regression analysis (feed by only significant variables identified in the Univariate negative binomial regression analysis)for negative binomial regression

Time series analysis

Set all cases to 1,  for counting the number of outbreaks instead of the number of birds affected

The data was converted into weekly bases. Take note here based on the user want one can convert it to want eh or she want. 

These methods fill in missing years for each country, by adding week 1 of the missing year (zero number of detections)

Create data, neighborhood and covariate matrices

Now calculate the neighborhood (matrix) and set column and row names

Now only keep countries in the neighborhood matrix where we have outbreak data and remove the newly created water connections as well

Create a shape file with only countries where we have data - this is needed for the model

Code for checking for spatial autocorrelation:

Examining whether there are any spatial patterns in the model's residuals is one quite easy method of finding spatial autocorrelation. In order to accomplish this, we plot the latitude and longitude coordinates of the sample unit so that the points' sizes, shapes, and/or colours correspond to the residuals related to these observations.

# Acknowledgments
Thanks to Beate Conrady for supporting, reviewing, and funding the project. 

Thank you to Lene Jung Kjaer and Ana Rita Pinheiro Marques for the helpful suggestions you provided. 
