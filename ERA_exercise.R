library(terra) # to read the netCDF files
library(lubridate) # to deal with dates and times
library(dplyr) # to wrangle and tidy the data
library(tidyr) # to wrangle and tidy the data
library(ggplot2) # to make statis maps
library(gganimate) # to make a temporal gif of climate variation
library(ecmwfr) # to request data from Copernicus
library(sf) # to extract coordinates from spatial objects
library(rnaturalearth) #natural earth library, good for making spatial things
library(RNetCDF)
library(ncdf4)
library(stars)
library(tidyverse)
library(ggpubr)
library(tidyterra)
library(rnaturalearthhires)
library(pkgbuild)



wf_set_key()

wf_datasets()


## Here we are creating an area file so we can attempt to 
## look at a location outside the USA

## Let's make a polygon (sf object) of Cornwall, UK

uk_states_ne <- ne_states(country = 'united kingdom',    
                          returnclass = 'sf')
cornwall_ne<-uk_states_ne[which(uk_states_ne$name == "Cornwall"), ]

## have a nice look at the lower left bit of things. 
## I used to go on holiday there as a child. 
## Fans of Poldark might also know it.
plot(cornwall_ne)

#Get the bounding box for your polygon
st_bbox(cornwall_ne)

#######################
## WARNING!
## This bounding box outcome is NOT in the right format 
## for your query to ERA (see below); the coordinate 
## pairs are 'flipped'
#######################

## Quick tip - take a coordinate pair and put it in 
## Google Maps to see if it's anywhere near your study area

## To send a data request, you make the list of the 
## information for the request 
## then, you send the request. Note the two different 
## chunks of script for this.
## The documentation in the ECMWF R package is there, 
## but not awesome.

## Go to your user profile on Copernicus, 
## and accept ALL the licenses. Second Tab!
request <- list(
  "dataset_short_name" = "reanalysis-era5-land-monthly-means",
  "format" = "netcdf",
  "product_type" = "monthly_averaged_reanalysis_by_hour_of_day",
  "variable" = c(
    "2m_temperature",
    "total_precipitation"
  ),
  "month" = sprintf("%02d", 1:12),
  "time" = sprintf("%02d:00", 0:23),
  "year" = as.character(2022),
  "target" = "ERA5land_hr_Cornwall_2022a.nc",
  "area" = "49.959133/-5.715647/50.922991/-4.180043"
)
wf_request(request = request)




test_r<-rast("data_stream-mnth.nc")
plot(test_r[[1]])

## If you used the Cornwall bounding box, you 
## should see a very large pixel version of Cornwall

## Extracting that one time slice and changing it to a 
## dataframe for plotting reasons
tr<-rast(test_r[[1]])
df <- as.data.frame(test_r[[1]], xy = TRUE)

## Rename columns if needed 
## (e.g., if the variable name is "temp")
colnames(df) <- c("x", "y", "Temp (K)")

## Making both our vector map of Cornwall and our dataframe
## into plotable objects for ggplot
cw<-vect(cornwall_ne)
df2<-rast(df)


ggplot()+
  geom_raster(data=df2, aes(x = x, y = y, fill = `Temp (K)`)) +
  scale_fill_viridis_c(na.value=NA) +
  geom_sf(data = cw, color="black",
          fill=NA, size=0.25)+
  theme_bw()


