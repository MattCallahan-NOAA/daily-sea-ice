library(akmarineareas2)
library(sf)
library(tidyverse)
library(rerddap)
library(pins)

# load spatial data
# combine BS and Arctic subregions/regions into one df
bs <-esr_dd %>% filter(Ecosystem_Area =="Eastern Bering Sea")
arctic <-lme_dd %>% filter(Ecosystem_Area =="Arctic") %>%
  mutate(Ecosystem_Subarea="Arctic")

bsa<-bs %>%
  bind_rows(arctic)

#save so I don't have to use akmarineareas2 on posit connect right now...
#saveRDS(bsa, "bsa.RDS")

#turn off spherical geometry
sf_use_s2(FALSE)

#specify ice extent... large since it includes the artic, which extends far east
min_long<- (-179.975)
max_long<- (-135)
min_lat<- (53)
max_lat<- (75)

# define data set
#info_NR_ice<-rerddap::info(datasetid = "nsidcG10016v2nh1day", url = "https://polarwatch.noaa.gov/erddap/")

# using CRW as an example since the the polar watch source is in m
info_NR_ice<-rerddap::info(datasetid = "dhw_5km", url = "https://pae-paha.pacioos.hawaii.edu/erddap/")


# load time series
nrt_ice_ts <- readRDS("nrt_ice_ts.RDS")

new_start <- as.character(max(nrt_ice_ts$date)+1)


# Download current day of data
recent_day_ice <- griddap(info_NR_ice, 
                          latitude = c(min_lat, max_lat), 
                          longitude = c(min_long, max_long), 
                          time = c(new_start,'last'), 
                          fields = 'CRW_SEAICE')

# save as a data frame
df<-as.data.frame(recent_day_ice$data)


# attribute region to ice points
recent_ice_sf <- df %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) 

#load spatial data
bsa<-readRDS("bsa.RDS")

recent_ice_bsa <- recent_ice_sf %>%
  st_join(bsa, join=st_intersects) %>%
  filter(!is.na(Ecosystem_Subarea))

recent_ice_bsa <-recent_ice_bsa %>%
  data.frame()

#load number of cells per region to calculate fraction
# number of cells per region
# The below code is how this was calculated, but only needed once.
# recent_ice_bsa %>%
#   group_by(Ecosystem_Subarea) %>%
#   summarize(count=n()) %>%
#   saveRDS("cells_per_region.RDS")
cells_per_region <-  readRDS("cells_per_region.RDS")

# Summarize
recent_ice_fraction <- recent_ice_bsa %>%
  group_by(Ecosystem_Subarea, time) %>%
  summarize(ice_total=sum(CRW_SEAICE, na.rm=T)) %>%
  left_join(cells_per_region, by="Ecosystem_Subarea") %>%
  mutate(ice_fraction=ice_total/count,
         date=as.Date(time)) %>%
  dplyr::select(date, Ecosystem_Subarea, ice_fraction)
  

# function to doublecheck that we aren't duplicating data
update_fun <- function(old_df, new_df) {
  new_date <- max(new_df$date)
  old_date <- max(old_df$date)
  if(new_date <= old_date) {stop()}
  return(old_df %>%
           bind_rows(new_df))
}

nrt_ice_ts<-update_fun(old_df=nrt_ice_ts, new_df = recent_ice_fraction)

saveRDS(nrt_ice_ts, "nrt_ice_ts.RDS")

