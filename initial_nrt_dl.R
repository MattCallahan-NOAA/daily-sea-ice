library(akmarineareas2)
library(sf)
library(tidyverse)
library(rerddap)
library(lubridate)

# load spatial data
# combine BS and Arctic subregions/regions into one df
bs <-esr_dd %>% filter(Ecosystem_Area =="Eastern Bering Sea")
arctic <-lme_dd %>% filter(Ecosystem_Area =="Arctic") %>%
  mutate(Ecosystem_Subarea="Arctic")

bsa<-bs %>%
  bind_rows(arctic)

#turn off spherical geometry
sf_use_s2(FALSE)

cells_per_region <-readRDS("cells_per_region.RDS")

#specify ice extent... large since it includes the artic, which extends far east
min_long<- (-179.975)
max_long<- (-135)
min_lat<- (53)
max_lat<- (75)

# using CRW as an example since the the polar watch source is in m
info_NR_ice<-rerddap::info(datasetid = "dhw_5km", url = "https://pae-paha.pacioos.hawaii.edu/erddap/")

# download nrt time series up to present once
nrt_ice <- griddap(info_NR_ice, 
                   latitude = c(min_lat, max_lat), 
                   longitude = c(min_long, max_long), 
                   time = c("2024-01-01T12:00:00Z",as.character(Sys.Date()-2)), 
                   fields = 'CRW_SEAICE')

nrt_ice <- as.data.frame(nrt_ice$data) %>%
  mutate(date=as.Date(time),
         week=week(date))

# set up weeks to run this in batches
myweeks <- 1:22

#write function to process
subregion_summarize <- function(data) {
  # convert to spatial object
  data <- data %>%
    st_as_sf(coords=c("longitude", "latitude"), crs=4326)
  
  #point in poly operation
  data <- data %>%
    st_join(bsa, join=st_intersects) %>%
    filter(!is.na(Ecosystem_Subarea)) %>%
    data.frame()
  # summarize by region and day
  data <- data %>%
    group_by(Ecosystem_Subarea, date) %>%
    summarize(ice_total=sum(CRW_SEAICE, na.rm=T)) %>%
    left_join(cells_per_region, by="Ecosystem_Subarea") %>%
    mutate(ice_fraction=ice_total/count) %>%
    dplyr::select(date, Ecosystem_Subarea, ice_fraction)
  
  return(data)
  
}

 nrt_ice_fraction <- lapply(myweeks, 
        FUN=function(x) subregion_summarize(nrt_ice %>% 
                                                       filter(week==x))) %>%
   bind_rows()


# save
saveRDS(nrt_ice_fraction, "nrt_ice_ts.RDS")