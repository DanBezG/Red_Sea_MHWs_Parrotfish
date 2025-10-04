
###################### Packages ######################

install.packages("pacman")
library(pacman)
p_load(rerddap, doParallel, tidyverse,ggplot2)
# rerddap -  For easily downloading subsets of data
# doParallel - For parallel processing
# tidyverse - For data manipulation

###################### Functions ######################

# Function to download a subset of OISST data
# Red Sea coordinates
OISST_sub_dl <- function(time_df){
  OISST_dat <- griddap(datasetx = "ncdcOisst21Agg_LonPM180", 
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = c(29.472, 29.578),
                       longitude = c(34.894, 35.017),
                       fields = "sst")$data %>% 
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst,lon = longitude, lat = latitude) %>% 
    select(lon, lat, t, temp) %>% 
    na.omit()
}

###################### Code ######################

## Get OISST
rerddap::info(datasetid = "ncdcOisst21Agg_LonPM180", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

dl_years <- data.frame(date_index = 1:5,
                       start = c("1982-01-01", "1990-01-01", 
                                 "1998-01-01", "2006-01-01", "2014-01-01"),
                       end = c("1989-12-31", "1997-12-31", 
                               "2005-12-31", "2013-12-31", "2022-12-31"))
# Download all of the data with one nested request
# The time this takes will vary greatly based on connection speed
system.time(
  OISST_data <- dl_years %>% 
    group_by(date_index) %>% 
    group_modify(~OISST_sub_dl(.x)) %>% 
    ungroup() %>% 
    select(lon, lat, t, temp)
)
saveRDS(OISST_data,"data/Eilat climetology/OISST_SST_Red.RDS")


## Plot your data to check it looks OK
OISST_data %>% 
  filter(lat == OISST_data$lat[1] & lon == OISST_data$lon[1]) %>% 
  ggplot(aes(x = t, y = temp)) +
  geom_point() +
  labs(title = "OISST Data for Red Sea Point",
       x = "Date",
       y = "SST (°C)") +
  theme_minimal()
