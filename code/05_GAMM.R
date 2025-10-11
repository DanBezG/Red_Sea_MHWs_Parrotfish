###################### Packages ######################

install.packages("pacman")
library(pacman)
p_load(tidyverse, lubridate, openxlsx, mgcv, gratia,suncalc)

###################### Functions #####################

get_MHWs_fish_df <- function(MHW_def,parrotdish_df) {
  MHWs_Eilat <- MHW_def
  
  MHWs_fish_data <- data.frame()
  for (event in 1:dim(MHWs_Eilat)[1]) {
    #get the fish id relates to the specific MHWs
    fish_ids <- unlist(strsplit(MHWs_Eilat$Fish_IDs, ", ")[[event]])
    #Check if there are any fish
    if(length(na.omit(fish_ids))>0)
    {
      #filter the fish to the widest period, add before.after and period of day
      #using the right database
      filter_fish <- parrotdish_df %>% 
        filter(parrotdish_df$fish_id %in% fish_ids) %>% 
        arrange(fish_id,real_datetime)
      # Takes all detections in all MHW stages
      MHW_Data <- filter_fish %>% filter(date>= MHWs_Eilat$start_1.5[event] & date<= MHWs_Eilat$end_1.5[event])
      MHW_Data$Before_After <- ifelse(MHW_Data$real_datetime < MHWs_Eilat$date_start[event],"Before",ifelse(MHW_Data$real_datetime > MHWs_Eilat$date_end[event],"After","MHW"))
      
      #remove column of fish ids
      names(MHWs_Eilat) <- make.names(names(MHWs_Eilat), unique = TRUE)
      temp_MHWs_Eilat <- MHWs_Eilat %>% select(-Fish_IDs)
      MHW_Data <- MHW_Data %>% arrange(fish_id,real_datetime)
      MHW_Data$Serial <- MHWs_Eilat$Serial[event]
      MHW_Data <- merge(MHW_Data,temp_MHWs_Eilat,by="Serial")
      MHWs_fish_data <- rbind(MHWs_fish_data,MHW_Data)  
    }
  }
  
  # Create a column with the time of the day
  MHWs_fish_data$TimeOfDay <- as.POSIXct(format(MHWs_fish_data$real_datetime,format = "%H:%M:%S"),format = "%H:%M:%S")
  
  # Convert time of day to decimal format for modeling
  MHWs_fish_data$DecimalTimeOfDay <- time_to_decimal(MHWs_fish_data$TimeOfDay) 
  
  MHWs_fish_data <- add_solar_events(MHWs_fish_data)
  
  # Convert species and Before_After columns to factor
  MHWs_fish_data$species <-as.factor(MHWs_fish_data$species) 
  MHWs_fish_data$Before_After <- as.factor(MHWs_fish_data$Before_After)
  MHWs_fish_data$Before_After <- relevel(MHWs_fish_data$Before_After,ref="Before")
  
  # Ensure date columns are in POSIXct format
  MHWs_Eilat$date_start <- as.POSIXct(MHWs_Eilat$date_start)
  MHWs_Eilat$date_end <- as.POSIXct(MHWs_Eilat$date_end)
  
  MHWs_fish_data$Serial_fish_id <- paste0(MHWs_fish_data$Serial,"_",MHWs_fish_data$fish_id) 
  MHWs_fish_data$Serial_fish_id <- as.factor(MHWs_fish_data$Serial_fish_id)
  return(MHWs_fish_data)
}


# Function to convert time to decimal format
time_to_decimal <- function(time) {
  hours <- as.numeric(format(time, "%H"))
  minutes <- as.numeric(format(time, "%M"))
  seconds <- as.numeric(format(time, "%S"))
  decimal_time <- hours + minutes / 60 + seconds / 3600
  return(decimal_time)
}

add_solar_events <- function(df, lat = 29.538417, lon = 34.954417,
                             tz_local = "Asia/Jerusalem") {
  # Get solar events per date
  solar_events <- getSunlightTimes(
    date = unique(df$date),
    lat = lat,
    lon = lon,
    keep = c("nadir", "sunrise", "solarNoon", "sunset"),
    tz = tz_local
  )
  solar_events$sunrise <- as.POSIXct(force_tz(solar_events$sunrise,"UTC"))
  solar_events$nadir <- as.POSIXct(force_tz(solar_events$nadir,"UTC"))
  solar_events$sunset <- as.POSIXct(force_tz(solar_events$sunset,"UTC"))
  solar_events$solarNoon <- as.POSIXct(force_tz(solar_events$solarNoon,"UTC"))
  
  # Join solar_events back to df
  df <- df %>% left_join(solar_events, by = "date")
  
  df <- df %>%
    mutate(
      # civil midnights bracketing the timestamp
      midnight0 = floor_date(real_datetime, unit = "day"),
      midnight1 = midnight0 + days(1)
    ) %>%
    mutate(
      TimeOrdinal = case_when(
        # 1) Midnight -> Sunrise  (1 to <2)
        real_datetime >= midnight0 & real_datetime < sunrise ~
          1 + as.numeric(difftime(real_datetime, midnight0, units = "secs")) /
          as.numeric(difftime(sunrise, midnight0, units = "secs")),
        
        # 2) Sunrise -> Solar noon  (2 to <3)
        real_datetime >= sunrise & real_datetime < solarNoon ~
          2 + as.numeric(difftime(real_datetime, sunrise, units = "secs")) /
          as.numeric(difftime(solarNoon, sunrise, units = "secs")),
        
        # 3) Solar noon -> Sunset  (3 to <4)
        real_datetime >= solarNoon & real_datetime < sunset ~
          3 + as.numeric(difftime(real_datetime, solarNoon, units = "secs")) /
          as.numeric(difftime(sunset, solarNoon, units = "secs")),
        
        # 4) Sunset -> next Midnight  (4 to <5)
        real_datetime >= sunset & real_datetime < midnight1 ~
          4 + as.numeric(difftime(real_datetime, sunset, units = "secs")) /
          as.numeric(difftime(midnight1, sunset, units = "secs")),
        
        TRUE ~ NA_real_
      )
    ) %>%
    ungroup() %>%
    select(-midnight0, -midnight1)
  
  return(df)
}
###################### Code ##########################

## Load data

# Fish acoustic data 2016-2018 & 2019-2021
combined_parrotfish_df <- readRDS("data/parrotfish data/parrotfish_acoustic_data_df.RDS") 

# MHWs definition
MHWs_Eilat_Fix_OISST <- read.xlsx("data/MHWs_def/MHW_OISST_Events_RedSea_Fix.xlsx",sheet = "Short_MHW_Events_Fixed")
MHWs_Eilat_detrended_OISST <- read.xlsx("data/MHWs_def/MHW_OISST_Events_RedSea_Jacox.xlsx",sheet = "Short_MHW_Events_Fixed")

## Formatting

################# Fix baseline OISST #################
MHWs_Eilat_Fix_OISST$date_start <- as.Date(MHWs_Eilat_Fix_OISST$date_start,origin = "1899-12-30")
MHWs_Eilat_Fix_OISST$date_peak <- as.Date(MHWs_Eilat_Fix_OISST$date_peak,origin = "1899-12-30")
MHWs_Eilat_Fix_OISST$date_end <- as.Date(MHWs_Eilat_Fix_OISST$date_end,origin = "1899-12-30")
MHWs_Eilat_Fix_OISST$start_1.5 <- as.Date(MHWs_Eilat_Fix_OISST$start_1.5,origin = "1899-12-30")
MHWs_Eilat_Fix_OISST$end_1.5 <- as.Date(MHWs_Eilat_Fix_OISST$end_1.5,origin = "1899-12-30")
MHWs_Eilat_Fix_OISST$Serial <- c(1:dim(MHWs_Eilat_Fix_OISST)[1])
MHWs_Eilat_Fix_OISST <- MHWs_Eilat_Fix_OISST %>% relocate(Serial,.before = date_start)

################# detrended baseline OISST ############################
MHWs_Eilat_detrended_OISST$date_start <- as.Date(MHWs_Eilat_detrended_OISST$date_start,origin = "1899-12-30")
MHWs_Eilat_detrended_OISST$date_end <- as.Date(MHWs_Eilat_detrended_OISST$date_end,origin = "1899-12-30")
MHWs_Eilat_detrended_OISST$start_1.5 <- as.Date(MHWs_Eilat_detrended_OISST$start_1.5,origin = "1899-12-30")
MHWs_Eilat_detrended_OISST$end_1.5 <- as.Date(MHWs_Eilat_detrended_OISST$end_1.5,origin = "1899-12-30")
MHWs_Eilat_detrended_OISST$Serial <- c(1:dim(MHWs_Eilat_detrended_OISST)[1])
MHWs_Eilat_detrended_OISST <- MHWs_Eilat_detrended_OISST %>% relocate(Serial,.before = date_start)
# Removes date peak
MHWs_Eilat_detrended_OISST <- MHWs_Eilat_detrended_OISST[-4]


# Get fish data during MHWs 
MHWs_fish_data <- get_MHWs_fish_df(MHWs_Eilat_Fix_OISST,combined_parrotfish_df)

######### Depth Models #########

hist(MHWs_fish_data$depth) # Check distribution

MHWs_fish_data$Serial_fish_id <- paste0(MHWs_fish_data$Serial,"_",MHWs_fish_data$fish_id)
MHWs_fish_data$Serial_fish_id <- as.factor(MHWs_fish_data$Serial_fish_id)
MHWs_fish_data <- MHWs_fish_data %>% relocate(Serial_fish_id,.after = fish_id)
MHWs_fish_data$date_start <- as.POSIXct(MHWs_fish_data$date_start)
MHWs_fish_data$date_end <- as.POSIXct(MHWs_fish_data$date_end)


# Log-transform depth to address skewness in distribution
MHWs_fish_data$log_depth <- log(MHWs_fish_data$depth + 0.05)
hist(MHWs_fish_data$log_depth) # Check distribution post-transformation

# Remove Serial_fish_id levels that have only NA values for depth
MHWs_fish_data_dep_filtered <- MHWs_fish_data %>%
  filter(!is.na(depth))

print(paste("number of fish before filter:",length(unique(MHWs_fish_data_dep_filtered$Serial_fish_id))))
print(paste("number of MHWs before filter:",length(unique(MHWs_fish_data_dep_filtered$Serial))))


# Add an "hour" column
MHWs_fish_data_dep_filtered <- MHWs_fish_data_dep_filtered %>%
  mutate(hour = hour(real_datetime)) 

# Count the number of data points per date for each Serial_fish_id
hourly_depth_counts <-  MHWs_fish_data_dep_filtered %>% 
  group_by(Serial_fish_id,date,hour,Before_After,species) %>% 
  summarise(n=n(), .groups = "drop")

# Compute the mean number of data points per Serial_fish_id
mean_counts <- hourly_depth_counts %>%
  group_by(Serial_fish_id) %>%
  summarise(mean_count = mean(n),
            prop_high_activity = mean(n > 2), .groups = "drop")

# Filter out Serial_fish_id values with mean count less than 2 detections per hour mean
MHWs_fish_data_dep_filtered <- MHWs_fish_data_dep_filtered %>%
  filter(Serial_fish_id %in% mean_counts$Serial_fish_id[mean_counts$mean_count >= 2])

points_per_stage <- MHWs_fish_data_dep_filtered %>%
  group_by(Before_After, Serial_fish_id) %>%
  summarise(n = n()) %>%
  ungroup()

# Filter out MHW stages with less than 100 points
filtered_points_per_stage <- points_per_stage %>%
  filter(n >= 100)

# Find Serial_fish_ids that have only one stage left after filtering
fish_to_remove <- filtered_points_per_stage %>%
  group_by(Serial_fish_id) %>%
  summarise(stages_count = n_distinct(Before_After)) %>%
  filter(stages_count == 1) %>%
  pull(Serial_fish_id)

# Filter the original dataset based on the remaining stages and exclude the fish with only one stage
MHWs_fish_data_dep_filtered <- MHWs_fish_data_dep_filtered %>%
  filter(Serial_fish_id %in% filtered_points_per_stage$Serial_fish_id) %>%
  filter(!(Serial_fish_id %in% fish_to_remove))

print(paste("number of fish after filter:",length(unique(MHWs_fish_data_dep_filtered$Serial_fish_id)))) # 21 individuals
print(paste("number of uniqe fish after filter:",length(unique(MHWs_fish_data_dep_filtered$fish_id)))) # 17 uniqe
print(paste("number of MHWs after filter:",length(unique(MHWs_fish_data_dep_filtered$Serial)))) # 6 MHWs

hist(MHWs_fish_data_dep_filtered$log_depth)

# Create the timeline plot with facets for each Serial_fish_id
ggplot(MHWs_fish_data_dep_filtered,aes(x=real_datetime,y=depth))+
  geom_rect(
    aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = Inf),
    fill = "red", alpha = 0.2
  ) +
  geom_point() +
  theme_minimal() +
  scale_y_reverse()+
  facet_wrap(~ Serial_fish_id,scales="free")+
  labs(
    title = "Time series of depth for Each Serial Fish ID",
    x = "Time",
    y = "Depth"
  ) 

# Look for missing data at specific time of day 
ggplot(MHWs_fish_data_dep_filtered,aes(x=OrdinalTime,y=depth))+
  geom_point() +
  theme_minimal() +
  facet_wrap(~ Serial_fish_id,scales="free_y")+
  labs(
    title = "Time series of depth for Each Serial Fish ID as function of time of day",
    x = "Time",
    y = "Depth"
  ) +
  scale_y_reverse()

## Filter based on previous plot
MHWs_fish_data_dep_filtered <- MHWs_fish_data_dep_filtered %>% filter(Serial_fish_id!="1_1168793" & Serial_fish_id!="2_1255785" & Serial_fish_id!="2_1255803" & Serial_fish_id!="3_1255800" & Serial_fish_id!="4_1255800" & Serial_fish_id!="4_1255815"& Serial_fish_id!="5_1255800"& Serial_fish_id!="5_1255815"& Serial_fish_id!="3_1255815" & Serial_fish_id!="2_1255800")

## OISST Fix fish filter
# Serial_fish_id!="1_1168793" & Serial_fish_id!="2_1255785" & Serial_fish_id!="2_1255803" 
# & Serial_fish_id!="3_1255800" & Serial_fish_id!="4_1255800" & Serial_fish_id!="4_1255815"
# & Serial_fish_id!="5_1255800"& Serial_fish_id!="5_1255815" & Serial_fish_id!="3_1255815" & Serial_fish_id!="2_1255800"

## OISST detrended fish filter
# Serial_fish_id!="1_1168793" & Serial_fish_id!="3_1255806" & Serial_fish_id!="4_1255792" & Serial_fish_id!="4_1255806"

# Build Models
## Choose time of day or ordinal scale in model equations
# Time of day = DecimalTimeOfDay
# Ordinal scale = TimeOrdinal

global_dep_mod <-  bam(log_depth~s(TimeOrdinal,bs="cc",k=15),
                       MHWs_fish_data_dep_filtered,
                       method = "REML", family = gaussian(),
                       correlation = corAR1(form = ~real_datetime|Serial_fish_id),parallel = T)

base_dep_mod <- bam(log_depth~s(TimeOrdinal,bs="cc",k=15) +
                      s(TimeOrdinal,Serial_fish_id, bs = "fs", k = 6,m=1,xt = list(bs = "cc")) ,
                    MHWs_fish_data_dep_filtered,,
                    method = "REML", family = gaussian(),
                    correlation = corAR1(form = ~real_datetime|Serial_fish_id),parallel = T)
dep_sum_base_mod <- summary(base_dep_mod)

intercept_dep_mod <- bam(log_depth~Before_After+
                           s(TimeOrdinal,bs="cc",k=15) +
                           s(TimeOrdinal,Serial_fish_id, bs = "fs", k = 6,m=1,xt = list(bs = "cc")),
                         MHWs_fish_data_dep_filtered,
                         method = "REML", family = gaussian(),
                         correlation = corAR1(form = ~real_datetime|Serial_fish_id),parallel = T)

smooth_dep_mod <- bam(log_depth~s(TimeOrdinal,Serial_fish_id, bs = "fs", k = 6,m=1,xt = list(bs = "cc")) +
                        s(TimeOrdinal,by=Before_After,bs="cc",k=15),
                      MHWs_fish_data_dep_filtered, method = "REML", family = gaussian(),
                      correlation = corAR1(form = ~real_datetime|Serial_fish_id),parallel = T)

full_dep_mod <- bam(log_depth~s(TimeOrdinal,Serial_fish_id, bs = "fs", k = 6,m=1,xt = list(bs = "cc")) +
                      s(TimeOrdinal,by=Before_After,bs="cc",k=15) +
                      Before_After ,
                    MHWs_fish_data_dep_filtered,
                    method = "REML", family = gaussian(),
                    correlation = corAR1(form = ~real_datetime|Serial_fish_id),parallel = T)


# Compare Models
BIC(global_dep_mod,base_dep_mod,intercept_dep_mod,smooth_dep_mod,full_dep_mod) # Full model is best

dep_sum_full_mod <- summary(full_dep_mod)

print(paste("Deviance explained of base model:",dep_sum_base_mod$dev.expl,"Deviance explained of full model:", dep_sum_full_mod$dev.expl,"Difference is :",dep_sum_full_mod$dev.expl-dep_sum_base_mod$dev.expl))

gam.check(base_dep_mod)
gam.check(full_dep_mod)
appraise_dep <- appraise(full_dep_mod)
appraise_dep

depth_models <- list(global_mod = global_dep_mod,
                     base_mod = base_dep_mod,
                     intercept_mod = intercept_dep_mod,
                     smooth_mod = smooth_dep_mod,
                     full_mod = full_dep_mod,
                     base_sum = dep_sum_base_mod,
                     full_mod_sum = dep_sum_full_mod)

#### Save depth models 
### Choose the name of the def !
# fix = Depth_Models_OISST_Fix_ordinal.RDS
# detrended =  Depth_Models_OISST_detrended_ordinal.RDS
# saveRDS(depth_models,"results/Models/Depth_Models_OISST_Fix_ordinal.RDS")

######## Activity Models #########

hist(MHWs_fish_data$activity) # Check activity distribution 
# Remove Serial_fish_id levels that have only NA values 
MHWs_fish_data_act_filtered <- MHWs_fish_data %>%
  filter(!is.na(activity))

print(paste("number of fish before filter:", length(unique(MHWs_fish_data_act_filtered$Serial_fish_id))))
print(paste("number of MHWs before filter:",length(unique(MHWs_fish_data_act_filtered$Serial))))

# Add an "hour" column
MHWs_fish_data_act_filtered <- MHWs_fish_data_act_filtered %>%
  mutate(hour = hour(real_datetime)) 

# Count the number of data points per date for each Serial_fish_id
hourly_act_counts <-  MHWs_fish_data_act_filtered %>% 
  group_by(Serial_fish_id,date,hour,Before_After,species) %>% 
  summarise(n=n(), .groups = "drop")

# Compute the mean number of data points per Serial_fish_id
mean_counts <- hourly_act_counts %>%
  group_by(Serial_fish_id,Before_After,hour) %>%
  summarise(mean_count = mean(n))

# Filter out Serial_fish_id values with mean count less than 2 detections per hour mean
MHWs_fish_data_act_filtered <- MHWs_fish_data_act_filtered %>%
  filter(Serial_fish_id %in% mean_counts$Serial_fish_id[mean_counts$mean_count >= 2]) 

# Compute the number of data points per MHW stage
points_per_stage <- MHWs_fish_data_act_filtered %>%
  group_by(Serial_fish_id, Before_After) %>%
  summarise(n = n(),
            time_range = range(DecimalTimeOfDay),
            .groups = "drop") %>%
  arrange(n) 

# Filter out MHW stages with less than 50 points
filtered_points_per_stage <- points_per_stage %>%
  filter(n >= 100)

# Find Serial_fish_ids that have only one stage left after filtering
fish_to_remove <- filtered_points_per_stage %>%
  group_by(Serial_fish_id) %>%
  summarise(stages_count = n_distinct(Before_After)) %>%
  filter(stages_count == 1) %>%
  pull(Serial_fish_id)

# Filter the original dataset based on the remaining stages and exclude the fish with only one stage
MHWs_fish_data_act_filtered <- MHWs_fish_data_act_filtered %>%
  filter(Serial_fish_id %in% filtered_points_per_stage$Serial_fish_id) %>%
  filter(!(Serial_fish_id %in% fish_to_remove))


print(paste("number of fish after filter:",length(unique(MHWs_fish_data_act_filtered$Serial_fish_id))))
print(paste("number of unique fish after filter:",length(unique(MHWs_fish_data_act_filtered$fish_id))))
print(paste("number of MHWs after filter:",length(unique(MHWs_fish_data_act_filtered$Serial))))

hist(MHWs_fish_data_act_filtered$activity)

# Create the timeline plot with facets for each Serial_fish_id
ggplot(MHWs_fish_data_act_filtered,aes(x=real_datetime,y=activity))+
  geom_rect(
    aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = Inf),
    fill = "red", alpha = 0.2
  ) +
  geom_point() +
  theme_minimal() +
  scale_y_reverse()+
  facet_wrap(~ Serial_fish_id,scales="free")+
  labs(
    title = "Time series of activity for Each Serial Fish ID",
    x = "Time",
    y = "Activity"
  ) 

# Look for missing data at specific time of day 
ggplot(MHWs_fish_data_act_filtered,aes(x=TimeOrdinal,y=activity))+
  geom_point() +
  theme_minimal() +
  facet_wrap(~ Serial_fish_id,scales="free_y")+
  labs(
    title = "Time series of activity for Each Serial Fish ID as function of time of day",
    x = "Time",
    y = "Activity"
  ) 

## Filter based on previous plot
MHWs_fish_data_act_filtered <- MHWs_fish_data_act_filtered %>% filter( Serial_fish_id!="7_1273489" & Serial_fish_id!="1_1212923" & Serial_fish_id!="2_1255785" & Serial_fish_id!="2_1255803" &Serial_fish_id!="3_1255791" & Serial_fish_id!="3_1255800"& Serial_fish_id!="3_1255815" & Serial_fish_id!="4_1255791"& Serial_fish_id!="4_1255800" & Serial_fish_id!="4_1255814" & Serial_fish_id!="4_1255815" & Serial_fish_id!="5_1255791" & Serial_fish_id!="5_1255800" & Serial_fish_id!="4_1255814" & Serial_fish_id!="5_1255815" & Serial_fish_id!="5_1255814")

## OISST Fix fish filter
# Serial_fish_id!="7_1273489" & Serial_fish_id!="1_1212923" & Serial_fish_id!="2_1255785" & Serial_fish_id!="2_1255803" &
# Serial_fish_id!="3_1255791" & Serial_fish_id!="3_1255800"& Serial_fish_id!="3_1255815" & Serial_fish_id!="4_1255791"&
# Serial_fish_id!="4_1255800" & Serial_fish_id!="4_1255814" & Serial_fish_id!="4_1255815" & Serial_fish_id!="5_1255791" &
# Serial_fish_id!="5_1255800" & Serial_fish_id!="4_1255814" & Serial_fish_id!="5_1255815" & Serial_fish_id!="5_1255814"

## OISST detrended fish filter
# Serial_fish_id!="1_1212923" & Serial_fish_id!="2_1255791" & Serial_fish_id!="2_1255800" & Serial_fish_id!="3_1255806" &
# Serial_fish_id!="4_1255792" & Serial_fish_id!="4_1255806"

hist(MHWs_fish_data_act_filtered$activity)

# Build Models
## Choose time of day or ordinal scale in model equations
# Time of day = DecimalTimeOfDay
# Ordinal scale = TimeOrdinal

global_act_mod <-  bam(activity~s(TimeOrdinal,bs="cc",k=20),
                       MHWs_fish_data_act_filtered,
                       method = "REML", family = "tw",
                       correlation = corAR1(form = ~real_datetime|Serial_fish_id),parallel = T)

base_act_mod <- bam(activity~s(TimeOrdinal,bs="cc",k=20) +
                      s(TimeOrdinal,Serial_fish_id, bs = "fs", k = 6,m=1,xt = list(bs = "cc")),
                    MHWs_fish_data_act_filtered,
                    method = "REML", family = "tw",
                    correlation = corAR1(form = ~real_datetime|Serial_fish_id),parallel = T)
act_sum_base_mod <- summary(base_act_mod)

intercept_act_mod <- bam(activity~ Before_After + s(TimeOrdinal,bs="cc",k=20) + 
                           s(TimeOrdinal,Serial_fish_id, bs = "fs", k = 6,m=1,xt = list(bs = "cc")),
                         MHWs_fish_data_act_filtered,
                         method = "REML", family = "tw",  
                         correlation = corAR1(form = ~real_datetime|Serial_fish_id),parallel = T)

smooth_act_mod <- bam(activity~s(TimeOrdinal,Serial_fish_id, bs = "fs", k = 6,m=1,xt = list(bs = "cc")) +
                        s(TimeOrdinal,by=Before_After,bs="cc",k=20),
                      MHWs_fish_data_act_filtered, method = "REML", family ="tw",
                      correlation = corAR1(form = ~real_datetime|Serial_fish_id),parallel = T)

full_act_mod <- bam(activity~s(TimeOrdinal,Serial_fish_id, bs = "fs", k = 6,m=1,xt = list(bs = "cc")) +
                      Before_After + s(TimeOrdinal,by=Before_After,bs="cc",k=20),MHWs_fish_data_act_filtered,
                    method = "REML", family = "tw",
                    correlation = corAR1(form = ~real_datetime|Serial_fish_id),parallel = T)
act_sum_full_mod <- summary(full_act_mod)

# Compare Models
BIC(global_act_mod,base_act_mod,full_act_mod,smooth_act_mod,intercept_act_mod)

print(paste("deviance explained of base model:",act_sum_base_mod$dev.expl,"R2 of full model:", act_sum_full_mod$dev.expl,"Differnece is:",act_sum_full_mod$dev.expl-act_sum_base_mod$dev.expl))

gam.check(base_act_mod)
gam.check(full_act_mod)
appraise_act <- appraise(full_act_mod)

activity_models <- list(global_mod = global_act_mod,
                        base_mod = base_act_mod,
                        intercept_mod = intercept_act_mod,
                        smooth_mod = smooth_act_mod,
                        full_mod = full_act_mod,
                        base_sum = act_sum_base_mod,
                        full_mod_sum = act_sum_full_mod)

#### Save activity models 
### Choose the name of the def !
# fix = Activity_Models_OISST_Fix_ordinal.RDS
# detrended =  Activity_Models_OISST_detrended_ordinal.RDS
# saveRDS(activity_models,"results/Models/Activity_Models_OISST_Fix_ordinal.RDS")

