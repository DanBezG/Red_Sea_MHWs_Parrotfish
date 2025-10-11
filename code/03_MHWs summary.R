###################### Packages ######################

install.packages("pacman")
library(pacman)
p_load(tidyverse, lubridate, suncalc,openxlsx)

###################### Functions ######################

#  Function to filter fish data frame by fish IDs and arrange by fish ID and datetime
Filter_Fish <- function(fish_ids,raw_fish_df) { 
  filter_fish <- raw_fish_df %>% filter(fish_id %in% fish_ids)
  filter_fish <- filter_fish %>%
    arrange(fish_id,real_datetime)
  return(filter_fish)
}

# Function to merge two data frames conditionally based on whether they are empty or not
conditional_merge <- function(df1, df2, by_cols) {
  if (nrow(df1) == 0) {
    return(df2)
  } else if (nrow(df2) == 0) {
    return(df1)
  } else {
    return(merge(df1, df2, by = by_cols,all= T))
  }
  
}

# Function to convert time to decimal format
time_to_decimal <- function(time) {
  hours <- as.numeric(format(time, "%H"))
  minutes <- as.numeric(format(time, "%M"))
  seconds <- as.numeric(format(time, "%S"))
  decimal_time <- hours + minutes / 60 + seconds / 3600
  return(decimal_time)
}

# Function to ensure all expected columns are present in the summary data frame
Fill_In_temp_sum_heat_wave <- function(sum_heat_wave_colnames, temp_sum_heat_wave) {
  missing_col_names <- sum_heat_wave_colnames[which(!(sum_heat_wave_colnames %in% colnames(temp_sum_heat_wave)))]
  for (name in missing_col_names) {
    temp_sum_heat_wave[,name] <- NA
  }
  return(temp_sum_heat_wave)
  
}

Add_Day_Period <- function(MHW_Data) {
  
  # Eilat coordinates
  date_sun_df <- getSunlightTimes(
    data = data.frame(date = unique(MHW_Data$date),
                      lat = 29.538417,
                      lon = 34.954417),
    keep = c("nauticalDawn", "sunriseEnd", "sunsetStart", "nauticalDusk"),
    tz = "Asia/Jerusalem"
  )
  
  # Fix timezone for datetime columns (to UTC if needed)
  date_sun_df[, 4:7] <- lubridate::force_tz(date_sun_df[, 4:7], tz = "UTC")
  
  for (date_num in date_sun_df$date) {
    temp_date <- MHW_Data %>% filter(date == date_num)
    temp_sun <- date_sun_df %>% filter(date == date_num)
    
    temp_date$Period <- ifelse(
      temp_date$real_datetime >= temp_sun$nauticalDusk[1] | temp_date$real_datetime < temp_sun$nauticalDawn[1],
      "Night",
      ifelse(
        temp_date$real_datetime >= temp_sun$nauticalDawn[1] & temp_date$real_datetime < temp_sun$sunriseEnd[1],
        "Dawn",
        ifelse(
          temp_date$real_datetime >= temp_sun$sunriseEnd[1] & temp_date$real_datetime < temp_sun$sunsetStart[1],
          "Day",
          ifelse(
            temp_date$real_datetime >= temp_sun$sunsetStart[1] & temp_date$real_datetime < temp_sun$nauticalDusk[1],
            "Dusk",
            NA_character_
          )
        )
      )
    )
    
    MHW_Data$Period[MHW_Data$date == date_num] <- temp_date$Period
  }
  return(MHW_Data)
}


MHW_distance_metrics <- function(MHW_data) {
  MHW_data <- MHW_data %>%
    mutate(hour = hour(real_datetime)) %>%     # Extract hour
    Add_Day_Period(.) %>% 
    arrange(fish_id,real_datetime) %>%
    mutate(solar_date = case_when(
      Period == "Night" & hour < 12 ~ date - 1,  # Early Night (after midnight) → subtract 1 day
      TRUE ~ date  # Otherwise keep the same
    )) %>% 
    relocate(hour,Period,solar_date,.after = date) %>% 
    ungroup()
  
  # Summary by Day
  MHW_daily_distance_df <- MHW_data %>%
    group_by(fish_id, solar_date,Before_After) %>%
    filter(!is.na(distance_shore)) %>%
    summarise(
      n = n(),
      # Max daily displacement: median of the top 10% - median of the bottom 10%
      max_daily_displacement = {
        filter_distances <- distance_shore[Period == "Day"]
        filter_distances <- sort(distance_shore, na.last = NA)
        n <- length(filter_distances)
        ifelse(n>50,
               {
                 
                 top_10_percent <- filter_distances[ceiling(0.9 * n):n]
                 bottom_10_percent <- filter_distances[1:floor(0.1 * n)]
                 abs(median(top_10_percent, na.rm = TRUE) - 
                       median(bottom_10_percent,na.rm = TRUE))         
               }
               ,NA)
        
      })
      
      
  # Calculate the mean for each serial_fish_id and Before_After
  MHW_disp_summary <- MHW_daily_distance_df %>%
    group_by(fish_id,Before_After) %>%
    summarise(
      mean_disp_max = mean(max_daily_displacement, na.rm = TRUE),
      sd_disp_max = sd(max_daily_displacement, na.rm = TRUE),  # Standard deviation
      disp_max_n = sum(!is.na(max_daily_displacement)),  # Number of non-NA values
      .groups = 'drop')
  
  return(list(MHW_disp_summary,MHW_daily_distance_df))
  
}

###################### Code ######################

## Load data

# Fish acoustic data 2016-2018 & 2019-2021
combined_parrotfish_df <- readRDS("data/parrotfish data/parrotfish_acoustic_data_df.RDS") 
# tags metadata
tags_metadata <- read.xlsx("data/parrotfish data/parrotfish_metadata.xlsx")
#MHWs metadata according to different def and databases
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

####### Choose which dataset to work on through MHWs_Eilat ###########

MHWs_Eilat <- MHWs_Eilat_Fix_OISST  # Set the dataset to work with

# Prepare the fish-MHWs summary dataframe 
sum_heat_wave_day <- data.frame(fish_id=numeric(),
                                Before_After=character(),
                                mean_activity=numeric(),
                                sd_activity=numeric(),
                                act_n=numeric(),
                                Conf_int_activity=numeric(),
                                mean_depth=numeric(),
                                sd_depth=numeric(),
                                dep_n=numeric(),
                                Conf_int_depth=numeric(),
                                mean_disp_max = numeric(),
                                sd_disp_max = numeric(),
                                disp_max_n = numeric(),
                                mean_disp_night = numeric(),
                                sd_disp_night = numeric(),
                                disp_night_n = numeric(),
                                Serial=numeric())

# Loop through each heatwave event
for (event in 1:dim(MHWs_Eilat)[1]) {
  print(paste("Serial number:",event))
  # Extract fish IDs associated with the current MHW
  fish_ids <- unlist(strsplit(MHWs_Eilat$Fish_IDs, ", ")[[event]])
  
  # Check if there are valid fish IDs
  if(length(na.omit(fish_ids))>0)
  {
    # Filter fish data for the widest period and annotate with before/after and period of day
    filter_fish <- Filter_Fish(fish_ids,combined_parrotfish_df)  
    
    MHW_Data <- filter_fish %>%
      filter(date>= MHWs_Eilat$start_1.5[event] &  date<=MHWs_Eilat$end_1.5[event]) 
    MHW_Data$Before_After <- ifelse(MHW_Data$real_datetime < MHWs_Eilat$date_start[event],"Before",ifelse(MHW_Data$real_datetime > MHWs_Eilat$date_end[event],"After","MHW"))
    MHW_Data <- MHW_Data %>% arrange(fish_id,real_datetime)
    
    # Need to change the column numbers depend on the definition df
    # 20 for fix, 19 for detrended
    window_size <- 20
    start_window <-MHWs_Eilat[event,window_size]
    end_window <-MHWs_Eilat[event,window_size+2]
      
    # Filter data to the specific time window
    Windowed_MHW <- MHW_Data %>% filter(date >= start_window & date<= 
                                            end_window)
      
    # Create hour bins and check for sufficient observations (> 2 observations)
    obs_thrsh <- 2
      Windowed_MHW$hour_bin_code <- cut(Windowed_MHW$real_datetime,breaks = "1 hour")
      Windowed_MHW$hour_bin_code <-  paste(Windowed_MHW$fish_id,Windowed_MHW$hour_bin_code,Windowed_MHW$Period)
      bins_count <- Windowed_MHW %>%  
        group_by(hour_bin_code) %>% 
        summarise(
          non_na_activity = sum(!is.na(activity)),
          non_na_depth = sum(!is.na(depth)),
          non_na_dis=sum(!is.na(distance_shore))
        )
      bins_count_act <- bins_count[bins_count$non_na_activity >= obs_thrsh,] %>% select(hour_bin_code,non_na_activity)
      bins_count_dep <- bins_count[bins_count$non_na_depth >= obs_thrsh,] %>% select(hour_bin_code,non_na_depth)
      bins_count_dis <- bins_count[bins_count$non_na_dis >= obs_thrsh,] %>% select(hour_bin_code,non_na_dis)
      
      # Calculate summary statistics for activity
      temp_sum_heat_wave_act <- Windowed_MHW %>%
        filter(hour_bin_code %in% bins_count_act$hour_bin_code)%>%
        group_by(fish_id,Before_After) %>%
        summarise_at(vars(activity),funs(mean_activity = mean(.,na.rm=T),
                                         sd_activity = sd(.,na.rm=T),act_n = sum(!is.na(.)))) %>%
        mutate(Conf_int_activity=sd_activity/sqrt(act_n) * qt(p=0.975,df=act_n-1))
      temp_sum_heat_wave_act <- temp_sum_heat_wave_act[temp_sum_heat_wave_act$act_n>50,]
      
      # Calculate summary statistics for depth
      temp_sum_heat_wave_dep <- Windowed_MHW %>%
        filter(hour_bin_code %in% bins_count_dep$hour_bin_code)%>%
        group_by(fish_id,Before_After) %>%
        summarise_at(vars(depth),funs(mean_depth = mean(.,na.rm=T),
                                      sd_depth = sd(.,na.rm=T),dep_n = sum(!is.na(.)))) %>%
        mutate(Conf_int_depth=sd_depth/sqrt(dep_n) * qt(p=0.975,df=dep_n-1))
      temp_sum_heat_wave_dep <- temp_sum_heat_wave_dep[temp_sum_heat_wave_dep$dep_n>50,]
      
      # Calculate summary statistics for displacement
      temp_sum_heat_wave_disp <- Windowed_MHW %>%
        filter(hour_bin_code %in% bins_count_dis$hour_bin_code)
      if(nrow(temp_sum_heat_wave_disp)>0){
        temp_sum_heat_wave_disp <- MHW_distance_metrics(temp_sum_heat_wave_disp)[[1]]
      }
      # Merge summary statistics for all metrics
      temp_sum_heat_wave <- conditional_merge(temp_sum_heat_wave_dep,temp_sum_heat_wave_act,c("fish_id","Before_After"))
      temp_sum_heat_wave <- conditional_merge(temp_sum_heat_wave,temp_sum_heat_wave_disp,c("fish_id","Before_After"))
      temp_sum_heat_wave$Serial <- MHWs_Eilat$Serial[event]
      temp_sum_heat_wave <- Fill_In_temp_sum_heat_wave(colnames(sum_heat_wave_day),temp_sum_heat_wave)
      
      # Append the processed data to the main summary dataframe
      sum_heat_wave_day <- rbind(sum_heat_wave_day,temp_sum_heat_wave)  
      
    
  }
}
rm(temp_sum_heat_wave,temp_sum_heat_wave_act,temp_sum_heat_wave_dep,temp_sum_heat_wave_disp,bins_count,bins_count_act,bins_count_dep,bins_count_dis,Windowed_MHW,filter_fish)
# Reorganize columns in the summary dataframe
sum_heat_wave_day <- sum_heat_wave_day %>% relocate(Serial,.before = fish_id)

# Add species information from the metadata
stat_tags_metadata_parrot <- tags_metadata_parrot %>% 
  select(fish_id,species) 
sum_heat_wave_day <- distinct(merge(sum_heat_wave_day,stat_tags_metadata_parrot,by = "fish_id"))
sum_heat_wave_day$Serial_fish_id <- paste(sum_heat_wave_day$Serial,sum_heat_wave_day$fish_id,sep = "_")

#### Save the summary data 
### Choose the name of the database and def 
# fix = OISST_Fix_MHW_Stage_summary.RDS
# detrended =  OISST_detrended_MHW_Stage_summary.RDS
# saveRDS(sum_heat_wave_day,paste("results/Individual Heatwaves/OISST_Fix_MHW_Stage_summary.RDS"))


####################### Statistical tests ##########################

################### Activity Test  #######################  
sum_heat_wave_day %>%
  ggplot(aes(x = mean_activity)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_grid(~ Before_After, scales = "free") +
  theme_minimal()

sum_heat_wave_day$Serial_fish_id <- paste(sum_heat_wave_day$Serial,sum_heat_wave_day$fish_id,sep = "_")
act_complete_triplets_day <- sum_heat_wave_day %>%
  filter(!is.na(mean_activity)) %>%
  group_by(Serial_fish_id) %>%
  filter(n_distinct(Before_After) == 3) %>%
  arrange(Serial_fish_id) %>% 
  ungroup()

normality_test <- act_complete_triplets_day %>%
  group_by(Before_After) %>%
  summarise(
    n = n(),
    shapiro_p = shapiro.test(mean_activity)$p.value,
    .groups = "drop"
  )
normality_test

activity_wide <- act_complete_triplets_day %>%
  select(Serial_fish_id, Before_After, mean_activity) %>%
  pivot_wider(names_from = Before_After, values_from = mean_activity)

# Differences
activity_diff_BA <- log(activity_wide$Before) - log(activity_wide$After)
hist(activity_diff_BA)
activity_diff_BM <- log(activity_wide$Before) - log(activity_wide$MHW)
hist(activity_diff_BM)
activity_diff_MA <- log(activity_wide$MHW) - log(activity_wide$After)
hist(activity_diff_MA)

# Shapiro-Wilk tests
shapiro.test(activity_diff_BA)
shapiro.test(activity_diff_BM)
shapiro.test(activity_diff_MA)


print(paste("Number of fish for paired t test:",length(unique(act_complete_triplets_day$Serial_fish_id))))

t.test(activity_wide$Before, activity_wide$After, paired = TRUE)
t.test(activity_wide$Before, activity_wide$MHW, paired = TRUE)
t.test(activity_wide$MHW, activity_wide$After, paired = TRUE)


################### Depth Test #######################

sum_heat_wave_day %>%
  ggplot(aes(x = log(mean_depth))) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_grid(~ Before_After, scales = "free") +
  theme_minimal()

sum_heat_wave_day$Serial_fish_id <- paste(sum_heat_wave_day$Serial,sum_heat_wave_day$fish_id,sep = "_")

dep_complete_triplets_day <- sum_heat_wave_day %>%
  filter(!is.na(mean_depth)) %>%
  group_by(Serial_fish_id) %>%
  filter(n_distinct(Before_After) == 3) %>%
  arrange(Serial_fish_id) %>% 
  ungroup() %>% 
  mutate(log_depth = log(mean_depth))


normality_test <- dep_complete_triplets_day %>%
  group_by(Before_After) %>%
  summarise(
    n = n(),
    shapiro_p = shapiro.test(mean_depth)$p.value,
    .groups = "drop"
  )

normality_test

depth_wide <- dep_complete_triplets_day %>%
  select(Serial_fish_id, Before_After, mean_depth) %>%
  pivot_wider(names_from = Before_After, values_from = mean_depth)
hist(depth_wide$After)
hist(depth_wide$MHW)
hist(depth_wide$After)

# Differences
depth_diff_BA <- depth_wide$Before - depth_wide$After
hist(depth_diff_BA)
depth_diff_BM <- depth_wide$Before - depth_wide$MHW
hist(depth_diff_BM)
depth_diff_MA <- depth_wide$MHW - depth_wide$After
hist(depth_diff_MA)

# Shapiro-Wilk tests
shapiro.test(depth_diff_BA)
shapiro.test(depth_diff_BM)
shapiro.test(depth_diff_MA)

print(paste("Number of fish for paired t test:",length(unique(dep_complete_triplets_day$Serial_fish_id))))

t.test(depth_wide$Before, depth_wide$After, paired = TRUE)
t.test(depth_wide$Before, depth_wide$MHW, paired = TRUE)
t.test(depth_wide$MHW, depth_wide$After, paired = TRUE)

# For non normal - depth detrended
wilcox.test(depth_wide$Before, depth_wide$After, paired = TRUE)
wilcox.test(depth_wide$Before, depth_wide$MHW, paired = TRUE)
wilcox.test(depth_wide$MHW, depth_wide$After, paired = TRUE)

################### Distance Test ####################### 

# Clean the data for displacment
# Remove unwanted columns
disp_test_df <- disp_test_df[,-5:-12]
# Remove duplicates
disp_test_df <- disp_test_df %>% distinct()
disp_test_df <- disp_test_df %>% filter(!is.na(mean_disp_max))

disp_test_df %>%
  ggplot(aes(x = mean_disp_max)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_grid(~Before_After, scales = "free") +
  theme_minimal()

disp_complete_triplets <- disp_test_df %>%
  group_by(Serial_fish_id) %>%
  filter(disp_max_n>=5) %>% 
  filter(n_distinct(Before_After) == 3) %>%
  arrange(Serial_fish_id,Before_After) %>% 
  ungroup()


normality_test <- disp_complete_triplets %>%
  group_by(Before_After) %>%
  summarise(
    n = n(),
    shapiro_p = shapiro.test(mean_disp_max)$p.value,
    .groups = "drop"
  )
normality_test

disp_wide <- disp_complete_triplets %>%
  select(Serial_fish_id, Before_After, mean_disp_max) %>%
  pivot_wider(names_from = Before_After, values_from = mean_disp_max)

# Differences
disp_diff_BA <- disp_wide$Before - disp_wide$After
hist(disp_diff_BA)
disp_diff_BM <- disp_wide$Before - disp_wide$MHW
hist(disp_diff_BM)
disp_diff_MA <- disp_wide$MHW - disp_wide$After
hist(disp_diff_MA)

# Shapiro-Wilk tests
shapiro.test(disp_diff_BA)
shapiro.test(disp_diff_BM)
shapiro.test(disp_diff_MA)


print(paste("Number of fish for parired t test:",length(unique(disp_complete_triplets$Serial_fish_id))))

t.test(disp_wide$Before, disp_wide$After, paired = TRUE)
t.test(disp_wide$Before, disp_wide$MHW, paired = TRUE)
t.test(disp_wide$MHW, disp_wide$After, paired = TRUE) 

effectsize::effectsize(t.test(disp_wide$Before, disp_wide$After, paired = TRUE))





