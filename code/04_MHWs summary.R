###################### Packages ######################

# Load necessary packages (using pacman for easy installation/loading)
install.packages("pacman")
library(pacman)
p_load(tidyverse, lubridate, suncalc,openxlsx,lme4,lmerTest,DHARMa,car,mgcv,gratia,emmeans)

###################### Functions ######################

# Filter and sort raw fish data by specific IDs
Filter_Fish <- function(fish_ids,raw_fish_df) { 
  filter_fish <- raw_fish_df %>% filter(fish_id %in% fish_ids)
  filter_fish <- filter_fish %>%
    arrange(fish_id,real_datetime)
  return(filter_fish)
}

# Safely merge two data frames, returning the non-empty one if the other is empty
conditional_merge <- function(df1, df2, by_cols) {
  if (nrow(df1) == 0) {
    return(df2)
  } else if (nrow(df2) == 0) {
    return(df1)
  } else {
    return(merge(df1, df2, by = by_cols,all= T))
  }
  
}

# Convert time format to decimal hours
time_to_decimal <- function(time) {
  hours <- as.numeric(format(time, "%H"))
  minutes <- as.numeric(format(time, "%M"))
  seconds <- as.numeric(format(time, "%S"))
  decimal_time <- hours + minutes / 60 + seconds / 3600
  return(decimal_time)
}

# Ensure all expected columns exist in a dataframe by filling missing ones with NA
Fill_In_temp_sum_heat_wave <- function(sum_heat_wave_colnames, temp_sum_heat_wave) {
  missing_col_names <- sum_heat_wave_colnames[which(!(sum_heat_wave_colnames %in% colnames(temp_sum_heat_wave)))]
  for (name in missing_col_names) {
    temp_sum_heat_wave[,name] <- NA
  }
  return(temp_sum_heat_wave)
  
}

# Classify each timestamp into Day, Night, Dawn, or Dusk using local sunlight times
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

# Calculate daily maximum displacement based on decile range (top 10% - bottom 10%)
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
    group_by(fish_id, solar_date,Before_After,array) %>%
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
    group_by(fish_id,Before_After,array) %>%
    summarise(
      mean_disp_max = mean(max_daily_displacement, na.rm = TRUE),
      sd_disp_max = sd(max_daily_displacement, na.rm = TRUE),  # Standard deviation
      disp_max_n = sum(!is.na(max_daily_displacement)),  # Number of non-NA values
      .groups = 'drop')
  
  return(list(MHW_disp_summary,MHW_daily_distance_df))
  
}

# Generate and save diagnostic plots for all models in a specific time window
create_diganostics_pdf <- function(target_window,baseline_type,all_models)
{
  mods <- all_models[[target_window]]$Models
  pdf_filename <- paste0("results/Mean Models/Diagnostics_Window_", target_window, "_", baseline_type, ".pdf")
  pdf(file = pdf_filename, width = 10, height = 8)
  
  print(appraise(mods$Activity$Maximal) + patchwork::plot_annotation(title = paste("Activity - Maximal Model - Window", target_window)))
  print(appraise(mods$Activity$MainEffects) + patchwork::plot_annotation(title = paste("Activity - Main Effects Model - Window", target_window)))
  print(appraise(mods$Activity$Basic) + patchwork::plot_annotation(title = paste("Activity - Basic Model - Window", target_window)))
  
  print(appraise(mods$Depth$Maximal) + patchwork::plot_annotation(title = paste("Depth - Maximal Model - Window", target_window)))
  print(appraise(mods$Depth$MainEffects) + patchwork::plot_annotation(title = paste("Depth - Main Effects Model - Window", target_window)))
  print(appraise(mods$Depth$Basic) + patchwork::plot_annotation(title = paste("Depth - Basic Model - Window", target_window)))
  
  if (!is.na(mods$Displacement$Maximal)[1]) {
    print(appraise(mods$Displacement$Maximal) + patchwork::plot_annotation(title = paste("Displacement - Maximal Model - Window", target_window)))
    print(appraise(mods$Displacement$MainEffects) + patchwork::plot_annotation(title = paste("Displacement - Main Effects Model - Window", target_window)))
    print(appraise(mods$Displacement$Basic) + patchwork::plot_annotation(title = paste("Displacement - Basic Model - Window", target_window)))
  } else {
    print("No Displacement models to plot for this window.")
  }
  
  dev.off()
  
  print(paste("All diagnostics for window", target_window, "saved to", pdf_filename))
}

###################### Code ######################

## Load data

# Fish acoustic data 2016-2018 & 2019-2021
combined_parrotfish_df <- readRDS("data/parrotfish data/parrotfish_acoustic_data_df.RDS")

# Define which array based on deployment date
combined_parrotfish_df$array <- ifelse(combined_parrotfish_df$real_datetime<as.POSIXct("2018-08-07"),"Linear","Paved")
combined_parrotfish_df$array <- as.factor(combined_parrotfish_df$array)

# Load tags and MHWs metadata
tags_metadata <- read.xlsx("data/parrotfish data/parrotfish_metadata.xlsx")
MHWs_Eilat_Fix_OISST <- read.xlsx("data/MHWs_def/MHWs_filtered/MHW_OISST_Events_RedSea_Fix_filtered.xlsx",sheet = "Short_MHW_Events")
MHWs_Eilat_detrended_OISST <- read.xlsx("data/MHWs_def/MHWs_filtered/MHW_OISST_Events_RedSea_Jacox_filtered.xlsx",sheet = "Short_MHW_Events")

## Formatting

################# Fix baseline OISST #################
# Convert dates and assign serial IDs
MHWs_Eilat_Fix_OISST$date_start <- convertToDate(MHWs_Eilat_Fix_OISST$date_start)
MHWs_Eilat_Fix_OISST$date_end <- convertToDate(MHWs_Eilat_Fix_OISST$date_end)
MHWs_Eilat_Fix_OISST$start_1week <- convertToDate(MHWs_Eilat_Fix_OISST$start_1week)
MHWs_Eilat_Fix_OISST$end_1week <- convertToDate(MHWs_Eilat_Fix_OISST$end_1week)
MHWs_Eilat_Fix_OISST$start_1.5 <- convertToDate(MHWs_Eilat_Fix_OISST$start_1.5)
MHWs_Eilat_Fix_OISST$end_1.5 <- convertToDate(MHWs_Eilat_Fix_OISST$end_1.5)
MHWs_Eilat_Fix_OISST$start_3 <- convertToDate(MHWs_Eilat_Fix_OISST$start_3)
MHWs_Eilat_Fix_OISST$end_3 <- convertToDate(MHWs_Eilat_Fix_OISST$end_3)
MHWs_Eilat_Fix_OISST$Serial <- c(1:dim(MHWs_Eilat_Fix_OISST)[1])
MHWs_Eilat_Fix_OISST <- MHWs_Eilat_Fix_OISST %>% relocate(Serial,.before = date_start)

################# detrended baseline OISST ############################
# Convert dates and assign serial IDs
MHWs_Eilat_detrended_OISST$date_start <- convertToDate(MHWs_Eilat_detrended_OISST$date_start)
MHWs_Eilat_detrended_OISST$date_end <- convertToDate(MHWs_Eilat_detrended_OISST$date_end)
MHWs_Eilat_detrended_OISST$start_1week <- convertToDate(MHWs_Eilat_detrended_OISST$start_1week)
MHWs_Eilat_detrended_OISST$end_1week <- convertToDate(MHWs_Eilat_detrended_OISST$end_1week)
MHWs_Eilat_detrended_OISST$start_1.5 <- convertToDate(MHWs_Eilat_detrended_OISST$start_1.5)
MHWs_Eilat_detrended_OISST$end_1.5 <- convertToDate(MHWs_Eilat_detrended_OISST$end_1.5)
MHWs_Eilat_detrended_OISST$start_3 <- convertToDate(MHWs_Eilat_detrended_OISST$start_3)
MHWs_Eilat_detrended_OISST$end_3 <- convertToDate(MHWs_Eilat_detrended_OISST$end_3)
MHWs_Eilat_detrended_OISST$Serial <- c(1:dim(MHWs_Eilat_detrended_OISST)[1])
MHWs_Eilat_detrended_OISST <- MHWs_Eilat_detrended_OISST %>% relocate(Serial,.before = date_start)
# Removes date peak
MHWs_Eilat_detrended_OISST <- MHWs_Eilat_detrended_OISST[-4]

####### Choose which dataset to work on through MHWs_Eilat ###########

MHWs_Eilat <- MHWs_Eilat_detrended_OISST  # Set the dataset to work with
baseline_type <- "fix"  # fix or detrended
time_windows <- c("1week","1.5", "3")
all_windows_results <- list()

# Loop through predefined time window multipliers
for (current_window in time_windows) {
  print(paste("======================================================"))
  print(paste("           Running analysis for window:", current_window))
  print(paste("======================================================"))
  
  # Initialize dataframe to store aggregated fish-MHW metrics
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
                                  Serial=numeric(),
                                  array = factor())
  
  # Loop through each heatwave event
  for (event in 1:dim(MHWs_Eilat)[1]) {
    print(paste("Serial number:",event))
    # Extract fish IDs associated with the current MHW
    fish_ids <- unlist(strsplit(MHWs_Eilat$Fish_IDs, ", ")[[event]])
    
    # Check if there are valid fish IDs
    if(length(na.omit(fish_ids))>0)
    {
      # Filter fish data for the specified window and annotate stages (Before/MHW/After)
      filter_fish <- Filter_Fish(fish_ids,combined_parrotfish_df)  
      col_start <- paste0("start_", current_window)
      col_end   <- paste0("end_", current_window)
      
      MHW_Data <- filter_fish %>%
        filter(date >= MHWs_Eilat[[col_start]][event] & 
                 date <= MHWs_Eilat[[col_end]][event])
      MHW_Data$Before_After <- ifelse(MHW_Data$real_datetime < MHWs_Eilat$date_start[event],"Before",ifelse(MHW_Data$real_datetime > MHWs_Eilat$date_end[event],"After","MHW"))
      MHW_Data <- MHW_Data %>% arrange(fish_id,real_datetime)
      
      # Create hour bins and ensure sufficient data density (> 2 observations per hour)
      obs_thrsh <- 2
      MHW_Data$hour_bin_code <- cut(MHW_Data$real_datetime,breaks = "1 hour")
      MHW_Data$hour_bin_code <-  paste(MHW_Data$fish_id,MHW_Data$hour_bin_code,MHW_Data$Period)
      bins_count <- MHW_Data %>%  
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
      temp_sum_heat_wave_act <- MHW_Data %>%
        filter(hour_bin_code %in% bins_count_act$hour_bin_code)%>%
        group_by(fish_id,Before_After,array) %>%
        summarise_at(vars(activity),funs(mean_activity = mean(.,na.rm=T),
                                         sd_activity = sd(.,na.rm=T),act_n = sum(!is.na(.)))) %>%
        mutate(Conf_int_activity=sd_activity/sqrt(act_n) * qt(p=0.975,df=act_n-1))
      temp_sum_heat_wave_act <- temp_sum_heat_wave_act[temp_sum_heat_wave_act$act_n>50,]
      
      # Calculate summary statistics for depth
      temp_sum_heat_wave_dep <- MHW_Data %>%
        filter(hour_bin_code %in% bins_count_dep$hour_bin_code)%>%
        group_by(fish_id,Before_After,array) %>%
        summarise_at(vars(depth),funs(mean_depth = mean(.,na.rm=T),
                                      sd_depth = sd(.,na.rm=T),dep_n = sum(!is.na(.)))) %>%
        mutate(Conf_int_depth=sd_depth/sqrt(dep_n) * qt(p=0.975,df=dep_n-1))
      temp_sum_heat_wave_dep <- temp_sum_heat_wave_dep[temp_sum_heat_wave_dep$dep_n>50,]
      
      # Calculate summary statistics for displacement
      temp_sum_heat_wave_disp <- MHW_Data %>%
        filter(hour_bin_code %in% bins_count_dis$hour_bin_code)
      if(nrow(temp_sum_heat_wave_disp)>0){
        temp_sum_heat_wave_disp <- MHW_distance_metrics(temp_sum_heat_wave_disp)[[1]]
      }
      # Merge summary statistics for all metrics into a single row per fish/stage
      temp_sum_heat_wave <- conditional_merge(temp_sum_heat_wave_dep,temp_sum_heat_wave_act,c("fish_id","Before_After","array"))
      temp_sum_heat_wave <- conditional_merge(temp_sum_heat_wave,temp_sum_heat_wave_disp,c("fish_id","Before_After","array"))
      temp_sum_heat_wave$Serial <- MHWs_Eilat$Serial[event]
      temp_sum_heat_wave <- Fill_In_temp_sum_heat_wave(colnames(sum_heat_wave_day),temp_sum_heat_wave)
      
      # Append the processed data to the main summary dataframe
      sum_heat_wave_day <- rbind(sum_heat_wave_day,temp_sum_heat_wave)  
      
      
    }
  }
  rm(temp_sum_heat_wave,temp_sum_heat_wave_act,temp_sum_heat_wave_dep,temp_sum_heat_wave_disp,bins_count,bins_count_act,bins_count_dep,bins_count_dis,MHW_Data,filter_fish)
  
  # Reorganize columns in the summary dataframe
  sum_heat_wave_day <- sum_heat_wave_day %>% relocate(Serial,.before = fish_id)
  
  # Append species morphology data (length, weight) from metadata
  stat_tags_metadata_parrot <- tags_metadata %>% 
    select(fish_id,species,`Weight.(gr)`,`Length.(cm)`) 
  sum_heat_wave_day <- distinct(merge(sum_heat_wave_day,stat_tags_metadata_parrot,by = "fish_id"))
  sum_heat_wave_day$Serial_fish_id <- paste(sum_heat_wave_day$Serial,sum_heat_wave_day$fish_id,sep = "_")
  sum_heat_wave_day <- sum_heat_wave_day %>%
    rename(Length_cm = `Length.(cm)`)
  
  # Convert relevant columns to factors
  sum_heat_wave_day$Serial <- as.factor(sum_heat_wave_day$Serial)
  sum_heat_wave_day$species <- as.factor(sum_heat_wave_day$species)
  sum_heat_wave_day$fish_id <- as.factor(sum_heat_wave_day$fish_id)
  sum_heat_wave_day$Before_After <- factor(sum_heat_wave_day$Before_After, levels = c("Before", "MHW", "After"))
  
  ####################### Statistical tests ##########################
  
  # Filter for fish that have data across all 3 stages (Before, MHW, After) for Activity
  act_complete_triplets_day <- sum_heat_wave_day %>%
    filter(!is.na(mean_activity)& !is.na(Length_cm)) %>%
    group_by(Serial_fish_id) %>%
    filter(n_distinct(Before_After) == 3) %>%
    arrange(Serial_fish_id) %>% 
    ungroup()
  top_species <- names(sort(table(act_complete_triplets_day$species), decreasing = TRUE))[1]
  act_complete_triplets_day$species <- relevel(as.factor(act_complete_triplets_day$species), ref = top_species)
  
  # Filter for fish that have data across all 3 stages for Depth
  dep_complete_triplets_day <- sum_heat_wave_day %>%
    filter(!is.na(mean_depth)& !is.na(Length_cm)) %>%
    group_by(Serial_fish_id) %>%
    filter(n_distinct(Before_After) == 3) %>%
    arrange(Serial_fish_id) %>% 
    ungroup() 
  top_species <- names(sort(table(dep_complete_triplets_day$species), decreasing = TRUE))[1]
  dep_complete_triplets_day$species <- relevel(as.factor(dep_complete_triplets_day$species), ref = top_species)
  
  # Clean and filter data for Displacement models
  disp_test_df <- sum_heat_wave_day
  disp_test_df <- disp_test_df[,-4:-11] # Remove unwanted columns
  disp_test_df <- disp_test_df %>% distinct()
  disp_test_df <- disp_test_df %>% filter(!is.na(mean_disp_max))
  disp_complete_triplets <- disp_test_df %>%
    group_by(Serial_fish_id) %>%
    filter(disp_max_n>=5& !is.na(Length_cm)) %>% 
    filter(n_distinct(Before_After) == 3) %>%
    arrange(Serial_fish_id,Before_After) %>% 
    ungroup()
  top_species <- names(sort(table(disp_complete_triplets$species), decreasing = TRUE))[1]
  disp_complete_triplets$species <- relevel(as.factor(disp_complete_triplets$species), ref = top_species)
  
  # Specific exclusion for the detrended baseline due to sensor anomaly
  if (baseline_type == "detrended") {
    print("Applying detrended baseline filters: Removing fish 1255815 (Serial 2) from Depth...")
    dep_complete_triplets_day <- dep_complete_triplets_day %>%
      filter(!(fish_id == 1255815 & Serial == 2))
  }
  
  # Define Generalized Additive Mixed Models (GAMM) formulas
  re_serial <- ifelse(baseline_type == "detrended", "", "+ s(Serial, bs = 're')")
  
  act_form_max  <- as.formula(paste("mean_activity ~ Before_After * species + scale(Length_cm) + s(fish_id, bs = 're')", re_serial))
  act_form_main <- as.formula(paste("mean_activity ~ Before_After + species + scale(Length_cm) + s(fish_id, bs = 're')", re_serial))
  act_form_base <- as.formula(paste("mean_activity ~ Before_After + species + s(fish_id, bs = 're')", re_serial))
  
  dep_form_max  <- as.formula(paste("mean_depth ~ Before_After * species + scale(Length_cm) + s(fish_id, bs = 're')", re_serial))
  dep_form_main <- as.formula(paste("mean_depth ~ Before_After + species + scale(Length_cm) + s(fish_id, bs = 're')", re_serial))
  dep_form_base <- as.formula(paste("mean_depth ~ Before_After + species + s(fish_id, bs = 're')", re_serial))
  
  disp_form_max  <- as.formula(paste("mean_disp_max ~ Before_After * species + scale(Length_cm) + s(fish_id, bs = 're')", re_serial))
  disp_form_main <- as.formula(paste("mean_disp_max ~ Before_After + species + scale(Length_cm) + s(fish_id, bs = 're')", re_serial))
  disp_form_base <- as.formula(paste("mean_disp_max ~ Before_After + species + s(fish_id, bs = 're')", re_serial))
  
  # Fit GAMs for Activity (using Tweedie distribution)
  print("Fitting Activity GAMs (Maximal, Main, Basic)...")
  act_gam_max  <- gam(act_form_max,  data = act_complete_triplets_day, family = tw(), method = "REML")
  act_gam_main <- gam(act_form_main, data = act_complete_triplets_day, family = tw(), method = "REML")
  act_gam_base <- gam(act_form_base, data = act_complete_triplets_day, family = tw(), method = "REML")
  
  # Fit GAMs for Depth (using Gaussian distribution on log-transformed data)
  print("Fitting Depth GAMs (Maximal, Main, Basic)...")
  dep_gam_max  <- gam(dep_form_max,  data = dep_complete_triplets_day, method = "REML",family = gaussian(link = "log"))
  dep_gam_main <- gam(dep_form_main, data = dep_complete_triplets_day, method = "REML",family = gaussian(link = "log"))
  dep_gam_base <- gam(dep_form_base, data = dep_complete_triplets_day, method = "REML",family = gaussian(link = "log"))
  
  # Fit GAMs for Displacement (skipping if using detrended baseline due to insufficient data)
  print("Fitting Displacement GAMs (with empty-data protection)...")
  if (baseline_type != "detrended") {
    disp_gam_max  <- gam(disp_form_max,  data = disp_complete_triplets, family = tw(), method = "REML")
    disp_gam_main <- gam(disp_form_main, data = disp_complete_triplets, family = tw(), method = "REML")
    disp_gam_base <- gam(disp_form_base, data = disp_complete_triplets, family = tw(), method = "REML")
  } else {
    print("No displacement data, skipping models.")
    disp_gam_max  <- NA
    disp_gam_main <- NA
    disp_gam_base <- NA
  }
  
  # Store all processed data and fitted models in a nested list
  all_windows_results[[current_window]] <- list(
    Data_Summary = sum_heat_wave_day,
    Triplets = list(Activity = act_complete_triplets_day, Depth = dep_complete_triplets_day, Displacement = disp_complete_triplets),
    Models = list(
      Activity = list(
        Maximal = act_gam_max, 
        MainEffects = act_gam_main, 
        Basic = act_gam_base
      ),
      Depth = list(
        Maximal = dep_gam_max, 
        MainEffects = dep_gam_main, 
        Basic = dep_gam_base
      ),
      Displacement = list(
        Maximal = disp_gam_max, 
        MainEffects = disp_gam_main, 
        Basic = disp_gam_base
      )
    )
  )
}

### Choose the name of the database and def 
# fix = All_Windows_Models_OISST_Fix.RDS
# detrended =  All_Windows_Models_OISST_detrended.RDS
# saveRDS(all_windows_results, "results/Mean Models/All_Windows_Models_OISST_Fix.RDS")


######################  Diagnostics  ##########################
###### Choose baseline and timeframe window
all_windows_results <- readRDS("results/Mean Models/All_Windows_Models_OISST_detrended.RDS")

###################### AIC Model Selection Summary  ##########################

# Compare candidate models using Akaike Information Criterion (AIC)
aic_results_list <- list()
counter <- 1

for (window in names(all_windows_results)) {
  for (metric in names(all_windows_results[[window]]$Models)) {
    
    mods <- all_windows_results[[window]]$Models[[metric]]
    
    valid_mods <- mods[sapply(mods, function(x) length(x) > 1 && !is.na(x)[1])]
    
    if (length(valid_mods) > 1) {
      
      aic_table <- do.call(AIC, unname(valid_mods))
      
      temp_aic_df <- data.frame(
        Window = window,
        Metric = metric,
        Model_Complexity = names(valid_mods), 
        df = aic_table$df,
        AIC = aic_table$AIC
      )
      
      # Calculate Delta AIC and Akaike weights to identify the best fit
      temp_aic_df <- temp_aic_df %>%
        mutate(
          Delta_AIC = AIC - min(AIC),
          Weight = exp(-0.5 * Delta_AIC) / sum(exp(-0.5 * Delta_AIC))
        ) %>%
        arrange(Delta_AIC)
      
      temp_aic_df$Winner <- ifelse(temp_aic_df$Delta_AIC == 0, "★ WINNER", "")
      
      aic_results_list[[counter]] <- temp_aic_df
      counter <- counter + 1
    }
  }
}

final_aic_comparison_table <- bind_rows(aic_results_list)

cat("\n>>> AIC Selection Summary <<<\n")
print(as.data.frame(final_aic_comparison_table))

###################### Create Diagnostics plots ##########################
# Export model residual diagnostics to PDF
create_diganostics_pdf(target_window = "1week", baseline_type = "fixed", all_models = all_windows_results)


###################### Models Results summary ##########################
# Extract and format ANOVA tables (parametric and smooth terms) for all fitted models
anova_tables_list <- list()
counter <- 1

for (window in names(all_windows_results)) {
  for (metric in names(all_windows_results[[window]]$Models)) {
    for (mod_complexity in names(all_windows_results[[window]]$Models[[metric]])) {
      
      
      mod <- all_windows_results[[window]]$Models[[metric]][[mod_complexity]]
      
      
      if (length(mod) > 1 && !is.na(mod)[1]) {
        
        
        mod_anova <- anova.gam(mod)
        
        
        if (!is.null(mod_anova$pTerms.table)) {
          p_table <- as.data.frame(mod_anova$pTerms.table)
          p_table$Term <- rownames(p_table)
          rownames(p_table) <- NULL
          p_table$Term_Type <- "Parametric (Fixed)"
          
          p_table <- p_table %>%
            rename(
              Degrees_of_Freedom = df,
              F_Value = F,
              p_value = `p-value`
            ) %>%
            mutate(Estimated_Degrees_of_Freedom = NA) # NA for fixed effects
        } else {
          p_table <- data.frame()
        }
        
        if (!is.null(mod_anova$s.table)) {
          s_table <- as.data.frame(mod_anova$s.table)
          s_table$Term <- rownames(s_table)
          rownames(s_table) <- NULL
          s_table$Term_Type <- "Smooth (Random/Spline)"
          
          s_table <- s_table %>%
            rename(
              Estimated_Degrees_of_Freedom = edf,
              Degrees_of_Freedom = Ref.df,
              F_Value = F,
              p_value = `p-value`
            )
        } else {
          s_table <- data.frame()
        }
        
        combined_table <- bind_rows(p_table, s_table)
        
        if (nrow(combined_table) > 0) {
          combined_table$Window <- window
          combined_table$Metric <- metric
          combined_table$Model_Complexity <- mod_complexity
          
          combined_table <- combined_table %>%
            relocate(Window, Metric, Model_Complexity, Term_Type, Term) %>%
            select(Window, Metric, Model_Complexity, Term_Type, Term, 
                   Degrees_of_Freedom, Estimated_Degrees_of_Freedom, F_Value, p_value)
          
          anova_tables_list[[counter]] <- combined_table
          counter <- counter + 1
        }
      }
    }
  }
}

final_anova_table <- bind_rows(anova_tables_list)

final_anova_table <- final_anova_table %>%
  mutate(
    Degrees_of_Freedom = round(Degrees_of_Freedom, 2),
    Estimated_Degrees_of_Freedom = round(Estimated_Degrees_of_Freedom, 2),
    F_Value = round(F_Value, 3),
    p_value = round(p_value, 4)
  )
summary(all_windows_results$`3`$Models$Depth$Basic)
