###################### Packages ######################
# install.packages("pacman")
library(pacman)
p_load(tidyverse, lubridate, openxlsx, mgcv, gratia, suncalc, glmmTMB, car, DHARMa, emmeans, itsadug,purrr)

###################### Functions #####################

# Extract and format fish telemetry data for specific MHWs and time windows
get_MHWs_fish_df <- function(MHW_def, parrotdish_df, window_suffix = "1.5") {
  MHWs_Eilat <- MHW_def
  
  MHWs_fish_data <- data.frame()
  for (event in 1:dim(MHWs_Eilat)[1]) {
    fish_ids <- unlist(strsplit(MHWs_Eilat$Fish_IDs, ", ")[[event]])
    
    if(length(na.omit(fish_ids)) > 0) {
      filter_fish <- parrotdish_df %>% 
        filter(fish_id %in% fish_ids) %>% 
        arrange(fish_id, real_datetime)
      
      start_col <- paste0("start_", window_suffix)
      end_col <- paste0("end_", window_suffix)
      
      current_start <- MHWs_Eilat[[start_col]][event]
      current_end <- MHWs_Eilat[[end_col]][event]
      
      # Filter data within the specific MHW extended window
      MHW_Data <- filter_fish %>% filter(date >= current_start & date <= current_end)
      
      # Annotate MHW stages (Before, During/MHW, After)
      MHW_Data$Before_After <- ifelse(MHW_Data$real_datetime < MHWs_Eilat$date_start[event], "Before",
                                      ifelse(MHW_Data$real_datetime > MHWs_Eilat$date_end[event], "After", "MHW"))
      
      names(MHWs_Eilat) <- make.names(names(MHWs_Eilat), unique = TRUE)
      temp_MHWs_Eilat <- MHWs_Eilat %>% select(-Fish_IDs)
      MHW_Data <- MHW_Data %>% arrange(fish_id, real_datetime)
      
      # Merge with MHW event metadata
      MHW_Data$Serial <- MHWs_Eilat$Serial[event]
      MHW_Data <- merge(MHW_Data, temp_MHWs_Eilat, by = "Serial")
      MHWs_fish_data <- rbind(MHWs_fish_data, MHW_Data)  
    }
  }
  
  # Format time variables
  MHWs_fish_data$TimeOfDay <- as.POSIXct(format(MHWs_fish_data$real_datetime, format = "%H:%M:%S"), format = "%H:%M:%S")
  MHWs_fish_data$DecimalTimeOfDay <- time_to_decimal(MHWs_fish_data$TimeOfDay) 
  MHWs_fish_data <- add_solar_events(MHWs_fish_data)
  
  # Set factors and baseline levels
  MHWs_fish_data$species <- as.factor(MHWs_fish_data$species) 
  MHWs_fish_data$Before_After <- as.factor(MHWs_fish_data$Before_After)
  MHWs_fish_data$Before_After <- relevel(MHWs_fish_data$Before_After, ref = "Before")
  
  MHWs_Eilat$date_start <- as.POSIXct(MHWs_Eilat$date_start)
  MHWs_Eilat$date_end <- as.POSIXct(MHWs_Eilat$date_end)
  
  # Create unique identifier for each fish per MHW event
  MHWs_fish_data$Serial_fish_id <- paste0(MHWs_fish_data$Serial, "_", MHWs_fish_data$fish_id) 
  MHWs_fish_data$Serial_fish_id <- as.factor(MHWs_fish_data$Serial_fish_id)
  
  return(MHWs_fish_data)
}  

# Convert HH:MM:SS to continuous decimal hours
time_to_decimal <- function(time) {
  hours <- as.numeric(format(time, "%H"))
  minutes <- as.numeric(format(time, "%M"))
  seconds <- as.numeric(format(time, "%S"))
  decimal_time <- hours + minutes / 60 + seconds / 3600
  return(decimal_time)
}

# Calculate and append daily solar events and ordinal time (scaled by sunlight)
add_solar_events <- function(df, lat = 29.538417, lon = 34.954417, tz_local = "Asia/Jerusalem") {
  solar_events <- getSunlightTimes(
    date = unique(df$date), lat = lat, lon = lon,
    keep = c("nadir", "sunrise", "solarNoon", "sunset"), tz = tz_local
  )
  solar_events$sunrise <- as.POSIXct(force_tz(solar_events$sunrise,"UTC"))
  solar_events$nadir <- as.POSIXct(force_tz(solar_events$nadir,"UTC"))
  solar_events$sunset <- as.POSIXct(force_tz(solar_events$sunset,"UTC"))
  solar_events$solarNoon <- as.POSIXct(force_tz(solar_events$solarNoon,"UTC"))
  
  df <- df %>% left_join(solar_events, by = "date") %>%
    mutate(midnight0 = floor_date(real_datetime, unit = "day"), midnight1 = midnight0 + days(1)) %>%
    mutate(
      # Standardize time into 4 phases (1: Night-to-Sunrise, 2: Sunrise-to-Noon, etc.)
      TimeOrdinal = case_when(
        real_datetime >= midnight0 & real_datetime < sunrise ~ 1 + as.numeric(difftime(real_datetime, midnight0, units = "secs")) / as.numeric(difftime(sunrise, midnight0, units = "secs")),
        real_datetime >= sunrise & real_datetime < solarNoon ~ 2 + as.numeric(difftime(real_datetime, sunrise, units = "secs")) / as.numeric(difftime(solarNoon, sunrise, units = "secs")),
        real_datetime >= solarNoon & real_datetime < sunset ~ 3 + as.numeric(difftime(real_datetime, solarNoon, units = "secs")) / as.numeric(difftime(sunset, solarNoon, units = "secs")),
        real_datetime >= sunset & real_datetime < midnight1 ~ 4 + as.numeric(difftime(real_datetime, sunset, units = "secs")) / as.numeric(difftime(midnight1, sunset, units = "secs")),
        TRUE ~ NA_real_
      )
    ) %>% ungroup() %>% select(-midnight0, -midnight1)
  
  return(df)
}

# Return specific list of fish IDs to exclude due to missing data or anomalies
get_excluded_fish <- function(model_type, baseline, win) {
  if (model_type == "depth") {
    if (baseline == "Fix") {
      if (win == "1week") return(c("1_1168793", "10_1911372", "11_1273489", "2_1168793", "3_1255785", "3_1255800", "3_1255803", "4_1255800", "4_1255815", "5_1255800", "5_1255815", "6_1255800", "6_1255815", "7_1255806", "8_1255792", "8_1255806", "9_1255792"))
      if (win == "1.5") return(c("1_1168792", "1_1168793", "11_1273489", "2_1168793", "11_1273484", "3_1255785","3_1255800", "3_1255803", "4_1255800", "4_1255815", "5_1255800", "5_1255815", "6_1255800", "6_1255815", "7_1255792", "7_1255806", "8_1255792", "8_1255806", "9_1255792","9_1273489"))
      if (win == "3") return(c("1_1168793", "11_1273484", "11_1273489", "3_1255785", "3_1255800", "4_1255800", "4_1255815", "5_1255800", "5_1255815", "6_1255800", "6_1255815", "7_1255806", "8_1255792", "8_1255806", "9_1255792"))
    } else if (baseline == "detrended") {
      if (win == "1week") return(c("1_1168793", "3_1255806", "4_1255792", "4_1255806"))
      if (win == "1.5") return(c("1_1168792","1_1168793", "3_1255806", "4_1255792", "4_1255806","3_1255792"))
      if (win == "3") return(c("1_1168792","1_1168793", "2_1255800", "3_1255806", "4_1255806","3_1255792"))
    }
  } else if (model_type == "activity") {
    if (baseline == "Fix") {
      if (win == "1week") return(c("10_19111372", "11_1273489", "2_1212923", "2_1212925", "3_1255784", "3_1255785", "3_1255800", "3_1255803", "3_1255804", "4_1255791", "4_1255800", "4_1255807", "4_1255814", "4_1255815", "6_1255791", "6_1255800", "6_1255814", "6_1255815", "7_1255806", "8_1255792", "8_1255806", "9_1255792"))
      if (win == "1.5") return(c("2_1212923", "3_1255785", "3_1255803", "3_1255804", "4_1255791", "4_1255800", "4_1255814", "4_1255815", "5_1255791", "5_1255800", "5_1255814", "5_1255815", "6_1255791", "6_1255800", "6_1255814", "6_1255815", "7_1255806", "8_1255792", "8_1255806", "9_1255792"))
      if (win == "3") return(c("11_1273489", "11_1273484", "2_1212923", "3_1255785", "4_1255815", "5_1255791", "5_1255814", "5_1255815", "4_1255814", "6_1255791", "6_1255800", "6_1255814", "6_1255815", "7_1255806", "8_1255792", "8_1255806", "9_1255792"))
    } else if (baseline == "detrended") {
      if (win == "1week") return(c("1_1212923", "2_1255791", "2_1255800", "2_1255815", "3_1255806", "4_1255792", "4_1255806"))
      if (win == "1.5") return(c("1_1212923", "2_1255791", "2_1255800", "3_1255806", "4_1255792", "4_1255806"))
      if (win == "3") return(c("1_1212923", "2_1255791", "2_1255800", "3_1255806", "4_1255806"))
    }
  }
  return(c())
}

###################### Global Data Loading ##########################

# Load acoustic data and define array type based on deployment date
combined_parrotfish_df <- readRDS("data/parrotfish data/parrotfish_acoustic_data_df.RDS") 
combined_parrotfish_df$array <- ifelse(combined_parrotfish_df$real_datetime < as.POSIXct("2018-08-07"), "Linear", "Paved")

# Load MHW definitions
MHWs_Eilat_Fix_OISST <- read.xlsx("data/MHWs_def/MHWs_filtered/MHW_OISST_Events_RedSea_Fix_filtered.xlsx", sheet = "Short_MHW_Events")
MHWs_Eilat_detrended_OISST <- read.xlsx("data/MHWs_def/MHWs_filtered/MHW_OISST_Events_RedSea_Jacox_filtered.xlsx", sheet = "Short_MHW_Events")

# Load fish biological metadata
tags_metadata <- read.xlsx("data/parrotfish data/parrotfish_metadata.xlsx") %>% 
  select(fish_id, `Weight.(gr)`, `Length.(cm)`) %>%
  mutate(fish_id = as.character(fish_id)) %>%
  rename(Length_cm = `Length.(cm)`, Weight_gr = `Weight.(gr)`)

# Convert date columns to appropriate format
date_cols <- c("date_start", "date_end", "start_1week", "end_1week", "start_1", "end_1", "start_1.5", "end_1.5", "start_3", "end_3")
for (col in date_cols) {
  MHWs_Eilat_Fix_OISST[[col]] <- convertToDate(MHWs_Eilat_Fix_OISST[[col]])
  MHWs_Eilat_detrended_OISST[[col]] <- convertToDate(MHWs_Eilat_detrended_OISST[[col]])
}

# Assign serial numbers to MHW events
MHWs_Eilat_Fix_OISST$Serial <- 1:nrow(MHWs_Eilat_Fix_OISST)
MHWs_Eilat_Fix_OISST <- MHWs_Eilat_Fix_OISST %>% relocate(Serial, .before = date_start)

MHWs_Eilat_detrended_OISST$Serial <- 1:nrow(MHWs_Eilat_detrended_OISST)
MHWs_Eilat_detrended_OISST <- MHWs_Eilat_detrended_OISST %>% relocate(Serial, .before = date_start)
MHWs_Eilat_detrended_OISST <- MHWs_Eilat_detrended_OISST[-4]

###################### THE MASTER LOOP ##########################
definitions <- c("Fix", "detrended")
windows <- c("1week", "1.5", "3")

# Loop through each baseline definition and time window combination
for (baseline in definitions) {
  for (win in windows) {
    
    cat("\n=======================================================================\n")
    cat(">>> STARTING NEW RUN: Baseline =", baseline, "| Time Window =", win, "<<<\n")
    cat("=======================================================================\n\n")
    
    current_MHW_def <- if(baseline == "Fix") MHWs_Eilat_Fix_OISST else MHWs_Eilat_detrended_OISST
    
    cat("Extracting fish data...\n")
    MHWs_fish_data <- get_MHWs_fish_df(current_MHW_def, combined_parrotfish_df, window_suffix = win)
    MHWs_fish_data <- merge(MHWs_fish_data, tags_metadata, by = "fish_id")
    MHWs_fish_data$fish_id <- as.factor(MHWs_fish_data$fish_id)
    
    ######### Detection Summary & GLMMs #########
    cat("\n--- Running Detection Summary & GLMMs ---\n")
    
    # Calculate operational array hours to assess tracking coverage
    array_active_hours <- MHWs_fish_data %>%
      mutate(hour_bin = floor_date(real_datetime, "hour")) %>%
      group_by(Serial, Before_After) %>%
      summarise(array_working_hours = n_distinct(hour_bin), .groups = "drop")
    
    # Calculate fish detection proportion and ping rate
    detection_summary <- MHWs_fish_data %>%
      mutate(hour_bin = floor_date(real_datetime, "hour")) %>%
      group_by(Serial, fish_id, species, Before_After) %>%
      summarise(fish_detected_hours = n_distinct(hour_bin), total_pings = n(), .groups = "drop") %>%
      left_join(array_active_hours, by = c("Serial", "Before_After")) %>%
      mutate(
        presence_proportion = fish_detected_hours / array_working_hours,
        ping_rate_per_hour = total_pings / array_working_hours
      )
    
    detection_summary$Before_After <- as.factor(detection_summary$Before_After)
    
    # Adjust variables slightly to accommodate Beta/Gamma model constraints (0/1 limits)
    detection_summary$ping_rate_adj <- detection_summary$ping_rate_per_hour + 0.001
    n_rows <- nrow(detection_summary)
    detection_summary$presence_proportion_adj <- (detection_summary$presence_proportion * (n_rows - 1) + 0.5) / n_rows
    
    # Beta GLMM: Check if probability of detection varies across MHW stages
    cat("Fitting Beta GLMM for presence proportion...\n")
    glmm_presence_beta <- glmmTMB(presence_proportion_adj ~ Before_After + (1 | fish_id), 
                                  data = detection_summary, family = beta_family(link = "logit"))
    plot(simulateResiduals(glmm_presence_beta))
    print(Anova(glmm_presence_beta, type = "III"))
    print(summary(glmm_presence_beta))
    
    # Gamma GLMM: Check if ping frequency varies across MHW stages
    cat("Fitting Gamma GLMM for ping intensity...\n")
    glmm_intensity <- glmmTMB(ping_rate_adj ~ Before_After + (1 | fish_id), 
                              data = detection_summary, family = Gamma(link = "log"))
    plot(simulateResiduals(glmm_intensity))
    print(Anova(glmm_intensity, type = "III"))
    print(summary(glmm_intensity))
    
    summary_filename <- paste0("results/Detection_Summary_", baseline, "_", win, ".csv")
    write.csv(detection_summary, summary_filename, row.names = FALSE)
    cat("Saved Detection Summary:", summary_filename, "\n")
    
    readline(prompt=">>> PAUSED: Inspect GLMM results and DHARMa plots. Press [Enter] to continue to Depth... ")
    
    ######### DEPTH MODELS #########
    cat("\n--- Running Depth Models ---\n")
    
    MHWs_fish_data$Serial_fish_id <- as.factor(paste0(MHWs_fish_data$Serial, "_", MHWs_fish_data$fish_id))
    MHWs_fish_data$date_start <- as.POSIXct(MHWs_fish_data$date_start)
    MHWs_fish_data$date_end <- as.POSIXct(MHWs_fish_data$date_end)
    MHWs_fish_data$log_depth <- log(MHWs_fish_data$depth + 0.05)
    
    MHWs_fish_data_dep_filtered <- MHWs_fish_data %>% filter(!is.na(depth)) %>% mutate(hour = hour(real_datetime))
    
    cat("Depth Data - BEFORE FILTERING:\n")
    print(paste("Number of Serial_fish_ids:", length(unique(MHWs_fish_data_dep_filtered$Serial_fish_id))))
    print(paste("Number of MHWs:", length(unique(MHWs_fish_data_dep_filtered$Serial))))
    
    # Filter 1: Retain fish with an average of >= 2 observations per hour bin
    hourly_depth_counts <- MHWs_fish_data_dep_filtered %>% 
      group_by(Serial_fish_id, date, hour, Before_After, species) %>% summarise(n=n(), .groups = "drop")
    mean_counts <- hourly_depth_counts %>% group_by(Serial_fish_id) %>% summarise(mean_count = mean(n), .groups = "drop")
    MHWs_fish_data_dep_filtered <- MHWs_fish_data_dep_filtered %>% filter(Serial_fish_id %in% mean_counts$Serial_fish_id[mean_counts$mean_count >= 2])
    
    # Filter 2: Remove individuals missing sufficient data in specific stages
    points_per_stage <- MHWs_fish_data_dep_filtered %>% group_by(Before_After, Serial_fish_id) %>% summarise(n = n(), .groups = "drop")
    filtered_points_per_stage <- points_per_stage %>% filter(n >= 100)
    fish_to_remove <- filtered_points_per_stage %>% group_by(Serial_fish_id) %>% summarise(stages_count = n_distinct(Before_After)) %>% filter(stages_count == 1) %>% pull(Serial_fish_id)
    
    MHWs_fish_data_dep_filtered <- MHWs_fish_data_dep_filtered %>%
      filter(Serial_fish_id %in% filtered_points_per_stage$Serial_fish_id) %>%
      filter(!(Serial_fish_id %in% fish_to_remove))
    
    # Filter 3: Apply predefined exclusion list
    depth_exclusions <- get_excluded_fish("depth", baseline, win)
    MHWs_fish_data_dep_filtered <- MHWs_fish_data_dep_filtered %>% filter(!(Serial_fish_id %in% depth_exclusions))
    
    cat("Depth Data - AFTER FILTERING:\n")
    print(paste("Number of Serial_fish_ids:", length(unique(MHWs_fish_data_dep_filtered$Serial_fish_id))))
    print(paste("Number of unique fish:", length(unique(MHWs_fish_data_dep_filtered$fish_id))))
    print(paste("Number of MHWs:", length(unique(MHWs_fish_data_dep_filtered$Serial))))
    
    # Plotting raw depth vs time
    p1 <- ggplot(MHWs_fish_data_dep_filtered, aes(x=real_datetime, y=depth)) +
      geom_rect(aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.2) +
      geom_point() + theme_minimal() + scale_y_reverse() + facet_wrap(~ Serial_fish_id, scales="free") +
      labs(title = "Time series of depth for Each Serial Fish ID", x = "Time", y = "Depth")
    print(p1)
    
    # Plotting depth vs time of day (diel pattern)
    p2 <- ggplot(MHWs_fish_data_dep_filtered, aes(x=TimeOrdinal, y=depth)) +
      geom_point() + theme_minimal() + facet_wrap(~ Serial_fish_id, scales="free_y") +
      labs(title = "Depth as function of time of day", x = "Time", y = "Depth") + scale_y_reverse()
    print(p2)
    
    readline(prompt=">>> PAUSED: Inspect Depth plots. Press [Enter] to run BAM models... ")
    
    # Downsample to 1 observation per hour per fish to reduce temporal autocorrelation
    top_species <- names(sort(table(MHWs_fish_data_dep_filtered$species), decreasing = TRUE))[1]
    MHWs_fish_data_dep_filtered$species <- relevel(as.factor(MHWs_fish_data_dep_filtered$species), ref = top_species)
    MHWs_thinned_dep <- MHWs_fish_data_dep_filtered %>%
      arrange(fish_id, real_datetime) %>%
      mutate(hour_bin = floor_date(real_datetime, "hour"),
             Length_scaled = as.numeric(scale(Length_cm))
      ) %>%
      group_by(fish_id, hour_bin) %>%
      slice(1) %>% ungroup() %>% droplevels()
    
    num_species <- length(unique(MHWs_thinned_dep$species))
    
    # Build Generalized Additive Models for large datasets (BAMs) for Depth
    cat("Fitting Depth GAMs...\n")
    if (num_species > 1) {
      base_dep_mod <- bam(log_depth ~ species + Length_scaled + s(TimeOrdinal,by=species,bs="cc",k=24) + s(TimeOrdinal,fish_id,bs="fs",k=10,m=1,xt=list(bs="cc")) + s(Serial,bs="re"), data=MHWs_thinned_dep, method="fREML", family=gaussian(), parallel=T, discrete=T)
      intercept_dep_mod <- bam(log_depth ~ Before_After + species + Length_scaled + s(TimeOrdinal,by=species,bs="cc",k=24) + s(TimeOrdinal,fish_id,bs="fs",k=10,m=1,xt=list(bs="cc")) + s(Serial,bs="re"), data=MHWs_thinned_dep, method="fREML", family=gaussian(), parallel=T, discrete=T)
      intercept_dep_mod_int <- bam(log_depth ~ Before_After * species + Length_scaled + s(TimeOrdinal,by=species,bs="cc",k=24) + s(TimeOrdinal,fish_id,bs="fs",k=10,m=1,xt=list(bs="cc")) + s(Serial,bs="re"), data=MHWs_thinned_dep, method="fREML", family=gaussian(), parallel=T, discrete=T)
      smooth_dep_mod <- bam(log_depth ~ species + Length_scaled + s(TimeOrdinal,by=species,bs="cc",k=24) + s(TimeOrdinal,fish_id,bs="fs",k=10,m=1,xt=list(bs="cc")) + s(TimeOrdinal,by=Before_After,bs="cc",k=8) + s(Serial,bs="re"), data=MHWs_thinned_dep, method="fREML", family=gaussian(), parallel=T, discrete=T)
      full_dep_mod <- bam(log_depth ~ Before_After + Length_scaled + s(TimeOrdinal,fish_id,bs="fs",k=10,m=1,xt=list(bs="cc")) + s(TimeOrdinal,by=Before_After,bs="cc",k=8) + s(Serial,bs="re"), data=MHWs_thinned_dep, method="fREML", family=gaussian(), parallel=T, discrete=T)
      full_dep_mod_sp <- bam(log_depth ~ Before_After + species + Length_scaled + s(TimeOrdinal,by=species,bs="cc",k=24) + s(TimeOrdinal,fish_id,bs="fs",k=10,m=1,xt=list(bs="cc")) + s(TimeOrdinal,by=Before_After,bs="cc",k=8) + s(Serial,bs="re"), data=MHWs_thinned_dep, method="fREML", family=gaussian(), parallel=T, discrete=T)
      full_dep_mod_sp_int <- bam(log_depth ~ Before_After * species + Length_scaled + s(TimeOrdinal,by=species,bs="cc",k=24) + s(TimeOrdinal,fish_id,bs="fs",k=10,m=1,xt=list(bs="cc")) + s(TimeOrdinal,by=Before_After,bs="cc",k=8) + s(Serial,bs="re"), data=MHWs_thinned_dep, method="fREML", family=gaussian(), parallel=T, discrete=T)
    } else {
      cat("Only 1 species detected. Fitting single-species models (dropping species terms).\n")
      
      # Adjusted models dropping species factors for single-species datasets
      base_dep_mod <- bam(log_depth ~ Length_scaled + s(TimeOrdinal,bs="cc",k=24) + s(TimeOrdinal,fish_id,bs="fs",k=10,m=1,xt=list(bs="cc")) + s(Serial,bs="re"), data=MHWs_thinned_dep, method="fREML", family=gaussian(), parallel=T, discrete=T)
      
      intercept_dep_mod <- bam(log_depth ~ Before_After + Length_scaled + s(TimeOrdinal,bs="cc",k=24) + s(TimeOrdinal,fish_id,bs="fs",k=10,m=1,xt=list(bs="cc")) + s(Serial,bs="re"), data=MHWs_thinned_dep, method="fREML", family=gaussian(), parallel=T, discrete=T)
      
      smooth_dep_mod <- bam(log_depth ~ Length_scaled + s(TimeOrdinal,bs="cc",k=24) + s(TimeOrdinal,fish_id,bs="fs",k=10,m=1,xt=list(bs="cc")) + s(TimeOrdinal,by=Before_After,bs="cc",k=8) + s(Serial,bs="re"), data=MHWs_thinned_dep, method="fREML", family=gaussian(), parallel=T, discrete=T)
      
      full_dep_mod <- bam(log_depth ~ Before_After + Length_scaled + s(TimeOrdinal,fish_id,bs="fs",k=10,m=1,xt=list(bs="cc")) + s(TimeOrdinal,by=Before_After,bs="cc",k=8) + s(Serial,bs="re"), data=MHWs_thinned_dep, method="fREML", family=gaussian(), parallel=T, discrete=T)
      
      full_dep_mod_sp <- bam(log_depth ~ Before_After + Length_scaled + s(TimeOrdinal,bs="cc",k=24) + s(TimeOrdinal,fish_id,bs="fs",k=10,m=1,xt=list(bs="cc")) + s(TimeOrdinal,by=Before_After,bs="cc",k=8) + s(Serial,bs="re"), data=MHWs_thinned_dep, method="fREML", family=gaussian(), parallel=T, discrete=T)
      
      full_dep_mod_sp_int <- full_dep_mod_sp 
    }
    # --- Parsimonious Model Selection for Depth (Delta AIC < 2) ---
    dep_model_list <- list("base_dep_mod" = base_dep_mod, "intercept_dep_mod" = intercept_dep_mod,"intercept_dep_mod_int" = intercept_dep_mod_int,
                           "smooth_dep_mod" = smooth_dep_mod, "full_dep_mod" = full_dep_mod,
                           "full_dep_mod_sp" = full_dep_mod_sp, "full_dep_mod_sp_int" = full_dep_mod_sp_int)
    
    # 1. Compare AIC and extract Effective Degrees of Freedom (edf)
    dep_aic_table <- data.frame(
      model_name = names(dep_model_list),
      AIC = sapply(dep_model_list, AIC),
      df  = sapply(dep_model_list, function(m) sum(m$edf)) 
    ) %>% 
      mutate(delta_aic = AIC - min(AIC)) %>% 
      arrange(AIC)
    
    # 2. Filter for top candidates within delta AIC < 2
    top_dep_contenders <- dep_aic_table %>% filter(delta_aic < 2)
    
    # 3. Select the simplest model (parsimony: lowest edf)
    best_dep_name <- top_dep_contenders %>% 
      filter(df == min(df)) %>% 
      slice(1) %>% 
      pull(model_name)
    
    best_dep_mod <- dep_model_list[[best_dep_name]]
    
    cat("\n>>> DEPTH AIC SELECTION TABLE:\n")
    print(dep_aic_table)
    
    
    gam.check(best_dep_mod)
    print(anova.gam(best_dep_mod))
    print(summary(best_dep_mod))
    cat("\n>>> WINNING DEPTH MODEL (Parsimony):", best_dep_name, "<<<\n")
    readline(prompt=">>> PAUSED: Inspect Winning Depth Model Results. Press [Enter] to continue to Activity... ")
    
    # Save Depth Models List
    depth_models <- list(
      base_mod      = base_dep_mod, 
      intercept_mod = intercept_dep_mod,
      intercept_mod_int = intercept_dep_mod_int, 
      smooth_mod    = smooth_dep_mod,
      full_mod      = full_dep_mod, 
      full_mod_sp   = full_dep_mod_sp, 
      full_mod_sp_int = full_dep_mod_sp_int,
      best_mod      = best_dep_mod,
      best_mod_name = best_dep_name,
      mhw_df        = MHWs_thinned_dep,
      aic_table     = dep_aic_table
    )
    
    depth_filename <- paste0("results/Models/Depth_Models_OISST_", baseline, "_ordinal_", win, ".RDS")
    saveRDS(depth_models, depth_filename)
    cat("Saved Depth Models:", depth_filename, "\n")
    
    
    ######### ACTIVITY MODELS #########
    cat("\n--- Running Activity Models ---\n")
    
    MHWs_fish_data_act_filtered <- MHWs_fish_data %>% filter(!is.na(activity)) %>% mutate(hour = hour(real_datetime))
    
    cat("Activity Data - BEFORE FILTERING:\n")
    print(paste("Number of Serial_fish_ids:", length(unique(MHWs_fish_data_act_filtered$Serial_fish_id))))
    print(paste("Number of MHWs:", length(unique(MHWs_fish_data_act_filtered$Serial))))
    
    # Filter 1: Retain fish with an average of >= 2 observations per hour bin
    hourly_act_counts <- MHWs_fish_data_act_filtered %>% group_by(Serial_fish_id, date, hour, Before_After, species) %>% summarise(n=n(), .groups = "drop")
    mean_counts_act <- hourly_act_counts %>% group_by(Serial_fish_id, Before_After, hour) %>% summarise(mean_count = mean(n), .groups = "drop")
    MHWs_fish_data_act_filtered <- MHWs_fish_data_act_filtered %>% filter(Serial_fish_id %in% mean_counts_act$Serial_fish_id[mean_counts_act$mean_count >= 2]) 
    
    # Filter 2: Remove individuals missing sufficient data in specific stages
    points_per_stage_act <- MHWs_fish_data_act_filtered %>% group_by(Serial_fish_id, Before_After) %>% summarise(n = n(), .groups = "drop")
    filtered_points_per_stage_act <- points_per_stage_act %>% filter(n >= 100)
    fish_to_remove_act <- filtered_points_per_stage_act %>% group_by(Serial_fish_id) %>% summarise(stages_count = n_distinct(Before_After)) %>% filter(stages_count == 1) %>% pull(Serial_fish_id)
    
    MHWs_fish_data_act_filtered <- MHWs_fish_data_act_filtered %>%
      filter(Serial_fish_id %in% filtered_points_per_stage_act$Serial_fish_id) %>%
      filter(!(Serial_fish_id %in% fish_to_remove_act))
    
    # Filter 3: Apply predefined exclusion list
    activity_exclusions <- get_excluded_fish("activity", baseline, win)
    MHWs_fish_data_act_filtered <- MHWs_fish_data_act_filtered %>% filter(!(Serial_fish_id %in% activity_exclusions))
    
    cat("Activity Data - AFTER FILTERING:\n")
    print(paste("Number of Serial_fish_ids:", length(unique(MHWs_fish_data_act_filtered$Serial_fish_id))))
    print(paste("Number of unique fish:", length(unique(MHWs_fish_data_act_filtered$fish_id))))
    print(paste("Number of MHWs:", length(unique(MHWs_fish_data_act_filtered$Serial))))
    
    # Plotting raw activity vs time
    p3 <- ggplot(MHWs_fish_data_act_filtered, aes(x=real_datetime, y=activity)) +
      geom_rect(aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.2) +
      geom_point() + theme_minimal() + facet_wrap(~ Serial_fish_id, scales="free") +
      labs(title = "Time series of activity for Each Serial Fish ID", x = "Time", y = "Activity")
    print(p3)
    
    # Plotting activity vs time of day (diel pattern)
    p4 <- ggplot(MHWs_fish_data_act_filtered, aes(x=TimeOrdinal, y=activity)) +
      geom_point() + theme_minimal() + facet_wrap(~ Serial_fish_id, scales="free_y") +
      labs(title = "Activity as function of time of day", x = "Time", y = "Activity")
    print(p4)
    
    readline(prompt=">>> PAUSED: Inspect Activity plots. Press [Enter] to run BAM models... ")
    
    # Downsample to 1 observation per hour per fish
    top_species_act <- names(sort(table(MHWs_fish_data_act_filtered$species), decreasing = TRUE))[1]
    MHWs_fish_data_act_filtered$species <- relevel(as.factor(MHWs_fish_data_act_filtered$species), ref = top_species_act)
    MHWs_thinned_act <- MHWs_fish_data_act_filtered %>%
      arrange(fish_id, real_datetime) %>%
      mutate(hour_bin = floor_date(real_datetime, "hour"),
             Length_scaled = as.numeric(scale(Length_cm))) %>%
      group_by(fish_id, hour_bin) %>%
      slice(1) %>%
      ungroup() %>% droplevels()
    
    # Build BAMs for Activity (Tweedie distribution)
    cat("Fitting Activity GAMs...\n")
    base_act_mod <- bam(activity ~ species + Length_scaled + s(TimeOrdinal,by=species,bs="cc",k=24) + s(TimeOrdinal,fish_id,bs="fs",k=10,m=1,xt=list(bs="cc")) + s(Serial,bs="re"), data=MHWs_thinned_act, method="fREML", family=Tweedie(p=1.5, link="log"), parallel=T, discrete=T)
    intercept_act_mod <- bam(activity ~ Before_After + species + Length_scaled + s(TimeOrdinal,by=species,bs="cc",k=24) + s(TimeOrdinal,fish_id,bs="fs",k=10,m=1,xt=list(bs="cc")) + s(Serial,bs="re"), data=MHWs_thinned_act, method="fREML", family=Tweedie(p=1.5, link="log"), parallel=T, discrete=T)
    intercept_act_mod_int <- bam(activity ~ Before_After * species + Length_scaled + s(TimeOrdinal,by=species,bs="cc",k=24) + s(TimeOrdinal,fish_id,bs="fs",k=10,m=1,xt=list(bs="cc")) + s(Serial,bs="re"), data=MHWs_thinned_act, method="fREML", family=Tweedie(p=1.5, link="log"), parallel=T, discrete=T)
    smooth_act_mod <- bam(activity ~ species + Length_scaled + s(TimeOrdinal,by=species,bs="cc",k=24) + s(TimeOrdinal,fish_id,bs="fs",k=10,m=1,xt=list(bs="cc")) + s(TimeOrdinal,by=Before_After,bs="cc",k=8) + s(Serial,bs="re"), data=MHWs_thinned_act, method="fREML", family=Tweedie(p=1.5, link="log"), parallel=T, discrete=T)
    full_act_mod <- bam(activity ~ Before_After + Length_scaled + s(TimeOrdinal,fish_id,bs="fs",k=10,m=1,xt=list(bs="cc")) + s(TimeOrdinal,by=Before_After,bs="cc",k=8) + s(Serial,bs="re"), data=MHWs_thinned_act, method="fREML", family=Tweedie(p=1.5, link="log"), parallel=T, discrete=T)
    full_act_mod_sp <- bam(activity ~ Before_After + species + Length_scaled + s(TimeOrdinal,by=species,bs="cc",k=24) + s(TimeOrdinal,fish_id,bs="fs",k=10,m=1,xt=list(bs="cc")) + s(TimeOrdinal,by=Before_After,bs="cc",k=8) + s(Serial,bs="re"), data=MHWs_thinned_act, method="fREML", family=Tweedie(p=1.5, link="log"), parallel=T, discrete=T)
    full_act_mod_sp_int <- bam(activity ~ Before_After * species + Length_scaled + s(TimeOrdinal,by=species,bs="cc",k=24) + s(TimeOrdinal,fish_id,bs="fs",k=10,m=1,xt=list(bs="cc")) + s(TimeOrdinal,by=Before_After,bs="cc",k=8) + s(Serial,bs="re"), data=MHWs_thinned_act, method="fREML", family=Tweedie(p=1.5, link="log"), parallel=T, discrete=T)
    
    # --- Parsimonious Model Selection for Activity (Delta AIC < 2) ---
    act_model_list <- list("base_act_mod" = base_act_mod, "intercept_act_mod" = intercept_act_mod,"intercept_act_mod_int" = intercept_act_mod_int,
                           "smooth_act_mod" = smooth_act_mod, "full_act_mod" = full_act_mod,
                           "full_act_mod_sp" = full_act_mod_sp, "full_act_mod_sp_int" = full_act_mod_sp_int)
    
    # 1. Create comparison table
    act_aic_table <- data.frame(
      model_name = names(act_model_list[1:6]), 
      AIC = sapply(act_model_list[1:6], AIC),
      df  = sapply(act_model_list[1:6], function(m) sum(m$edf))
    ) %>% 
      mutate(delta_aic = AIC - min(AIC)) %>% 
      arrange(AIC)
    
    # 2. Top candidates
    top_act_contenders <- act_aic_table %>% filter(delta_aic < 2)
    
    # 3. Parsimony check
    best_act_name <- top_act_contenders %>% 
      filter(df == min(df)) %>% 
      slice(1) %>% 
      pull(model_name)
    
    best_act_mod <- act_model_list[[best_act_name]]
    
    cat("\n>>> ACTIVITY AIC SELECTION TABLE:\n")
    print(act_aic_table)
    
    gam.check(best_act_mod)
    print(anova.gam(best_act_mod))
    print(summary(best_act_mod))
    print(appraise(best_act_mod))
    
    # Save Activity Models List
    act_model_list <- list(
      base_mod      = base_act_mod, 
      intercept_mod = intercept_act_mod,
      intercept_mod_int = intercept_act_mod_int, 
      smooth_mod    = smooth_act_mod,
      full_mod      = full_act_mod, 
      full_mod_sp   = full_act_mod_sp, 
      full_mod_sp_int = full_act_mod_sp_int,
      best_mod      = best_act_mod,
      best_mod_name = best_act_name,
      mhw_df        = MHWs_thinned_act,
      aic_table     = act_aic_table
    )
    
    depth_filename <- paste0("results/Models/Depth_Models_OISST_", baseline, "_ordinal_", win, ".RDS")
    saveRDS(depth_models, depth_filename)
    
    cat("\n>>> WINNING ACTIVITY MODEL (Parsimony):", best_act_name, "<<<\n")
    readline(prompt=">>> PAUSED: Inspect Winning Activity Model Results. Press [Enter] to move to the NEXT run... ")
    activity_filename <- paste0("results/Models/Activity_Models_OISST_", baseline, "_ordinal_", win, ".RDS")
    saveRDS(act_model_list, activity_filename)
    cat("Saved Activity Models:", activity_filename, "\n")
    
  }
}

cat("\n!!! ALL RUNS COMPLETED SUCCESSFULLY !!!\n")


# --- TEMPORAL AUTOCORRELATION CORRECTION (DEPTH) ---
cat("\n--- Checking and Correcting Autocorrelation for Depth ---\n")

# Load model list to evaluate AR1 correlation
ac_model_list <- readRDS("results/Models/Activity_Models_OISST_detrended_ordinal_1.5.RDS")
models_to_compare <- ac_model_list[sapply(ac_model_list, function(x) inherits(x, "gam"))]
aic_table <- imap_dfr(models_to_compare, ~{
  data.frame(
    model = .y,
    df = attr(logLik(.x), "df"),
    AIC = AIC(.x)
  )
}) %>% arrange(AIC)

print(aic_table)
best_model_name <- aic_table$model[which.min(aic_table$AIC)]
best_model <- models_to_compare[[best_model_name]]

# 1. Define start events to prevent AR1 from correlating across different fish
MHWs_thinned <- ac_model_list$dep_df 
MHWs_thinned <- MHWs_thinned %>% arrange(Serial_fish_id, real_datetime)
MHWs_thinned$start_event <- !duplicated(MHWs_thinned$Serial_fish_id)

# 2. Check Autocorrelation Function (ACF) of uncorrected model residuals
acf_val <- acf_resid(best_model, main = paste("ACF:", best_model_name))
rho_val <- acf_val[2] # Extract lag 1 correlation coefficient

cat("Detected Rho (lag 1):", round(rho_val, 3), "\n")

# 3. Fit the corrected model updating with AR1 parameter
cat("Fitting corrected model with AR1...\n")
best_mod_ar1 <- update(best_model, 
                       rho = rho_val, 
                       AR.start = MHWs_thinned$start_event,
                       data = MHWs_thinned,
                       discrete = TRUE)

# 4. Compare ACF plots before and after AR1 correction
par(mfrow=c(1,2))
acf_resid(best_model, main = "Before AR1")
acf_resid(best_mod_ar1, main = "After AR1")
par(mfrow=c(1,1))

# Update and store results
summary(best_mod_ar1)
ac_model_list$best_gam_mod_ar1 <- best_mod_ar1
ac_model_list$rho <- rho_val

# saveRDS(ac_model_list,"results/Models/Activity_Models_OISST_detrended_ordinal_1.5.RDS")