###################### Packages ######################

install.packages("pacman")
library(pacman)
p_load(tidyverse, lubridate, openxlsx,broom,gratia)

###################### Code ######################

## Load data

# MHWs summary
sum_heat_wave_day <- readrds("results/Individual Heatwaves/OISST_Fix_MHW_Stage_summary.RDS")
# tags metadata
tags_metadata <- read.xlsx("data/parrotfish data/parrotfish_metadata.xlsx")

# Nest the data by Serial 
nest_hw_day <- sum_heat_wave_day %>% 
  ungroup() %>%
  nest_by(Serial)

## Calculate stages comparison

# Initialize an empty dataframe to store the results
MHWs_comparison <- data.frame(
  fish_id=numeric(), # ID of the fish
  Serial = numeric(),              # Serial number for the heatwave event
  ln_activity_ratio = numeric(),   # Logarithmic ratio of activity levels between stages
  delta_depth = numeric(),         # Change in depth between stages
  Stage_period = character()       # Describes the stage comparison (e.g., "MHW : Before")
)

# Iterate over each heatwave event
for (event_calc in 1:dim(nest_hw_day)[1]) {
  temp_heat_wave <- nest_hw_day$data[[event_calc]]  # Extract data for the current event
  
  # Iterate over each unique fish ID in the heatwave data
  for (fish in unique(temp_heat_wave$fish_id)) {
    temp_fish <- temp_heat_wave %>% filter(fish_id==fish) # Filter data for the current fish
    
    # Initialize a temporary row for storing results
    temp_row <- data.frame(
      fish_id=rep(fish,3),# Repeat fish ID
      Serial = rep(nest_hw_day$Serial[[event_calc]], 3),   # Repeat serial for current event
      ln_activity_ratio = rep(NA, 3),                 # Placeholder for activity ratio
      delta_depth = rep(NA, 3),                       # Placeholder for depth change
      Stage_period = rep(NA, 3)                       # Placeholder for stage comparison label
    )
    
    uniqe_stages <- unique(temp_fish$Before_After) # Get unique stages (e.g., "Before", "MHW", "After")
    row_i <- 1 # Initialize row index for temporary row
    
    # Check and calculate metrics for "MHW : Before" stage comparison
    if(all(c("MHW","Before") %in% uniqe_stages))
    {
      temp_row$ln_activity_ratio[row_i] <- log(temp_fish$mean_activity[temp_fish$Before_After=="MHW"] /temp_fish$mean_activity[temp_fish$Before_After=="Before"])
      temp_row$delta_depth[row_i] <- temp_fish$mean_depth[temp_fish$Before_After=="MHW"] - temp_fish$mean_depth[temp_fish$Before_After=="Before"]
      temp_row$Stage_period[row_i] <- "MHW : Before" # Label the stage comparison
      row_i <- row_i+1 # Increment row index
    }
    
    # Check and calculate metrics for "After : MHW" stage comparison
    if(all(c("After","MHW") %in% uniqe_stages))
    {
      temp_row$ln_activity_ratio[row_i] <- log(temp_fish$mean_activity[temp_fish$Before_After=="After"]/temp_fish$mean_activity[temp_fish$Before_After=="MHW"])
      temp_row$delta_depth[row_i] <- temp_fish$mean_depth[temp_fish$Before_After=="After"] - temp_fish$mean_depth[temp_fish$Before_After=="MHW"]
      temp_row$Stage_period[row_i] <- "After : MHW"
      row_i <- row_i+1
    }
    
    # Check and calculate metrics for "After : Before" stage comparison
    if(all(c("After","Before") %in% uniqe_stages))
    {
      temp_row$ln_activity_ratio[row_i] <- log(temp_fish$mean_activity[temp_fish$Before_After=="After"]/temp_fish$mean_activity[temp_fish$Before_After=="Before"])
      temp_row$delta_depth[row_i] <- temp_fish$mean_depth[temp_fish$Before_After=="After"] - temp_fish$mean_depth[temp_fish$Before_After=="Before"]
      temp_row$Stage_period[row_i] <- "After : Before"
    }
    # Append the temporary row to the main results dataframe
    MHWs_comparison <- rbind(MHWs_comparison,temp_row)
  }
}
rm(temp_row,temp_fish,temp_heat_wave)

# Remove rows where all metrics (activity, depth, distance) are NA
MHWs_comparison <- MHWs_comparison %>% filter(!(is.na(ln_activity_ratio) & is.na(delta_depth)))

MHW_disp_comparison <- sum_heat_wave_day %>%
  filter( disp_max_n > 3) %>%  # ✅Only include displacements with more than 3 days
  select(Serial, fish_id, Before_After, mean_disp_max) %>%
  distinct() %>%
  pivot_wider(names_from = Before_After, values_from = mean_disp_max) %>%
  mutate(
    delta_MHW_Before = abs(`MHW` - `Before`),
    delta_After_Before = abs(`After` - `Before`),
    log_MHW_Before = ifelse(is.finite(log(`MHW` / `Before`)), log(`MHW` / `Before`), NA_real_),
    log_After_Before = ifelse(is.finite(log(`After` / `Before`)), log(`After` / `Before`), NA_real_)
  ) %>%
  select(Serial, fish_id, starts_with("delta_"), starts_with("log_")) %>%
  pivot_longer(
    cols = -c(Serial, fish_id),
    names_to = c(".value", "Stage_period"),
    names_pattern = "(delta|log)_(.*)"
  ) %>%
  mutate(
    Stage_period = recode(Stage_period,
                          "MHW_Before" = "MHW : Before",
                          "After_Before" = "After : Before")
  ) %>%
  rename(
    delta_max_displacement = delta,
    log_max_displacement = log
  )

# Merge with heatwave metadata
MHWs_comparison <- merge(MHWs_comparison,MHW_disp_comparison,by=c("Serial","fish_id","Stage_period"),all.x = T)
rm(MHW_disp_comparison)

MHWs_comparison <- merge(MHWs_comparison,MHWs_Eilat,by = "Serial") %>%
  select(-Fish_IDs)

# Add species information from the metadata
tags_metadata <- tags_metadata %>% 
  select(fish_id,species) 
MHWs_comparison <- distinct(merge(MHWs_comparison,tags_metadata,by = "fish_id"))

#### Save the comparison data 
### Choose the name of the database and def !
# fix = OISST_Fix_MHW_Stage_comparison.RDS
# detrended =  OISST_detrended_MHW_Stage_comparison.RDS
# saveRDS(MHWs_comparison,"results/Individual Heatwaves/OISST_Fix_MHW_Stage_comparison.RDS")


## Check relationship with MHWs characteristics - linear models!
MHWs_comparison_lm <- MHWs_comparison %>% filter(Stage_period=="MHW : Before")

# Activity
lm_act_cum <- lm(ln_activity_ratio ~ intensity_cumulative,data = MHWs_comparison_lm) 
lm_act_max <- lm(ln_activity_ratio ~ intensity_max,data = MHWs_comparison_lm) 
lm_act_max_abs <- lm(ln_activity_ratio ~ intensity_max_abs,data = MHWs_comparison_lm) 
lm_act_onset <- lm(ln_activity_ratio ~ rate_onset,data = MHWs_comparison_lm) 
lm_act_mean <- lm(ln_activity_ratio ~ intensity_mean,data = MHWs_comparison_lm) 
lm_act_duration <- lm(ln_activity_ratio ~ duration,data = MHWs_comparison_lm)

# Depth
lm_dep_cum<- lm(delta_depth ~ intensity_cumulative,data = MHWs_comparison_lm)
lm_dep_max <- lm(delta_depth ~ intensity_max,data = MHWs_comparison_lm)
lm_dep_max_abs <- lm(delta_depth ~ intensity_max_abs,data = MHWs_comparison_lm)
lm_dep_onset <- lm(delta_depth ~ rate_onset,data = MHWs_comparison_lm)
lm_dep_mean <- lm(delta_depth ~ intensity_mean,data = MHWs_comparison_lm)
lm_dep_duration <- lm(delta_depth ~ duration,data = MHWs_comparison_lm)

# Displacement
lm_disp_cum<- lm(log_max_displacement ~ intensity_cumulative,data = MHWs_comparison_lm)
lm_disp_max <- lm(log_max_displacement ~ intensity_max,data = MHWs_comparison_lm)
lm_disp_max_abs <- lm(log_max_displacement ~ intensity_max_abs,data = MHWs_comparison_lm)
lm_disp_onset <- lm(log_max_displacement ~ rate_onset,data = MHWs_comparison_lm)
lm_disp_mean <- lm(log_max_displacement ~ intensity_mean,data = MHWs_comparison_lm)
lm_disp_duration <- lm(log_max_displacement ~ duration,data = MHWs_comparison_lm)

# Function to check models 
appraise(lm_act_cum)

# put all models in a named list
models <- list(lm_act_cum,lm_act_max,lm_act_max_abs,lm_act_onset,lm_act_mean,lm_act_duration,
  lm_dep_cum,lm_dep_max,lm_dep_max_abs,lm_dep_onset,lm_dep_mean,lm_dep_duration,lm_disp_cum,
  lm_disp_max,lm_disp_max_abs,lm_disp_onset,lm_disp_mean,lm_disp_duration)

# extract model summaries
model_summaries <- lapply(models, function(m) {
  coefs <- tidy(m) %>% filter(term != "(Intercept)")   # predictor only
  fit   <- glance(m)
  
  data.frame(
    response   = all.vars(formula(m))[1],
    predictor  = coefs$term,
    estimate   = coefs$estimate,
    std_error  = coefs$std.error,
    t_value    = coefs$statistic,
    p_value    = coefs$p.value,
    r_squared  = fit$r.squared,
    adj_r2     = fit$adj.r.squared,
    AIC        = fit$AIC,
    BIC        = fit$BIC,
    stringsAsFactors = FALSE
  )
})

# combine into one dataframe
model_summaries <- bind_rows(model_summaries, .id = "model_num")

#### Save the lm summaries 
### Choose the name of the def !
# fix = OISST_Fix_MHW_lm_summary.RDS
# detrended =  OISST_detrended_lm_summary.RDS
# saveRDS(MHWs_comparison,"results/Individual Heatwaves/OISST_Fix_MHW_lm_summary.RDS")



