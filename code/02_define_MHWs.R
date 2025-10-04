###################### Packages ######################

install.packages("pacman")
library(pacman)
p_load(heatwaveR, lubridate, tidyverse, sjPlot,openxlsx)

###################### Functions ######################

Detrend_MHW_days_to_MHWs <- function(deterend_df, date_col_num, MHW_day_col_num, residuals_col_num, origin_col_num, threshold_col_num) {
# This function identifies Marine Heatwave (MHW) events from a detrended data frame and calculates various metrics for each event.
  # Initialize an empty data frame to store MHWs data with various attributes
  MHWs_df <- data.frame(date_start=NA,
                        date_end=NA,
                        date_peak=NA,
                        duration=NA,
                        intensity_mean=NA,
                        intensity_max=NA,
                        intensity_var=NA,
                        intensity_cum=NA,
                        intensity_mean_abs=NA,
                        intensity_max_abs=NA,
                        intensity_var_abs=NA,
                        intensity_mean_relThresh=NA,
                        intensity_max_relThresh=NA,
                        intensity_var_relThresh=NA,
                        rate_onset=NA,
                        rate_decline=NA
  )
  
  # Create lists to store information about MHWs
  resid_mhw <- list()       # Stores the residual values during the MHW event
  absolute_mhw <- list()    # Stores the original values during the MHW event
  thresh_mhw <- list()      # Stores the threshold values during the MHW event
  
  # Variable to store the start date of an MHW event
  date_start <- NA
  for (day_i in 1:length(deterend_df[, date_col_num])) {
    
    # If it's the start of a new MHW event
    if(deterend_df[day_i, MHW_day_col_num] == TRUE && is.na(date_start)) {
      date_start <- deterend_df[day_i, date_col_num]  # Set the start date of the event
      resid_mhw <- append(resid_mhw, deterend_df[day_i, residuals_col_num])  # Append the residual value
      absolute_mhw <- append(absolute_mhw, deterend_df[day_i, origin_col_num])  # Append the original value
      thresh_mhw <- append(thresh_mhw, deterend_df[day_i, residuals_col_num] - deterend_df[day_i, threshold_col_num])  # Append the threshold difference
      mhw_gap <- 0  # Initialize gap counter
      
    } 
    
    # If the event continues (another day marked as MHW)
    else if(deterend_df[day_i, MHW_day_col_num] == TRUE && !is.na(date_start)) {
      resid_mhw <- append(resid_mhw, deterend_df[day_i, residuals_col_num])
      absolute_mhw <- append(absolute_mhw, deterend_df[day_i, origin_col_num])
      thresh_mhw <- append(thresh_mhw, deterend_df[day_i, residuals_col_num] - deterend_df[day_i, threshold_col_num])
      mhw_gap <- 0
      
    } 
    
    # If the event has ended and the gap exceeds 2 days
    else if (deterend_df[day_i, MHW_day_col_num] == FALSE && !is.na(date_start)) {
      mhw_gap <- mhw_gap + 1  # Increase the gap counter
      
      # If the gap exceeds 2 days, the MHW event is considered finished
      if(mhw_gap > 2) {
        
        # If the MHW event lasted more than 5 days, calculate various metrics and store them
        if(length(resid_mhw) > 5) {
          temp_df <- data.frame(date_start = date_start,
                                date_end = deterend_df[day_i - 1, date_col_num],  # End date is the last day of the event
                                date_peak = date_start + which.max(unlist(resid_mhw) - 1),  # Peak date based on maximum residual
                                duration = deterend_df[day_i, date_col_num] - date_start,  # Duration of the event
                                intensity_mean = mean(unlist(resid_mhw)),  # Mean intensity of the MHW event
                                intensity_max = max(unlist(resid_mhw)),  # Maximum intensity of the MHW event
                                intensity_var = sd(unlist(resid_mhw)),  # Standard deviation of intensity
                                intensity_cum = sum(unlist(resid_mhw)),  # Cumulative intensity of the event
                                intensity_mean_abs = mean(unlist(absolute_mhw)),  # Mean absolute intensity
                                intensity_max_abs = max(unlist(absolute_mhw)),  # Maximum absolute intensity
                                intensity_var_abs = sd(unlist(absolute_mhw)),  # Standard deviation of absolute intensity
                                intensity_mean_relThresh = mean(unlist(thresh_mhw)),  # Mean intensity relative to the threshold
                                intensity_max_relThresh = max(unlist(thresh_mhw)),  # Maximum intensity relative to the threshold
                                intensity_var_relThresh = sd(unlist(thresh_mhw)),  # Standard deviation of intensity relative to threshold
                                rate_onset = max(unlist(resid_mhw)) / which.max(unlist(resid_mhw)),  # Rate of onset (intensity over time)
                                rate_decline = max(unlist(resid_mhw)) / if(length(resid_mhw) - which.max(unlist(resid_mhw)) == 0) 1 else (length(resid_mhw) - which.max(unlist(resid_mhw)) + 1)  # Rate of decline
          )
          
          # Append the calculated MHW event data to the MHWs_df data frame
          MHWs_df <- rbind(MHWs_df, temp_df)
        }
        
        # Reset the variables for the next MHW event
        mhw_gap <- 0
        resid_mhw <- list()
        absolute_mhw <- list()
        thresh_mhw <- list()
        date_start <- NA
      }
    }
  }
  
  # If any rows were added to MHWs_df, remove the initial NA row
  if(dim(MHWs_df)[1] > 0) {
    MHWs_df <- MHWs_df[-1,]
  }
  
  # Convert date columns to Date type
  MHWs_df$date_start <- as.Date(MHWs_df$date_start)
  MHWs_df$date_end <- as.Date(MHWs_df$date_end)
  MHWs_df$date_peak <- as.Date(MHWs_df$date_peak)
  
  # Return the MHW events data frame
  return(MHWs_df)
}

###################### Code ######################

## Load data
OISST_data_red <- readRDS("data/Eilat climetology/OISST_SST_Red.RDS")
# The next file can be obtained upon request and isn't necessary for the MHWs analysis - just to validate SST
IUI_daily_temp <-read.xlsx("data/Eilat climetology/IUI Obs Pier SST measured and interpolated 1988-2023.xlsx",sheet = "Interpolation") 

## Fix baseline
OISST_data_red <- OISST_data_red %>%  select(-lat,-lon)
OISST_data_red <- OISST_data_red %>% arrange(t)
clim_period <- c(OISST_data_red$t[1],as.Date("2011-12-31"))
MHW_climatelogy_tresh <- ts2clm(OISST_data_red,climatologyPeriod = clim_period,pctile = 90)
MHW_events <- detect_event(MHW_climatelogy_tresh)
MHW_events_cat <- category(MHW_events)
MHW_events_cat <- MHW_events_cat %>% rename("date_peak" = "peak_date")
climatelogy <- MHW_events$climatology
MHW_events <- MHW_events$event %>% select(-index_start, -index_peak, -index_end)
MHW_events <- merge(MHW_events,MHW_events_cat[,c("event_no","category")],by="event_no")

excel <- createWorkbook()
addWorksheet(excel,sheetName = "MHW_Events")
addWorksheet(excel,sheetName = "Climatelogy")
writeData(excel,"MHW_Events",MHW_events)
writeData(excel,"Climatelogy",climatelogy)
saveWorkbook(excel,"data/MHWs_def/MHW_OISST_Events_RedSea_Fix")


## Detrended baseline
OISST_data_red <- readRDS("data/Eilat climetology/OISST_SST_Red.RDS")

red_sst_JACOX <- OISST_data_red %>% filter(lat == OISST_data_red$lat[1] &lon == OISST_data_red$lon[1])
red_sst_JACOX <- red_sst_JACOX %>%  select(-lat,-lon)
red_sst_JACOX <- red_sst_JACOX %>% arrange(t)

# Climatology calculation
clim_period <- c(red_sst_JACOX$t[1],as.Date("2011-12-31"))
MHW_climatelogy_tresh <- ts2clm(red_sst_JACOX,climatologyPeriod = clim_period,pctile = 90)
MHW_climatelogy_tresh$anomaly <- MHW_climatelogy_tresh$temp-MHW_climatelogy_tresh$seas
MHW_climatelogy_tresh$anomaly_thresh <- MHW_climatelogy_tresh$thresh-MHW_climatelogy_tresh$seas

# Detrend anomaly
MHW_climatelogy_tresh$num_date <- as.numeric(MHW_climatelogy_tresh$t)
MHW_climatelogy_tresh <- na.omit(MHW_climatelogy_tresh)
warming_lm <- lm(anomaly~num_date,MHW_climatelogy_tresh)
# Validate the model
sjPlot::plot_model(warming_lm,type = 'pred',show.data = T)
appraise(warming_lm)
MHW_climatelogy_tresh <- cbind(MHW_climatelogy_tresh,warming_lm$residuals)
names(MHW_climatelogy_tresh)[9] <- "anomaly_detrend"
MHW_climatelogy_tresh$MHW_d <- ifelse(MHW_climatelogy_tresh$anomaly_detrend>MHW_climatelogy_tresh$anomaly_thresh,T,F)
colnames(MHW_climatelogy_tresh)[2] <- "date"
MHW_climatelogy_tresh$date <- as.Date(MHW_climatelogy_tresh$date)
detrended_mhws <- Detrend_MHW_days_to_MHWs(MHW_climatelogy_tresh,2,10,9,3,7)

excel <- createWorkbook()
addWorksheet(excel,sheetName = "MHW_Events")
addWorksheet(excel,sheetName = "Climatelogy")
writeData(excel,"MHW_Events",mhws)
writeData(excel,"Climatelogy",MHW_climatelogy_tresh)
saveWorkbook(excel,"data/MHWs_def/MHW_IUI_Events_RedSea_Jacox.xlsx")


# The next step - filtering to short period mhws (< 1 month), assign appropraite fish and calculate before and after limits 
# was done manually and the results for each definition can be found in the MHWs_def directory under the same names they appear
# in this code


## Compare IUI and OISST
IUI_daily_temp <- IUI_daily_temp %>%
  mutate(date = as.Date(Doy - 1, origin = paste0(Year, "-01-01")))
OISST_data_red <- OISST_data_red %>%
  rename("date" = "t")
compare_df <- merge(IUI_daily_temp,OISST_data_red,by="date")
cor(compare_df$temp,compare_df$SST) # 0.95
compare_lm <- lm(SST ~ temp,compare_df)
summary(compare_lm) # R2 0.91
sjPlot::plot_model(compare_lm,type = 'pred',show.data = T)
