###################### Packages ######################

install.packages("pacman")
library(pacman)
p_load(heatwaveR, lubridate, tidyverse, openxlsx, ggplot2)
###################### Functions #####################
# Find relevant fish for each MHW row
# This function checks if a fish has data during the MHW AND in the before/after periods
find_relevant_fish <- function(m_date_start, m_date_end, m_start_3, m_end_3, fish_data) {
  
  # Filter all fish data to the maximum wide window
  window_data <- fish_data %>%
    filter(date >= m_start_3, date <= m_end_3)
  
  # If there is absolutely no fish data in this window, return NA early
  if (nrow(window_data) == 0) return(NA)
  
  # Categorize dates and count observations per fish
  fish_counts <- window_data %>%
    mutate(
      period = case_when(
        date < m_date_start ~ "Before",
        date > m_date_end ~ "After",
        TRUE ~ "During"
      )
    ) %>%
    group_by(fish_id) %>%
    summarise(
      B = sum(period == "Before"),
      D = sum(period == "During"),
      A = sum(period == "After"),
      .groups = "drop"
    ) %>%
    # The rule: Must be present DURING (D > 0) AND at least one of BEFORE/AFTER (B > 0 or A > 0)
    filter(D > 0 & (B > 0 | A > 0))
  
  # If no fish meet the rule after counting, return NA
  if (nrow(fish_counts) == 0) return(NA)
  
  # Combine all fish into one string separated by " | "
  return(paste(fish_counts$fish_id, collapse = ", "))
}


# Function to plot fish observations for a specific MHW
plot_mhw_timeline <- function(mhw_row_index, mhw_data, fish_data) {
  
  # 1. Extract the specific MHW row
  mhw <- mhw_data[mhw_row_index, ]
  
  # 2. Filter fish data for the maximum relevant window (start_3 to end_3)
  window_data <- fish_data %>%
    filter(date >= mhw$start_3, date <= mhw$end_3)
  
  # If there is no data at all, stop gracefully
  if(nrow(window_data) == 0) {
    print(paste("No fish data for MHW at index", mhw_row_index))
    return(NULL)
  }
  
  # 3. Create the plot
  p <- ggplot() +
    # Background for 'Before' period (using start_3 to start date)
    geom_rect(aes(xmin = mhw$start_3, xmax = mhw$date_start, ymin = -Inf, ymax = Inf), 
              fill = "lightgreen", alpha = 0.2) +
    
    # Background for 'During' period (MHW)
    geom_rect(aes(xmin = mhw$date_start, xmax = mhw$date_end, ymin = -Inf, ymax = Inf), 
              fill = "red", alpha = 0.2) +
    
    # Background for 'After' period (end date to end_3)
    geom_rect(aes(xmin = mhw$date_end, xmax = mhw$end_3, ymin = -Inf, ymax = Inf), 
              fill = "lightblue", alpha = 0.2) +
    
    # Fish observation points
    # geom_jitter adds a tiny bit of vertical noise so points on the same day don't hide each other
    geom_jitter(data = window_data, aes(x = date, y = as.factor(fish_id)), 
                width = 0, height = 0.1, size = 2, color = "black", alpha = 0.7) +
    
    # Vertical lines for exact MHW boundaries
    geom_vline(xintercept = mhw$date_start, linetype = "dashed", color = "darkred") +
    geom_vline(xintercept = mhw$date_end, linetype = "dashed", color = "darkred") +
    
    # Styling and Labels
    theme_minimal() +
    labs(
      title = paste("Fish Observations for MHW:", mhw$date_start, "to", mhw$date_end),
      subtitle = "Green: Before | Red: During MHW | Blue: After",
      x = "Date",
      y = "Fish ID"
    ) +
    theme(axis.text.y = element_text(size = 8))
  
  return(p)
}

# Function to plot continuous activity over time for a SPECIFIC fish during a specific MHW
plot_continuous_fish <- function(mhw_row_index, specific_fish_id, mhw_data, fish_data, 
                                          time_col = "date", metric_col = "activity_level") {
  
  # 1. Extract the specific MHW row
  mhw <- mhw_data[mhw_row_index, ]
  
  # 2. Filter fish data for the specific fish AND the maximum relevant window
  fish_window <- fish_data %>%
    filter(
      fish_id == specific_fish_id,
      # Convert MHW dates to POSIXct so they match continuous datetime data if necessary
      !!sym(time_col) >= as.POSIXct(mhw$start_3), 
      !!sym(time_col) <= as.POSIXct(mhw$end_3 + 1) # +1 to include the end of the last day
    )
  
  # If there is no data for this specific fish in this window, stop gracefully
  if(nrow(fish_window) == 0) {
    message(paste("No data found for Fish ID", specific_fish_id, "in MHW index", mhw_row_index))
    return(NULL)
  }
  
  # 3. Create the plot
  p <- ggplot() +
    # Background for 'Before' period
    geom_rect(aes(xmin = as.POSIXct(mhw$start_3), xmax = as.POSIXct(mhw$date_start), ymin = -Inf, ymax = Inf), 
              fill = "lightgreen", alpha = 0.2) +
    
    # Background for 'During' period (MHW)
    geom_rect(aes(xmin = as.POSIXct(mhw$date_start), xmax = as.POSIXct(mhw$date_end), ymin = -Inf, ymax = Inf), 
              fill = "red", alpha = 0.15) +
    
    # Background for 'After' period
    geom_rect(aes(xmin = as.POSIXct(mhw$date_end), xmax = as.POSIXct(mhw$end_3 + 1), ymin = -Inf, ymax = Inf), 
              fill = "lightblue", alpha = 0.2) +
    
    # Continuous points representing activity over exact time
    geom_point(data = fish_window, aes(x = !!sym(time_col), y = !!sym(activity_col)), 
               color = "black", size = 1) +
    
    # Vertical lines for exact MHW boundaries
    geom_vline(xintercept = as.POSIXct(mhw$date_start), linetype = "dashed", color = "darkred", size = 0.8) +
    geom_vline(xintercept = as.POSIXct(mhw$date_end), linetype = "dashed", color = "darkred", size = 0.8) +
    # Styling and Labels
    theme_minimal() +
    labs(
      title = paste("Continuous Activity for Fish ID:", specific_fish_id),
      subtitle = paste("MHW Period:", mhw$date_start, "to", mhw$date_end),
      x = "Continuous Time",
      y = activity_col
    ) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1) # Make time labels readable
    )
  
  return(p)
}

# Function to plot continuous activity/depth for ALL relevant fish during a specific MHW in separate panels
plot_all_fish_continuous <- function(mhw_row_index, mhw_data, fish_data, 
                                     time_col = "real_datetime", metric_col = "activity",
                                     window = "3") { 
  
  # 1. Extract the specific MHW row
  mhw <- mhw_data[mhw_row_index, ]
  
  # Determine which start/end columns to use based on the window argument
  start_col <- paste0("start_", window)
  end_col <- paste0("end_", window)
  
  start_date <- as.POSIXct(mhw[[start_col]])
  end_date <- as.POSIXct(mhw[[end_col]] + 1) 
  mhw_start <- as.POSIXct(mhw$date_start)
  mhw_end <- as.POSIXct(mhw$date_end)
  
  # 2. Extract the fish IDs
  if (is.na(mhw$Fish_IDs)) {
    message(paste("No Fish IDs listed for MHW index", mhw_row_index))
    return(NULL)
  }
  
  relevant_fish <- as.numeric(unlist(strsplit(as.character(mhw$Fish_IDs), ",\\s*")))
  
  # 3. Filter fish data
  fish_window <- fish_data %>%
    filter(
      fish_id %in% relevant_fish,
      !!sym(time_col) >= start_date, 
      !!sym(time_col) <= end_date 
    )
  
  if(nrow(fish_window) == 0) {
    message(paste("No continuous data found for the listed fish in MHW index", mhw_row_index))
    return(NULL)
  }
  
  # 4. Create the plot with facets (Using annotate instead of geom_rect to prevent gtable errors)
  p <- ggplot() +
    # Background for 'Before' period
    annotate("rect", xmin = start_date, xmax = mhw_start, ymin = -Inf, ymax = Inf, 
             fill = "lightgreen", alpha = 0.2) +
    
    # Background for 'During' period (MHW)
    annotate("rect", xmin = mhw_start, xmax = mhw_end, ymin = -Inf, ymax = Inf, 
             fill = "red", alpha = 0.15) +
    
    # Background for 'After' period
    annotate("rect", xmin = mhw_end, xmax = end_date, ymin = -Inf, ymax = Inf, 
             fill = "lightblue", alpha = 0.2) +
    
    # Continuous points
    geom_point(data = fish_window, aes(x = !!sym(time_col), y = !!sym(metric_col)), 
               color = "black", size = 0.5, alpha = 0.6) +
    
    # Vertical lines for exact MHW boundaries
    geom_vline(xintercept = mhw_start, linetype = "dashed", color = "darkred", linewidth = 0.8) +
    geom_vline(xintercept = mhw_end, linetype = "dashed", color = "darkred", linewidth = 0.8) +
    
    # Create a separate panel for each fish
    facet_wrap(~ fish_id, scales = "free_y") + 
    
    # Styling and Labels
    theme_minimal() +
    labs(
      title = paste("Continuous", tools::toTitleCase(metric_col), "for All Relevant Fish"),
      subtitle = paste("MHW Index:", mhw_row_index, "| Dates:", as.Date(mhw_start), "to", as.Date(mhw_end)),
      x = "Date & Time",
      y = tools::toTitleCase(metric_col)
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      strip.background = element_rect(fill = "grey90", color = NA), 
      strip.text = element_text(face = "bold", size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
    )
  
  # Reverse Y-axis for depth
  if (metric_col %in% c("depth", "mean_depth")) {
    p <- p + scale_y_reverse()
  }
  
  return(p)
}
###################### Data ######################
# Define file paths for both MHWs defintions
files_to_update <- c("data/MHWs_def/MHW_OISST_Events_RedSea_Jacox.xlsx", 
                     "data/MHWs_def/MHW_OISST_Events_RedSea_Fix.xlsx")
# Fish acoustic data 2016-2018 & 2019-2021
combined_parrotfish_df <- readRDS("data/parrotfish data/parrotfish_acoustic_data_df.RDS")



###################### Main ######################

## Create sheet for short period MHWs (< 1 month)

for (file_path in files_to_update) {
  
  # 1. Load the existing Excel workbook (to preserve existing sheets like 'Climatelogy')
  wb <- loadWorkbook(file_path)
  
  # 2. Read the data from the 'MHW_Events' sheet
  mhw_events <- read.xlsx(wb, sheet = "MHW_Events")
  
  # 3. Process the FULL dataset first to find adjacent MHWs
  mhw_events <- mhw_events %>%
    mutate(
      date_start = convertToDate(date_start),
      date_end   = convertToDate(date_end)
    )%>%
    # Ensure chronological order to correctly identify previous and next events
    arrange(date_start) %>%
    # Define the boundaries of adjacent MHWs (the entire dataset, long and short)
    mutate(
      prev_mhw_end   = lag(date_end),      # The end date of the previous MHW
      next_mhw_start = lead(date_start)    # The start date of the next MHW
    )
  
  # 4. Filter for short MHWs in the required timeframe
  short_mhw_events <- mhw_events %>%
    filter(
      duration < 31,                                      
      date_start >= as.Date("2016-07-01"),                
      date_start <= as.Date("2021-01-31")                 
    ) %>%
    # 5. Calculate buffers and apply truncation rules
    mutate(
      
      # --- 1 Week Buffer ---
      # Calculate theoretical boundaries
      raw_start_1week = date_start - 7,
      raw_end_1week   = date_end + 7,
      # Truncate: Start no earlier than (prev_mhw_end + 1), End no later than (next_mhw_start - 1)
      # na.rm = TRUE ensures that if there is no previous/next MHW (first/last row), it keeps the raw date
      start_1week = pmax(raw_start_1week, prev_mhw_end + 1, na.rm = TRUE),
      end_1week   = pmin(raw_end_1week, next_mhw_start - 1, na.rm = TRUE),
      
      # --- 1.5x Duration Buffer ---
      dur_1_5 = ceiling(duration * 1.5),
      raw_start_1_5 = date_start - dur_1_5,
      raw_end_1_5   = date_end + dur_1_5,
      start_1.5     = pmax(raw_start_1_5, prev_mhw_end + 1, na.rm = TRUE),
      end_1.5       = pmin(raw_end_1_5, next_mhw_start - 1, na.rm = TRUE),
      
      # --- 3x Duration Buffer ---
      dur_3 = duration * 3,
      raw_start_3 = date_start - dur_3,
      raw_end_3   = date_end + dur_3,
      start_3     = pmax(raw_start_3, prev_mhw_end + 1, na.rm = TRUE),
      end_3       = pmin(raw_end_3, next_mhw_start - 1, na.rm = TRUE)
      
    ) %>%
    # 6. Clean up the dataframe: remove the temporary calculation columns
    select(
      -starts_with("raw_"), 
      -dur_1_5, -dur_3, 
      -prev_mhw_end, -next_mhw_start
    )  
  
  short_mhw_events <- short_mhw_events %>%
    rowwise() %>%
    mutate(
      Fish_IDs = find_relevant_fish(m_date_start = date_start, 
                                                            m_date_end   = date_end, 
                                                            m_start_3    = start_3, 
                                                            m_end_3      = end_3, 
                                                            fish_data    = combined_parrotfish_df
      )
    ) %>%
    ungroup() %>% 
    filter(!is.na(Fish_IDs)) # Keep only rows where we found relevant fish data
  
  # 4. Add a new worksheet to the workbook
  # We check if the sheet already exists to prevent errors during multiple runs
  if (!("Short_MHW_Events" %in% names(wb))) {
    addWorksheet(wb, sheetName = "Short_MHW_Events")
  }
  
  # 5. Write the filtered data into the new worksheet
  writeData(wb, sheet = "Short_MHW_Events", x = short_mhw_events)
  
  # 6. Save the updated workbook (overwrites the old file with the new additions)
  saveWorkbook(wb, file_path, overwrite = TRUE)
}

print("Successfully updated the files and added the Short_MHW_Events sheet!")


## The next steps conducted manually for each MHW defintion, each MHW and each fish_id
## The relevnat files after the maually filtration can be found in the directory
## data\MHWs_def\MHWs_filtered

short_mhw_events <- read.xlsx("data/MHWs_def/MHW_OISST_Events_RedSea_Fix.xlsx", sheet = "Short_MHW_Events") %>%
  mutate(date_start = convertToDate(date_start),
         date_end = convertToDate(date_end),
         start_3 = convertToDate(start_3),
         end_3 = convertToDate(end_3))

# The next step - look at the data for one of the MHWs and plot the fish observations
# in the 3x duration window around it, how it looks, if it makes sense, and which
# fish need to be removed.
plot_mhw_timeline(mhw_row_index = 9,short_mhw_events, combined_parrotfish_df)

plot_all_fish_continuous(
  mhw_row_index = 2,
  mhw_data = short_mhw_events,
  fish_data = combined_parrotfish_df,
  metric_col = "depth", # can be changed to 'depth'  
  window = "3" 
)

# The next step - look at activity/depth data of each specific fish during each MHW,
# but now with the continuous data (instead of just presence/absence) to make sure
# it has enough data and it is alive, and if not filter it out.
plot_continuous_fish(
  mhw_row_index = 2,
  specific_fish_id = 1255815,
  mhw_data = short_mhw_events,
  fish_data = combined_parrotfish_df,
  time_col = "real_datetime",      
  metric_col = "activity"   # can be changed to 'depth'  
)


### Examples for fish being removed due to being dead or having not enogh data
### the examples are from the detrended baseline
short_mhw_events <- read.xlsx("data/MHWs_def/MHW_OISST_Events_RedSea_Jacox.xlsx", sheet = "Short_MHW_Events") %>%
  mutate(date_start = convertToDate(date_start),
         date_end = convertToDate(date_end),
         start_3 = convertToDate(start_3),
         end_3 = convertToDate(end_3))

# Being dead 
plot_continuous_fish(
  mhw_row_index = 2, 
  specific_fish_id = 1255808, 
  mhw_data = short_mhw_events,
  fish_data = combined_parrotfish_df,
  time_col = "real_datetime",      
  metric_col = "depth"     
)

# Not enough data 
plot_continuous_fish(
  mhw_row_index = 4,
  specific_fish_id = 1726032,
  mhw_data = short_mhw_events,
  fish_data = combined_parrotfish_df,
  time_col = "real_datetime",      
  metric_col = "activity"     
)
