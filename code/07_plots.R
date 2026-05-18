################################################################################
# 1. PACKAGES & SETUP
################################################################################
library(pacman)
p_load(tidyverse, lubridate, openxlsx, mgcv, ggplot2, ggpubr, ggpmisc, ggtext, patchwork)

# Define standard chronological order for MHW stages
stage_levels <- c("Before", "MHW", "After")

################################################################################
# 2. DATA LOADING & FORMATTING
################################################################################
# Load MHW definitions and Sea Surface Temperature (SST) climatology
MHWs_Eilat_detrended_OISST <- read.xlsx("data/MHWs_def/MHWs_filtered/MHW_OISST_Events_RedSea_Jacox_filtered.xlsx", sheet = "Short_MHW_Events")
Eilat_detrended_OISST_temp <- read.xlsx("data/MHWs_def/MHWs_filtered/MHW_OISST_Events_RedSea_Jacox_filtered.xlsx", sheet = "Climatelogy") %>% rename(t = date)
MHWs_Eilat_Fix_OISST <- read.xlsx("data/MHWs_def/MHWs_filtered/MHW_OISST_Events_RedSea_Fix_filtered.xlsx",sheet = "Short_MHW_Events")
Eilat_Fix_OISST_temperature <- read.xlsx("data/MHWs_def/MHWs_filtered/MHW_OISST_Events_RedSea_Fix_filtered.xlsx",sheet = "Climatelogy")

# Load pre-computed GAM models and statistical comparison datasets
all_heat_wave_sum <- readRDS("results/Individual Heatwaves/All_Windows_Models_OISST_detrended.RDS")
MHWs_comparison   <- readRDS("results/Individual Heatwaves/1.5/OISST_detrended_MHW_Stage_comparison.RDS")
act_models        <- readRDS("results/Models/Activity_Models_OISST_detrended_ordinal_1.5.RDS")
dep_models        <- readRDS("results/Models/Depth_Models_OISST_detrended_ordinal_1.5.RDS")

# Prepare "Triplets" (data filtered to include only fish present in all 3 stages)
act_triplets  <- all_heat_wave_sum$`1.5`$Triplets$Activity %>% mutate(Before_After = factor(Before_After, levels = stage_levels))
dep_triplets  <- all_heat_wave_sum$`1.5`$Triplets$Depth    %>% mutate(Before_After = factor(Before_After, levels = stage_levels))
disp_triplets <- all_heat_wave_sum$`1.5`$Triplets$Displacement %>% mutate(Before_After = factor(Before_After, levels = stage_levels))

# Append a dummy row for S. niger to force it to appear in the combined plots' legend
act_triplets <- act_triplets %>% bind_rows(data.frame(species = "S. niger", Before_After = "Before", mean_activity = NA))


################################################################################
# 3. GLOBAL AESTHETICS (Italic Species, Plain Stages)
################################################################################
legend_title <- "MHW Stage"

# Define consistent shapes, HTML italic labels, and colors for all plots
species_shapes <- c("S. fuscopurpureus" = 21, "C. gibbus" = 22, "C. sordidus" = 24, "S. ferrugineus" = 25, "S. niger" = 8)
species_labels <- setNames(paste0("<i>", names(species_shapes), "</i>"), names(species_shapes))
mhw_colors     <- c("Before" = "#009E73", "MHW" = "#B22222", "After" = "#002D72")

# Create a unified publication-ready theme
main_theme <- theme_bw() + theme(
  panel.grid = element_blank(),
  axis.text  = element_text(size = 16, color = "black"),
  axis.title = element_text(size = 18),
  legend.text  = element_markdown(size = 14, color = "black"), # Allows <i> tags
  legend.title = element_text(size = 16, face = "bold"),
  legend.position = "top",
  plot.tag = element_text(size = 22, face = "bold"),
  strip.background = element_rect(fill = "grey90"),
  strip.text = element_text(size = 14, face = "bold")
)

################################################################################
# 4. MODULE A: MEAN STAGE PLOTS 
################################################################################
# Reusable function to generate standard point/error-bar plots for stage means
create_mean_plot <- function(data, y_var, y_label, y_breaks, reverse = FALSE) {
  p <- ggplot(data, aes(x = Before_After, y = !!sym(y_var), fill = Before_After)) +
    geom_point(aes(shape = species, color = Before_After), size = 3, alpha = 0.25, stroke = 0.8, position = position_jitter(width = 0.1, height = 0)) +
    stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.15, color = "black", linewidth = 0.8) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 5, color = "black", stroke = 1.2, aes(fill = Before_After)) +
    scale_shape_manual(values = species_shapes, labels = species_labels, drop = FALSE) +
    scale_color_manual(values = mhw_colors) + scale_fill_manual(values = mhw_colors) +
    labs(y = y_label, shape = "Species") + main_theme + theme(axis.title.x = element_blank())
  if(reverse) p <- p + scale_y_reverse() else p <- p + scale_y_continuous(breaks = y_breaks)
  return(p)
}

# Generate overall mean plots for the three behavioral metrics
act_mean_p  <- create_mean_plot(act_triplets, "mean_activity", expression("Mean Activity [" * m ~ s^{-2} * "]"), seq(0, 3, 0.5))
dep_mean_p  <- create_mean_plot(dep_triplets, "mean_depth", "Mean depth [m]", NULL, reverse = TRUE)
disp_mean_p <- create_mean_plot(disp_triplets, "mean_disp_max", "Mean daily displacement [m]", seq(0, 1500, 250))

################################################################################
# 5. MODULE B: GAMM & PC1 REGRESSION (Right Column)
################################################################################
# Helper function to extract predictions and confidence intervals from GAMM objects
get_gam_preds <- function(model, type = "response") {
  grid <- expand.grid(TimeOrdinal = seq(1, 5, length.out = 100), Before_After = factor(stage_levels, levels = stage_levels),
                      species = factor(levels(model$model$species)[1]), Length_scaled = 0, 
                      fish_id = levels(model$model$fish_id)[1], Serial = model$model$Serial[1])
  preds <- predict(model, newdata = grid, type = type, se.fit = TRUE, exclude = c("s(TimeOrdinal,fish_id)", "s(Serial)"))
  grid$predicted <- if(type == "link") exp(preds$fit)-0.05 else preds$fit
  grid$se <- preds$se.fit
  grid <- grid %>% mutate(conf.low = if(type == "link") exp(preds$fit - 1.96*se)-0.05 else predicted - 1.96*se,
                          conf.high = if(type == "link") exp(preds$fit + 1.96*se)-0.05 else predicted + 1.96*se)
  grid$Before_After <- factor(grid$Before_After, levels = stage_levels)
  return(grid)
}

# Generate population-level diel prediction plot for Activity
act_models$mhw_df$Before_After <- factor(act_models$dep_df$Before_After, levels = stage_levels)
act_mod_p <- ggplot(get_gam_preds(act_models$best_mod), aes(x = TimeOrdinal, y = predicted, color = Before_After)) +
  geom_rect(xmin = -Inf, xmax = 2, ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.25, color = NA, inherit.aes = FALSE) +
  geom_rect(xmin = 4, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.25, color = NA, inherit.aes = FALSE) +
  # geom_point(data = act_models$dep_df, aes(x = TimeOrdinal,y = activity),size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Before_After), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.2) + scale_color_manual(values = mhw_colors) + scale_fill_manual(values = mhw_colors) +
  scale_x_continuous(expand = c(0,0)) + labs(y = expression("Activity [" * m ~ s^{-2} * "]"), x = "Relative time of day") + main_theme + 
  guides(color = guide_legend(override.aes = list(size = 3)))

# Generate population-level diel prediction plot for Depth
dep_mod_p <- ggplot(get_gam_preds(dep_models$best_gam_mod_ar1, "link"), aes(x = TimeOrdinal, y = predicted, color = Before_After)) +
  geom_rect(xmin = -Inf, xmax = 2, ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.25, color = NA, inherit.aes = FALSE) +
  geom_rect(xmin = 4, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.25, color = NA, inherit.aes = FALSE) +
  geom_point(data = dep_models$dep_df, aes(x = TimeOrdinal,y = depth),size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Before_After), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.2) + scale_color_manual(values = mhw_colors) + scale_fill_manual(values = mhw_colors) +
  scale_y_reverse() + coord_cartesian(ylim = c(14, 0)) + scale_x_continuous(expand = c(0,0)) +
  labs(y = "Mean Depth [m]", x = "Relative time of day") + main_theme + 
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 3)))

# PC1 Regression plot (Displacement vs MHW Severity)
reg_plot_pc1 <- ggplot(MHWs_comparison %>% filter(Stage_period == "MHW : Before"), aes(x = PC1, y = log_max_displacement)) +
  stat_summary(fun = mean, geom = "point", size = 2) + stat_summary(fun.data = mean_cl_boot, geom = "linerange") +
  geom_smooth(method = "lm", color = "darkred", linewidth = 1.2) +
  stat_poly_eq(aes(label = paste(after_stat(rr.label), after_stat(p.value.label), sep = "~~~")), formula = y ~ x, parse = TRUE, size = 6) +
  labs(y = "ln(Displacement ratio)", x = "MHW Severity (PC1)") + geom_hline(yintercept = 0, linetype = "dashed") + main_theme

################################################################################
# 7. FIGURE 2 ASSEMBLY (1:3 Ratio)
################################################################################
# Group left and right columns and force shared legend ('guides = collect')
left_col  <- (act_mean_p / dep_mean_p / disp_mean_p)
right_col <- (act_mod_p / dep_mod_p / reg_plot_pc1) & theme(legend.position = "none")

# Final patchwork assembly for Main Figure 2
final_figure2 <- (left_col | right_col) + 
  plot_layout(widths = c(1, 3), guides = "collect") +
  plot_annotation(tag_prefix = '(',tag_levels = 'a', tag_suffix = ')') & 
  theme(legend.position = "top", legend.box = "horizontal")

ggsave("results/plots/Figure_2_Main.svg", x, width = 16, height = 15, dpi = 300)

# Assemble and save supplementary figures
figure_s4 <- (act_mod_p / dep_mod_p) + plot_layout(guides = "collect") & theme(legend.position = "top")
ggsave("results/plots/Figure_S4.svg", figure_s4, width = 16, height = 15, dpi = 300)

figure_s11 <- dep_mean_p +act_mean_p + act_mod_p
ggsave("results/plots/Figure_S11.svg", figure_s11, width = 16, height = 12, dpi = 300)

################################################################################
# 8. MODULE C: FULL REGRESSIONS (All Predictors)
################################################################################
# Reshape MHW characteristics into long format for facet plotting
mhws_comparison_lm_long <- MHWs_comparison %>% filter(Stage_period == "MHW : Before") %>%
  pivot_longer(cols = c(duration, intensity_mean, intensity_max, rate_onset, intensity_cumulative, intensity_max_abs, PC1),
               names_to = "predictor", values_to = "predictor_value") %>%
  mutate(predictor = factor(predictor, levels = c("intensity_max", "intensity_mean", "duration", "rate_onset", "intensity_cumulative", "intensity_max_abs", "PC1")))

# Define custom labels for the facet panels
x_titles <- c(duration = "Duration [days]", intensity_mean = "Mean intensity [°C]", intensity_max = "Max intensity [°C]",
              rate_onset = "Onset rate [°C/day]", intensity_cumulative = "Cumulative intensity [°C·days]",
              intensity_max_abs = "Absolute maximum intensity [°C]", PC1 = "MHW severity (PC1)")

# Function to generate multi-panel regression plots
create_full_reg <- function(y_var, y_label) {
  ggplot(mhws_comparison_lm_long, aes(x = predictor_value, y = !!sym(y_var))) +
    stat_summary(fun = mean, geom = "point", size = 2) + stat_summary(fun.data = mean_cl_boot, geom = "linerange") +
    geom_smooth(method = "lm", color = "darkred") +
    stat_poly_eq(aes(label = paste(after_stat(rr.label), after_stat(p.value.label), sep = "~~~")), formula = y ~ x, parse = TRUE, size = 5) +
    facet_wrap(~predictor, scales = "free_x", labeller = as_labeller(x_titles), nrow = 1) +
    labs(y = y_label, x = NULL) + geom_hline(yintercept = 0, linetype = "dashed") + main_theme + theme(panel.spacing.x = unit(1, "lines"))
}

# Generate and combine regression plots for each behavioral metric
reg_full_act  <- create_full_reg("ln_activity_ratio", "ln (Activity ratio)")
reg_full_dep  <- create_full_reg("delta_depth", expression(Delta~"Depth [m]"))
reg_full_disp <- create_full_reg("log_max_displacement", "ln(Displacement ratio)")

combined_reg_full <- (reg_full_act / reg_full_dep / reg_full_disp) + plot_annotation(tag_prefix = '(',tag_levels = 'a',tag_suffix = ')')
ggsave("results/plots/Figure_S3_Full_Regressions_Fix_1.5.svg", combined_reg_full, width = 18, height = 12)

################################################################################
# 9. MODULE D: MHW METRICS & SST TIMELINE
################################################################################
# --- MHW Characteristics (Boxplots) ---
MHWs_Eilat <- MHWs_Eilat_detrended_OISST
colnames(MHWs_Eilat) <- make.names(colnames(MHWs_Eilat), unique = TRUE)
MHW_metrics_df <- MHWs_Eilat %>% select(intensity_mean, intensity_max, rate_onset, intensity_cumulative, intensity_max_abs, duration)
names(MHW_metrics_df) <- c("Mean intensity [°C]", "Max intensity [°C]", "Onset rate [°C/day]", "Cumulative intensity [°C·days]", "Abs max intensity [°C]", "Duration [days]")

# Plot large scale and small scale variables separately
p_large <- ggplot(pivot_longer(MHW_metrics_df[,4:6], everything()), aes(x = name, y = value)) +
  geom_boxplot(color = "red", outlier.shape = NA) + geom_point() + labs(title = "(a) Large-scale variables", y = "MHW characteristic value") + main_theme + theme(axis.title.x = element_blank())

p_small <- ggplot(pivot_longer(MHW_metrics_df[,1:3], everything()), aes(x = name, y = value)) +
  geom_boxplot(color = "red", outlier.shape = NA) + geom_point() + labs(title = "(b) Small-scale variables", y = "MHW characteristic value") + main_theme + theme(axis.title.x = element_blank())

combined_metrics <- (p_large / p_small)
ggsave("results/plots/Figure_S2_MHW_Metrics_detrended.svg", combined_metrics, width = 10, height = 12)

# --- SST Timeline ---
# Format dates and extract timeline subset
MHWs_Eilat <- MHWs_Eilat %>% mutate(across(contains("date") | contains("start") | contains("end"), ~as.Date(.x, origin = "1899-12-30")))
MHWs_Eilat$MHW_id <- c(1:nrow(MHWs_Eilat))
temp_timeseries <- Eilat_detrended_OISST_temp %>% mutate(t = as.Date(t, origin = "1899-12-30")) %>%
  filter(t >= min(MHWs_Eilat$start_1.5) - 20 & t <= max(MHWs_Eilat$end_1.5) + 20)

# Calculate ribbons to highlight temperature crossing the threshold
ribbon_df <- temp_timeseries %>% rowwise() %>%
  mutate(MHW_id = list(which(t >= MHWs_Eilat$date_start & t <= MHWs_Eilat$date_end))) %>%
  filter(length(MHW_id) > 0) %>% mutate(MHW_id = MHW_id[1], ymin = pmin(temp, thresh), ymax = pmax(temp, thresh))

# Define labels (roman numerals) for MHW events
mhw_labels <- MHWs_Eilat %>%
  mutate(
    x = date_start + (date_end - date_start)/2,  # midpoint
    y = 29.5,     # a bit above the max temperature
    label = as.character(as.roman(row_number()))        # label text
  )
Sys.setlocale("LC_TIME", "C")

# Plot comprehensive SST timeline with events
timeline_p <- ggplot() +
  geom_line(data = temp_timeseries, aes(x = t, y = temp, color = "Temperature"), size = 1) +
  geom_line(data = temp_timeseries, aes(x = t, y = seas, color = "Seasonal climatology"), linetype = "dashed",size=1) +
  geom_line(data = temp_timeseries, aes(x = t, y = thresh, color = "MHW threshold (90th precentile)"), linetype = "dotdash",size=1) +
  geom_rect(data = MHWs_Eilat, aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = Inf, fill = "MHW event"), alpha = 0.2) +
  geom_ribbon(data = ribbon_df, aes(x = t, ymin = ymin, ymax = ymax, group = MHW_id), fill = "red") +
  geom_text(data = mhw_labels,aes(x = x, y = y, label = label),color = "black",size = 4,fontface = "bold" ) +
  scale_color_manual(values = c("Temperature" = "black", "Seasonal climatology" = "blue", "MHW threshold (90th precentile)" = "darkred")) +
  scale_fill_manual(values = c("MHW event" = "red")) + labs(x = "Date", y = "Temperature [°C]") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 month",expand = c(0,0)) + main_theme + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ggsave("results/plots/Figure_S1_SST_Timeline_setrended.svg", timeline_p, width = 15, height = 6)


################################################################################
# 10. MODULE G: GAMM Individuals Predictions
################################################################################

# Function to generate individual-level predictions (per fish and MHW event)
get_gam_preds <- function(model, original_df, type = "response") {
  
  # 1. Create a grid based on actual observed combinations in the data
  # This ensures we only predict for existing fish/MHW pairings (Serial)
  grid <- original_df %>%
    distinct(fish_id, Serial, species, Before_After) %>%
    group_by(fish_id, Serial, species, Before_After) %>%
    # Generate a time sequence (1-5) for each unique combination
    do(data.frame(TimeOrdinal = seq(1, 5, length.out = 100))) %>%
    ungroup() %>%
    mutate(
      Length_scaled = 0, # Standardize to mean fish length
      # Ensure factor levels are correctly ordered for the legend
      Before_After = factor(Before_After, levels = stage_levels)
    )
  
  # 2. Generate predictions including random/factor-smooth effects
  # We do NOT exclude "fish_id" smooths here to get individual-level curves
  preds <- predict(model, newdata = grid, type = type, se.fit = TRUE)
  
  # 3. Process fits and calculate confidence intervals
  # Back-transform from log scale if type is "link" (for depth models)
  grid$predicted <- if(type == "link") exp(preds$fit) - 0.05 else preds$fit
  grid$se <- preds$se.fit
  
  grid <- grid %>% mutate(
    conf.low  = if(type == "link") exp(preds$fit - 1.96*se) - 0.05 else predicted - 1.96*se,
    conf.high = if(type == "link") exp(preds$fit + 1.96*se) - 0.05 else predicted + 1.96*se
  )
  grid$Serial_fish_id <- paste0(grid$Serial,"_",grid$fish_id)
  grid$Serial_fish_id <- as.factor(grid$Serial_fish_id)  
  return(grid)
}

# Generate individual activity predictions and calculate stage differences (Delta)
act_indiv_preds <- get_gam_preds(act_models$best_mod, act_models$dep_df)

# 1. Take the predictions we already generated for the individuals
act_delta_data <- act_indiv_preds %>%
  select(TimeOrdinal, fish_id, Serial, Before_After, predicted) %>%
  # Spread the stages into columns to do math on them
  pivot_wider(names_from = Before_After, values_from = predicted) %>%
  # Calculate the exact difference from the "Before" stage at every time point
  mutate(
    Delta_MHW = MHW - Before,
    Delta_After = After - Before
  ) %>%
  # Bring it back to long format for ggplot
  pivot_longer(cols = starts_with("Delta"), 
               names_to = "Stage", 
               values_to = "Delta_Activity") %>%
  mutate(
    # Clean up names for the legend
    Stage = factor(str_replace(Stage, "Delta_", ""), levels = c("Before","MHW", "After")),
    Serial_fish_id = paste(Serial, fish_id, sep = "_")
  ) %>% 
  filter(!is.na(Delta_Activity))

# Plot Individual Activity Deltas 
act_delta_p <- ggplot(act_delta_data, aes(x = TimeOrdinal, y = Delta_Activity, color = Stage)) +
  
  # Background shading for day/night
  geom_rect(xmin = -Inf, xmax = 2, ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.25, color = NA, inherit.aes = FALSE) +
  geom_rect(xmin = 4, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.25, color = NA, inherit.aes = FALSE) +
  
  # THE BASELINE: A dashed line at zero representing the "Before" stage
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
  
  # The actual difference curves for MHW and After
  geom_line(linewidth = 1.2) +
  
  facet_wrap(~Serial_fish_id, scales = "free_y") +
  
  # Note: Use only the colors for MHW and After from your palette
  scale_color_manual(values = mhw_colors[c("MHW", "After")]) +
  scale_x_continuous(expand = c(0,0)) + 
  coord_cartesian(ylim = c(-0.12,0))+
  scale_y_continuous(breaks = seq(-0.12,0,0.04))+
  labs(y = expression(Delta * " Activity [" * m ~ s^{-2} * "]"), 
       x = "Relative time of day")+ 
  main_theme +
  guides(color = guide_legend(override.aes = list(linewidth = 2)))

# --- ACTIVITY PLOT (Individual Level) ---
act_mod_p_ind <- ggplot( get_gam_preds(act_models$best_mod, act_models$dep_df), 
                         aes(x = TimeOrdinal, y = predicted, color = Before_After)) +
  # Background shading for day/night periods
  geom_rect(xmin = -Inf, xmax = 2, ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.25, color = NA, inherit.aes = FALSE) +
  geom_rect(xmin = 4, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.25, color = NA, inherit.aes = FALSE) +
  # Raw activity data points
  # geom_point(data = act_models$dep_df, aes(x = TimeOrdinal, y = activity), size = 1, alpha = 0.1) +
  # Model fit: Ribbon and Line
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Before_After), alpha = 0.2, color = NA) +
  geom_line(linewidth = 0.6) + 
  facet_wrap(~Serial_fish_id,scales="free_y") +
  # Unified scales for color and fill to merge the legend
  scale_color_manual(values = mhw_colors, name = "MHW Stage") + 
  scale_fill_manual(values = mhw_colors, name = "MHW Stage") +
  scale_x_continuous(expand = c(0,0)) + 
  labs(y = expression("Activity [" * m ~ s^{-2} * "]"), x = "Relative time of day") + 
  main_theme + 
  # coord_cartesian(ylim = c(0, 6)) +
  # Customize legend appearance
  guides(color = guide_legend(override.aes = list( size = 3)))

# --- DEPTH PLOT (Individual Level) ---
dep_mod_p_ind <- ggplot(get_gam_preds(dep_models$best_gam_mod_ar1, dep_models$dep_df, type = "link"), 
                        aes(x = TimeOrdinal, y = predicted, color = Before_After)) +
  # Background shading
  geom_rect(xmin = -Inf, xmax = 2, ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.25, color = NA, inherit.aes = FALSE) +
  geom_rect(xmin = 4, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.25, color = NA, inherit.aes = FALSE) +
  # Raw depth data points
  # geom_point(data = dep_models$dep_df, aes(x = TimeOrdinal, y = depth), size = 1, alpha = 0.1) +
  # Model fit: Ribbon and Line
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Before_After), alpha = 0.2, color = NA) +
  geom_line(linewidth = 0.7) + 
  facet_wrap(~Serial_fish_id,scales="free_y") +
  # Styling and axis inversion for depth
  scale_color_manual(values = mhw_colors, name = "MHW Stage") + 
  scale_fill_manual(values = mhw_colors, name = "MHW Stage") +
  scale_y_reverse(breaks = seq(0,24,6)) + 
  coord_cartesian(ylim = c(25, 0)) +
  scale_x_continuous(expand = c(0,0)) + 
  labs(y = "Mean Depth [m]", x = "Relative time of day") + 
  main_theme + 
  guides(color = guide_legend(override.aes = list(linewidth = 2)))

# ggsave("results/plots/Figure_S6.svg",act_mod_p_ind, width = 15, height = 9)
# ggsave("results/plots/Figure_S5.svg", dep_mod_p_ind, width = 15, height = 9)


##### Fish Summary Table: Counting Unique MHW Events per Fish across All Datasets #####
# Define target column name
mhw_col_name <- "Serial" 

# 1. Helper function to count unique MHW events per fish
# Returns a summary table with fish_id and the count of unique events
get_mhw_counts <- function(df, source_name) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  df %>%
    # Ensure fish_id is character and remove NAs
    filter(!is.na(fish_id)) %>%
    mutate(Serial_fish_id = as.character(Serial_fish_id)) %>%
    group_by(Serial_fish_id) %>%
    summarise(!!paste0("mhw_count_", source_name) := n_distinct(get(mhw_col_name))) %>%
    ungroup()
}

# 2. Extract counts from all 5 sources
# We use the specific dataframe locations from your console output
counts_list <- list(
  get_mhw_counts(act_triplets, "act_trip"),
  get_mhw_counts(dep_triplets, "dep_trip"),
  get_mhw_counts(disp_triplets, "disp_trip"),
  get_mhw_counts(act_models$dep_df, "act_mod"),
  get_mhw_counts(dep_models$dep_df, "dep_mod")
)

# 3. Create the master list of all unique fish IDs
all_fish_ids <- unique(unlist(lapply(counts_list, function(x) x$Serial_fish_id)))

# 4. Join all counts into one master summary table
fish_summary <- data.frame(Serial_fish_id = all_fish_ids)

for (count_df in counts_list) {
  if (!is.null(count_df)) {
    fish_summary <- left_join(fish_summary, count_df, by = "Serial_fish_id")
  }
}

# 5. Final polish: Replace NAs with 0 and calculate total stats
fish_summary <- fish_summary %>%
  mutate(across(starts_with("mhw_count"), ~replace_na(., 0))) %>%
  mutate(
    # How many datasets does this fish appear in?
    total_datasets = rowSums(select(., starts_with("mhw_count")) > 0),
    # Total sum of MHW experiences across all datasets
    total_mhw_experiences = rowSums(select(., starts_with("mhw_count")))
  ) %>%
  # Sort by fish with the most "action" first
  arrange(desc(total_mhw_experiences))

# View the result
print(fish_summary)
