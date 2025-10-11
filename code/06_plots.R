###################### Packages ######################

install.packages("pacman")
library(pacman)
p_load(tidyverse, lubridate, openxlsx, mgcv,ggplot2, patchwork,ggpmisc)

###################### Code ##########################

## Load data
sum_heat_wave <- readRDS("results/Individual Heatwaves/OISST_Fix_MHW_Stage_summary.RDS")
sum_heat_wave$Before_After <- factor(sum_heat_wave$Before_After,levels = c("Before","MHW","After"))

MHWs_comparison <- readRDS("results/Individual Heatwaves/OISST_Fix_MHW_Stage_comparison.RDS")
act_models <- readRDS("results/Models/Activity_Models_OISST_Fix_ordinal.RDS")
dep_models <- readRDS("results/Models/Depth_Models_OISST_Fix_ordinal.RDS")

###### Mean plots ######
act_complete_triplets <- sum_heat_wave %>%
  filter(!is.na(mean_activity)) %>%
  group_by(Serial_fish_id) %>%
  filter(n_distinct(Before_After) == 3) %>%
  arrange(Serial_fish_id) %>% 
  ungroup()

dep_complete_triplets <- sum_heat_wave %>%
  filter(!is.na(mean_depth)) %>%
  group_by(Serial_fish_id) %>%
  filter(n_distinct(Before_After) == 3) %>%
  arrange(Serial_fish_id) %>% 
  ungroup()

disp_complete_triplets <- sum_heat_wave %>%
  filter(!is.na(mean_disp_max)) %>%
  group_by(Serial_fish_id) %>%
  filter(disp_max_n>=5) %>% 
  filter(n_distinct(Before_After) == 3) %>%
  arrange(Serial_fish_id,Before_After) %>% 
  ungroup()


act_mean_stage_plot <- ggplot(act_complete_triplets) +
  geom_point(aes(x = Before_After, y = mean_activity, fill = Before_After),
             shape = 21, size = 1.5, alpha = 0.3,
             position = position_jitter(width = 0.1, height = 0)) +
  stat_summary(
    aes(x = Before_After, y = mean_activity,color = Before_After, fill = Before_After),
    fun.data = mean_cl_boot,
    size = 1,
    linewidth = 0.8
  ) +
  scale_color_manual(values = c("Before" = "#2E8B57",
                               "MHW" = "#DC143C",
                               "After" = "#4169E1")) +
  scale_fill_manual(values = c("Before" = "#2E8B57",
                               "MHW" = "#DC143C",
                               "After" = "#4169E1")) +
  scale_y_continuous(breaks = seq(0, 3, 0.5)) +
  labs(
    y = expression("Mean Activity [" * m ~ s^{-2} * "]")
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 18, color = "black"),
    axis.title = element_text(size = 20)
  )
act_mean_stage_plot

dep_mean_stage_plot <- ggplot(dep_complete_triplets) +
  geom_point(aes(x = Before_After, y = mean_depth, fill = Before_After),
             shape = 22, size = 1.5, alpha = 0.3,
             position = position_jitter(width = 0.1, height = 0)) +
  stat_summary(
    aes(x = Before_After, y = mean_depth,color = Before_After, fill = Before_After),
    fun.data = mean_cl_boot,
    shape = 22,
    size = 1,
    linewidth = 0.8
  ) +
  scale_color_manual(values = c("Before" = "#2E8B57",
                                "MHW" = "#DC143C",
                                "After" = "#4169E1")) +
  scale_fill_manual(values = c("Before" = "#2E8B57",
                               "MHW" = "#DC143C",
                               "After" = "#4169E1")) +
  scale_y_reverse() +
  labs(
    y = expression("Mean depth [m]")
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 18, color = "black"),
    axis.title = element_text(size = 20)
  )
dep_mean_stage_plot

disp_mean_stage_plot <- ggplot(disp_complete_triplets) +
  geom_point(aes(x = Before_After, y = mean_disp_max, fill = Before_After),
             shape = 24, size = 1.5, alpha = 0.3,
             position = position_jitter(width = 0.1, height = 0)) +
  stat_summary(
    aes(x = Before_After, y = mean_disp_max,color = Before_After, fill = Before_After),
    fun.data = mean_cl_boot,
    shape = 24,
    size = 1,
    linewidth = 0.8
  ) +
  scale_color_manual(values = c("Before" = "#2E8B57",
                                "MHW" = "#DC143C",
                                "After" = "#4169E1")) +
  scale_fill_manual(values = c("Before" = "#2E8B57",
                               "MHW" = "#DC143C",
                               "After" = "#4169E1")) +
  scale_y_continuous(breaks = seq(0,1500,250))+
  labs(
    y = expression("Mean daily displacment [m]")
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 18, color = "black"),
    axis.title = element_text(size = 20)
  )
disp_mean_stage_plot
(act_mean_stage_plot / dep_mean_stage_plot / disp_mean_stage_plot) +
  plot_annotation(tag_levels = 'a') & 
  theme(plot.tag = element_text(size = 24))



###### GAMM plots ######
# Activity
act_mod <- act_models$full_mod

# Get predictions 
new_dat_act <- expand.grid(
  TimeOrdinal = seq(1,5,length.out = 50),
  Before_After     = factor(c("Before","MHW","After"),
                            levels = levels(act_mod$model$Before_After)),
  Serial_fish_id = "1_1212924"  # placeholder; will be excluded
)

pred_act <- predict(
  act_mod,
  newdata = new_dat_act,
  type = "response",
  se.fit = TRUE,
  exclude = "s(TimeOrdinal,Serial_biv_id)"  # removes individual deviations
)

new_dat_act$predicted <- pred_act$fit
new_dat_act$conf.low <- pred_act$fit - 1.96 * pred_act$se.fit
new_dat_act$conf.high <- pred_act$fit + 1.96 * pred_act$se.fit
new_dat_act$Before_After <- factor(new_dat_act$Before_After, levels = c("Before", "MHW", "After"))

# Plot
act_mod_plot <- ggplot(new_dat_act, aes(x = TimeOrdinal, y = predicted, color = Before_After)) +
  # Shading for night time (before sunrise) - with color = NA
  geom_rect(xmin = -Inf, xmax = 2, ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.3, color = NA) +
  # Shading for night time (after sunset, i.e., 18:00) - with color = NA
  geom_rect(xmin = 4, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.3, color = NA) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Before_After), alpha = 0.2, color = NA) +
  scale_x_continuous(limits = c(1, 5), breaks = seq(1, 5, 1), expand = c(0, 0)) + scale_y_continuous(breaks = seq(0,5,1))+
  labs(
    x = "Time of Day (Ordinal scale)",
    y = "Activity [m s⁻²]",
    color = "MHW Stage",
    fill = "MHW Stage",
  ) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 18, color = "black"),
        axis.title = element_text(size = 19.8),
        legend.text = element_text(size = 14, color = "black"),
        legend.title = element_text(size = 16, color = "black"),
        legend.position = "right")+ 
  scale_color_manual(values = c("#2E8B57", "#DC143C", "#4169E1")) +
  scale_fill_manual(values = c("#2E8B57", "#DC143C", "#4169E1"))
act_mod_plot

# Get predictions 
new_dat_act_ind <- expand.grid(
  TimeOrdinal = seq(1,5,length.out = 50),
  Before_After     = factor(c("Before","MHW","After"),
                            levels = levels(act_mod$model$Before_After)),
  Serial_fish_id    = unique(act_mod$model$Serial_fish_id)  # placeholder; will be excluded
)

pred_act_ind <- predict(
  act_mod,
  newdata = new_dat_act_ind,
  type = "response",
  se.fit = TRUE
)

new_dat_act_ind$predicted <- pred_act_ind$fit
new_dat_act_ind$conf.low <- pred_act_ind$fit - 1.96 * pred_act_ind$se.fit
new_dat_act_ind$conf.high <- pred_act_ind$fit + 1.96 * pred_act_ind$se.fit
new_dat_act_ind$Before_After <- factor(new_dat_act_ind$Before_After, levels = c("Before", "MHW", "After"))
new_dat_act_ind$Serial_fish_id <- as.factor(new_dat_act_ind$Serial_fish_id)


# Plot Individuals
act_mod_ind_plot <- ggplot(new_dat_act_ind, aes(TimeOrdinal, y = predicted, color = Before_After)) +
  # Shading for night time (before sunrise) - with color = NA
  geom_rect(xmin = -Inf, xmax = 2, ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.3, color = NA) +
  # Shading for night time (after sunset, i.e., 18:00) - with color = NA
  geom_rect(xmin = 4, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.3, color = NA) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Before_After), alpha = 0.2, color = NA) +
  scale_x_continuous(limits = c(1, 5), breaks = seq(1, 5, 1), expand = c(0, 0)) + 
  scale_y_continuous(breaks = seq(0,4,1))+
  facet_wrap(~Serial_fish_id)+
  labs(
    x = "Time of Day (Oridnal scale)",
    y = "Activity [m s⁻²]",
    color = "MHW Stage",
    fill = "MHW Stage",
  ) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 18, color = "black"),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 14, color = "black"),
        legend.title = element_text(size = 16, color = "black"),
        legend.position = "top",
        panel.spacing.x = unit(1.2, "lines"),
        panel.spacing.y = unit(0.8, "lines")) +
  scale_color_manual(values = c("#2E8B57", "#DC143C", "#4169E1")) +
  scale_fill_manual(values = c("#2E8B57", "#DC143C", "#4169E1"))
act_mod_ind_plot

# Depth
dep_mod <- dep_models$full_mod
# Get predictions 
new_dat_dep <- expand.grid(
  TimeOrdinal = seq(1,5,length.out = 50),
  Before_After     = factor(c("Before","MHW","After"),
                            levels = levels(dep_mod$model$Before_After)),
  Serial_fish_id    = "1_1168792"  # placeholder; will be excluded
)

pred_dep <- predict(
  full_dep_mod,
  newdata = new_dat_dep,
  type = "response",
  se.fit = TRUE,
  exclude = "s(TimeOrdinal,Serial_biv_id)"  # removes individual deviations
)

pred_dep$fit <- exp(pred_dep$fit)-0.05
pred_dep$se.fit <- exp(pred_dep$se.fit)-0.05

new_dat_dep$predicted <- pred_dep$fit
new_dat_dep$conf.low <- pred_dep$fit - 1.96 * pred_dep$se.fit
new_dat_dep$conf.high <- pred_dep$fit + 1.96 * pred_dep$se.fit
new_dat_dep$Before_After <- factor(new_dat_dep$Before_After, levels = c("Before", "MHW", "After"))


# Plot
dep_mod_plot <- ggplot(new_dat_dep, aes(x = TimeOrdinal, y = predicted, color = Before_After)) +
  # Shading for night time (before 6 AM) - with color = NA
  geom_rect(xmin = -Inf, xmax = 2, ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.3, color = NA) +
  # Shading for night time (after 6 PM, i.e., 18:00) - with color = NA
  geom_rect(xmin = 4, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.3, color = NA) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Before_After), alpha = 0.2, color = NA) +
  scale_x_continuous(limits = c(1, 5), breaks = seq(1, 5, 1), expand = c(0, 0)) + scale_y_reverse()+
  labs(
    x = "Time of Day",
    y = "Depth [m]",
    color = "MHW Stage",
    fill = "MHW Stage",
  ) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 18, color = "black"),
        axis.title = element_text(size = 19.8),
        legend.text = element_text(size = 14, color = "black"),
        legend.title = element_text(size = 16, color = "black"),
        legend.position = "right")+ 
  scale_color_manual(values = c("#2E8B57", "#DC143C", "#4169E1")) +
  scale_fill_manual(values = c("#2E8B57", "#DC143C", "#4169E1"))
dep_mod_plot

# Get predictions 
new_dat_dep_ind <- expand.grid(
  TimeOrdinal = seq(1,5,length.out = 50),
  Before_After     = factor(c("Before","MHW","After"),
                            levels = levels(dep_mod$model$Before_After)),
  Serial_fish_id    = unique(dep_mod$model$Serial_fish_id)  # placeholder; will be excluded
)

pred_dep_ind <- predict(
  full_dep_mod,
  newdata = new_dat_dep_ind,
  type = "response",
  se.fit = TRUE
)
pred_dep_ind$fit <- exp(pred_dep_ind$fit)-0.05
pred_dep_ind$se.fit <- exp(pred_dep_ind$se.fit)-0.05

new_dat_dep_ind$predicted <- pred_dep_ind$fit
new_dat_dep_ind$conf.low <- pred_dep_ind$fit - 1.96 * pred_dep_ind$se.fit
new_dat_dep_ind$conf.high <- pred_dep_ind$fit + 1.96 * pred_dep_ind$se.fit
new_dat_dep_ind$Before_After <- factor(new_dat_dep_ind$Before_After, levels = c("Before", "MHW", "After"))
new_dat_dep_ind$Serial_fish_id <- as.factor(new_dat_dep_ind$Serial_fish_id)


# Plot Individuals
dep_mod_ind_plot <- ggplot(new_dat_dep_ind, aes(TimeOrdinal, y = predicted, color = Before_After)) +
  # Shading for night time (before 6 AM) - with color = NA
  geom_rect(xmin = -Inf, xmax = 2, ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.3, color = NA) +
  # Shading for night time (after 6 PM, i.e., 18:00) - with color = NA
  geom_rect(xmin = 4, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.3, color = NA) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Before_After), alpha = 0.2, color = NA) +
  scale_x_continuous(limits = c(1, 5), breaks = seq(1, 5, 1), expand = c(0, 0)) + 
  scale_y_reverse()+
  facet_wrap(~Serial_fish_id)+
  labs(
    x = "Time of Day ",
    y = "Depth [m]",
    color = "MHW Stage",
    fill = "MHW Stage",
  ) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 18, color = "black"),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 14, color = "black"),
        legend.title = element_text(size = 16, color = "black"),
        legend.position = "top",
        panel.spacing.x = unit(1.2, "lines"),
        panel.spacing.y = unit(0.8, "lines")) +
  scale_color_manual(values = c("#2E8B57", "#DC143C", "#4169E1")) +
  scale_fill_manual(values = c("#2E8B57", "#DC143C", "#4169E1"))
dep_mod_ind_plot


###### Reg plots ######
# Only MHW:Before
MHWs_comparison_lm <- MHWs_comparison %>% filter(Stage_period=="MHW : Before" )
# Activity
mhws_comparison_lm_long <- MHWs_comparison_lm %>%
  pivot_longer(
    cols = c(duration, intensity_mean, intensity_max, rate_onset, intensity_cumulative,intensity_max_abs),
    names_to = "predictor",
    values_to = "predictor_value"
  )
mhws_comparison_lm_long$predictor <- factor(
  mhws_comparison_lm_long$predictor,
  levels = c("intensity_max", "intensity_mean", "duration", "rate_onset", "intensity_cumulative","intensity_max_abs")
)

# 2️⃣ define custom x-axis titles
x_titles <- c(
  duration = "Duration [days]",
  intensity_mean = "Mean intensity [°C]",
  intensity_max = "Maximum intensity [°C]",
  rate_onset = "Onset rate [°C/day]",
  intensity_cumulative = "Cumulative intensity [°C·days]",
  intensity_max_abs = "Absolute maximum intensity [°C]"
)

# 4️⃣ Plot
reg_plot_act_ratio <- ggplot(mhws_comparison_lm_long, aes(x = predictor_value, y = ln_activity_ratio)) +
  stat_summary(fun = mean, geom = "point", size = 2, color = "black") +
  stat_summary(fun.data = mean_cl_boot, geom = "linerange", color = "black") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", linewidth = 1.2) +
  # R² and p-value in upper left
  stat_poly_eq(
    aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 6,
    color = "black", # We'll override per facet below
    data = mhws_comparison_lm_long
  ) +
  facet_wrap(~predictor, scales = "free_x",
             labeller = as_labeller(x_titles),nrow = 1) +
  labs(y = "ln (Activity ratio)", x = NULL) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        strip.background = element_rect(fill = "grey90", color = "black"),
        strip.text = element_text(size = 16, face = "bold") # facet titles bigger
  )
reg_plot_act_ratio

# Depth
reg_plot_dep <- ggplot(mhws_comparison_lm_long, aes(x = predictor_value, y = delta_depth)) +
  stat_summary(fun = mean, geom = "point", size = 2, color = "black") +
  stat_summary(fun.data = mean_cl_boot, geom = "linerange", color = "black") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", linewidth = 1.2) +
  # R² and p-value in upper left
  stat_poly_eq(
    aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 6,
    color = "black", # We'll override per facet below
    data = mhws_comparison_lm_long
  ) +
  facet_wrap(~predictor, scales = "free_x",
             labeller = as_labeller(x_titles),nrow = 1) +
  labs(y = expression(Delta~"Depth [m]"), x = NULL) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        strip.background = element_rect(fill = "grey90", color = "black"),
        strip.text = element_text(size = 16, face = "bold") # facet titles bigger
  )
reg_plot_dep

# Displacement
reg_plot_disp <- ggplot(mhws_comparison_lm_long, aes(x = predictor_value, y = log_max_displacement)) +
  stat_summary(fun = mean, geom = "point", size = 2, color = "black") +
  stat_summary(fun.data = mean_cl_boot, geom = "linerange", color = "black") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", linewidth = 1.2) +
  # R² and p-value in upper left
  stat_poly_eq(
    aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 6,
    color = "black", # We'll override per facet below
    data = mhws_comparison_lm_long
  ) +
  facet_wrap(~predictor, scales = "free_x",
             labeller = as_labeller(x_titles),nrow = 1) +
  labs(y = expression("ln(Displacement ratio)"), x = NULL) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        strip.background = element_rect(fill = "grey90", color = "black"),
        strip.text = element_text(size = 16, face = "bold") # facet titles bigger
  )
reg_plot_disp
