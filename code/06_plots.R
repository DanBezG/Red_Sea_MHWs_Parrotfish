###################### Packages ######################

install.packages("pacman")
library(pacman)
p_load(tidyverse, lubridate, openxlsx, mgcv,ggplot2, patchwork)

###################### Code ##########################

## Load data
sum_heat_wave <- readRDS("results/Individual Heatwaves/OISST_Fix_MHW_Stage_summary.RDS")
sum_heat_wave$Before_After <- factor(sum_heat_wave$Before_After,levels = c("Before","MHW","After"))

MHWs_comparison <- readRDS("results/Individual Heatwaves/OISST_Fix_MHW_Stage_comparison.RDS")
act_models <- readRDS("results/Models/Activity_Models_OISST_Fix.RDS")
dep_models <- readRDS("results/Models/Depth_Models_OISST_Fix.RDS")

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
    size = 2,
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
    size = 2,
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
    size = 2,
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
