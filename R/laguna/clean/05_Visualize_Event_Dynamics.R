# R/laguna/clean/05_Visualize_Event_Dynamics.R
# Visualize event dynamics with fisher line at Y=0

library(tidyverse)
library(patchwork)
library(viridis)

# Load processed data
event_data <- read_csv("./data/processed/test_event_processed_filtered.csv", show_col_types = FALSE)
synchrony <- read_csv("./data/processed/test_event_synchrony_corrected.csv", show_col_types = FALSE)

FISHER_LINE_Y <- 0

# Join synchrony metrics to position data
event_with_sync <- event_data %>%
  mutate(time_bin = floor(time_to_event_sec / 5) * 5) %>%
  left_join(
    synchrony %>% select(time_bin, spatial_cohesion, mean_dist_to_fishers),
    by = "time_bin"
  )

# ==============================================================================
# Plot 1: Trajectories colored by distance to fishers
# ==============================================================================

p1 <- ggplot(event_with_sync, aes(x = CenterX_m, y = CenterY_m)) +
  # Fisher line and zone
  geom_hline(yintercept = FISHER_LINE_Y, color = "red", 
             linetype = "solid", linewidth = 2, alpha = 0.8) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = FISHER_LINE_Y + 0.5,
           fill = "red", alpha = 0.1) +
  annotate("text", x = mean(event_data$CenterX_m), y = FISHER_LINE_Y + 2,
           label = "← FISHERS (Y=0)", color = "red", fontface = "bold", size = 6) +
  # Dolphin paths
  geom_path(aes(group = PersistentID, color = CenterY_m), 
            linewidth = 1.5, alpha = 0.8) +
  # Positions
  geom_point(aes(color = CenterY_m, shape = is_interpolated), 
             size = 3, alpha = 0.9) +
  # Style
  scale_color_viridis_c(option = "plasma", 
                        name = "Distance to\nFishers (m)",
                        limits = c(0, max(event_data$CenterY_m))) +
  scale_shape_manual(values = c(16, 1), 
                     labels = c("Surfaced", "Interpolated")) +
  coord_fixed() +
  labs(
    title = "Dolphin Trajectories: Distance from Fishers",
    subtitle = "Purple = Close to fishers | Yellow = Far from fishers | Red zone = Fisher area",
    x = "X Position (m)",
    y = "Y Position (m)",
    shape = "Detection"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggsave("./plots/01_trajectories_distance_to_fishers.png", p1, 
       width = 14, height = 10, dpi = 300)

# ==============================================================================
# Plot 2: Synchrony metrics over time
# ==============================================================================

sync_plot_data <- synchrony %>%
  filter(!is.na(phase)) %>%
  select(time_bin, phase, spatial_cohesion, mean_dist_to_fishers, 
         fisher_approach_rate, area_proxy) %>%
  pivot_longer(cols = c(spatial_cohesion, mean_dist_to_fishers, 
                        fisher_approach_rate, area_proxy),
               names_to = "metric", 
               values_to = "value") %>%
  mutate(
    metric_label = case_when(
      metric == "spatial_cohesion" ~ "Spatial Cohesion\n(1 = tight, 0 = spread)",
      metric == "mean_dist_to_fishers" ~ "Distance to Fishers (m)\n(lower = closer)",
      metric == "fisher_approach_rate" ~ "Approach Rate (m/s)\n(+ = approaching, - = leaving)",
      metric == "area_proxy" ~ "Group Spread (m²)\n(lower = compressed)"
    )
  )

p2 <- ggplot(sync_plot_data, aes(x = time_bin, y = value, color = phase)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1.5) +
  annotate("text", x = 0, y = Inf, label = "EVENT", 
           vjust = 1.5, hjust = -0.1, color = "red", fontface = "bold", size = 5) +
  facet_wrap(~metric_label, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c(
    "early_approach" = "#3B9AB2",
    "pre_cornering" = "#F21A00",
    "cornering" = "#E1AF00"
  )) +
  labs(
    title = "Dolphin Group Dynamics During Event",
    subtitle = "4 dolphins over 180 seconds before cue",
    x = "Time to Event (seconds)",
    y = "Value",
    color = "Phase"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 11, face = "bold"),
    panel.spacing = unit(1.5, "lines")
  )

ggsave("./plots/02_synchrony_dynamics.png", p2, 
       width = 12, height = 14, dpi = 300)

# ==============================================================================
# Plot 3: Distance to fishers over time (by dolphin)
# ==============================================================================

p3 <- ggplot(event_with_sync, aes(x = time_to_event_sec, y = CenterY_m)) +
  # Fisher line
  geom_hline(yintercept = FISHER_LINE_Y, color = "red", 
             linetype = "solid", linewidth = 2) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 5,
           fill = "red", alpha = 0.1) +
  # Individual tracks
  geom_line(aes(color = as.factor(PersistentID), group = PersistentID),
            linewidth = 1.2, alpha = 0.7) +
  geom_point(aes(color = as.factor(PersistentID), shape = is_interpolated),
             size = 2) +
  # Event marker
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1.5) +
  # Style
  scale_color_viridis_d(option = "plasma") +
  scale_shape_manual(values = c(16, 1), 
                     labels = c("Surfaced", "Interpolated")) +
  labs(
    title = "Individual Dolphin Approach to Fishers",
    subtitle = "Each line is one dolphin",
    x = "Time to Event (seconds)",
    y = "Distance to Fishers (m)",
    color = "Dolphin ID",
    shape = "Detection"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggsave("./plots/03_individual_fisher_approach.png", p3, 
       width = 14, height = 8, dpi = 300)

cat("\n✓ Saved 3 plots to ./plots/\n")
