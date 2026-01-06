# Test_Laguna_Metrics.R
# Quick test of the Laguna dolphin metrics on example data

# Source the main functions
source("/mnt/user-data/outputs/Laguna_Bottlenose_Dolphin_Group_Metrics.R")

# Load data
df <- read_csv("/mnt/user-data/uploads/20240521_DJI_0007_output.csv")

cat("=== DATA OVERVIEW ===\n")
cat("Total rows:", nrow(df), "\n")
cat("Frames:", min(df$FrameIndex), "to", max(df$FrameIndex), "\n")
cat("Unique dolphins:", length(unique(df$ObjectID)), "\n")
cat("Dolphins IDs:", paste(unique(df$ObjectID), collapse = ", "), "\n\n")

# Check for angle column
if ("MovingAvgAngle_deg" %in% names(df)) {
  cat("Angle data available: MovingAvgAngle_deg\n")
  cat("Angle range:", min(df$MovingAvgAngle_deg, na.rm = TRUE), "to",
      max(df$MovingAvgAngle_deg, na.rm = TRUE), "degrees\n\n")
}

# PROCESS DATA
cat("=== PROCESSING ===\n")
results <- compute_laguna_dolphin_metrics(
  df,
  correct_angles = TRUE,      # Apply angle flip correction
  interpolate = TRUE,          # Fill short gaps
  maxgap = 2,                  # Max 2 frames to interpolate
  Nmin = 1                     # Allow single dolphins (change to 2 for groups only)
)

cat("\n=== FRAME METRICS SUMMARY ===\n")
print(summary(results$frame_metrics))

# Extract individual-level data
cat("\n=== EXTRACTING INDIVIDUAL METRICS ===\n")
individual_data <- extract_individual_metrics(results$data)

cat("Individual position categories:\n")
print(table(individual_data$position_category))

# VISUALIZATIONS
cat("\n=== CREATING VISUALIZATIONS ===\n")

# 1. Time series plots
p_timeseries <- plot_group_metrics_timeseries(results$frame_metrics)
ggsave("/mnt/user-data/outputs/group_metrics_timeseries.png", 
       p_timeseries, width = 14, height = 12, dpi = 300)
cat("Saved: group_metrics_timeseries.png\n")

# 2. Spatial trajectories
p_trajectories <- plot_spatial_trajectories(results$data)
ggsave("/mnt/user-data/outputs/dolphin_trajectories.png", 
       p_trajectories, width = 10, height = 8, dpi = 300)
cat("Saved: dolphin_trajectories.png\n")

# ADDITIONAL ANALYSES
cat("\n=== CORNERING/HERDING BEHAVIOR ANALYSIS ===\n")

# Identify potential cornering events
# Criteria: Low convergence angle + high elongation + decreasing distance to fishers
cornering_frames <- results$frame_metrics %>%
  filter(!is.na(Convergence_angle), !is.na(Polygon_elongation)) %>%
  mutate(
    is_cornering = Convergence_angle < 45 &  # Heading toward fishers
                   Polygon_elongation > median(Polygon_elongation, na.rm = TRUE) &  # Linear formation
                   Distance_to_fishers_min < 15  # Close to fishers
  )

n_cornering <- sum(cornering_frames$is_cornering, na.rm = TRUE)
cat("Potential cornering events:", n_cornering, "frames\n")
cat("Percentage of frames:", round(100 * n_cornering / nrow(cornering_frames), 1), "%\n")

# Plot cornering events
p_cornering <- ggplot(cornering_frames, aes(x = FrameIndex)) +
  geom_line(aes(y = Convergence_angle), color = "blue", alpha = 0.6) +
  geom_line(aes(y = Distance_to_fishers_min * 3), color = "red", alpha = 0.6) +
  geom_point(data = filter(cornering_frames, is_cornering),
             aes(y = Convergence_angle), color = "orange", size = 2) +
  scale_y_continuous(
    name = "Convergence Angle (degrees)",
    sec.axis = sec_axis(~./3, name = "Distance to Fishers (m)")
  ) +
  labs(title = "Cornering Behavior Detection",
       subtitle = "Orange points = potential cornering (low angle + elongated + close to fishers)",
       x = "Frame") +
  theme_minimal() +
  theme(
    axis.title.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red")
  )

ggsave("/mnt/user-data/outputs/cornering_detection.png", 
       p_cornering, width = 12, height = 6, dpi = 300)
cat("Saved: cornering_detection.png\n")

# EXPORT DATA
cat("\n=== EXPORTING DATA ===\n")
write_csv(results$frame_metrics, "/mnt/user-data/outputs/frame_metrics_laguna.csv")
write_csv(individual_data, "/mnt/user-data/outputs/individual_metrics_laguna.csv")
write_csv(cornering_frames, "/mnt/user-data/outputs/cornering_analysis.csv")

cat("Exported CSV files:\n")
cat("  - frame_metrics_laguna.csv\n")
cat("  - individual_metrics_laguna.csv\n")
cat("  - cornering_analysis.csv\n")

cat("\n=== KEY INSIGHTS ===\n")
cat("Mean group size:", round(mean(results$frame_metrics$N, na.rm = TRUE), 2), "dolphins\n")
cat("Mean distance to fishers:", round(mean(results$frame_metrics$Distance_to_fishers_mean, na.rm = TRUE), 2), "m\n")
cat("Closest approach:", round(min(results$frame_metrics$Distance_to_fishers_min, na.rm = TRUE), 2), "m\n")
cat("Mean heading coordination (MRL):", round(mean(results$frame_metrics$Heading_MRL, na.rm = TRUE), 3), "\n")
cat("Mean approach variance:", round(mean(results$frame_metrics$Approach_variance, na.rm = TRUE), 3), "\n")

cat("\n=== DONE ===\n")
