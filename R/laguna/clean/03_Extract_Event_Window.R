# R/laguna/clean/03_Extract_Event_Window.R
# Extract time window around events and process dolphin trajectories

library(tidyverse)
library(lubridate)

source("./R/laguna/clean/Animated_Dolphin_Polygon_Visualization.R")

# ==============================================================================
# Extract window around event and add metadata
# ==============================================================================

extract_event_window_with_metadata <- function(csv_filepath,
                                               event_frame,
                                               event_info,
                                               fps = 30,
                                               pre_event_sec = 180,
                                               post_event_sec = 10) {
  
  cat(sprintf("\n=== EXTRACTING EVENT WINDOW ===\n"))
  cat(sprintf("Event ID: %s\n", event_info$event_id))
  cat(sprintf("Event frame: %d\n", event_frame))
  cat(sprintf("Expected dolphins: %d\n", event_info$n_dolphins))
  cat(sprintf("Buzz occurred: %s\n", event_info$buzz_occurred))
  
  drone_data <- read_csv(csv_filepath, show_col_types = FALSE)
  
  # Define frame window
  frame_start <- event_frame - (pre_event_sec * fps)
  frame_end <- event_frame + (post_event_sec * fps)
  
  cat(sprintf("Window: frames %d to %d\n", frame_start, frame_end))
  
  # Extract and add context
  window_data <- drone_data %>%
    filter(FrameIndex >= frame_start, FrameIndex <= frame_end) %>%
    mutate(
      event_id = event_info$event_id,
      event_frame = event_frame,
      n_dolphins_expected = event_info$n_dolphins,
      buzz_occurred = event_info$buzz_occurred,
      treatment = event_info$treatment,
      time_to_event_sec = (FrameIndex - event_frame) / fps,
      time_to_event_frames = FrameIndex - event_frame,
      phase = case_when(
        time_to_event_sec < -60 ~ "background",
        time_to_event_sec < -30 ~ "early_approach",
        time_to_event_sec < -15 ~ "pre_cornering",
        time_to_event_sec < 0 ~ "cornering",
        time_to_event_sec <= 10 ~ "post_event",
        TRUE ~ "other"
      )
    )
  
  cat(sprintf("✓ Extracted %d detections\n", nrow(window_data)))
  cat(sprintf("  Unique ObjectIDs: %d\n", n_distinct(window_data$ObjectID)))
  
  # Summary by phase
  phase_summary <- window_data %>%
    group_by(phase) %>%
    summarize(
      n_detections = n(),
      n_dolphins = n_distinct(ObjectID),
      frame_range = sprintf("%d-%d", min(FrameIndex), max(FrameIndex)),
      .groups = "drop"
    )
  
  cat("\nDetections by phase:\n")
  print(phase_summary)
  
  return(window_data)
}


# ==============================================================================
# Identify which dolphins were involved in the event
# ==============================================================================

identify_event_dolphins <- function(data, 
                                    event_time_window = c(-60, 10),
                                    method = "closest_to_event") {
  
  n_dolphins_expected <- unique(data$n_dolphins_expected)[1]
  
  cat(sprintf("\n=== IDENTIFYING EVENT DOLPHINS ===\n"))
  cat(sprintf("Expected dolphins: %d\n", n_dolphins_expected))
  
  # Focus on time window around event
  event_window <- data %>%
    filter(time_to_event_sec >= event_time_window[1],
           time_to_event_sec <= event_time_window[2])
  
  cat(sprintf("Dolphins detected in event window (%.0f to %.0f sec): %d\n", 
              event_time_window[1], event_time_window[2],
              n_distinct(event_window$PersistentID)))
  
  # Score dolphins by activity around event
  dolphin_scores <- event_window %>%
    group_by(PersistentID) %>%
    summarize(
      n_detections = n(),
      mean_time_to_event = mean(abs(time_to_event_sec)),
      closest_time = min(abs(time_to_event_sec)),
      mean_y = mean(CenterY_m, na.rm = TRUE),
      # Cornering phase is most important
      n_cornering = sum(phase == "cornering", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      # Prioritize cornering activity, overall presence, and proximity to fishers
      score = (n_cornering * 2) + n_detections / (mean_time_to_event + 1) - mean_y * 0.1
    ) %>%
    arrange(desc(score))
  
  event_dolphins <- head(dolphin_scores, n_dolphins_expected)$PersistentID
  
  cat(sprintf("\nSelected %d dolphins:\n", n_dolphins_expected))
  print(dolphin_scores %>% head(n_dolphins_expected) %>% 
          select(PersistentID, n_detections, n_cornering, closest_time, score))
  
  cat("\nDolphins NOT selected:\n")
  print(dolphin_scores %>% tail(max(0, nrow(dolphin_scores) - n_dolphins_expected)) %>%
          head(5) %>%
          select(PersistentID, n_detections, n_cornering, closest_time, score))
  
  return(event_dolphins)
}


# ==============================================================================
# Process event: re-ID, interpolate, filter
# ==============================================================================

process_event_window <- function(window_data,
                                 max_gap_frames = 90,
                                 max_distance_m = 15,
                                 filter_to_event_dolphins = TRUE) {
  
  cat("\n=== PROCESSING EVENT ===\n")
  
  # Match dolphins across surfacing gaps
  cat("\nStep 1: Re-identifying dolphins...\n")
  df_matched <- match_dolphins_across_surfacings(
    window_data,
    frame_col = "FrameIndex",
    id_col = "ObjectID",
    x_col = "CenterX_m",
    y_col = "CenterY_m",
    max_gap_frames = max_gap_frames,
    max_distance_m = max_distance_m
  )
  
  # Fill in dive trajectories
  cat("\nStep 2: Interpolating trajectories...\n")
  df_interpolated <- interpolate_dive_trajectories(
    df_matched,
    frame_col = "FrameIndex",
    id_col = "PersistentID",
    x_col = "CenterX_m",
    y_col = "CenterY_m"
  )
  
  # Keep only dolphins involved in event
  if (filter_to_event_dolphins) {
    event_dolphins <- identify_event_dolphins(df_interpolated)
    
    cat(sprintf("\nStep 3: Filtering to %d event dolphins...\n", length(event_dolphins)))
    df_interpolated <- df_interpolated %>%
      filter(PersistentID %in% event_dolphins)
    
    cat(sprintf("  Kept %d positions\n", nrow(df_interpolated)))
  }
  
  # Summary
  cat("\n✓ Processing complete:\n")
  cat(sprintf("  Persistent dolphins: %d\n", n_distinct(df_interpolated$PersistentID)))
  cat(sprintf("  Total positions: %d\n", nrow(df_interpolated)))
  cat(sprintf("  Interpolated: %d (%.1f%%)\n",
              sum(df_interpolated$is_interpolated, na.rm = TRUE),
              100 * mean(df_interpolated$is_interpolated, na.rm = TRUE)))
  
  return(df_interpolated)
}


# ==============================================================================
# Run full pipeline
# ==============================================================================

# Event info
test_event_info <- tibble(
  event_id = "20240521_DJI0007_frame8298",
  date = ymd("2024-05-21"),
  n_dolphins = 4,
  buzz_occurred = FALSE,
  treatment = "cooperative"
)

# Extract 3-minute window before cue surfacing
window_data <- extract_event_window_with_metadata(
  csv_filepath = "./data/raw/laguna/20240521_DJI_0007_output_laguna.csv",
  event_frame = 8298,  # cue surfacing
  event_info = test_event_info,
  fps = 30,
  pre_event_sec = 180,
  post_event_sec = 10
)

# Process trajectories
event_processed <- process_event_window(
  window_data,
  filter_to_event_dolphins = TRUE
)

# Save processed data
write_csv(event_processed, "./data/processed/test_event_processed_filtered.csv")

cat("\n✓ Saved: ./data/processed/test_event_processed_filtered.csv\n")

# Calculate synchrony metrics
source("./R/laguna/clean/04_Calculate_Synchrony_Metrics.R")
synchrony_metrics <- calculate_event_synchrony_fixed(event_processed, bin_sec = 5)

write_csv(synchrony_metrics, "./data/processed/test_event_synchrony_corrected.csv")

cat("\n✓ Saved: ./data/processed/test_event_synchrony_corrected.csv\n")

# Preview results
cat("\n=== SYNCHRONY WITH CORRECT EVENT FRAME ===\n")
print(synchrony_metrics %>%
        filter(!is.na(phase)) %>%
        select(time_bin, phase, n_dolphins, mean_dist_to_fishers, 
               fisher_approach_rate, spatial_cohesion, cornering_active))
