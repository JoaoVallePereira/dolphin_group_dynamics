# R/laguna/clean/04_Calculate_Synchrony_Metrics_FIXED.R
# Calculate synchrony and cornering metrics with correct fisher line

library(tidyverse)
library(circular)
library(sf)

# ==============================================================================
# Calculate cornering polygon with fisher line at frame edges
# ==============================================================================

calculate_cornering_polygon_metrics <- function(data, 
                                                fisher_line_y = 0,
                                                fisher_line_x_min = 0,
                                                fisher_line_x_max = NULL,
                                                bin_sec = 5) {
  
  # Default to full frame width
  if (is.null(fisher_line_x_max)) {
    fisher_line_x_max <- max(data$CenterX_m, na.rm = TRUE)
  }
  
  cat(sprintf("\nCalculating cornering polygon metrics:\n"))
  cat(sprintf("  Fisher line: from (%.1f, %.1f) to (%.1f, %.1f)\n", 
              fisher_line_x_min, fisher_line_y, 
              fisher_line_x_max, fisher_line_y))
  
  if (!"time_bin" %in% names(data)) {
    data <- data %>%
      mutate(time_bin = floor(time_to_event_sec / bin_sec) * bin_sec)
  }
  
  # Fisher line endpoints stay fixed
  fisher_points <- tibble(
    CenterX_m = c(fisher_line_x_min, fisher_line_x_max),
    CenterY_m = c(fisher_line_y, fisher_line_y)
  )
  
  # Process each time bin
  cornering_list <- list()
  
  for (tb in unique(data$time_bin)) {
    # Dolphin positions in this bin
    dolphin_points <- data %>%
      filter(time_bin == tb) %>%
      select(CenterX_m, CenterY_m)
    
    if (nrow(dolphin_points) >= 2) {
      # Combine dolphins with fixed fisher line endpoints
      all_points <- bind_rows(dolphin_points, fisher_points)
      
      # Measure dolphin spread along X axis (for fisher contact)
      dolphin_x_min <- min(dolphin_points$CenterX_m)
      dolphin_x_max <- max(dolphin_points$CenterX_m)
      
      # Build convex hull
      tryCatch({
        pts_sf <- st_as_sf(all_points, coords = c("CenterX_m", "CenterY_m"))
        hull <- st_convex_hull(st_union(pts_sf))
        
        cornering_list[[length(cornering_list) + 1]] <- tibble(
          time_bin = tb,
          area = as.numeric(st_area(hull)),
          perimeter = as.numeric(st_length(st_cast(hull, "LINESTRING"))),
          # How much of fisher line is covered by dolphins
          fisher_contact_length = dolphin_x_max - dolphin_x_min,
          # Proportion of full line covered
          fisher_contact_proportion = (dolphin_x_max - dolphin_x_min) / 
            (fisher_line_x_max - fisher_line_x_min)
        )
      }, error = function(e) {
        cornering_list[[length(cornering_list) + 1]] <- tibble(
          time_bin = tb,
          area = NA_real_,
          perimeter = NA_real_,
          fisher_contact_length = dolphin_x_max - dolphin_x_min,
          fisher_contact_proportion = (dolphin_x_max - dolphin_x_min) / 
            (fisher_line_x_max - fisher_line_x_min)
        )
      })
    }
  }
  
  cornering_metrics <- bind_rows(cornering_list)
  
  # Add phase labels
  phase_info <- data %>%
    distinct(time_bin, phase)
  
  cornering_metrics <- cornering_metrics %>%
    left_join(phase_info, by = "time_bin") %>%
    mutate(
      cornering_polygon_area = area,
      cornering_polygon_perimeter = perimeter,
      cornering_polygon_elongation = perimeter^2 / (4 * pi * area)
    ) %>%
    select(-area, -perimeter)
  
  # Rates of change
  cornering_metrics <- cornering_metrics %>%
    arrange(time_bin) %>%
    mutate(
      cornering_polygon_shrink_rate = -(lead(cornering_polygon_area) - cornering_polygon_area) / bin_sec,
      fisher_contact_growth_rate = (lead(fisher_contact_length) - fisher_contact_length) / bin_sec,
      cornering_polygon_active = !is.na(cornering_polygon_shrink_rate) & 
        cornering_polygon_shrink_rate > 0
    )
  
  cat(sprintf("  Calculated cornering polygons for %d bins\n", nrow(cornering_metrics)))
  
  return(cornering_metrics)
}


# ==============================================================================
# Main synchrony calculation
# ==============================================================================

calculate_event_synchrony_fixed <- function(data, bin_sec = 5, fps = 30) {
  
  cat("\n=== CALCULATING SYNCHRONY METRICS ===\n\n")
  
  # Fisher line spans full frame width at Y=0
  fisher_line_y <- 0
  fisher_line_x_min <- 0
  fisher_line_x_max <- max(data$CenterX_m, na.rm = TRUE)
  
  cat(sprintf("Fisher line: Y=%.1f, X from %.1f to %.1f m (%.1f m total)\n", 
              fisher_line_y, fisher_line_x_min, fisher_line_x_max, fisher_line_x_max))
  cat(sprintf("Dolphin range: X[%.1f-%.1f], Y[%.1f-%.1f]\n",
              min(data$CenterX_m), max(data$CenterX_m),
              min(data$CenterY_m), max(data$CenterY_m)))
  
  # Bin data by time
  data <- data %>%
    mutate(time_bin = floor(time_to_event_sec / bin_sec) * bin_sec)
  
  # Basic synchrony metrics
  synchrony <- data %>%
    group_by(time_bin, phase) %>%
    summarize(
      n_dolphins = n_distinct(PersistentID),
      n_detections = n(),
      n_surfaced = sum(!is_interpolated, na.rm = TRUE),
      n_interpolated = sum(is_interpolated, na.rm = TRUE),
      prop_surfaced = n_surfaced / n(),
      center_x = mean(CenterX_m, na.rm = TRUE),
      center_y = mean(CenterY_m, na.rm = TRUE),
      spread_x = sd(CenterX_m, na.rm = TRUE),
      spread_y = sd(CenterY_m, na.rm = TRUE),
      mean_pairwise_dist = ifelse(n() > 1, 
                                  mean(dist(cbind(CenterX_m, CenterY_m))),
                                  NA_real_),
      spatial_cohesion = 1 / (mean_pairwise_dist + 1),
      n_headings = sum(!is.na(MovingAvgAngle_deg)),
      mean_dist_to_fishers = mean(CenterY_m, na.rm = TRUE),
      min_dist_to_fishers = min(CenterY_m, na.rm = TRUE),
      max_dist_to_fishers = max(CenterY_m, na.rm = TRUE),
      x_range = max(CenterX_m, na.rm = TRUE) - min(CenterX_m, na.rm = TRUE),
      y_range = max(CenterY_m, na.rm = TRUE) - min(CenterY_m, na.rm = TRUE),
      area_proxy = x_range * y_range,
      .groups = "drop"
    )
  
  # Heading synchrony
  synchrony <- synchrony %>%
    rowwise() %>%
    mutate(
      bin_headings = list(
        data %>%
          filter(time_bin == .data$time_bin, !is.na(MovingAvgAngle_deg)) %>%
          pull(MovingAvgAngle_deg)
      )
    ) %>%
    mutate(
      heading_MRL = ifelse(length(bin_headings[[1]]) >= 2,
                           rho.circular(circular(bin_headings[[1]] * pi / 180)),
                           NA_real_),
      heading_variance = ifelse(length(bin_headings[[1]]) >= 2,
                                var.circular(circular(bin_headings[[1]] * pi / 180)),
                                NA_real_),
      mean_heading = ifelse(length(bin_headings[[1]]) >= 1,
                            mean.circular(circular(bin_headings[[1]] * pi / 180)) * 180 / pi,
                            NA_real_)
    ) %>%
    select(-bin_headings) %>%
    ungroup()
  
  # Add cornering polygon metrics
  cornering_metrics <- calculate_cornering_polygon_metrics(
    data, 
    fisher_line_y = fisher_line_y,
    fisher_line_x_min = fisher_line_x_min,
    fisher_line_x_max = fisher_line_x_max,
    bin_sec = bin_sec
  )
  
  synchrony <- synchrony %>%
    left_join(cornering_metrics, by = c("time_bin", "phase"))
  
  # Rates of change
  synchrony <- synchrony %>%
    arrange(time_bin) %>%
    mutate(
      area_shrink_rate = -(lead(area_proxy) - area_proxy) / bin_sec,
      fisher_approach_rate = -(lead(mean_dist_to_fishers) - mean_dist_to_fishers) / bin_sec,
      cornering_active = !is.na(area_shrink_rate) & !is.na(fisher_approach_rate) &
        (area_shrink_rate > 0) & (fisher_approach_rate > 0)
    )
  
  cat(sprintf("\n✓ Calculated synchrony for %d time bins\n", nrow(synchrony)))
  
  # Quick summaries
  cat("\n=== DISTANCE TO FISHERS ===\n")
  print(synchrony %>% filter(!is.na(phase)) %>%
          group_by(phase) %>%
          summarize(mean_dist = mean(mean_dist_to_fishers, na.rm = TRUE),
                    min_dist = min(min_dist_to_fishers, na.rm = TRUE),
                    .groups = "drop"))
  
  cat("\n=== CORNERING POLYGON ===\n")
  print(synchrony %>% filter(!is.na(phase)) %>%
          group_by(phase) %>%
          summarize(mean_polygon_area = mean(cornering_polygon_area, na.rm = TRUE),
                    mean_contact_prop = mean(fisher_contact_proportion, na.rm = TRUE),
                    n_active = sum(cornering_polygon_active, na.rm = TRUE),
                    .groups = "drop"))
  
  return(synchrony)
}

# Run calculation
event_data <- read_csv("./data/processed/test_event_processed_filtered.csv", show_col_types = FALSE)
synchrony_metrics <- calculate_event_synchrony_fixed(event_data, bin_sec = 5)
write_csv(synchrony_metrics, "./data/processed/test_event_synchrony_corrected.csv")

cat("\n✓ Saved: ./data/processed/test_event_synchrony_corrected.csv\n")
