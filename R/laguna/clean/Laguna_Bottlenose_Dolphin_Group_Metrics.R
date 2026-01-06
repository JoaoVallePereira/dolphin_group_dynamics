# Laguna_Bottlenose_Dolphin_Group_Metrics.R
# Analysis of individual and group-level dolphin behavior during cooperative foraging with fishers
# Based on drone tracker output
# Author: Based on Cantor/Valle-Pereira framework, adapted for Laguna system
# Date: 2025-01-04

# OVERVIEW:
# This script processes drone tracking data of bottlenose dolphins foraging with fishers
# Key adaptations for Laguna system:
#  1. Fisher line constraint (x-axis = beach where fishers stand)
#  2. Angle correction for tracker errors (180° flips)
#  3. Cornering/herding behavior metrics (polygon area, convergence angles)
#  4. Individual-level role classification

# Required columns from dolphin-tracker CSV:
#  - FrameIndex, ObjectID, CenterX_m, CenterY_m, MovingAvgAngle_deg
# Optional: FilteredAngle_deg, Angle_deg

# ---------------------------
# SETUP
# ---------------------------

library(tidyverse)
library(zoo)        # for interpolation
library(igraph)     # for MST
library(sf)         # for polygon calculations
library(circular)   # for angular statistics
library(patchwork)  # for plot arrangements
library(viridis)    # for color scales

# ---------------------------
# HELPER FUNCTIONS
# ---------------------------

# 1. Angle Correction Function
# Problem: Tracker sometimes flips angle 180° (e.g., dolphin going right but angle points left)
# Solution: Detect sudden 180° flips and correct them
correct_angle_flips <- function(angles, threshold_deg = 120) {
  # angles: numeric vector of angles in degrees
  # threshold_deg: minimum change to consider a "flip" (default 120°)
  
  if (length(angles) < 2) return(angles)
  
  corrected <- angles
  for (i in 2:length(angles)) {
    if (is.na(angles[i]) | is.na(angles[i-1])) next
    
    # Calculate angular difference (accounting for wraparound)
    diff <- abs(angles[i] - angles[i-1])
    if (diff > 180) diff <- 360 - diff
    
    # If change > threshold, assume it's a flip and correct by adding/subtracting 180°
    if (diff > threshold_deg) {
      # Determine which direction to flip
      if (angles[i] < 0) {
        corrected[i] <- angles[i] + 180
      } else {
        corrected[i] <- angles[i] - 180
      }
      # Normalize to -180 to 180
      if (corrected[i] > 180) corrected[i] <- corrected[i] - 360
      if (corrected[i] < -180) corrected[i] <- corrected[i] + 360
    }
  }
  
  return(corrected)
}


# 2. Interpolate Tracks
# Fill short gaps in tracking data
interpolate_tracks <- function(df,
                               frame_col = "FrameIndex",
                               id_col = "ObjectID",
                               x_col = "CenterX_m",
                               y_col = "CenterY_m",
                               angle_col = "MovingAvgAngle_deg",
                               maxgap = 2) {  
  
  df <- df %>% arrange(.data[[id_col]], .data[[frame_col]])
  all_ids <- unique(df[[id_col]])
  
  out_list <- vector("list", length(all_ids))
  
  for (i in seq_along(all_ids)) {
    id <- all_ids[i]
    sub <- df %>% filter(.data[[id_col]] == id)
    
    # Create full frame sequence
    frmin <- min(sub[[frame_col]], na.rm = TRUE)
    frmax <- max(sub[[frame_col]], na.rm = TRUE)
    full_idx <- tibble(!!frame_col := seq(frmin, frmax))
    
    sub2 <- full_idx %>%
      left_join(sub, by = frame_col) %>%
      mutate(!!id_col := id)
    
    # Interpolate coordinates
    sub2[[x_col]] <- na.approx(sub2[[x_col]], maxgap = maxgap, na.rm = FALSE)
    sub2[[y_col]] <- na.approx(sub2[[y_col]], maxgap = maxgap, na.rm = FALSE)
    
    # Interpolate angle (circular - requires special handling)
    if (angle_col %in% names(sub2)) {
      # Convert to radians, split into x/y components, interpolate, convert back
      rad <- sub2[[angle_col]] * pi / 180
      x_comp <- cos(rad)
      y_comp <- sin(rad)
      
      x_comp_int <- na.approx(x_comp, maxgap = maxgap, na.rm = FALSE)
      y_comp_int <- na.approx(y_comp, maxgap = maxgap, na.rm = FALSE)
      
      sub2[[angle_col]] <- atan2(y_comp_int, x_comp_int) * 180 / pi
    }
    
    out_list[[i]] <- sub2
  }
  
  bind_rows(out_list)
}


# 3. Calculate Distance to Fisher Line
# Fisher line is along x-axis (y ≈ 0 or some fixed y-coordinate)
# This function assumes fisher line is at the minimum y-value in the data
calculate_distance_to_fisher_line <- function(df, 
                                              x_col = "CenterX_m", 
                                              y_col = "CenterY_m",
                                              fisher_line_y = NULL) {
  # If fisher_line_y not provided, use minimum y (assuming fishers at bottom/beach edge)
  if (is.null(fisher_line_y)) {
    fisher_line_y <- min(df[[y_col]], na.rm = TRUE)
  }
  
  df %>%
    mutate(Distance_to_fishers = abs(.data[[y_col]] - fisher_line_y))
}


# ---------------------------
# POLYGON & CORNERING METRICS
# ---------------------------

# 4. Calculate Convex Hull Area
# Measures spatial spread of group
compute_convex_hull_area <- function(points_df, x_col = "CenterX_m", y_col = "CenterY_m") {
  pts <- points_df %>% 
    select(all_of(c(x_col, y_col))) %>% 
    drop_na()
  
  if (nrow(pts) < 3) return(NA_real_)  # Need at least 3 points for polygon
  
  # Convert to sf points
  pts_sf <- st_as_sf(pts, coords = c(x_col, y_col))
  
  # Calculate convex hull
  hull <- st_convex_hull(st_union(pts_sf))
  
  # Return area
  return(as.numeric(st_area(hull)))
}


# 5. Calculate Polygon Elongation
# Ratio of hull perimeter to area - higher = more elongated (linear herding formation)
compute_polygon_elongation <- function(points_df, x_col = "CenterX_m", y_col = "CenterY_m") {
  pts <- points_df %>% 
    select(all_of(c(x_col, y_col))) %>% 
    drop_na()
  
  if (nrow(pts) < 3) return(NA_real_)
  
  pts_sf <- st_as_sf(pts, coords = c(x_col, y_col))
  hull <- st_convex_hull(st_union(pts_sf))
  
  area <- as.numeric(st_area(hull))
  perimeter <- as.numeric(st_length(st_cast(hull, "LINESTRING")))
  
  # Elongation index: perimeter^2 / area (higher = more elongated)
  if (area > 0) {
    return(perimeter^2 / area)
  } else {
    return(NA_real_)
  }
}


# 6. Calculate Convergence Angle
# Angle between dolphin trajectories and line toward fishers
# Lower angles = converging toward fishers (herding/cornering)
compute_convergence_angle <- function(points_df, 
                                     x_col = "CenterX_m", 
                                     y_col = "CenterY_m",
                                     angle_col = "MovingAvgAngle_deg",
                                     fisher_line_y = NULL) {
  
  pts <- points_df %>% 
    select(all_of(c(x_col, y_col, angle_col))) %>% 
    drop_na()
  
  if (nrow(pts) == 0) return(NA_real_)
  
  # Determine fisher line position
  if (is.null(fisher_line_y)) {
    fisher_line_y <- min(pts[[y_col]], na.rm = TRUE)
  }
  
  # For each dolphin, calculate angle toward fisher line
  # Angle toward fishers = straight down (toward smaller y) = -90° or 270°
  target_angle <- -90  # Pointing toward decreasing y
  
  # Calculate angular difference between heading and target
  angles <- pts[[angle_col]]
  
  diffs <- sapply(angles, function(a) {
    diff <- abs(a - target_angle)
    # Wraparound
    if (diff > 180) diff <- 360 - diff
    return(diff)
  })
  
  # Return mean convergence angle (lower = more aligned toward fishers)
  return(mean(diffs, na.rm = TRUE))
}


# 7. Calculate Approach Vector Variance
# How much do dolphins' approach angles differ from each other?
# High variance = approaching from different directions (McGarvey et al. finding)
compute_approach_vector_variance <- function(points_df,
                                            angle_col = "MovingAvgAngle_deg") {
  
  angles <- points_df[[angle_col]] %>% na.omit()
  if (length(angles) < 2) return(NA_real_)
  
  # Circular variance (1 - R, where R is mean resultant length)
  rad <- angles * pi / 180
  R <- sqrt((sum(cos(rad)))^2 + (sum(sin(rad)))^2) / length(rad)
  
  circular_var <- 1 - R
  return(circular_var)
}


# 8. Identify Flanking Positions
# Classify dolphins as "front", "left flank", "right flank", "rear" relative to centroid
compute_flanking_positions <- function(points_df,
                                      x_col = "CenterX_m",
                                      y_col = "CenterY_m",
                                      id_col = "ObjectID") {
  
  pts <- points_df %>% 
    select(all_of(c(id_col, x_col, y_col))) %>% 
    drop_na()
  
  if (nrow(pts) < 2) return(tibble())
  
  # Calculate centroid
  centroid_x <- mean(pts[[x_col]])
  centroid_y <- mean(pts[[y_col]])
  
  # For each dolphin, calculate position relative to centroid
  pts <- pts %>%
    mutate(
      relative_x = .data[[x_col]] - centroid_x,
      relative_y = .data[[y_col]] - centroid_y,
      distance_from_centroid = sqrt(relative_x^2 + relative_y^2),
      bearing_from_centroid = atan2(relative_y, relative_x) * 180 / pi
    ) %>%
    mutate(
      # Classify position
      # Front = toward fishers (lower y, so negative relative_y)
      # Rear = away from fishers (higher y, so positive relative_y)
      # Left/Right based on x
      position = case_when(
        relative_y < -distance_from_centroid * 0.5 ~ "front",
        relative_y > distance_from_centroid * 0.5 ~ "rear",
        relative_x < 0 ~ "left_flank",
        relative_x > 0 ~ "right_flank",
        TRUE ~ "center"
      )
    )
  
  return(pts)
}


# ---------------------------
# CORE METRICS 
# ---------------------------

# Mean Pairwise Distance
compute_mpd_frame <- function(points_df, x_col = "CenterX_m", y_col = "CenterY_m") {
  pts <- points_df %>% select(all_of(c(x_col, y_col))) %>% drop_na()
  n <- nrow(pts)
  if (n < 2) return(NA_real_)
  
  dvec <- as.vector(dist(as.matrix(pts)))
  return(mean(dvec))
}


# Minimum Spanning Tree mean edge
compute_mst_mean_frame <- function(points_df, x_col = "CenterX_m", y_col = "CenterY_m") {
  pts <- points_df %>% select(all_of(c(x_col, y_col))) %>% drop_na()
  n <- nrow(pts)
  if (n < 2) return(NA_real_)
  
  dmat <- as.matrix(dist(as.matrix(pts)))
  g <- graph_from_adjacency_matrix(dmat, mode = "undirected", weighted = TRUE, diag = FALSE)
  mst_g <- mst(g, weights = E(g)$weight)
  weights <- E(mst_g)$weight
  
  if (length(weights) == 0) return(0)
  return(mean(weights))
}


# Heading Similarity (MRL and pairwise)
compute_heading_similarity_frame <- function(points_df, angle_col = "MovingAvgAngle_deg") {
  if (!(angle_col %in% names(points_df))) return(list(MRL = NA_real_, PairwiseSim = NA_real_))
  
  angs <- points_df[[angle_col]] %>% na.omit() %>% as.numeric()
  n <- length(angs)
  if (n == 0) return(list(MRL = NA_real_, PairwiseSim = NA_real_))
  
  # Convert to radians
  rad <- angs * pi / 180
  
  # Mean resultant length
  R <- sqrt((sum(cos(rad)))^2 + (sum(sin(rad)))^2) / n
  
  # Pairwise similarity
  if (n >= 2) {
    radmat <- abs(outer(rad, rad, "-"))
    radmat <- pmin(radmat, 2*pi - radmat)
    iu <- which(upper.tri(radmat), arr.ind = TRUE)
    diffs <- radmat[iu]
    pair_mean_diff <- mean(diffs, na.rm = TRUE)
    pairwise_sim <- 1 - (pair_mean_diff / pi)
  } else {
    pairwise_sim <- NA_real_
  }
  
  return(list(MRL = R, PairwiseSim = pairwise_sim))
}


# ---------------------------
# MAIN PROCESSING FUNCTION
# ---------------------------

compute_laguna_dolphin_metrics <- function(df,
                                          frame_col = "FrameIndex",
                                          id_col = "ObjectID",
                                          x_col = "CenterX_m",
                                          y_col = "CenterY_m",
                                          angle_col = "MovingAvgAngle_deg",
                                          fisher_line_y = NULL,
                                          Nmin = 2,
                                          correct_angles = TRUE,
                                          interpolate = TRUE,
                                          maxgap = 2) {
  
  cat("Starting Laguna dolphin metrics calculation...\n")
  cat("Input data:", nrow(df), "rows\n")
  cat("Unique frames:", length(unique(df[[frame_col]])), "\n")
  cat("Unique individuals:", length(unique(df[[id_col]])), "\n\n")
  
  # Step 1: Angle correction (if requested)
  if (correct_angles && angle_col %in% names(df)) {
    cat("Correcting angle flips...\n")
    df <- df %>%
      group_by(.data[[id_col]]) %>%
      arrange(.data[[frame_col]]) %>%
      mutate(!!paste0(angle_col, "_original") := .data[[angle_col]],
             !!angle_col := correct_angle_flips(.data[[angle_col]])) %>%
      ungroup()
  }
  
  # Step 2: Interpolation (if requested)
  if (interpolate) {
    cat("Interpolating tracks (maxgap =", maxgap, "frames)...\n")
    df <- interpolate_tracks(df, frame_col, id_col, x_col, y_col, angle_col, maxgap)
  }
  
  # Step 3: Calculate distance to fishers
  cat("Calculating distance to fisher line...\n")
  df <- calculate_distance_to_fisher_line(df, x_col, y_col, fisher_line_y)
  
  # Step 4: Per-frame metrics
  cat("Computing per-frame group metrics...\n")
  
  frames <- sort(unique(df[[frame_col]]))
  metrics_list <- vector("list", length(frames))
  
  for (i in seq_along(frames)) {
    f <- frames[i]
    sub <- df %>% filter(.data[[frame_col]] == f)
    
    # Only consider detected dolphins
    pts <- sub %>% filter(!is.na(.data[[x_col]]) & !is.na(.data[[y_col]]))
    N <- nrow(pts)
    
    rec <- tibble(FrameIndex = f, N = N)
    
    if (N < Nmin) {
      # Not enough dolphins - fill with NA
      rec <- rec %>% mutate(
        MPD = NA_real_,
        MST_mean = NA_real_,
        Heading_MRL = NA_real_,
        Heading_pairSim = NA_real_,
        ConvexHull_area = NA_real_,
        Polygon_elongation = NA_real_,
        Convergence_angle = NA_real_,
        Approach_variance = NA_real_,
        Distance_to_fishers_mean = NA_real_,
        Distance_to_fishers_min = NA_real_
      )
    } else {
      # Compute metrics
      heading_res <- compute_heading_similarity_frame(pts, angle_col)
      
      rec <- rec %>% mutate(
        MPD = compute_mpd_frame(pts, x_col, y_col),
        MST_mean = compute_mst_mean_frame(pts, x_col, y_col),
        Heading_MRL = heading_res$MRL,
        Heading_pairSim = heading_res$PairwiseSim,
        ConvexHull_area = compute_convex_hull_area(pts, x_col, y_col),
        Polygon_elongation = compute_polygon_elongation(pts, x_col, y_col),
        Convergence_angle = compute_convergence_angle(pts, x_col, y_col, angle_col, fisher_line_y),
        Approach_variance = compute_approach_vector_variance(pts, angle_col),
        Distance_to_fishers_mean = mean(pts$Distance_to_fishers, na.rm = TRUE),
        Distance_to_fishers_min = min(pts$Distance_to_fishers, na.rm = TRUE)
      )
    }
    
    metrics_list[[i]] <- rec
    
    # Progress
    if (i %% 100 == 0) cat("  Processed", i, "/", length(frames), "frames\n")
  }
  
  frame_metrics <- bind_rows(metrics_list)
  
  cat("\nDone! Computed metrics for", nrow(frame_metrics), "frames\n")
  
  # Return both processed data and metrics
  return(list(
    data = df,
    frame_metrics = frame_metrics
  ))
}


# ---------------------------
# INDIVIDUAL-LEVEL EXTRACTION
# ---------------------------

extract_individual_metrics <- function(df,
                                      frame_col = "FrameIndex",
                                      id_col = "ObjectID",
                                      x_col = "CenterX_m",
                                      y_col = "CenterY_m",
                                      angle_col = "MovingAvgAngle_deg") {
  
  cat("Extracting individual-level metrics...\n")
  
  # Per individual, per frame
  indiv_metrics <- df %>%
    filter(!is.na(.data[[x_col]]) & !is.na(.data[[y_col]])) %>%
    group_by(.data[[frame_col]]) %>%
    mutate(
      # Group centroid
      centroid_x = mean(.data[[x_col]], na.rm = TRUE),
      centroid_y = mean(.data[[y_col]], na.rm = TRUE),
      
      # Individual's distance from centroid
      dist_from_centroid = sqrt((.data[[x_col]] - centroid_x)^2 + 
                                (.data[[y_col]] - centroid_y)^2),
      
      # Individual's bearing from centroid
      bearing_from_centroid = atan2(.data[[y_col]] - centroid_y, 
                                    .data[[x_col]] - centroid_x) * 180 / pi,
      
      # Relative position (front/rear/flank)
      relative_y = .data[[y_col]] - centroid_y,
      relative_x = .data[[x_col]] - centroid_x,
      
      position_category = case_when(
        relative_y < -dist_from_centroid * 0.3 ~ "front",
        relative_y > dist_from_centroid * 0.3 ~ "rear",
        relative_x < -dist_from_centroid * 0.3 ~ "left_flank",
        relative_x > dist_from_centroid * 0.3 ~ "right_flank",
        TRUE ~ "center"
      )
    ) %>%
    ungroup()
  
  return(indiv_metrics)
}


# ---------------------------
# VISUALIZATION FUNCTIONS
# ---------------------------

plot_group_metrics_timeseries <- function(frame_metrics) {
  
  # 1. Group size over time
  p1 <- ggplot(frame_metrics, aes(x = FrameIndex, y = N)) +
    geom_line(color = "steelblue", size = 0.5) +
    geom_point(size = 0.5, alpha = 0.5) +
    labs(title = "Group Size", y = "N dolphins", x = "Frame") +
    theme_minimal()
  
  # 2. Spatial metrics
  p2 <- frame_metrics %>%
    select(FrameIndex, MPD, MST_mean, Distance_to_fishers_mean) %>%
    pivot_longer(-FrameIndex, names_to = "Metric", values_to = "Value") %>%
    ggplot(aes(x = FrameIndex, y = Value, color = Metric)) +
    geom_line(alpha = 0.7) +
    labs(title = "Spatial Metrics", y = "Distance (m)", x = "Frame") +
    scale_color_viridis_d() +
    theme_minimal()
  
  # 3. Heading coordination
  p3 <- frame_metrics %>%
    select(FrameIndex, Heading_MRL, Heading_pairSim) %>%
    pivot_longer(-FrameIndex, names_to = "Metric", values_to = "Value") %>%
    ggplot(aes(x = FrameIndex, y = Value, color = Metric)) +
    geom_line(alpha = 0.7) +
    labs(title = "Heading Coordination", y = "Value (0-1)", x = "Frame") +
    scale_color_viridis_d() +
    theme_minimal()
  
  # 4. Polygon metrics
  p4 <- frame_metrics %>%
    select(FrameIndex, ConvexHull_area, Polygon_elongation) %>%
    pivot_longer(-FrameIndex, names_to = "Metric", values_to = "Value") %>%
    ggplot(aes(x = FrameIndex, y = Value, color = Metric)) +
    geom_line(alpha = 0.7) +
    labs(title = "Polygon Shape", y = "Value", x = "Frame") +
    scale_color_viridis_d() +
    theme_minimal()
  
  # 5. Convergence/herding metrics
  p5 <- frame_metrics %>%
    select(FrameIndex, Convergence_angle, Approach_variance) %>%
    pivot_longer(-FrameIndex, names_to = "Metric", values_to = "Value") %>%
    ggplot(aes(x = FrameIndex, y = Value, color = Metric)) +
    geom_line(alpha = 0.7) +
    labs(title = "Herding Behavior", y = "Angle/Variance", x = "Frame") +
    scale_color_viridis_d() +
    theme_minimal()
  
  # Combine
  combined <- (p1 + p2) / (p3 + p4) / p5
  combined <- combined + plot_annotation(
    title = "Laguna Dolphin Group Metrics - Time Series",
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )
  
  return(combined)
}


plot_spatial_trajectories <- function(df, 
                                     frame_col = "FrameIndex",
                                     id_col = "ObjectID",
                                     x_col = "CenterX_m",
                                     y_col = "CenterY_m",
                                     fisher_line_y = NULL) {
  
  # Determine fisher line
  if (is.null(fisher_line_y)) {
    fisher_line_y <- min(df[[y_col]], na.rm = TRUE)
  }
  
  # Plot trajectories
  p <- ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]], 
                      color = as.factor(.data[[id_col]]))) +
    geom_path(alpha = 0.6, size = 0.8) +
    geom_point(size = 0.5, alpha = 0.3) +
    geom_hline(yintercept = fisher_line_y, linetype = "dashed", 
               color = "red", size = 1) +
    annotate("text", x = mean(df[[x_col]], na.rm = TRUE), 
             y = fisher_line_y - 2, 
             label = "Fisher Line", color = "red", size = 4) +
    coord_fixed() +
    labs(title = "Dolphin Trajectories",
         subtitle = "Red dashed line = Fisher position",
         x = "X position (m)", y = "Y position (m)",
         color = "Dolphin ID") +
    scale_color_viridis_d() +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(p)
}


# ---------------------------
# EXAMPLE USAGE
# ---------------------------

# # Read data
# df <- read_csv("20240521_DJI_0007_output.csv")
# 
# # Process
# results <- compute_laguna_dolphin_metrics(
#   df,
#   correct_angles = TRUE,
#   interpolate = TRUE,
#   maxgap = 2,
#   Nmin = 2
# )
# 
# # Extract individual metrics
# individual_data <- extract_individual_metrics(results$data)
# 
# # Visualize
# p_timeseries <- plot_group_metrics_timeseries(results$frame_metrics)
# p_trajectories <- plot_spatial_trajectories(results$data)
# 
# # Save
# ggsave("group_metrics_timeseries.png", p_timeseries, width = 14, height = 12)
# ggsave("dolphin_trajectories.png", p_trajectories, width = 10, height = 8)
# 
# # Export metrics
# write_csv(results$frame_metrics, "frame_metrics_laguna.csv")
# write_csv(individual_data, "individual_metrics_laguna.csv")
