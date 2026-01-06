# Enhanced_Dolphin_Polygon_Animation.R
# Creates polygons using BOTH surfaced AND interpolated positions
# Shows "imagined" dolphin formation even when not all surfaced simultaneously

library(tidyverse)
library(sf)
library(magick)
library(viridis)

# Enhanced polygon creation using ALL positions (real + interpolated)
create_polygon_with_interpolation <- function(df,
                                             frame_col = "FrameIndex",
                                             id_col = "PersistentID",
                                             x_col = "CenterX_m",
                                             y_col = "CenterY_m",
                                             min_dolphins = 2) {
  
  cat("Creating polygons using real + interpolated positions...\n")
  
  frames <- sort(unique(df[[frame_col]]))
  polygon_list <- vector("list", length(frames))
  n_created <- 0
  
  for (i in seq_along(frames)) {
    f <- frames[i]
    
    # Get ALL dolphins at this frame (real + interpolated)
    frame_data <- df %>% 
      filter(.data[[frame_col]] == f) %>%
      filter(!is.na(.data[[x_col]]) & !is.na(.data[[y_col]]))
    
    # Count how many are real vs interpolated
    n_real <- sum(!frame_data$is_interpolated, na.rm = TRUE)
    n_interp <- sum(frame_data$is_interpolated, na.rm = TRUE)
    
    if (nrow(frame_data) >= min_dolphins) {
      # Create convex hull from ALL positions
      pts <- frame_data %>% select(all_of(c(x_col, y_col)))
      pts_sf <- st_as_sf(pts, coords = c(x_col, y_col))
      hull <- st_convex_hull(st_union(pts_sf))
      
      # Convert to coordinates
      hull_coords <- st_coordinates(hull)[, 1:2] %>% 
        as.data.frame() %>%
        rename(x = X, y = Y)
      
      polygon_list[[i]] <- hull_coords %>%
        mutate(
          !!sym(frame_col) := f,
          n_dolphins = nrow(frame_data),
          n_real = n_real,
          n_interpolated = n_interp,
          polygon_type = ifelse(n_real >= min_dolphins, "real", "predicted")
        )
      
      n_created <- n_created + 1
    }
    
    if (i %% 200 == 0) cat(sprintf("  Processed %d/%d frames (%d polygons)\n", 
                                    i, length(frames), n_created))
  }
  
  polygon_data <- bind_rows(polygon_list)
  
  if (nrow(polygon_data) > 0) {
    summary_data <- polygon_data %>%
      distinct(.data[[frame_col]], .keep_all = TRUE)
    
    cat(sprintf("\nCreated polygons for %d frames:\n", nrow(summary_data)))
    cat(sprintf("  - Based on real surfacings: %d\n", 
                sum(summary_data$polygon_type == "real")))
    cat(sprintf("  - Based on predictions: %d\n", 
                sum(summary_data$polygon_type == "predicted")))
  } else {
    cat("\nNo polygons created.\n")
  }
  
  return(polygon_data)
}


# Enhanced animation with clear distinction between real and predicted
create_enhanced_dolphin_animation <- function(df_interpolated,
                                             polygon_data,
                                             frame_col = "FrameIndex",
                                             id_col = "PersistentID",
                                             x_col = "CenterX_m",
                                             y_col = "CenterY_m",
                                             output_file = "dolphin_polygon_enhanced.gif",
                                             n_frames = 50,
                                             pause = 0.2) {
  
  cat("Creating enhanced animation with real + predicted positions...\n")
  
  # Select frames
  all_frames <- sort(unique(df_interpolated[[frame_col]]))
  
  if (length(all_frames) > n_frames) {
    indices <- round(seq(1, length(all_frames), length.out = n_frames))
    key_frames <- all_frames[indices]
  } else {
    key_frames <- all_frames
  }
  
  # Temp directory
  temp_dir <- tempdir()
  frame_files <- character(length(key_frames))
  
  # Fisher line
  fisher_line_y <- min(df_interpolated[[y_col]], na.rm = TRUE)
  
  # Create frames
  for (i in seq_along(key_frames)) {
    f <- key_frames[i]
    
    # Historical trails (only real detections)
    df_history_real <- df_interpolated %>% 
      filter(.data[[frame_col]] <= f, !is_interpolated | is.na(is_interpolated)) %>%
      group_by(.data[[id_col]]) %>%
      filter(n() >= 2) %>%
      ungroup()
    
    # Current frame - separate real and interpolated
    df_current <- df_interpolated %>% filter(.data[[frame_col]] == f)
    df_real <- df_current %>% filter(!is_interpolated | is.na(is_interpolated))
    df_predicted <- df_current %>% filter(is_interpolated)
    
    # Polygon for this frame
    polygon_current <- polygon_data %>% 
      filter(.data[[frame_col]] == f)
    
    # Check polygon type
    if (nrow(polygon_current) > 0) {
      poly_type <- polygon_current$polygon_type[1]
      poly_color <- ifelse(poly_type == "real", "blue", "orange")
      poly_alpha <- ifelse(poly_type == "real", 0.3, 0.2)
      poly_linetype <- ifelse(poly_type == "real", "solid", "dashed")
    }
    
    # Create plot
    p <- ggplot() +
      # Historical trails (faint)
      {if(nrow(df_history_real) > 0) {
        geom_path(data = df_history_real,
                  aes(x = .data[[x_col]], y = .data[[y_col]], 
                      group = .data[[id_col]]),
                  color = "gray80", alpha = 0.4, linewidth = 0.3)
      }} +
      
      # Polygon (if exists)
      {if(nrow(polygon_current) > 0) {
        list(
          geom_polygon(data = polygon_current,
                       aes(x = x, y = y),
                       fill = poly_color, alpha = poly_alpha, 
                       color = poly_color, linewidth = 1.5, linetype = poly_linetype),
          # Add label
          annotate("text", x = min(polygon_current$x), 
                   y = max(polygon_current$y) + 2,
                   label = ifelse(poly_type == "real", 
                                 "REAL FORMATION", 
                                 "PREDICTED FORMATION"),
                   color = poly_color, size = 3.5, fontface = "bold", hjust = 0)
        )
      }} +
      
      # Predicted positions (hollow circles, dashed connections)
      {if(nrow(df_predicted) > 0) {
        list(
          geom_point(data = df_predicted,
                     aes(x = .data[[x_col]], y = .data[[y_col]], 
                         color = as.factor(.data[[id_col]])),
                     size = 4, shape = 21, stroke = 1.5, fill = NA),
          # Connect predicted positions with dashed lines
          {if(nrow(df_predicted) >= 2) {
            # Get convex hull order for connecting
            pts_pred <- df_predicted %>% select(all_of(c(x_col, y_col, id_col)))
            pts_sf <- st_as_sf(pts_pred, coords = c(x_col, y_col))
            hull <- st_convex_hull(st_union(pts_sf))
            hull_coords <- st_coordinates(hull)[, 1:2] %>% as.data.frame()
            
            geom_path(data = hull_coords,
                      aes(x = X, y = Y),
                      color = "orange", linewidth = 1, linetype = "dashed", alpha = 0.6)
          }}
        )
      }} +
      
      # Real surfaced positions (solid circles)
      {if(nrow(df_real) > 0) {
        geom_point(data = df_real,
                   aes(x = .data[[x_col]], y = .data[[y_col]], 
                       color = as.factor(.data[[id_col]])),
                   size = 5, shape = 16)
      }} +
      
      # Fisher line
      geom_hline(yintercept = fisher_line_y,
                 linetype = "dashed", color = "red", linewidth = 1.5, alpha = 0.7) +
      annotate("text", x = mean(df_interpolated[[x_col]], na.rm = TRUE),
               y = fisher_line_y - 1.5,
               label = "FISHERS", color = "red", size = 4, fontface = "bold") +
      
      # Styling
      coord_fixed() +
      scale_color_viridis_d() +
      labs(
        title = sprintf("Dolphin Group Formation - Frame %d (%d/%d)", 
                       f, i, length(key_frames)),
        subtitle = sprintf("Solid circles = surfaced | Hollow circles = predicted underwater | %d dolphins total",
                          nrow(df_current)),
        x = "X position (m)", 
        y = "Y position (m)",
        color = "Dolphin ID"
      ) +
      theme_minimal() +
      theme(
        legend.position = "right",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10),
        panel.grid.minor = element_blank()
      )
    
    # Save
    frame_files[i] <- file.path(temp_dir, sprintf("frame_%04d.png", i))
    ggsave(frame_files[i], p, width = 12, height = 8, dpi = 100)
    
    if (i %% 10 == 0) cat(sprintf("  Created frame %d/%d\n", i, length(key_frames)))
  }
  
  # Combine
  cat("Combining into GIF...\n")
  frames <- image_read(frame_files)
  
  # Calculate fps (must be factor of 100)
  target_fps <- round(1 / pause)
  valid_fps <- c(1, 2, 4, 5, 10, 20, 25, 50, 100)
  fps <- valid_fps[which.min(abs(valid_fps - target_fps))]
  
  animation <- image_animate(frames, fps = fps)
  image_write(animation, output_file)
  
  # Cleanup
  file.remove(frame_files)
  
  cat(sprintf("✓ Saved animation to: %s (%d fps)\n", output_file, fps))
  cat(sprintf("  Total frames: %d | Duration: %.1f seconds\n", 
              length(key_frames), length(key_frames) / fps))
  
  return(animation)
}


# COMPLETE WORKFLOW
create_enhanced_polygon_gif <- function(df,
                                       frame_col = "FrameIndex",
                                       id_col = "ObjectID",
                                       x_col = "CenterX_m",
                                       y_col = "CenterY_m",
                                       output_file = "dolphin_polygon_enhanced.gif",
                                       max_gap_frames = 120,
                                       max_distance_m = 15,
                                       min_dolphins = 2,
                                       n_frames = 40,
                                       pause = 0.25) {
  
  cat("\n=== ENHANCED DOLPHIN POLYGON ANIMATION ===\n\n")
  
  # Step 1: Match dolphins
  cat("Step 1: Matching dolphins across surfacings...\n")
  df_matched <- match_dolphins_across_surfacings(
    df, frame_col, id_col, x_col, y_col, max_gap_frames, max_distance_m
  )
  
  # Step 2: Interpolate
  cat("\nStep 2: Interpolating underwater trajectories...\n")
  df_interpolated <- interpolate_dive_trajectories(
    df_matched, frame_col, "PersistentID", x_col, y_col
  )
  
  # Step 3: Create polygons using ALL positions
  cat("\nStep 3: Creating polygons (real + predicted)...\n")
  polygon_data <- create_polygon_with_interpolation(
    df_interpolated, frame_col, "PersistentID", x_col, y_col, min_dolphins
  )
  
  # Step 4: Animate
  cat("\nStep 4: Creating animation...\n")
  anim <- create_enhanced_dolphin_animation(
    df_interpolated, polygon_data, frame_col, "PersistentID", x_col, y_col,
    output_file, n_frames, pause
  )
  
  cat("\n=== COMPLETE ===\n")
  
  return(list(
    data = df_interpolated,
    polygon_data = polygon_data,
    animation = anim
  ))
}


# USAGE:
df <- read_csv("./data/raw/20240521_DJI_0007_output_laguna.csv")

result <- create_enhanced_polygon_gif(
  df,
  output_file = "dolphin_polygon_final.gif",
  max_gap_frames = 120,  # 4 seconds at 30fps
  max_distance_m = 20,   # Dolphins can travel up to 20m between surfacings
  min_dolphins = 2,      # Need at least 2 dolphins for polygon
  n_frames = 40,         # 40 frames in GIF
  pause = 0.25           # 0.25 sec per frame = 4 fps
)
