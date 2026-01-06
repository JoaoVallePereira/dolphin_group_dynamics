# Fixed_Dolphin_Animation.R
# Fixes the geom_path grouping issue and gifski encoding errors

library(tidyverse)
library(gganimate)
library(magick)  # Alternative to gifski
library(viridis)

# Fixed animation function
create_dolphin_animation_fixed <- function(df_interpolated,
                                          polygon_data = NULL,
                                          frame_col = "FrameIndex",
                                          id_col = "PersistentID",
                                          x_col = "CenterX_m",
                                          y_col = "CenterY_m",
                                          output_file = "dolphin_animation.gif",
                                          fps = 10,
                                          width = 800,
                                          height = 600) {
  
  cat("Creating fixed animation...\n")
  
  # Subsample frames for manageability
  all_frames <- sort(unique(df_interpolated[[frame_col]]))
  max_frames <- fps * 15  # 15 second max
  
  if (length(all_frames) > max_frames) {
    skip <- ceiling(length(all_frames) / max_frames)
    frames_to_plot <- all_frames[seq(1, length(all_frames), by = skip)]
  } else {
    frames_to_plot <- all_frames
  }
  
  df_anim <- df_interpolated %>% filter(.data[[frame_col]] %in% frames_to_plot)
  
  # Fix: Only create paths for dolphins with 2+ points
  trajectory_data <- df_anim %>%
    group_by(.data[[id_col]]) %>%
    filter(n() >= 2) %>%  # KEY FIX: Remove single-point "paths"
    ungroup()
  
  # Determine fisher line
  fisher_line_y <- min(df_anim[[y_col]], na.rm = TRUE)
  
  # Create base plot
  p <- ggplot() +
    # Fisher line (static)
    geom_hline(yintercept = fisher_line_y, 
               linetype = "dashed", color = "red", size = 1.5, alpha = 0.7) +
    
    # Dolphin trajectories (only if 2+ points) - FIXED
    {if(nrow(trajectory_data) > 0) {
      geom_path(data = trajectory_data,
                aes(x = .data[[x_col]], y = .data[[y_col]], 
                    group = .data[[id_col]], color = as.factor(.data[[id_col]])),
                alpha = 0.4, size = 0.8)
    }} +
    
    # Current positions (all dolphins)
    geom_point(data = df_anim,
               aes(x = .data[[x_col]], y = .data[[y_col]], 
                   color = as.factor(.data[[id_col]]),
                   size = ifelse(is_interpolated, 2, 4),
                   alpha = ifelse(is_interpolated, 0.3, 0.9))) +
    
    # Polygon if available
    {if(!is.null(polygon_data) && nrow(polygon_data) > 0) {
      polygon_anim <- polygon_data %>% filter(.data[[frame_col]] %in% frames_to_plot)
      geom_polygon(data = polygon_anim,
                   aes(x = x, y = y, group = .data[[frame_col]]),
                   fill = "lightblue", alpha = 0.2, color = "blue", size = 1)
    }} +
    
    # Styling
    coord_fixed() +
    scale_color_viridis_d() +
    scale_size_identity() +
    scale_alpha_identity() +
    labs(title = "Dolphin Movement & Formation",
         subtitle = "Frame: {frame_time} | Large dots = surfaced | Small dots = interpolated",
         x = "X position (m)", 
         y = "Y position (m)",
         color = "Dolphin ID") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10),
      legend.position = "right"
    ) +
    
    # Animation
    transition_time(.data[[frame_col]]) +
    ease_aes('linear')
  
  # Render using magick (more stable than gifski)
  cat(sprintf("Rendering %d frames...\n", length(frames_to_plot)))
  
  anim <- animate(p, 
                  nframes = length(frames_to_plot), 
                  fps = fps,
                  width = width, 
                  height = height,
                  renderer = magick_renderer())  # USING MAGICK INSTEAD
  
  # Save
  anim_save(output_file, animation = anim)
  
  cat(sprintf("✓ Saved animation to: %s\n", output_file))
  
  return(anim)
}


# Alternative: Simple static frames approach (most reliable)
create_dolphin_static_frames <- function(df_interpolated,
                                        polygon_data = NULL,
                                        frame_col = "FrameIndex",
                                        id_col = "PersistentID",
                                        x_col = "CenterX_m",
                                        y_col = "CenterY_m",
                                        output_file = "dolphin_frames.gif",
                                        n_frames = 50,
                                        pause = 0.2) {
  
  cat("Creating animation from static frames (most reliable method)...\n")
  
  # Select key frames
  all_frames <- sort(unique(df_interpolated[[frame_col]]))
  
  if (length(all_frames) > n_frames) {
    indices <- round(seq(1, length(all_frames), length.out = n_frames))
    key_frames <- all_frames[indices]
  } else {
    key_frames <- all_frames
  }
  
  # Create temp directory for frames
  temp_dir <- tempdir()
  frame_files <- character(length(key_frames))
  
  # Fisher line
  fisher_line_y <- min(df_interpolated[[y_col]], na.rm = TRUE)
  
  # Create each frame
  for (i in seq_along(key_frames)) {
    f <- key_frames[i]
    
    # Get data up to this frame (for trails)
    df_history <- df_interpolated %>% 
      filter(.data[[frame_col]] <= f) %>%
      group_by(.data[[id_col]]) %>%
      filter(n() >= 2) %>%
      ungroup()
    
    # Current frame
    df_current <- df_interpolated %>% filter(.data[[frame_col]] == f)
    
    # Polygon (if available)
    if (!is.null(polygon_data) && nrow(polygon_data) > 0) {
      polygon_current <- polygon_data %>% filter(.data[[frame_col]] == f)
    } else {
      polygon_current <- NULL
    }
    
    # Create plot
    p <- ggplot() +
      # History trails
      {if(nrow(df_history) > 0) {
        geom_path(data = df_history,
                  aes(x = .data[[x_col]], y = .data[[y_col]], 
                      group = .data[[id_col]]),
                  color = "gray70", alpha = 0.3, size = 0.5)
      }} +
      
      # Polygon
      {if(!is.null(polygon_current) && nrow(polygon_current) > 0) {
        geom_polygon(data = polygon_current,
                     aes(x = x, y = y),
                     fill = "lightblue", alpha = 0.3, color = "blue", size = 1.2)
      }} +
      
      # Current positions
      geom_point(data = df_current,
                 aes(x = .data[[x_col]], y = .data[[y_col]], 
                     color = as.factor(.data[[id_col]])),
                 size = 5) +
      
      # Fisher line
      geom_hline(yintercept = fisher_line_y,
                 linetype = "dashed", color = "red", size = 1.2) +
      
      # Styling
      coord_fixed() +
      scale_color_viridis_d() +
      labs(title = sprintf("Dolphin Formation - Frame %d (%d/%d)", 
                          f, i, length(key_frames)),
           x = "X (m)", y = "Y (m)", color = "Dolphin") +
      theme_minimal() +
      theme(legend.position = "right",
            plot.title = element_text(size = 14, face = "bold"))
    
    # Save frame
    frame_files[i] <- file.path(temp_dir, sprintf("frame_%04d.png", i))
    ggsave(frame_files[i], p, width = 10, height = 8, dpi = 100)
    
    if (i %% 10 == 0) cat(sprintf("  Created frame %d/%d\n", i, length(key_frames)))
  }
  
  # Combine with magick
  cat("Combining frames into GIF...\n")
  frames <- image_read(frame_files)
  animation <- image_animate(frames, fps = 1/pause)  # pause seconds per frame
  image_write(animation, output_file)
  
  # Clean up
  file.remove(frame_files)
  
  cat(sprintf("✓ Saved %d-frame animation to: %s\n", length(key_frames), output_file))
  
  return(animation)
}


# USAGE EXAMPLE:

# After running the matching and interpolation:
# df_matched <- match_dolphins_across_surfacings(df, ...)
# df_interpolated <- interpolate_dive_trajectories(df_matched, ...)
# polygon_data <- create_polygon_data(df_interpolated, ...) # May be empty

# METHOD 1: Fixed gganimate (faster but may still have issues)
# anim1 <- create_dolphin_animation_fixed(
#   df_interpolated,
#   polygon_data,
#   output_file = "dolphin_anim_v1.gif"
# )

# METHOD 2: Static frames approach (slower but bulletproof)
# anim2 <- create_dolphin_static_frames(
#   df_interpolated,
#   polygon_data,
#   output_file = "dolphin_anim_v2.gif",
#   n_frames = 30,
#   pause = 0.3  # seconds per frame
# )
