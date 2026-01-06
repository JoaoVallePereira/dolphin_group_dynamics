# Animated_Dolphin_Polygon_Visualization.R
# Create GIF showing polygon formed by surfacing dolphins over time
# Handles ID switching when dolphins surface/dive
# Shows imagined trajectories between surfacings

# CHALLENGE: Dolphin tracker assigns new ObjectID each time a dolphin surfaces
# SOLUTION: Track dolphins across IDs using spatial-temporal proximity matching

library(tidyverse)
library(sf)           # For polygons
library(gganimate)    # For animations
library(gifski)       # GIF rendering
library(viridis)      # Colors
library(scales)       # Alpha scaling

# ---------------------------
# DOLPHIN RE-IDENTIFICATION ACROSS SURFACINGS
# ---------------------------

# Match dolphins across ID switches using spatial-temporal proximity
# Logic: If dolphin at position (x1, y1) at time t1 disappears,
# and dolphin appears at nearby (x2, y2) at time t2 (within reasonable dive time),
# they're likely the same individual

match_dolphins_across_surfacings <- function(df,
                                             frame_col = "FrameIndex",
                                             id_col = "ObjectID",
                                             x_col = "CenterX_m",
                                             y_col = "CenterY_m",
                                             max_gap_frames = 90,  # 3 sec at 30fps
                                             max_distance_m = 10) { # Max distance dolphin could travel
  
  cat("Matching dolphins across surfacing events...\n")
  
  # Sort by frame
  df <- df %>% arrange(.data[[frame_col]])
  
  # Track persistent dolphin IDs
  df$PersistentID <- NA_integer_
  next_persistent_id <- 1
  
  # For each unique ObjectID, determine if it's a continuation of previous dolphin
  unique_ids <- unique(df[[id_col]])
  
  for (current_id in unique_ids) {
    # Get data for this ObjectID
    current_data <- df %>% filter(.data[[id_col]] == current_id)
    
    # First appearance of this ObjectID
    first_frame <- min(current_data[[frame_col]])
    first_x <- current_data %>% filter(.data[[frame_col]] == first_frame) %>% pull(.data[[x_col]]) %>% first()
    first_y <- current_data %>% filter(.data[[frame_col]] == first_frame) %>% pull(.data[[y_col]]) %>% first()
    
    # Check if any previous dolphin disappeared recently and nearby
    # Look back max_gap_frames
    search_frames <- seq(max(1, first_frame - max_gap_frames), first_frame - 1)
    
    if (length(search_frames) > 0) {
      # Get all dolphins that were present in search window but NOT in current frame
      previous_dolphins <- df %>%
        filter(.data[[frame_col]] %in% search_frames,
               !is.na(PersistentID)) %>%
        group_by(PersistentID) %>%
        summarize(
          last_frame = max(.data[[frame_col]]),
          last_x = last(.data[[x_col]]),
          last_y = last(.data[[y_col]]),
          .groups = "drop"
        ) %>%
        # Filter: not present in current frame (i.e., they disappeared)
        filter(!PersistentID %in% (df %>% 
                                     filter(.data[[frame_col]] == first_frame) %>% 
                                     pull(PersistentID)))
      
      if (nrow(previous_dolphins) > 0) {
        # Calculate distance from current position to each previous dolphin's last position
        previous_dolphins <- previous_dolphins %>%
          mutate(
            distance = sqrt((last_x - first_x)^2 + (last_y - first_y)^2),
            gap_frames = first_frame - last_frame
          )
        
        # Find closest match within thresholds
        matches <- previous_dolphins %>%
          filter(distance < max_distance_m, gap_frames <= max_gap_frames) %>%
          arrange(distance)
        
        if (nrow(matches) > 0) {
          # Assign to closest match
          matched_id <- matches$PersistentID[1]
          df$PersistentID[df[[id_col]] == current_id] <- matched_id
          
          cat(sprintf("  Matched ObjectID %s -> PersistentID %d (dist=%.1fm, gap=%d frames)\n",
                      current_id, matched_id, matches$distance[1], matches$gap_frames[1]))
          next
        }
      }
    }
    
    # No match found - assign new PersistentID
    df$PersistentID[df[[id_col]] == current_id] <- next_persistent_id
    cat(sprintf("  New dolphin: ObjectID %s -> PersistentID %d\n", current_id, next_persistent_id))
    next_persistent_id <- next_persistent_id + 1
  }
  
  cat(sprintf("\nIdentified %d persistent dolphins across %d ObjectIDs\n", 
              max(df$PersistentID, na.rm = TRUE), length(unique_ids)))
  
  return(df)
}


# ---------------------------
# INTERPOLATE UNDERWATER TRAJECTORIES
# ---------------------------

# When dolphin disappears and reappears, create imagined trajectory
interpolate_dive_trajectories <- function(df,
                                         frame_col = "FrameIndex",
                                         id_col = "PersistentID",
                                         x_col = "CenterX_m",
                                         y_col = "CenterY_m",
                                         interpolate_points = 10) {
  
  cat("Interpolating underwater trajectories...\n")
  
  df <- df %>% arrange(.data[[id_col]], .data[[frame_col]])
  
  # For each dolphin
  persistent_ids <- unique(df[[id_col]])
  interpolated_list <- vector("list", length(persistent_ids))
  
  for (i in seq_along(persistent_ids)) {
    pid <- persistent_ids[i]
    dolphin_data <- df %>% filter(.data[[id_col]] == pid)
    
    # Detect gaps (frames where dolphin not detected)
    all_frames <- seq(min(dolphin_data[[frame_col]]), max(dolphin_data[[frame_col]]))
    present_frames <- dolphin_data[[frame_col]]
    
    # Create full frame sequence
    full_data <- tibble(!!frame_col := all_frames) %>%
      left_join(dolphin_data, by = frame_col) %>%
      mutate(!!id_col := pid)
    
    # Interpolate x, y using smooth spline for more natural curves
    # (dolphins don't swim in straight lines when diving)
    full_data <- full_data %>%
      mutate(
        is_interpolated = is.na(.data[[x_col]]),
        !!x_col := zoo::na.spline(.data[[x_col]], na.rm = FALSE),
        !!y_col := zoo::na.spline(.data[[y_col]], na.rm = FALSE)
      )
    
    interpolated_list[[i]] <- full_data
  }
  
  result <- bind_rows(interpolated_list)
  
  n_interpolated <- sum(result$is_interpolated, na.rm = TRUE)
  cat(sprintf("Interpolated %d underwater positions\n", n_interpolated))
  
  return(result)
}


# ---------------------------
# POLYGON VISUALIZATION
# ---------------------------

# Create convex hull polygon from surfaced dolphins at each frame
create_polygon_data <- function(df,
                                frame_col = "FrameIndex",
                                id_col = "PersistentID",
                                x_col = "CenterX_m",
                                y_col = "CenterY_m",
                                min_dolphins = 3) {
  
  cat("Creating polygon data...\n")
  
  # Check if is_interpolated column exists
  has_interp_col <- "is_interpolated" %in% names(df)
  
  frames <- sort(unique(df[[frame_col]]))
  polygon_list <- vector("list", length(frames))
  n_polygons_created <- 0
  
  # Diagnostic: check surfacing patterns
  if (has_interp_col) {
    n_real_detections <- sum(!df$is_interpolated, na.rm = TRUE)
    n_interpolated <- sum(df$is_interpolated, na.rm = TRUE)
    cat(sprintf("  Total detections: %d real, %d interpolated\n", n_real_detections, n_interpolated))
  }
  
  for (i in seq_along(frames)) {
    f <- frames[i]
    
    # Get dolphins at this frame
    frame_data <- df %>% filter(.data[[frame_col]] == f)
    
    # Get surfaced dolphins (not interpolated)
    if (has_interp_col) {
      surfaced <- frame_data %>% filter(!is_interpolated | is.na(is_interpolated))
    } else {
      # If no interpolation column, use all data
      surfaced <- frame_data
    }
    
    # Remove any NA coordinates
    surfaced <- surfaced %>% filter(!is.na(.data[[x_col]]) & !is.na(.data[[y_col]]))
    
    if (nrow(surfaced) >= min_dolphins) {
      # Create convex hull
      pts <- surfaced %>% select(all_of(c(x_col, y_col)))
      pts_sf <- st_as_sf(pts, coords = c(x_col, y_col))
      hull <- st_convex_hull(st_union(pts_sf))
      
      # Convert to dataframe
      hull_coords <- st_coordinates(hull)[, 1:2] %>% 
        as.data.frame() %>%
        rename(x = X, y = Y)
      
      polygon_list[[i]] <- hull_coords %>%
        mutate(!!sym(frame_col) := f,
               n_dolphins = nrow(surfaced))
      
      n_polygons_created <- n_polygons_created + 1
    } else {
      polygon_list[[i]] <- NULL
    }
    
    if (i %% 100 == 0) cat(sprintf("  Processed %d/%d frames (%d polygons so far)\n", 
                                    i, length(frames), n_polygons_created))
  }
  
  polygon_data <- bind_rows(polygon_list)
  
  if (nrow(polygon_data) == 0) {
    cat("  WARNING: No polygons created!\n")
    cat("  Checking surfacing patterns...\n")
    
    # Diagnostic: how many dolphins per frame?
    dolphins_per_frame <- df %>%
      group_by(.data[[frame_col]]) %>%
      summarize(
        n_total = n(),
        n_real = if(has_interp_col) sum(!is_interpolated, na.rm = TRUE) else n(),
        .groups = "drop"
      )
    
    cat(sprintf("  Max dolphins in any frame: %d total, %d real\n",
                max(dolphins_per_frame$n_total),
                max(dolphins_per_frame$n_real)))
    cat(sprintf("  Frames with 3+ real dolphins: %d\n",
                sum(dolphins_per_frame$n_real >= 3)))
    
    cat("  TIP: Try reducing min_dolphins parameter or check if dolphins rarely surface together\n")
  } else {
    cat(sprintf("Created polygons for %d frames\n", length(unique(polygon_data[[frame_col]]))))
  }
  
  return(polygon_data)
}


# ---------------------------
# ANIMATED PLOT FUNCTION
# ---------------------------

create_dolphin_polygon_animation <- function(df,
                                            polygon_data,
                                            frame_col = "FrameIndex",
                                            id_col = "PersistentID",
                                            x_col = "CenterX_m",
                                            y_col = "CenterY_m",
                                            fisher_line_y = NULL,
                                            output_file = "dolphin_polygon.gif",
                                            fps = 10,
                                            duration = 10,
                                            width = 800,
                                            height = 600) {
  
  cat("Creating animation...\n")
  
  # Determine fisher line
  if (is.null(fisher_line_y)) {
    fisher_line_y <- min(df[[y_col]], na.rm = TRUE)
  }
  
  # Calculate frame range to animate
  frame_range <- range(df[[frame_col]])
  total_frames <- diff(frame_range) + 1
  
  # Subsample frames if too many
  max_frames <- fps * duration
  if (total_frames > max_frames) {
    frame_skip <- ceiling(total_frames / max_frames)
    frames_to_plot <- seq(frame_range[1], frame_range[2], by = frame_skip)
  } else {
    frames_to_plot <- seq(frame_range[1], frame_range[2])
  }
  
  # Filter data
  df_anim <- df %>% filter(.data[[frame_col]] %in% frames_to_plot)
  polygon_anim <- polygon_data %>% filter(.data[[frame_col]] %in% frames_to_plot)
  
  # Base plot
  p <- ggplot() +
    # Fisher line
    geom_hline(yintercept = fisher_line_y, 
               linetype = "dashed", color = "red", size = 1.5, alpha = 0.7) +
    
    # Polygon (filled, semi-transparent)
    geom_polygon(data = polygon_anim,
                 aes(x = x, y = y, group = .data[[frame_col]]),
                 fill = "lightblue", alpha = 0.3, color = "blue", size = 1) +
    
    # Dolphin trajectories (full history, fading)
    geom_path(data = df_anim,
              aes(x = .data[[x_col]], y = .data[[y_col]], 
                  group = .data[[id_col]], color = as.factor(.data[[id_col]])),
              alpha = 0.3, size = 0.5) +
    
    # Interpolated positions (dashed)
    geom_point(data = df_anim %>% filter(is_interpolated),
               aes(x = .data[[x_col]], y = .data[[y_col]], 
                   color = as.factor(.data[[id_col]])),
               size = 1, alpha = 0.3, shape = 1) +
    
    # Current surfaced positions (solid)
    geom_point(data = df_anim %>% filter(!is_interpolated | is.na(is_interpolated)),
               aes(x = .data[[x_col]], y = .data[[y_col]], 
                   color = as.factor(.data[[id_col]])),
               size = 3, alpha = 0.8) +
    
    # Labels
    labs(title = "Dolphin Group Polygon Evolution",
         subtitle = "Frame: {frame_time} | Surfaced dolphins form blue polygon | Dashed = fisher line",
         x = "X position (m)", 
         y = "Y position (m)",
         color = "Dolphin ID") +
    
    # Styling
    coord_fixed() +
    scale_color_viridis_d() +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      legend.position = "right"
    ) +
    
    # Animation
    transition_time(.data[[frame_col]]) +
    ease_aes('linear')
  
  # Render
  cat(sprintf("Rendering %d frames at %d fps...\n", length(frames_to_plot), fps))
  
  anim <- animate(p, 
                  nframes = length(frames_to_plot), 
                  fps = fps,
                  width = width, 
                  height = height,
                  renderer = gifski_renderer(output_file))
  
  cat(sprintf("Saved animation to: %s\n", output_file))
  
  return(anim)
}


# ---------------------------
# ALTERNATIVE: STEP-BY-STEP FRAMES
# ---------------------------

# Instead of smooth animation, show key moments (surfacing events)
create_polygon_step_frames <- function(df,
                                       polygon_data,
                                       frame_col = "FrameIndex",
                                       id_col = "PersistentID",
                                       x_col = "CenterX_m",
                                       y_col = "CenterY_m",
                                       n_frames = 20,
                                       output_file = "dolphin_polygon_steps.gif") {
  
  cat("Creating step-by-step animation...\n")
  
  # Select key frames (evenly spaced)
  all_frames <- sort(unique(polygon_data[[frame_col]]))
  
  if (length(all_frames) > n_frames) {
    indices <- round(seq(1, length(all_frames), length.out = n_frames))
    key_frames <- all_frames[indices]
  } else {
    key_frames <- all_frames
  }
  
  # Create individual plots for each step
  plot_list <- vector("list", length(key_frames))
  
  for (i in seq_along(key_frames)) {
    f <- key_frames[i]
    
    # Data up to this frame
    df_history <- df %>% filter(.data[[frame_col]] <= f)
    df_current <- df %>% filter(.data[[frame_col]] == f)
    polygon_current <- polygon_data %>% filter(.data[[frame_col]] == f)
    
    # Plot
    p <- ggplot() +
      # History (faint trails)
      geom_path(data = df_history,
                aes(x = .data[[x_col]], y = .data[[y_col]], group = .data[[id_col]]),
                color = "gray70", alpha = 0.3, size = 0.3) +
      
      # Current polygon
      geom_polygon(data = polygon_current,
                   aes(x = x, y = y),
                   fill = "lightblue", alpha = 0.4, color = "blue", size = 1.2) +
      
      # Current positions
      geom_point(data = df_current %>% filter(!is_interpolated | is.na(is_interpolated)),
                 aes(x = .data[[x_col]], y = .data[[y_col]], 
                     color = as.factor(.data[[id_col]])),
                 size = 4) +
      
      # Fisher line
      geom_hline(yintercept = min(df[[y_col]], na.rm = TRUE),
                 linetype = "dashed", color = "red", size = 1) +
      
      # Labels
      labs(title = sprintf("Frame %d - Step %d/%d", f, i, length(key_frames)),
           x = "X (m)", y = "Y (m)", color = "Dolphin") +
      coord_fixed() +
      scale_color_viridis_d() +
      theme_minimal() +
      theme(legend.position = "right")
    
    plot_list[[i]] <- p
  }
  
  # Combine into GIF using ggsave + magick
  # Save individual frames first
  temp_dir <- tempdir()
  frame_files <- character(length(plot_list))
  
  for (i in seq_along(plot_list)) {
    frame_files[i] <- file.path(temp_dir, sprintf("frame_%03d.png", i))
    ggsave(frame_files[i], plot_list[[i]], width = 10, height = 8, dpi = 100)
  }
  
  # Combine with magick
  library(magick)
  frames <- image_read(frame_files)
  animation <- image_animate(frames, fps = 2)  # 2 fps for step-by-step
  image_write(animation, output_file)
  
  # Clean up
  file.remove(frame_files)
  
  cat(sprintf("Saved %d-frame animation to: %s\n", length(key_frames), output_file))
  
  return(animation)
}


# ---------------------------
# COMPLETE WORKFLOW FUNCTION
# ---------------------------

create_dolphin_polygon_gif <- function(df,
                                       frame_col = "FrameIndex",
                                       id_col = "ObjectID",
                                       x_col = "CenterX_m",
                                       y_col = "CenterY_m",
                                       output_file = "dolphin_polygon_animation.gif",
                                       animation_type = "smooth",  # "smooth" or "steps"
                                       max_gap_frames = 90,
                                       max_distance_m = 10,
                                       min_dolphins = 2,  # Lower default - dolphins often solo
                                       use_all_detections = FALSE,  # If TRUE, don't filter interpolated
                                       fps = 10,
                                       duration = 10) {
  
  cat("\n=== CREATING DOLPHIN POLYGON ANIMATION ===\n\n")
  
  # Step 1: Match dolphins across surfacings
  df_matched <- match_dolphins_across_surfacings(
    df, frame_col, id_col, x_col, y_col, max_gap_frames, max_distance_m
  )
  
  # Step 2: Interpolate trajectories
  df_interpolated <- interpolate_dive_trajectories(
    df_matched, frame_col, "PersistentID", x_col, y_col
  )
  
  # Step 3: Create polygon data
  if (use_all_detections) {
    cat("\nUsing ALL detections (including interpolated) for polygons\n")
    # Remove the is_interpolated filter
    df_interpolated$is_interpolated <- FALSE
  }
  
  polygon_data <- create_polygon_data(
    df_interpolated, frame_col, "PersistentID", x_col, y_col, min_dolphins
  )
  
  # Check if we got any polygons
  if (nrow(polygon_data) == 0) {
    cat("\nERROR: No polygons created. Trying with relaxed parameters...\n")
    cat("  Setting min_dolphins = 2 and using all detections\n")
    df_interpolated$is_interpolated <- FALSE
    polygon_data <- create_polygon_data(
      df_interpolated, frame_col, "PersistentID", x_col, y_col, min_dolphins = 2
    )
  }
  
  if (nrow(polygon_data) == 0) {
    stop("Still no polygons created. Check that your data has multiple dolphins detected simultaneously.")
  }
  
  # Step 4: Create animation
  if (animation_type == "smooth") {
    anim <- create_dolphin_polygon_animation(
      df_interpolated, polygon_data, frame_col, "PersistentID", x_col, y_col,
      output_file = output_file, fps = fps, duration = duration
    )
  } else {
    anim <- create_polygon_step_frames(
      df_interpolated, polygon_data, frame_col, "PersistentID", x_col, y_col,
      output_file = output_file
    )
  }
  
  cat("\n=== DONE ===\n")
  
  return(list(
    data = df_interpolated,
    polygon_data = polygon_data,
    animation = anim
  ))
}


# ---------------------------
# USAGE EXAMPLE
# ---------------------------

# # Load data
# df <- read_csv("20240521_DJI_0007_output.csv")
# 
# # Create smooth animation (10 seconds)
# result <- create_dolphin_polygon_gif(
#   df,
#   output_file = "dolphin_polygon_smooth.gif",
#   animation_type = "smooth",
#   max_gap_frames = 90,    # 3 seconds at 30 fps
#   max_distance_m = 10,     # 10m max travel between surfacings
#   fps = 15,
#   duration = 10
# )
# 
# # Create step-by-step animation (slower, clearer)
# result_steps <- create_dolphin_polygon_gif(
#   df,
#   output_file = "dolphin_polygon_steps.gif",
#   animation_type = "steps",
#   max_gap_frames = 90,
#   max_distance_m = 10
# )
