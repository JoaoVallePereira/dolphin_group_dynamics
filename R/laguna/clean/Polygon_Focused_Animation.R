# Polygon_Focused_Animation.R
# Only show frames where polygons actually exist
# Add visual enhancements to make polygons more visible

library(tidyverse)
library(sf)
library(magick)
library(viridis)

# Create animation showing ONLY frames with polygons
create_polygon_focused_animation <- function(df_interpolated,
                                            polygon_data,
                                            frame_col = "FrameIndex",
                                            id_col = "PersistentID",
                                            x_col = "CenterX_m",
                                            y_col = "CenterY_m",
                                            output_file = "dolphin_polygons_only.gif",
                                            max_frames = 50,
                                            pause = 0.3) {
  
  cat("Creating polygon-focused animation...\n")
  
  # Get only frames that have polygons
  frames_with_polygons <- unique(polygon_data[[frame_col]])
  cat(sprintf("Found %d frames with polygons\n", length(frames_with_polygons)))
  
  if (length(frames_with_polygons) == 0) {
    stop("No frames with polygons found!")
  }
  
  # Subsample if needed
  if (length(frames_with_polygons) > max_frames) {
    indices <- round(seq(1, length(frames_with_polygons), length.out = max_frames))
    key_frames <- frames_with_polygons[indices]
  } else {
    key_frames <- frames_with_polygons
  }
  
  cat(sprintf("Creating animation with %d frames\n", length(key_frames)))
  
  # Temp directory
  temp_dir <- tempdir()
  frame_files <- character(length(key_frames))
  
  # Fisher line
  fisher_line_y <- min(df_interpolated[[y_col]], na.rm = TRUE)
  
  # Create frames
  for (i in seq_along(key_frames)) {
    f <- key_frames[i]
    
    # Get data for this frame
    df_current <- df_interpolated %>% filter(.data[[frame_col]] == f)
    df_real <- df_current %>% filter(!is_interpolated | is.na(is_interpolated))
    df_predicted <- df_current %>% filter(is_interpolated)
    
    # Get polygon
    polygon_current <- polygon_data %>% filter(.data[[frame_col]] == f)
    
    # Get polygon summary
    poly_summary <- polygon_current %>% 
      distinct(.data[[frame_col]], .keep_all = TRUE) %>%
      select(n_dolphins, n_real, n_interpolated, polygon_type)
    
    # Historical trajectories (last 50 frames)
    df_history <- df_interpolated %>%
      filter(.data[[frame_col]] > (f - 50), .data[[frame_col]] <= f) %>%
      filter(!is_interpolated | is.na(is_interpolated)) %>%
      group_by(.data[[id_col]]) %>%
      filter(n() >= 2) %>%
      ungroup()
    
    # Set colors based on polygon type
    if (nrow(poly_summary) > 0) {
      is_real <- poly_summary$polygon_type[1] == "real"
      poly_color <- ifelse(is_real, "#0066CC", "#FF6600")  # Blue or orange
      poly_fill <- ifelse(is_real, "#0066CC", "#FF6600")
      poly_alpha_fill <- ifelse(is_real, 0.25, 0.15)
      poly_alpha_line <- ifelse(is_real, 0.8, 0.6)
      poly_linewidth <- ifelse(is_real, 2, 1.5)
      poly_linetype <- ifelse(is_real, "solid", "dashed")
    }
    
    # Create plot
    p <- ggplot() +
      # Historical trails (very faint)
      {if(nrow(df_history) > 0) {
        geom_path(data = df_history,
                  aes(x = .data[[x_col]], y = .data[[y_col]], 
                      group = .data[[id_col]]),
                  color = "gray85", linewidth = 0.4, alpha = 0.5)
      }} +
      
      # POLYGON - Main feature (draw twice for emphasis)
      {if(nrow(polygon_current) > 0) {
        list(
          # Filled polygon
          geom_polygon(data = polygon_current,
                       aes(x = x, y = y),
                       fill = poly_fill, alpha = poly_alpha_fill),
          # Outline
          geom_path(data = polygon_current,
                    aes(x = x, y = y),
                    color = poly_color, linewidth = poly_linewidth, 
                    alpha = poly_alpha_line, linetype = poly_linetype)
        )
      }} +
      
      # Lines connecting dolphins to polygon vertices
      {if(nrow(df_current) >= 2 && nrow(polygon_current) > 0) {
        geom_segment(data = df_current,
                     aes(x = .data[[x_col]], y = .data[[y_col]],
                         xend = .data[[x_col]], yend = .data[[y_col]]),
                     color = "gray60", linewidth = 0.3, alpha = 0.4)
      }} +
      
      # Predicted positions (hollow, smaller)
      {if(nrow(df_predicted) > 0) {
        geom_point(data = df_predicted,
                   aes(x = .data[[x_col]], y = .data[[y_col]], 
                       color = as.factor(.data[[id_col]])),
                   size = 3, shape = 21, stroke = 1.2, fill = NA, alpha = 0.7)
      }} +
      
      # Real surfaced positions (solid, larger)
      {if(nrow(df_real) > 0) {
        geom_point(data = df_real,
                   aes(x = .data[[x_col]], y = .data[[y_col]], 
                       color = as.factor(.data[[id_col]])),
                   size = 6, shape = 16, alpha = 0.9)
      }} +
      
      # Labels on dolphin positions
      {if(nrow(df_real) > 0) {
        geom_text(data = df_real,
                  aes(x = .data[[x_col]], y = .data[[y_col]], 
                      label = .data[[id_col]]),
                  color = "white", size = 2.5, fontface = "bold")
      }} +
      
      # Fisher line
      geom_hline(yintercept = fisher_line_y,
                 linetype = "dashed", color = "red", linewidth = 1.8, alpha = 0.8) +
      annotate("text", x = mean(df_interpolated[[x_col]], na.rm = TRUE),
               y = fisher_line_y - 1.8,
               label = "FISHERS", color = "red", size = 5, fontface = "bold") +
      
      # Polygon info box
      {if(nrow(poly_summary) > 0) {
        annotate("label", 
                 x = min(df_interpolated[[x_col]], na.rm = TRUE) + 5,
                 y = max(df_interpolated[[y_col]], na.rm = TRUE) - 3,
                 label = sprintf("%s POLYGON\n%d dolphins (%d surfaced, %d predicted)",
                                toupper(poly_summary$polygon_type[1]),
                                poly_summary$n_dolphins[1],
                                poly_summary$n_real[1],
                                poly_summary$n_interpolated[1]),
                 fill = poly_color, color = "white", 
                 size = 3.5, fontface = "bold", alpha = 0.9,
                 label.padding = unit(0.5, "lines"))
      }} +
      
      # Styling
      coord_fixed() +
      scale_color_viridis_d(option = "plasma") +
      labs(
        title = sprintf("Dolphin Polygon Formation - Frame %d", f),
        subtitle = sprintf("Step %d of %d | Solid = surfaced | Hollow = predicted", 
                          i, length(key_frames)),
        x = "X position (m)", 
        y = "Y position (m)",
        color = "Dolphin"
      ) +
      theme_minimal() +
      theme(
        legend.position = "right",
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 11),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "gray98", color = NA),
        plot.background = element_rect(fill = "white", color = NA)
      )
    
    # Save
    frame_files[i] <- file.path(temp_dir, sprintf("frame_%04d.png", i))
    ggsave(frame_files[i], p, width = 14, height = 9, dpi = 110)
    
    if (i %% 10 == 0) cat(sprintf("  Created frame %d/%d\n", i, length(key_frames)))
  }
  
  # Combine
  cat("Combining into GIF...\n")
  frames <- image_read(frame_files)
  
  # Calculate fps
  target_fps <- round(1 / pause)
  valid_fps <- c(1, 2, 4, 5, 10, 20, 25, 50, 100)
  fps <- valid_fps[which.min(abs(valid_fps - target_fps))]
  
  animation <- image_animate(frames, fps = fps)
  image_write(animation, output_file)
  
  # Cleanup
  file.remove(frame_files)
  
  cat(sprintf("\n✓ SUCCESS! Animation saved to: %s\n", output_file))
  cat(sprintf("  Frames: %d | FPS: %d | Duration: %.1f seconds\n", 
              length(key_frames), fps, length(key_frames) / fps))
  
  return(animation)
}


# NEW FUNCTION: Include fisher line as polygon vertices
create_polygon_with_fisher_line <- function(df_interpolated,
                                           polygon_data,
                                           frame_col = "FrameIndex",
                                           id_col = "PersistentID",
                                           x_col = "CenterX_m",
                                           y_col = "CenterY_m",
                                           output_file = "dolphin_polygons_with_fishers.gif",
                                           max_frames = 50,
                                           pause = 0.3) {
  
  cat("Creating polygon animation INCLUDING fisher line as vertices...\n")
  
  # Get frames with polygons
  frames_with_polygons <- unique(polygon_data[[frame_col]])
  cat(sprintf("Found %d frames with polygons\n", length(frames_with_polygons)))
  
  if (length(frames_with_polygons) == 0) {
    stop("No frames with polygons found!")
  }
  
  # Subsample if needed
  if (length(frames_with_polygons) > max_frames) {
    indices <- round(seq(1, length(frames_with_polygons), length.out = max_frames))
    key_frames <- frames_with_polygons[indices]
  } else {
    key_frames <- frames_with_polygons
  }
  
  cat(sprintf("Creating animation with %d frames\n", length(key_frames)))
  
  # Temp directory
  temp_dir <- tempdir()
  frame_files <- character(length(key_frames))
  
  # Fisher line position
  fisher_line_y <- min(df_interpolated[[y_col]], na.rm = TRUE)
  
  # Create frames
  for (i in seq_along(key_frames)) {
    f <- key_frames[i]
    
    # Get data for this frame
    df_current <- df_interpolated %>% filter(.data[[frame_col]] == f)
    df_real <- df_current %>% filter(!is_interpolated | is.na(is_interpolated))
    df_predicted <- df_current %>% filter(is_interpolated)
    
    # CREATE NEW POLYGON INCLUDING FISHER LINE POINTS
    # Add two points on the fisher line at the x-extent of the dolphins
    if (nrow(df_current) > 0) {
      x_min <- min(df_current[[x_col]], na.rm = TRUE)
      x_max <- max(df_current[[x_col]], na.rm = TRUE)
      
      # Create augmented point set: dolphins + fisher line endpoints
      augmented_points <- bind_rows(
        df_current %>% select(all_of(c(x_col, y_col))),
        tibble(!!sym(x_col) := x_min, !!sym(y_col) := fisher_line_y),
        tibble(!!sym(x_col) := x_max, !!sym(y_col) := fisher_line_y)
      )
      
      # Create convex hull with fisher line included
      pts_sf <- st_as_sf(augmented_points, coords = c(x_col, y_col))
      hull <- st_convex_hull(st_union(pts_sf))
      
      polygon_with_fishers <- st_coordinates(hull)[, 1:2] %>% 
        as.data.frame() %>%
        rename(x = X, y = Y)
    } else {
      polygon_with_fishers <- NULL
    }
    
    # Get polygon summary
    poly_summary <- polygon_data %>%
      filter(.data[[frame_col]] == f) %>%
      distinct(.data[[frame_col]], .keep_all = TRUE) %>%
      select(n_dolphins, n_real, n_interpolated, polygon_type)
    
    # Historical trajectories
    df_history <- df_interpolated %>%
      filter(.data[[frame_col]] > (f - 50), .data[[frame_col]] <= f) %>%
      filter(!is_interpolated | is.na(is_interpolated)) %>%
      group_by(.data[[id_col]]) %>%
      filter(n() >= 2) %>%
      ungroup()
    
    # Set colors
    if (nrow(poly_summary) > 0) {
      is_real <- poly_summary$polygon_type[1] == "real"
      poly_color <- ifelse(is_real, "#0066CC", "#FF6600")
      poly_fill <- ifelse(is_real, "#0066CC", "#FF6600")
      poly_alpha_fill <- 0.2
      poly_alpha_line <- 0.9
      poly_linewidth <- 2.5
    } else {
      poly_color <- "#0066CC"
      poly_fill <- "#0066CC"
      poly_alpha_fill <- 0.2
      poly_alpha_line <- 0.9
      poly_linewidth <- 2.5
    }
    
    # Create plot
    p <- ggplot() +
      # Historical trails
      {if(nrow(df_history) > 0) {
        geom_path(data = df_history,
                  aes(x = .data[[x_col]], y = .data[[y_col]], 
                      group = .data[[id_col]]),
                  color = "gray85", linewidth = 0.4, alpha = 0.5)
      }} +
      
      # POLYGON INCLUDING FISHER LINE
      {if(!is.null(polygon_with_fishers) && nrow(polygon_with_fishers) > 0) {
        list(
          # Filled polygon
          geom_polygon(data = polygon_with_fishers,
                       aes(x = x, y = y),
                       fill = poly_fill, alpha = poly_alpha_fill),
          # Outline
          geom_path(data = polygon_with_fishers,
                    aes(x = x, y = y),
                    color = poly_color, linewidth = poly_linewidth, 
                    alpha = poly_alpha_line)
        )
      }} +
      
      # Fisher line points (show where polygon touches fishers)
      {if(nrow(df_current) > 0) {
        x_extent <- range(df_current[[x_col]], na.rm = TRUE)
        fisher_points <- tibble(x = x_extent, y = fisher_line_y)
        
        geom_point(data = fisher_points,
                   aes(x = x, y = y),
                   color = "red", size = 5, shape = 17, alpha = 0.8)  # Triangles
      }} +
      
      # Predicted positions (hollow)
      {if(nrow(df_predicted) > 0) {
        geom_point(data = df_predicted,
                   aes(x = .data[[x_col]], y = .data[[y_col]], 
                       color = as.factor(.data[[id_col]])),
                   size = 3, shape = 21, stroke = 1.2, fill = NA, alpha = 0.7)
      }} +
      
      # Real surfaced positions (solid)
      {if(nrow(df_real) > 0) {
        geom_point(data = df_real,
                   aes(x = .data[[x_col]], y = .data[[y_col]], 
                       color = as.factor(.data[[id_col]])),
                   size = 6, shape = 16, alpha = 0.9)
      }} +
      
      # Labels on dolphins
      {if(nrow(df_real) > 0) {
        geom_text(data = df_real,
                  aes(x = .data[[x_col]], y = .data[[y_col]], 
                      label = .data[[id_col]]),
                  color = "white", size = 2.5, fontface = "bold")
      }} +
      
      # Fisher line
      geom_hline(yintercept = fisher_line_y,
                 linetype = "solid", color = "red", linewidth = 2.5, alpha = 0.9) +
      annotate("text", x = mean(df_interpolated[[x_col]], na.rm = TRUE),
               y = fisher_line_y - 1.8,
               label = "FISHERS (Polygon Boundary)", 
               color = "red", size = 5, fontface = "bold") +
      
      # Info box
      {if(nrow(poly_summary) > 0) {
        annotate("label", 
                 x = min(df_interpolated[[x_col]], na.rm = TRUE) + 5,
                 y = max(df_interpolated[[y_col]], na.rm = TRUE) - 3,
                 label = sprintf("CORNERING POLYGON\n%d dolphins + Fisher Line\n(%d surfaced, %d predicted)",
                                poly_summary$n_dolphins[1],
                                poly_summary$n_real[1],
                                poly_summary$n_interpolated[1]),
                 fill = poly_color, color = "white", 
                 size = 3.5, fontface = "bold", alpha = 0.9,
                 label.padding = unit(0.5, "lines"))
      }} +
      
      # Styling
      coord_fixed() +
      scale_color_viridis_d(option = "plasma") +
      labs(
        title = sprintf("Dolphin Cornering Formation - Frame %d", f),
        subtitle = sprintf("Step %d of %d | Polygon includes fisher line as boundary | Red triangles = polygon-fisher contact", 
                          i, length(key_frames)),
        x = "X position (m)", 
        y = "Y position (m)",
        color = "Dolphin"
      ) +
      theme_minimal() +
      theme(
        legend.position = "right",
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 10),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "gray98", color = NA),
        plot.background = element_rect(fill = "white", color = NA)
      )
    
    # Save
    frame_files[i] <- file.path(temp_dir, sprintf("frame_%04d.png", i))
    ggsave(frame_files[i], p, width = 14, height = 9, dpi = 110)
    
    if (i %% 10 == 0) cat(sprintf("  Created frame %d/%d\n", i, length(key_frames)))
  }
  
  # Combine
  cat("Combining into GIF...\n")
  frames <- image_read(frame_files)
  
  # Calculate fps
  target_fps <- round(1 / pause)
  valid_fps <- c(1, 2, 4, 5, 10, 20, 25, 50, 100)
  fps <- valid_fps[which.min(abs(valid_fps - target_fps))]
  
  animation <- image_animate(frames, fps = fps)
  image_write(animation, output_file)
  
  # Cleanup
  file.remove(frame_files)
  
  cat(sprintf("\n✓ SUCCESS! Animation saved to: %s\n", output_file))
  cat(sprintf("  Frames: %d | FPS: %d | Duration: %.1f seconds\n", 
              length(key_frames), fps, length(key_frames) / fps))
  cat("\nThis animation shows the CORNERING polygon with fisher line as base!\n")
  
  return(animation)
}


# USAGE:
# After running create_enhanced_polygon_gif() to get df_interpolated and polygon_data:
df <- read.csv("./data/raw/E16_SB02_14fev25_(1)_corte_106_output.csv")
df <- read.csv("./data/raw/20240521_DJI_0007_output_laguna.csv")
result <- create_enhanced_polygon_gif(df) 

# METHOD 1: Standard polygon (dolphins only)
anim <- create_polygon_focused_animation(
  result$data,
  result$polygon_data,
  output_file = "dolphin_polygons_highlighted.gif",
  max_frames = 40,
  pause = 0.3
)

# METHOD 2: NEW - Polygon including fisher line as boundary (CORNERING visualization)
anim_cornering <- create_polygon_with_fisher_line(
  result$data,
  result$polygon_data,
  output_file = "dolphin_cornering_polygon.gif",
  max_frames = 40,
  pause = 0.3
)
