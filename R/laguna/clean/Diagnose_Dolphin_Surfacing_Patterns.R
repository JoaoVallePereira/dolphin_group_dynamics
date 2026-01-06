# Diagnose_Dolphin_Surfacing_Patterns.R
# Understand why polygons aren't being created

library(tidyverse)

# Function to analyze surfacing patterns
diagnose_surfacing_patterns <- function(df,
                                       frame_col = "FrameIndex",
                                       id_col = "ObjectID",
                                       x_col = "CenterX_m",
                                       y_col = "CenterY_m") {
  
  cat("=== DOLPHIN SURFACING PATTERN DIAGNOSIS ===\n\n")
  
  # Basic stats
  cat("1. BASIC DATA STRUCTURE:\n")
  cat(sprintf("   Total rows: %d\n", nrow(df)))
  cat(sprintf("   Total frames: %d (from %d to %d)\n", 
              length(unique(df[[frame_col]])),
              min(df[[frame_col]]), max(df[[frame_col]])))
  cat(sprintf("   Unique ObjectIDs: %d\n", length(unique(df[[id_col]]))))
  cat(sprintf("   Unique dolphins (IDs): %s\n", 
              paste(sort(unique(df[[id_col]])), collapse=", ")))
  
  # Dolphins per frame
  cat("\n2. DOLPHINS PER FRAME:\n")
  dolphins_per_frame <- df %>%
    group_by(.data[[frame_col]]) %>%
    summarize(n_dolphins = n(), .groups = "drop")
  
  cat(sprintf("   Mean dolphins/frame: %.2f\n", mean(dolphins_per_frame$n_dolphins)))
  cat(sprintf("   Max dolphins/frame: %d\n", max(dolphins_per_frame$n_dolphins)))
  cat(sprintf("   Min dolphins/frame: %d\n", min(dolphins_per_frame$n_dolphins)))
  
  # Distribution
  cat("\n   Distribution of group sizes:\n")
  table_result <- table(dolphins_per_frame$n_dolphins)
  for (i in seq_along(table_result)) {
    cat(sprintf("     %d dolphins: %d frames (%.1f%%)\n", 
                as.numeric(names(table_result)[i]),
                table_result[i],
                100 * table_result[i] / nrow(dolphins_per_frame)))
  }
  
  # Frames with enough dolphins for polygon
  cat("\n   Frames with enough dolphins for polygon:\n")
  for (min_n in 2:5) {
    n_frames <- sum(dolphins_per_frame$n_dolphins >= min_n)
    cat(sprintf("     %d+ dolphins: %d frames (%.1f%%)\n",
                min_n, n_frames,
                100 * n_frames / nrow(dolphins_per_frame)))
  }
  
  # ObjectID persistence
  cat("\n3. OBJECTID PERSISTENCE:\n")
  id_stats <- df %>%
    group_by(.data[[id_col]]) %>%
    summarize(
      n_frames = n(),
      first_frame = min(.data[[frame_col]]),
      last_frame = max(.data[[frame_col]]),
      duration = last_frame - first_frame + 1,
      .groups = "drop"
    )
  
  cat(sprintf("   Mean detection duration: %.1f frames\n", mean(id_stats$duration)))
  cat(sprintf("   Max detection duration: %d frames\n", max(id_stats$duration)))
  cat(sprintf("   Mean consecutive detections: %.1f frames\n", mean(id_stats$n_frames)))
  
  # Show longest-tracked dolphins
  cat("\n   Longest-tracked dolphins:\n")
  top_ids <- id_stats %>% arrange(desc(duration)) %>% head(5)
  for (i in 1:nrow(top_ids)) {
    cat(sprintf("     ID %s: %d frames across %d-frame span\n",
                top_ids[[id_col]][i], top_ids$n_frames[i], top_ids$duration[i]))
  }
  
  # Gap analysis
  cat("\n4. SURFACING GAP ANALYSIS:\n")
  gaps <- id_stats %>%
    mutate(gaps = duration - n_frames) %>%
    filter(gaps > 0)
  
  if (nrow(gaps) > 0) {
    cat(sprintf("   IDs with gaps: %d out of %d\n", nrow(gaps), nrow(id_stats)))
    cat(sprintf("   Mean gap size: %.1f frames\n", mean(gaps$gaps)))
    cat(sprintf("   Max gap size: %d frames\n", max(gaps$gaps)))
  } else {
    cat("   No gaps detected (all IDs continuously tracked)\n")
  }
  
  # Concurrent dolphins
  cat("\n5. CONCURRENT DOLPHIN PAIRS:\n")
  # Find frames where multiple dolphins present
  multi_dolphin_frames <- dolphins_per_frame %>% filter(n_dolphins >= 2)
  
  if (nrow(multi_dolphin_frames) > 0) {
    cat(sprintf("   Frames with 2+ dolphins: %d (%.1f%%)\n",
                nrow(multi_dolphin_frames),
                100 * nrow(multi_dolphin_frames) / nrow(dolphins_per_frame)))
    
    # Sample some multi-dolphin frames
    sample_frames <- multi_dolphin_frames %>%
      arrange(desc(n_dolphins)) %>%
      head(5)
    
    cat("\n   Sample multi-dolphin frames:\n")
    for (i in 1:nrow(sample_frames)) {
      f <- sample_frames[[frame_col]][i]
      ids <- df %>% filter(.data[[frame_col]] == f) %>% pull(.data[[id_col]])
      cat(sprintf("     Frame %d: %d dolphins (IDs: %s)\n",
                  f, sample_frames$n_dolphins[i], paste(ids, collapse=", ")))
    }
  } else {
    cat("   NO frames with 2+ dolphins detected!\n")
    cat("   This explains why no polygons were created.\n")
  }
  
  # Spatial analysis
  cat("\n6. SPATIAL DISTRIBUTION:\n")
  cat(sprintf("   X range: %.1f to %.1f m (span: %.1f m)\n",
              min(df[[x_col]], na.rm=TRUE), max(df[[x_col]], na.rm=TRUE),
              diff(range(df[[x_col]], na.rm=TRUE))))
  cat(sprintf("   Y range: %.1f to %.1f m (span: %.1f m)\n",
              min(df[[y_col]], na.rm=TRUE), max(df[[y_col]], na.rm=TRUE),
              diff(range(df[[y_col]], na.rm=TRUE))))
  
  # Recommendations
  cat("\n7. RECOMMENDATIONS:\n")
  max_concurrent <- max(dolphins_per_frame$n_dolphins)
  
  if (max_concurrent < 2) {
    cat("   ⚠️  PROBLEM: Never more than 1 dolphin detected per frame\n")
    cat("   → This data cannot create polygons (need 2+ dolphins)\n")
    cat("   → Possible causes:\n")
    cat("      - Video shows solo dolphin foraging\n")
    cat("      - Tracker failed to detect multiple dolphins\n")
    cat("      - Need to merge data from multiple ObjectIDs\n")
  } else if (max_concurrent < 3) {
    cat("   ⚠️  ISSUE: Maximum 2 dolphins per frame\n")
    cat("   → Can create lines but not polygons (need 3+ for area)\n")
    cat("   → Try setting min_dolphins = 2 in create_polygon_data()\n")
    cat("   → Or use use_all_detections = TRUE to include interpolated positions\n")
  } else {
    pct_polygon_frames <- 100 * sum(dolphins_per_frame$n_dolphins >= 3) / nrow(dolphins_per_frame)
    if (pct_polygon_frames < 10) {
      cat(sprintf("   ⚠️  WARNING: Only %.1f%% of frames have 3+ dolphins\n", pct_polygon_frames))
      cat("   → Polygons will be sparse\n")
      cat("   → Consider using min_dolphins = 2 or use_all_detections = TRUE\n")
    } else {
      cat("   ✓ Data looks good for polygon creation!\n")
      cat(sprintf("   → %.1f%% of frames have 3+ dolphins\n", pct_polygon_frames))
    }
  }
  
  cat("\n=== END DIAGNOSIS ===\n")
  
  # Return summary
  return(list(
    dolphins_per_frame = dolphins_per_frame,
    id_stats = id_stats,
    max_concurrent = max_concurrent
  ))
}


# USAGE:
# df <- read_csv("20240521_DJI_0007_output.csv")
# diagnosis <- diagnose_surfacing_patterns(df)
