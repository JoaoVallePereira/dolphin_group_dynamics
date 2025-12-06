# dolphin_group_metrics.R
# title: "Dolphin-group-dynamics-metrics for Julia Pierry"
# author: "Mauricio Cantor and João Valle-Pereira"
# date: "2025-12-05"
#
# Modular pipeline to compute group-level metrics from drone detections.
#
# Required columns in the CSV output from dolphin-tracker:
#  - FrameIndex   : integer frame number
#  - ObjectID     : unique ID per tracked individual (may be numeric or character)
#  - CenterX_m    : x coordinate in meters
#  - CenterY_m    : y coordinate in meters
#  - MovingAvgAngle_deg : heading angle in degrees (optional but recommended)
# Optional:
#  - BreathCol : name of a binary column marking surfacing/breathing (1 / 0). Presence detection will be used as a proxy.
#
# Defaults:
#  fps = 30, window_sec = 5, Nmin = 1, interp_gap = 1 (frames)



# ---------------------------
#  Load packages and Helper functions
# ---------------------------

library(tidyverse)
library(zoo)       
library(igraph)    
library(viridis)   
library(patchwork) 



# interpolate_tracks:
# Fill short detection gaps per ObjectID using linear interpolation up to maxgap frames.
# - df: tibble with FrameIndex, ObjectID, CenterX_m, CenterY_m, optional angle col
# - frame_col, id_col, x_col, y_col, angle_col (string names)
# - maxgap: maximum number of consecutive missing frames to interpolate
interpolate_tracks <- function(df,
                               frame_col = "FrameIndex",
                               id_col = "ObjectID",
                               x_col = "CenterX_m",
                               y_col = "CenterY_m",
                               angle_col = "MovingAvgAngle_deg",
                               maxgap = 1) {
  # ensure frame col numeric and sorted
  df <- df %>% arrange(.data[[id_col]], .data[[frame_col]])
  all_ids <- unique(df[[id_col]])
  out_list <- vector("list", length(all_ids))
  i <- 1
  for (id in all_ids) {
    sub <- df %>% filter(.data[[id_col]] == id)
    frmin <- min(sub[[frame_col]], na.rm = TRUE)
    frmax <- max(sub[[frame_col]], na.rm = TRUE)
    full_idx <- tibble(!!frame_col := seq(frmin, frmax))
    sub2 <- full_idx %>%
      left_join(sub, by = frame_col)
    # ensure id column exists
    sub2[[id_col]] <- id
    # interpolate x,y,angle with maxgap
    sub2[[x_col]] <- na.approx(sub2[[x_col]], maxgap = maxgap, na.rm = FALSE)
    sub2[[y_col]] <- na.approx(sub2[[y_col]], maxgap = maxgap, na.rm = FALSE)
    if (angle_col %in% names(sub2)) {
      sub2[[angle_col]] <- na.approx(sub2[[angle_col]], maxgap = maxgap, na.rm = FALSE)
    }
    out_list[[i]] <- sub2
    i <- i + 1
  }
  out <- bind_rows(out_list)
  # place columns consistently
  return(out)
}

# frames_presence_matrix:
# Build binary presence matrix (rows = ObjectID, columns = FrameIndex) using x_col as presence indicator
frames_presence_matrix <- function(df_interp, id_col = "ObjectID", frame_col = "FrameIndex", x_col = "CenterX_m") {
  pres <- df_interp %>%
    mutate(pres = ifelse(!is.na(.data[[x_col]]), 1, 0)) %>%
    select(all_of(c(id_col, frame_col, "pres"))) %>%
    pivot_wider(names_from = all_of(frame_col), values_from = pres, values_fill = 0)
  rownames_pres <- pres[[id_col]]
  M <- pres %>% select(-all_of(id_col)) %>% as.matrix()
  rownames(M) <- rownames_pres
  return(M)
}


# extract information from filed path
extract_path_info_regex <- function(file_path) {
  # Regex explanation:
  # Data_processing/ matches the fixed part of the path
  # (.*?)/           matches and captures the 'student' part (non-greedy match up to the next /)
  # (.*?)/           matches and captures the 'site' part
  # output/          matches the fixed 'output' folder
  # (.*?)/           matches and captures the 'flight_altitude' part
  pattern <- "Data_processing/(.*?)/(.*?)/output/(.*?)/"
  
  # str_match returns a matrix; the first column is the full match, 
  # subsequent columns are the captured groups.
  matches <- str_match(file_path, pattern)
  
  # Return the captured groups as a vector (columns 2, 3, and 4)
  return(as.vector(matches[c(3,4)]))
}





# ---------------------------
# Per-frame metric functions
# ---------------------------

# 1. MPD_f: Mean Pairwise Distance per frame.
# Formula: MPD = mean_{i<j} d_{ij}
# Rationale: direct measure of group spatial compactness. Lower = tighter group.
# Biological meaning: smaller MPD indicates cohesive group behavior.
compute_mpd_frame <- function(points_df) {
  # points_df: tibble with CenterX_m, CenterY_m for that frame
  pts <- points_df %>% select(CenterX_m, CenterY_m) %>% drop_na()
  n <- nrow(pts)
  if (n < 2) return(NA_real_)
  dvec <- as.vector(dist(as.matrix(pts)))
  mpd <- mean(dvec)
  return(mpd)
}


# 7. Minimum Spanning Tree (MST) mean edge length
# Formula: construct MST on pairwise distances; MST_mean = mean(edge lengths)
# Rationale: captures topological connectedness, robust to outliers.
# Biological meaning: low MST mean suggests close functional connectivity among individuals.
compute_mst_mean_frame <- function(points_df) {
  pts <- points_df %>% select(CenterX_m, CenterY_m) %>% drop_na()
  n <- nrow(pts)
  if (n < 2) return(NA_real_)
  dmat <- as.matrix(dist(as.matrix(pts)))
  g <- graph_from_adjacency_matrix(dmat, mode = "undirected", weighted = TRUE, diag = FALSE)
  mst_g <- mst(g, weights = E(g)$weight)
  weights <- E(mst_g)$weight
  if (length(weights) == 0) return(0)
  return(mean(weights))
}


# 8. Heading Similarity / Directional Coordination
# We compute two related measures:
#  - Heading_MRL: mean resultant length (MRL) of unit heading vectors (range 0..1)
#    Formula: R = || sum_i (u_i) || / n, where u_i = unit vector of heading angle
#  - Heading_pairwise_similarity: 1 - (mean absolute angular difference / pi), scaled 0..1
# Rationale: captures alignment across group in heading direction.
# Biological meaning: high values indicate traveling/aligned states; low values indicate milling/divergence.
compute_heading_similarity_frame <- function(points_df, angle_col = "MovingAvgAngle_deg") {
  if (!(angle_col %in% names(points_df))) return(list(MRL = NA_real_, PairwiseSim = NA_real_))
  angs <- points_df[[angle_col]] %>% na.omit() %>% as.numeric()
  n <- length(angs)
  if (n == 0) return(list(MRL = NA_real_, PairwiseSim = NA_real_))
  # convert to radians
  rad <- angs * pi / 180
  # Mean resultant length
  R <- sqrt((sum(cos(rad)))^2 + (sum(sin(rad)))^2) / n
  # pairwise absolute angular differences
  if (n >= 2) {
    radmat <- abs(outer(rad, rad, "-"))
    # wrap angles > pi
    radmat <- pmin(radmat, 2*pi - radmat)
    iu <- which(upper.tri(radmat), arr.ind = TRUE)
    diffs <- radmat[iu]
    pair_mean_diff <- mean(diffs, na.rm = TRUE)
    pairwise_sim <- 1 - (pair_mean_diff / pi) # scale to 0..1
  } else {
    pairwise_sim <- NA_real_
  }
  return(list(MRL = R, PairwiseSim = pairwise_sim))
}

# 9. Breathing / Surfacing Synchrony
# Input: either a dedicated Breath column (binary 0/1), or we use presence (detected => 1) as proxy.
# Per-frame synchrony S(t) = fraction of dyads that share the same binary state (0 or 1).
# Range 0..1, where 1 = all same state (all up or all down).
compute_breathing_synchrony_frame <- function(points_df, breath_col = NULL) {
  # points_df should contain ObjectID and either breath_col or presence proxy from CenterX_m
  if (!is.null(breath_col) && breath_col %in% names(points_df)) {
    states <- points_df[[breath_col]]
  } else {
    # presence proxy: 1 = detected (we assume detected at surface), 0 otherwise
    states <- ifelse(!is.na(points_df$CenterX_m), 1, 0)
  }
  states <- na.omit(states)
  n <- length(states)
  if (n <= 1) return(NA_real_)
  # compute fraction of dyads with equal state
  # Number equal dyads:
  tot_pairs <- choose(n, 2)
  eq_pairs <- 0
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if (states[i] == states[j]) eq_pairs <- eq_pairs + 1
    }
  }
  return(eq_pairs / tot_pairs)
}


# 12. Angular Velocity (mean turning rate per frame)
# Formula: Angular_Velocity = |θₜ - θₜ₋₁| / Δt
# Rationale: indicates turning rate and behavioral flexibility; requires ordered data per individual.
# Biological meaning: compare between habitats (open vs. confined); higher = more turning.
# Note: This computes per-individual angular changes within a frame if multiple angles exist.
# Reference: Proposed metrics from research framework PDF
compute_angular_velocity_frame <- function(points_df, angle_col = "MovingAvgAngle_deg", dt = 1) {
  if (!(angle_col %in% names(points_df))) return(NA_real_)
  angs <- points_df[[angle_col]] %>% na.omit() %>% as.numeric()
  n <- length(angs)
  if (n < 2) return(NA_real_)
  
  # Compute angular changes (handling wraparound)
  ang_diffs <- numeric(n-1)
  for (i in 1:(n-1)) {
    diff_raw <- abs(angs[i+1] - angs[i])
    ang_diffs[i] <- ifelse(diff_raw > 180, 360 - diff_raw, diff_raw)
  }
  return(mean(ang_diffs) / dt)
}


# ---------------------------
# Core driver functions
# ---------------------------

# compute_metrics_per_frame:
# given an interpolated df (FrameIndex, ObjectID, CenterX_m, CenterY_m, MovingAvgAngle_deg )
# returns a tibble with per-frame metrics
compute_metrics_per_frame <- function(df_interp,
                                      frame_col = "FrameIndex",
                                      id_col = "ObjectID",
                                      x_col = "CenterX_m",
                                      y_col = "CenterY_m",
                                      angle_col = "MovingAvgAngle_deg",
                                      breath_col = NULL,
                                      Nmin = 2) {
  # per-frame grouping
  frames <- sort(unique(df_interp[[frame_col]]))
  out <- vector("list", length(frames))
  k <- 1
  for (f in frames) {
    sub <- df_interp %>% filter(.data[[frame_col]] == f)
    # optionally filter only rows where x,y present
    pts <- sub %>% filter(!is.na(.data[[x_col]]) & !is.na(.data[[y_col]]))
    N <- nrow(pts)
    rec <- tibble(FrameIndex = f, N = N)
    if (N < Nmin) {
      rec <- rec %>% mutate(MPD = NA_real_, MNN = NA_real_, CHAI = NA_real_,
                            CentroidDist = NA_real_, StandardDist = NA_real_, 
                            DispersionIndex = NA_real_, MST_mean = NA_real_,
                            Heading_MRL = NA_real_, Heading_pairSim = NA_real_,
                            SynchronyIndex = NA_real_, CircularVariance = NA_real_,
                            AngularVelocity = NA_real_, BreathSync = NA_real_)
    } else {
      rec <- rec %>% mutate(
        MPD = compute_mpd_frame(pts),
        MST_mean = compute_mst_mean_frame(pts)
      )
      # heading metrics
      heading_res <- compute_heading_similarity_frame(pts, angle_col = angle_col)
      rec$Heading_MRL <- heading_res$MRL
      rec$Heading_pairSim <- heading_res$PairwiseSim
      rec$AngularVelocity <- compute_angular_velocity_frame(pts, angle_col = angle_col)
      # breathing/surfacing
      rec$BreathSync <- compute_breathing_synchrony_frame(sub, breath_col = breath_col)
    }
    out[[k]] <- rec
    k <- k + 1
  }
  outdf <- bind_rows(out)
  return(outdf)
}

# sliding_window_summary:
# window_sec (seconds), 
# fps (frames per second of the original video)
# slide_step_frames (how many frames to jump between windows)
# summary_funs: which statistics to compute (defaults median for central tendency)
sliding_window_summary <- function(frame_metrics,
                                   fps = 30,
                                   window_sec = 5,
                                   slide_step_frames = NULL,
                                   whole_video = FALSE) {
  
  # OPTION 1: Compute metrics over the entire video
  if (whole_video) {
    sub <- frame_metrics   # the entire dataset
    
    return(tibble(
      WindowStart = min(sub$FrameIndex),
      WindowEnd   = max(sub$FrameIndex),
      # Number of individuals
      N_med = median(sub$N, na.rm = TRUE),
      N_mean = mean(sub$N, na.rm = TRUE),
      N_sd = sd(sub$N, na.rm = TRUE),
      # Spatial Cohesion: MPD
      MPD_med = median(sub$MPD, na.rm = TRUE),
      MPD_mean = mean(sub$MPD, na.rm = TRUE),
      MPD_sd = sd(sub$MPD, na.rm = TRUE),
      # Spatial Cohesion: MST
      MST_med = median(sub$MST_mean, na.rm = TRUE),
      MST_mean = mean(sub$MST_mean, na.rm = TRUE),
      MST_sd = sd(sub$MST_mean, na.rm = TRUE),
      # Heading coordination: MRL
      Heading_MRL_med = median(sub$Heading_MRL, na.rm = TRUE),
      Heading_MRL_mean = mean(sub$Heading_MRL, na.rm = TRUE),
      Heading_MRL_sd = sd(sub$Heading_MRL, na.rm = TRUE),
      # Heading coordination: Angular velocity
      AngularVelocity_med = median(sub$AngularVelocity, na.rm = TRUE),
      AngularVelocity_mean = mean(sub$AngularVelocity, na.rm = TRUE),
      AngularVelocity_sd = sd(sub$AngularVelocity, na.rm = TRUE),
      # Diving synchrony: surface breathing
      BreathSync_med = median(sub$BreathSync, na.rm = TRUE),
      BreathSync_mean = mean(sub$BreathSync, na.rm = TRUE),
      BreathSync_sd = sd(sub$BreathSync, na.rm = TRUE)
    ))
  }
  
  # OPTION 2: Sliding windows
  if (is.null(slide_step_frames))
    slide_step_frames <- max(1, floor((fps * window_sec) / 2))
  
  window_size <- fps * window_sec
  frame_ids <- sort(frame_metrics$FrameIndex)
  starts <- seq(min(frame_ids), max(frame_ids), by = slide_step_frames)
  
  out <- list()
  idx <- 1
  
  for (s in starts) {
    e <- s + window_size - 1
    sub <- frame_metrics %>% filter(FrameIndex >= s & FrameIndex <= e)
    
    if (nrow(sub) == 0) next
    
    rec <- tibble(
      WindowStart = s,
      WindowEnd = e,
      WindowStart = min(sub$FrameIndex),
      WindowEnd   = max(sub$FrameIndex),
      # Number of individuals
      N_med = median(sub$N, na.rm = TRUE),
      N_mean = mean(sub$N, na.rm = TRUE),
      N_sd = sd(sub$N, na.rm = TRUE),
      # Spatial Cohesion: MPD
      MPD_med = median(sub$MPD, na.rm = TRUE),
      MPD_mean = mean(sub$MPD, na.rm = TRUE),
      MPD_sd = sd(sub$MPD, na.rm = TRUE),
      # Spatial Cohesion: MST
      MST_med = median(sub$MST_mean, na.rm = TRUE),
      MST_mean = mean(sub$MST_mean, na.rm = TRUE),
      MST_sd = sd(sub$MST_mean, na.rm = TRUE),
      # Heading coordination: MRL
      Heading_MRL_med = median(sub$Heading_MRL, na.rm = TRUE),
      Heading_MRL_mean = mean(sub$Heading_MRL, na.rm = TRUE),
      Heading_MRL_sd = sd(sub$Heading_MRL, na.rm = TRUE),
      # Heading coordination: Angular velocity
      AngularVelocity_med = median(sub$AngularVelocity, na.rm = TRUE),
      AngularVelocity_mean = mean(sub$AngularVelocity, na.rm = TRUE),
      AngularVelocity_sd = sd(sub$AngularVelocity, na.rm = TRUE),
      # Diving synchrony: surface breathing
      BreathSync_med = median(sub$BreathSync, na.rm = TRUE),
      BreathSync_mean = mean(sub$BreathSync, na.rm = TRUE),
      BreathSync_sd = sd(sub$BreathSync, na.rm = TRUE)
    )
    
    out[[idx]] <- rec
    idx <- idx + 1
  }
  
  return(bind_rows(out))
}



# ---------------------------
# Master function that runs everything
# ---------------------------
# compute_group_metrics:
# - csv_path: path to input csv
# - fps: frames per second of the video (check with exiftool)
# - sliding = TRUE/FALSE: whether to compute sliding window summaries 
# - window_sec:  length of the sliding window, in seconds: should the metrics be averaged over this window?
# - whole_video = TRUE/FALSE: should the metrics be averaged by the entire video (ie, whole csv)?
# - interp_gap: pass to interpolate_tracks(); should gaps in detection be interpolated, and by how many frame?
# - Nmin: minimum number of detections in the frame to compute the metrics; default is 2 individuals, it will be NA for a single detection
# - angle_col, breath_col: column names if present
# frame_selection: input: a vector with 2 values, min and max frame number. E.g. c(0, 100) to analyse only the first 100 frames of the video (ie, only the 100 rows of the csv file)

compute_group_metrics <- function(csv_path,
                                  frame_selection = NA,
                                  fps = 30,
                                  window_sec = 5,
                                  whole_video = FALSE,
                                  interp_gap = 1,
                                  Nmin = 2,
                                  angle_col = "MovingAvgAngle_deg",
                                  breath_col = NULL,
                                  sliding = TRUE,
                                  slide_step_frames = NULL) {
  # Read
  df_raw <- read_csv(csv_path)
  req_cols <- c("FrameIndex", "ObjectID", "CenterX_m", "CenterY_m")
  missing <- setdiff(req_cols, names(df_raw))
  if (length(missing) > 0) stop(paste("Missing required columns:", paste(missing, collapse = ", ")))
  
  # apply frame restrictions (rows)
  if(!is.na(frame_selection[1])){
    df_raw = df_raw[df_raw$FrameIndex>frame_selection[1] &
                      df_raw$FrameIndex<frame_selection[2], ]
  } 
  
  # Interpolate short gaps
  df_interp <- interpolate_tracks(df_raw,
                                  frame_col = "FrameIndex",
                                  id_col = "ObjectID",
                                  x_col = "CenterX_m",
                                  y_col = "CenterY_m",
                                  angle_col = angle_col,
                                  maxgap = interp_gap)
  
  # Per-frame metrics
  frame_metrics <- compute_metrics_per_frame(df_interp,
                                             frame_col = "FrameIndex",
                                             id_col = "ObjectID",
                                             x_col = "CenterX_m",
                                             y_col = "CenterY_m",
                                             angle_col = angle_col,
                                             breath_col = breath_col,
                                             Nmin = Nmin)
  
  # sliding window
  window_metrics <- if (sliding) sliding_window_summary(frame_metrics, 
                                                        fps = fps, 
                                                        window_sec = window_sec, 
                                                        slide_step_frames = slide_step_frames,
                                                        whole_video = whole_video) else NULL
  
  # pairwise presence overlap median (synchrony proxy)
  pres_mat <- frames_presence_matrix(df_interp, 
                                     id_col = "ObjectID", 
                                     frame_col = "FrameIndex", 
                                     x_col = "CenterX_m")
  pair_overlaps <- c()
  if (nrow(pres_mat) >= 2) {
    for (i in 1:(nrow(pres_mat)-1)) {
      for (j in (i+1):nrow(pres_mat)) {
        a <- pres_mat[i,]; b <- pres_mat[j,]
        denom <- sum((a==1)|(b==1))
        pair_overlaps <- c(pair_overlaps, ifelse(denom==0, NA_real_, sum((a==1)&(b==1))/denom))
      }
    }
  }
  pair_overlap_median <- ifelse(length(pair_overlaps)>0, median(pair_overlaps, na.rm = TRUE), NA_real_)
  
  
  return(list(
    df_interp = df_interp,
    frame_metrics = frame_metrics,
    window_metrics = window_metrics,
    pair_overlap_median = pair_overlap_median
  ))
}
