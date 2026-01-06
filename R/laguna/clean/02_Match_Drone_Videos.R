# R/laguna/clean/02_Match_Events_With_Video_Metadata.R
# Match field observation events to drone video frames

library(tidyverse)
library(lubridate)

# ==============================================================================
# Extract video start time from metadata
# ==============================================================================

get_video_metadata <- function(video_path) {
  
  cat(sprintf("Reading metadata: %s\n", basename(video_path)))
  
  if (!file.exists(video_path)) {
    warning(sprintf("Video not found: %s", video_path))
    return(NULL)
  }
  
  # Pull multiple timestamp fields from EXIF
  exif_cmd <- sprintf(
    'exiftool -CreateDate -DateTimeOriginal -MediaCreateDate -TrackCreateDate -Duration -s3 "%s"',
    video_path
  )
  
  exif_output <- system(exif_cmd, intern = TRUE)
  
  cat("  Raw exiftool output:\n")
  for (line in exif_output) {
    cat(sprintf("    %s\n", line))
  }
  
  # Parse first valid timestamp (DJI typically uses MediaCreateDate or TrackCreateDate)
  for (line in exif_output) {
    datetime_match <- str_extract(line, "\\d{4}:\\d{2}:\\d{2} \\d{2}:\\d{2}:\\d{2}")
    if (!is.na(datetime_match)) {
      video_start_utc <- ymd_hms(datetime_match, tz = "UTC")
      cat(sprintf("  Parsed start time (UTC): %s\n", video_start_utc))
      return(video_start_utc)
    }
  }
  
  warning("Could not parse video start time")
  return(NA)
}


# ==============================================================================
# Alternate method: Calculate start from known end time
# ==============================================================================

# Video end time from PowerShell: 5/21/2024 7:25:34 AM UTC
# Use CSV frame range to calculate start time

get_video_start_from_csv_and_end <- function(csv_filepath, 
                                             video_end_utc,
                                             fps = 30) {
  
  cat(sprintf("Calculating video start from: %s\n", basename(csv_filepath)))
  
  drone_data <- read_csv(csv_filepath, show_col_types = FALSE)
  
  # Frame range in CSV
  min_frame <- min(drone_data$FrameIndex)
  max_frame <- max(drone_data$FrameIndex)
  total_frames <- max_frame - min_frame
  
  cat(sprintf("  Frame range: %d to %d\n", min_frame, max_frame))
  cat(sprintf("  Total frames: %d\n", total_frames))
  
  # Convert frames to time
  duration_sec <- total_frames / fps
  duration_min <- duration_sec / 60
  
  cat(sprintf("  Duration: %.2f minutes\n", duration_min))
  
  # Work backwards from end time
  video_start_utc <- video_end_utc - seconds(duration_sec)
  
  cat(sprintf("  Video start (UTC): %s\n", video_start_utc))
  cat(sprintf("  Video end (UTC): %s\n", video_end_utc))
  
  return(list(
    start_utc = video_start_utc,
    end_utc = video_end_utc,
    min_frame = min_frame,
    max_frame = max_frame,
    fps = fps
  ))
}


# ==============================================================================
# Calculate which frame an event occurs at
# ==============================================================================

calculate_event_frame <- function(event_datetime_utc,
                                  video_start_utc,
                                  video_min_frame = 0,
                                  fps = 30) {
  
  # Seconds between video start and event
  time_diff_sec <- as.numeric(difftime(
    event_datetime_utc,
    video_start_utc,
    units = "secs"
  ))
  
  # Convert to frames
  frame_offset <- round(time_diff_sec * fps)
  
  # Adjust for video's frame numbering
  event_frame <- video_min_frame + frame_offset
  
  cat(sprintf("  Time difference: %.2f seconds (%.2f minutes)\n", 
              time_diff_sec, time_diff_sec/60))
  cat(sprintf("  Frame offset: %d\n", frame_offset))
  cat(sprintf("  Event frame: %d\n", event_frame))
  
  return(event_frame)
}


# ==============================================================================
# Test workflow
# ==============================================================================

# Known video end time from file properties
VIDEO_END_UTC <- ymd_hms("2024-05-21 07:25:34", tz = "UTC")

# Calculate video timing from CSV
video_info <- get_video_start_from_csv_and_end(
  csv_filepath = "./data/raw/laguna/20240521_DJI_0007_output_laguna.csv",
  video_end_utc = VIDEO_END_UTC,
  fps = 30
)

# Load field observations
field_obs <- read_csv("./data/processed/field_observations_analyzable.csv")

# Get first event from May 21 with drone
test_event <- field_obs %>%
  filter(date == ymd("2024-05-21"), has_drone == TRUE) %>%
  head()

cat("\n=== TEST EVENT ===\n")
cat(sprintf("Event ID: %s\n", test_event$event_id))
cat(sprintf("Event time (local): %s\n", test_event$event_datetime_local))
cat(sprintf("Event time (UTC): %s\n", test_event$event_datetime_utc))
cat(sprintf("Expected dolphins: %d\n", test_event$n_dolphins))
cat(sprintf("Buzz occurred: %s\n", test_event$buzz_occurred))

# Find event frame
event_frame <- calculate_event_frame(
  event_datetime_utc = test_event$event_datetime_utc,
  video_start_utc = video_info$start_utc,
  video_min_frame = video_info$min_frame,
  fps = 30
)

cat(sprintf("\n✓ Event frame: %d\n", event_frame))

# Verify frame is in video
if (event_frame >= video_info$min_frame && event_frame <= video_info$max_frame) {
  cat("✓ Frame exists in video!\n")
} else {
  cat("✗ Frame outside video range!\n")
  cat(sprintf("  Video range: %d to %d\n", video_info$min_frame, video_info$max_frame))
}
