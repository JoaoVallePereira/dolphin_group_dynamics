# R/laguna/clean/01_Load_Field_Observations.R
# Load and clean field observation data from master spreadsheet

library(tidyverse)
library(lubridate)

load_field_observations <- function(file_path = "data/raw/laguna/2024_DATABASE_LAGUNA__Master_Sheet_.csv") {
  
  cat("Loading field observations...\n")
  
  field_raw <- read_csv(file_path, show_col_types = FALSE)
  cat(sprintf("  Loaded %d observations\n", nrow(field_raw)))
  
  field_clean <- field_raw %>%
    # Parse dates from DMY format
    mutate(date = dmy(date_dmy)) %>%
    
    # Fix time format issues (semicolons and periods instead of colons)
    mutate(
      time_event_clean = str_replace_all(time_event_hh_mm, "[;.]", ":"),
      event_time_local = parse_time(time_event_clean, format = "%H:%M:%S")
    ) %>%
    
    # Build datetime in local timezone, then convert to UTC
    mutate(
      datetime_string = paste(as.character(date), as.character(event_time_local)),
      event_datetime_local = ymd_hms(datetime_string, tz = "America/Sao_Paulo"),
      # Brazil is UTC-3, so this adds 3 hours
      event_datetime_utc = with_tz(event_datetime_local, tz = "UTC")
    ) %>%
    
    # Extract sampling start and end times
    mutate(
      sampling_parts = str_split(sampling_start_final_time_hh_mm, "-"),
      sampling_start = map_chr(sampling_parts, ~.x[1]),
      sampling_end = map_chr(sampling_parts, ~ifelse(length(.x) > 1, .x[2], NA_character_)),
      sampling_start_time = parse_time(sampling_start, format = "%H:%M:%S"),
      sampling_end_time = parse_time(sampling_end, format = "%H:%M:%S")
    ) %>%
    
    # Recode treatment labels
    mutate(
      treatment = case_when(
        treatment == "CO" ~ "cooperative",
        treatment %in% c("FO1", "FO2", "FO3") ~ "non_cooperative",
        TRUE ~ treatment
      )
    ) %>%
    
    # Flag events with drone coverage
    mutate(
      event_number_in_drone = drone_start_final_time_hh_mm,
      has_drone = !is.na(event_number_in_drone) & event_number_in_drone != "NA"
    ) %>%
    
    # Extract buzz information
    mutate(
      buzz_occurred = !is.na(hydro_buzz) & hydro_buzz != "NA",
      n_buzzes = if_else(buzz_occurred, 1, 0)
    ) %>%
    
    # Clarify dolphin counts: site vs. event-specific
    mutate(
      # All dolphins in general area
      n_dolphins_in_area = as.numeric(dolphins_number_site),
      # Only dolphins interacting with fishers
      n_dolphins_interacting = as.numeric(dolphins_number_throw),
      # Prefer interacting count when available
      n_dolphins = coalesce(n_dolphins_interacting, n_dolphins_in_area)
    ) %>%
    
    # Fisher counts
    mutate(
      n_fishers_nets = as.numeric(fishers_number_nets),
      n_fishers_line = as.numeric(fishers_number_line)
    ) %>%
    
    # Fish catch data
    mutate(
      n_nets_with_fish = as.numeric(nets_with_fish),
      fish_catch_clean = str_replace_all(as.character(fish_catch_total), "[^0-9]", ""),
      n_fish_total = as.numeric(fish_catch_clean),
      fisher_success = coalesce(n_fish_total > 0, FALSE)
    ) %>%
    
    # Create unique event identifier
    mutate(
      event_id = paste(
        format(date, "%Y%m%d"), 
        format(event_datetime_local, "%H%M%S"),
        site,
        sep = "_"
      )
    ) %>%
    
    # Keep relevant columns
    select(
      event_id, date, site,
      event_datetime_utc, event_datetime_local,
      treatment, event_interaction,
      n_dolphins, n_dolphins_in_area, n_dolphins_interacting,
      dolphin_photoid,
      n_fishers_nets, n_fishers_line, fishers_number_tags,
      buzz_occurred, n_buzzes,
      fisher_success, n_nets_with_fish, n_fish_total,
      has_drone, event_number_in_drone,
      hydrophone_track_on_off, camera_photo, photoid_number,
      observation
    )
  
  cat(sprintf("  Cleaned %d events\n", nrow(field_clean)))
  cat(sprintf("  Cooperative events: %d (%.1f%%)\n",
              sum(field_clean$treatment == "cooperative", na.rm = TRUE),
              100 * mean(field_clean$treatment == "cooperative", na.rm = TRUE)))
  cat(sprintf("  Events with buzzes: %d (%.1f%%)\n",
              sum(field_clean$buzz_occurred, na.rm = TRUE),
              100 * mean(field_clean$buzz_occurred, na.rm = TRUE)))
  cat(sprintf("  Events with drone: %d (%.1f%%)\n",
              sum(field_clean$has_drone, na.rm = TRUE),
              100 * mean(field_clean$has_drone, na.rm = TRUE)))
  
  # Quick check that UTC conversion worked
  cat("\n  Timezone check (first event):\n")
  check <- field_clean %>% 
    filter(!is.na(event_datetime_local)) %>% 
    head(1) %>%
    select(event_datetime_local, event_datetime_utc)
  print(check)
  
  return(field_clean)
}


filter_analyzable_events <- function(field_data, 
                                     require_drone = FALSE,
                                     require_cooperative = TRUE,
                                     min_dolphins = 1) {
  
  cat("\nFiltering analyzable events...\n")
  
  filtered <- field_data %>%
    filter(
      !is.na(event_datetime_utc),
      !is.na(n_dolphins),
      n_dolphins >= min_dolphins
    )
  
  if (require_cooperative) {
    filtered <- filtered %>%
      filter(treatment == "cooperative")
  }
  
  if (require_drone) {
    filtered <- filtered %>%
      filter(has_drone == TRUE)
  }
  
  cat(sprintf("  Retained %d/%d events (%.1f%%)\n",
              nrow(filtered), nrow(field_data),
              100 * nrow(filtered) / nrow(field_data)))
  
  cat("\n  Summary:\n")
  summary_table <- filtered %>%
    group_by(treatment) %>%
    summarize(
      n_events = n(),
      n_with_buzz = sum(buzz_occurred, na.rm = TRUE),
      pct_buzz = 100 * mean(buzz_occurred, na.rm = TRUE),
      n_with_drone = sum(has_drone, na.rm = TRUE),
      pct_drone = 100 * mean(has_drone, na.rm = TRUE),
      mean_dolphins = mean(n_dolphins, na.rm = TRUE),
      .groups = "drop"
    )
  print(summary_table)
  
  return(filtered)
}

# ==============================================================================
# Run processing
# ==============================================================================

field_obs <- load_field_observations("./data/raw/laguna/2024_DATABASE_LAGUNA__Master_Sheet_.csv")

# Keep cooperative events for now, don't require drone yet
analyzable <- filter_analyzable_events(
  field_obs,
  require_drone = FALSE,
  require_cooperative = TRUE,
  min_dolphins = 1
)

# Write outputs
write_csv(field_obs, "./data/processed/field_observations_all.csv")
write_csv(analyzable, "./data/processed/field_observations_analyzable.csv")
