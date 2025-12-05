# dolphin_group_metrics.R
# title: "Dolphin-group-dynamics-metrics"
# author: "Mauricio Cantor and João Valle-Pereira"
# date: "2025-12-04"
# 
# compute metrics for all csv files in a folder and prepare database

# Read all csv files in a folder
# Example for CSV files in a folder a working directory
folder_path <- "/Volumes/LABIRINTO/Data_processing/JuliaPierry/Cananeia/output/20m"
file_list <- list.files(path = folder_path,
                        pattern = "\\.csv$",
                        recursive = TRUE,     # Search within subdirectories
                        full.names = TRUE)

file_list

# # Read CSV files into a list of data frames
# data_list <- lapply(file_list, read.csv)
# # rename list objects with the file name
# names(data_list) = basename(file_list)

# Loop metric function across files, saving only the dataframes with the aggregated metrics per entire videos
result_list = list()
for(i in 1:length(file_list)){
 aux1 = compute_group_metrics(csv_path = file_list[[i]],
                                           whole_video = TRUE,
                                           fps = 30, 
                                           sliding = TRUE, 
                                           window_sec = NA,  
                                           slide_step_frames = NULL,
                                           interp_gap = 0, 
                                           Nmin = 1,
                                           angle_col = "MovingAvgAngle_deg",
                                           breath_col = NULL, 
                                           return_plots = FALSE)
 # add video name and save
 aux2 = cbind(Video = gsub("_output.csv", "", names(data_list)[i]),
              aux1$window_metrics)
 result_list[[i]] = aux2
}

# Combine all into a dataframe
result_full_videos = as.data.frame(dplyr::bind_rows(result_list))



