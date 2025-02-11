## METADATA ===============================================================
## Description: Support Functions for scripts at MAPAS
## East Africa Marta Project
## 
## R version: 4.4.2 for Windows
## Date: 2025-02-11 12:11:49.480917
## License: GPL3
## Author: Oskar Hagen (oskar@hagen.bio)
##=======================================================================##

compute_stats <- function(data, group = "ALL", type="extinct") {
 
   # Ensure group is one of the allowed names
  group <- match.arg(group, c("ALL", "N", "S"))
  type <- match.arg(type, c("extinct", "new"))

  # Extract the mean movement distances for extinct cells.
  # (Assuming that NS_dummy_vector[[group]]$extinct is either already a numeric vector,
  #  or a list that can be unlisted to yield one.)
  mean_movement_type <- unlist(lapply(data[[group]][[type]], mean))
  
  # For each time step, compute the sum of movement distances.
  # (sapply is used to simplify the list into a numeric vector.)

  sum_movement_type <- sapply(data[[group]]$extinct, sum)
  
  # For each time step, determine the "habitat area" as the number of cells.
  area_movement_type <- sapply(data[[group]]$extinct, length)
  
  # Option 1: Return a data frame with one row per time step:
  stats_df <- data.frame(
    timeStep = seq_along(mean_movement_type),
    mean   = unlist(mean_movement_type),
    sum    = sum_movement_type,
    area   = area_movement_type
  )
  
  # Option 2 (alternative): Return a list with the three vectors
  # stats_list <- list(
  #   timeStep = seq_along(mean_movement_type),
  #   mean     = unlist(mean_movement_type),
  #   sum      = sum_movement_type,
  #   area     = area_movement_type
  # )
  # return(stats_list)
  
  return(stats_df)
}

# Example usage:
# (This will compute the stats for group "ALL". You can change "ALL" to "N" or "S" as needed.)
# extinct_stats_ALL <- compute_extinct_stats("ALL")
# print(extinct_stats_ALL)