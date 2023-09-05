#' Compute RMSE data frame from unweighted Matilda result
#'
#' @param data An unweighted matilda result with NAs omitted.
#' @param year_start What year to start the data subset - Must be within the year 
#' range of the observed data. Here I am using observed GMST (i.e., 1950-2023)
#' @param year_end What year to end the data subset- Must be within the year 
#' range of the observed data. Here I am using observed GMST (i.e., 1950-2023)
#'
#' @return returns a data frame of RMSE values and associated run numbers.
#' @export
#'
RMSE_df <- function(data, year_start, year_end) {
  subset_data <- subset(
    data,
    year >= year_start &
      year <= year_end
  )
  
  # Create observed data frame
  obs_dat <- data.frame(
    year = criterion_gmst_obs()$year,
    value_obs = criterion_gmst_obs()$obs_values
  )
  
  # Split data by scenario
  scenario_split <- split(subset_data, subset_data$scenario)
  
  # Initialize a list to store RMSE dataframes
  rmse_list <- list()
  
  # Iterate through scenarios
  for (scenario_name in names(scenario_split)) {
    scenario_df <- scenario_split[[scenario_name]]
    
    # Split data within the scenario by run_number
    rmse_split <- split(scenario_df, scenario_df$run_number)
    
    # Calculate RMSE for each run_number and store in a list
    rmse_vals <- lapply(rmse_split, function(df) {
      RMSE_calc(df$value, obs_dat$value_obs)
    })
    
    # Combine scenario and run_number information
    scenario_run_numbers <- rep(as.factor(names(rmse_vals)), lengths(rmse_vals))
    
    # Combine RMSE values for the scenario
    rmse_values <- unlist(rmse_vals)
    
    # Combine all information into a dataframe
    scenario_rmse_df <- data.frame(
      scenario = rep(scenario_name, length(rmse_values)),
      run_number = scenario_run_numbers,
      RMSE = rmse_values
    )
    
    # Append the dataframe to the list
    rmse_list <- append(rmse_list, list(scenario_rmse_df))
  }
  
  # Combine all dataframes into a single dataframe
  rmse_data <- do.call(rbind, rmse_list)
  
  return(rmse_data)
}

