# 1 Loading packages and data ---------------------------------------------
library(ggplot2)
library(ggpubr)
library(matilda)
library(parallel)

# 2 Generate parameters and run model -------------------------------------

# initiate hector core
ssp119 <- newcore(system.file("input/hector_ssp119.ini", package = "hector"),
                  name = "SSP1-1.9")

# generate parameters
n = 1000
set.seed(245)
params <- generate_params(core, n)
params_list <- split(params, 1:10)

# run hector
cl <- makeCluster(detectCores() - 1)

clusterExport(cl, c("ini", "newcore", "iterate_model", "params_list", "options"))

result <- parLapply(cl, params_list, function(chunk) {
  
  options(matilda.verbose = FALSE)
  
  core = newcore(ini)
  
  iterate_model(core,
                chunk,
                save_years = 1950:2023,
                save_vars = "gmst")
})

# combining results to have continuous run_number
for (i in 2:length(result)) {
  # calculate the max value of the previous element in the result list
  max_run_number <- max(result[[i - 1]]$run_number)
  # Add the max value of the previous element to the run_number of the current
  # element to get an updated run_number that is continuous from the previous element.
  result[[i]]$run_number <- result[[i]]$run_number + max_run_number
}

# use rbind to bind the dfs in result to make one df of all results
result_df <- do.call(rbind, result)
row.names(result_df) <- NULL

# 3 Compute RMSE values for plotting --------------------------------------

# creating a result matrix that can be used in in RMSE_calc
result_matrix <- hector_matrix(result_df)
temp_data <- criterion_gmst_obs()$obs_values
rmse_input_matrix <- cbind(temp_data, result_matrix)

# indicate that observed data are in the first column of the matrix
obs_data <- rmse_input_matrix[, 1]
model_data <- rmse_input_matrix[, -1]

# Compute RMSE values for each model
rmse_vals <- apply(model_data, 2, function(model) RMSE_calc(model, obs_data))
rmse_vals2 <- apply(model_data, 2, function(model) RMSE_calc(model, obs_data))

# 4 Weight w/ score_bayesian using different sensitivity ------------------

# Compute likelihood for each model
likelihood <- score_bayesian(rmse_input_matrix, sensitivity = 1)
likelihood2 <- score_bayesian(rmse_input_matrix, sensitivity = 2)

# 5 Plotting Likelihood vs. RMSE ------------------------------------------

# Create a data frame for plotting
plot_data <- data.frame(
  RMSE = rmse_vals,
  Likelihood = likelihood
)

plot_data2 <- data.frame(
  RMSE = rmse_vals2,
  Likelihood = likelihood2
)

# Plot RMSE v. Likelihood
Likelihood_decay <- 
  ggplot() +
  geom_line(data = plot_data, 
            aes(x = RMSE, y = Likelihood), 
            color = "blue",
            linewidth = 1) +
  geom_point(data = plot_data, 
             aes(x = RMSE, y = Likelihood), 
             color = "black",
             shape = 1,
             size = 2) +
  geom_line(data = plot_data2, 
            aes(x = RMSE, y = Likelihood), 
            color = "red",
            linewidth = 1) +
  geom_point(data = plot_data2,
             aes(x = RMSE, y = Likelihood),
             color = "black",
             shape = 1,
             size = 2) +
  theme_light()

# Display plot
Likelihood_decay

# Save the plot
ggsave(
  "figures/figure_05.tiff",
  Likelihood_decay,
  device = "tiff",
  height = 10,
  width = 11,
  units = "cm",
  dpi = 300
)
