## Contents:  Code for running a Matilda analysis across a list of SSP scenarios.
library(matilda)

# 1 Running iterate_model() across 4 SSPs ------------------------------------

# SSPs to initiate a Hector instance
ssp119 <- newcore(system.file("input/hector_ssp119.ini", package = "hector"),
                  name = "SSP1-1.9"
)
ssp126 <- newcore(system.file("input/hector_ssp126.ini", package = "hector"),
                  name = "SSP1-2.6"
)
ssp245 <- newcore(system.file("input/hector_ssp245.ini", package = "hector"),
                  name = "SSP2-4.5"
)
ssp370 <- newcore(system.file("input/hector_ssp370.ini", package = "hector"),
                  name = "SSP3-7.0"
)

# Create a list of environments
env_list <- list(ssp119, ssp126, ssp245, ssp370)

# Generate parameters to use for all running the model.
# Only use one core generate parameter values and replicate in a list. 
# This will keep parameter configurations for each scenario identical.
set.seed(123)
param_core <- ssp370
params_25 <- generate_params(param_core, 25)
param_list_25 <- rep(list(params_25), length(env_list))

# Run the model across the list of SSPs using the list of parameter configurations.
# Time the analysis to determine how long it takes to run Matilda 1000x across
# four SSP scenarios.
start <- Sys.time()
m_result_25 <- list()
for (i in 1:length(env_list)) {
  result <- iterate_model(env_list[[i]], param_list_25[[i]], save_vars = CONCENTRATIONS_CO2())
  m_result_25[[i]] <- result
}
run_time <- Sys.time() - start
print(run_time)

# Bind the list of data frames into one large data frame result of Matilda results.
# Note: this result is not yet weighted.
matilda_result_25 <- do.call(rbind, m_result_25)

# 2 Save unweighted Matilda result ----------------------------------------

write.csv(matilda_result_25, "data/matilda_result_25.csv", row.names = F)