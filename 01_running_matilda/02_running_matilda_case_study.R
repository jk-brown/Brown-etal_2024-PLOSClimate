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
set.seed(245)
param_core <- ssp370
params <- generate_params(param_core, 1000)
param_list <- rep(list(params), length(env_list))

# Run the model across the list of SSPs using the list of parameter configurations.
# Time the analysis to determine how long it takes to run Matilda 1000x across
# four SSP scenarios.
start <- Sys.time()
m_result <- list()
for (i in 1:length(env_list)) {
  result <- iterate_model(env_list[[i]], param_list[[i]], save_vars = GMST())
  m_result[[i]] <- result
}
run_time <- Sys.time() - start
print(run_time)

# Bind the list of data frames into one large data frame result of Matilda results.
# Note: this result is not yet weighted.
matilda_1k <- do.call(rbind, m_result)

# saving unweighted Matilda result
write.csv(matilda_1k, "data/matilda_1k.csv", row.names = F)

# 2 Weighting Matilda results using score_bayesian() ----------------------------------

# Compute weights across for ensemble members using observed GMST().
# Use default level of sigma (sd(obs_data)) and default multiplier (1).
weights_df <- score_runs(
  matilda_1k,
  criterion_gmst_obs(),
  score_bayesian
)

# save data frame of weights
write.csv(weights_df, "data/use_weights.csv", row.names = F)

# 3 Merging weights with matilda_result -----------------------------------

# Merge results so each ensemble member is assigned its corresponding weight.
matilda_weighted <- merge(weights_df, matilda_1k, by = "run_number")

# save weighted Matilda result
write.csv(matilda_weighted, "data/matilda_weighted.csv", row.names = F)
