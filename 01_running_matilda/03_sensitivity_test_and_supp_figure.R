# Load Libraries 
remotes::install_github("jgcri/matilda", force = T)
library(matilda)
library(parallel)
library(ggplot2)

# set-up core
ini_list <- list(
  ssp119 = system.file("input/hector_ssp119.ini", package = "hector"),
  ssp126 = system.file("input/hector_ssp126.ini", package = "hector"),
  ssp245 = system.file("input/hector_ssp245.ini", package = "hector"),
  ssp370 = system.file("input/hector_ssp370.ini", package = "hector")
)
core_params <- newcore(ini_list[[4]])

# sample params 
n = 1000
set.seed(245)
init_param <- generate_params(core_params, n)

# split params for parallel computing
params_list <- split(init_param, 1:100)

# run the model with parallel computing
cl <- makeCluster(detectCores() - 1)
clusterExport(cl, c("params_list", 
                    "ini_list",
                    "newcore",
                    "iterate_model"))
# run the model with parallel computing
start <- proc.time()
init_result <- parLapply(cl, names(ini_list), function(name) {
  
  scenario <- ini_list[[name]]
  
  core <- newcore(scenario, name = name)
  
  result_list <- list()
  
  for (i in seq_along(params_list)) {
    result_list[[i]] <- iterate_model(core = core, 
                                      params = params_list[[i]],
                                      save_years = 1800:2100,
                                      save_vars = c("CO2_concentration", "gmst"))
  }
  
  for (i in 2:length(result_list)) {
    
    # calculate the max value of the previous element in the result list
    max_run_number <- max(result_list[[i - 1]]$run_number)
    
    # Add the max value of the previous element to the run_number of the current 
    # element to get an updated run_number that is continuous from the previous element.
    result_list[[i]]$run_number <- result_list[[i]]$run_number + max_run_number
    
  }
  
  result <- do.call(rbind, result_list)
  
  return(result)
  
})

stopCluster(cl)
proc.time() - start

# compute scores using CO2 and gmst
scores <- lapply(init_result, function(df) {
  
  scores_co2 = score_runs(df,
                          criterion = criterion_co2_obs(),
                          score_bayesian)
  scores_co2= na.omit(scores_co2)
  
  scores_gmst = score_runs(df, 
                           criterion = criterion_gmst_obs(),
                           score_bayesian)
  scores_gmst = na.omit(scores_gmst)
  
  list = list(scores_co2, scores_gmst)
  
})

# evenly weighted multi-criterion scoring 
even_mc_scores <- lapply(scores, function(score_list) {
  
  mc_weight_df <- multi_criteria_weighting(scores_list = score_list)
  
})

# 80-20 temp dominated mc scoring
temp_dom_score <- lapply(scores, function(score_list) {
  
  temp_dom_weight <- multi_criteria_weighting(scores_list = score_list, 
                                              criterion_weights = c(0.20, 0.80))
})

# 80-20 co2 dominated mc scoring
co2_dom_score <- lapply(scores, function(score_list) {
  
  co2_dom_weight <- multi_criteria_weighting(scores_list = score_list, 
                                              criterion_weights = c(0.80, 0.20))
})

# define the metric of interest
long_term_metric <- new_metric(var = GMST(), years = 2080:2100, mean)

# compute metrics for each scenario
metric_result <- lapply(init_result, function(df) {
  
  result_na_rm <- na.omit(df)
  
  metric_calc(result_na_rm, long_term_metric)
})

# Adding a scenario column to each metric df
scenario_identifiers <- c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0")

metric_result <- Map(function(df, scenario) {
  df$scenario <- scenario
  return(df)
}, metric_result, scenario_identifiers)

# merge scores with metrics
metric_even_scored <- Map(merge, metric_result, even_mc_scores, by = "run_number")

metric_temp_dom_scored <- Map(merge, metric_result, temp_dom_score, by = "run_number")

metric_co2_dom_scored <- Map(merge, metric_result, co2_dom_score, by = "run_number")

# Establish bins for sorting probabilities
bins <- c(1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, Inf)

# Compute probabilities for even weighting
prob_even_wt <- lapply(metric_even_scored, function(df){
  prob_calc(metrics = df$metric_result,
            bins = bins,
            scores = df$mc_weight)
})
# Adding scenario column
prob_even_wt <- Map(function(df, col_name) {
  df$scenario <- col_name
  return(df)
}, prob_even_wt, scenario_identifiers)

# Compute probabilities for temperature dominated weighting
prob_temp_dom_wt <- lapply(metric_temp_dom_scored, function(df){
  prob_calc(metrics = df$metric_result,
            bins = bins,
            scores = df$mc_weight)
})
# Adding scenario column
prob_temp_dom_wt <- Map(function(df, col_name) {
  df$scenario <- col_name
  return(df)
}, prob_temp_dom_wt, scenario_identifiers)

# Computing probabilities for Co2 dominated weighting
prob_co2_dom_wt <- lapply(metric_co2_dom_scored, function(df){
  prob_calc(metrics = df$metric_result,
            bins = bins,
            scores = df$mc_weight)
})
# Adding scenario column
prob_co2_dom_wt <- Map(function(df, col_name) {
  df$scenario <- col_name
  return(df)
}, prob_co2_dom_wt, scenario_identifiers)

# bind the probabilities and add weighting case label
even_weighting_df <- do.call(rbind, prob_even_wt)
even_weighting_df$case <- rep(c("Temperature:CO2 (50:50)"))

temp_dom_weighting_df <- do.call(rbind, prob_temp_dom_wt)
temp_dom_weighting_df$case <- rep(c("Temperature:CO2 (80:20)"))

co2_dom_weighting_df <- do.call(rbind, prob_co2_dom_wt)
co2_dom_weighting_df$case <- rep(c("Temperature:CO2 (20:80)"))

# Probabilities data frame for plotting
prob_sensitivity_df <- rbind(even_weighting_df, temp_dom_weighting_df, co2_dom_weighting_df)

# plotting the sensitivity test
# Change the colnames to make is easier to produce an aesthetic plot
colnames(prob_sensitivity_df) <- c(
  "Warming",
  "Score",
  "Probability",
  "Scenario",
  "Scoring_Case"
)

prob_sensitivity_plot <- ggplot(
  prob_sensitivity_df,
  aes(
    fill = Warming,
    y = Probability,
    x = Scenario
  )
) +
  geom_bar(
    position = position_fill(reverse = T),
    stat = "identity",
    width = 0.6
  ) +
  scale_y_continuous(breaks = seq(0, 1.0, 0.1)) +
  scale_fill_manual(
    values = c(
      "#2166AC",
      "#4393C3",
      "#D1E5F0",
      "#FDDBC7",
      "#F4A582",
      "#D6604D",
      "#B2182B",
      "#67001F"
    ),
    labels = c(
      "1.0 to 1.5 C",
      "1.5 to 2.0 C",
      "2.0 to 2.5 C",
      "2.5 to 3.0 C",
      "3.0 to 3.5 C",
      "3.5 to 4 C",
      "4 to 4.5 C",
      ">4.5 C"
    )
  ) +
  coord_flip() +
  ggtitle("Scoring Sensitivity Test") +
  theme_light() +
  theme(
    legend.text = element_text(size = 8),
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  facet_wrap(~Scoring_Case)
prob_sensitivity_plot

ggsave(
  "figures/figure_S1.tiff",
  plot = prob_sensitivity_plot,
  device = "tiff",
  height = 8,
  width = 22,
  units = "cm",
  dpi = 300
)
