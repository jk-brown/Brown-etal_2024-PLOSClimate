# 1 Loading packages ------------------------------------------------------

library(matilda)
library(ggplot2)
library(ggpubr)

# 2 Processing data -------------------------------------------------------

# Only read in data if needed
matilda_weighted <- read.csv("data/matilda_weighted.csv")
anyNA(matilda_weighted) # check for NAs

# Omit NAs from the weighted Matilda result
matilda_weighted <- na.omit(matilda_weighted)

# Load data frame of weights for ensemble members
use_weights <- read.csv("data/use_weights.csv")
anyNA(use_weights)

# Omit NAs from the use_weights data frame
use_weights <- na.omit(use_weights)

# Write function to normalize Matilda data to reference period
normalize_temperature <- function(data, reference_start_year, reference_end_year) {
  # Filter data for the reference period
  reference_period <- subset(
    data,
    year >= reference_start_year &
      year <= reference_end_year
  )
  
  # Calculate the mean values of reference period
  mean_reference_period <- mean(reference_period$value)
  
  # Calculate normalized values for each year in the data set
  ## subtract data values by reference period mean
  normalized_values <- data$value - mean_reference_period
  
  # Create a new data frame with the normalized data
  normalized_data <- data.frame(
    year = data$year,
    adjusted_value = normalized_values
  )
  
  return(normalized_data)
}

# Normalize Matilda result to 1850-1900 reference period
data_adjusted <- normalize_temperature(matilda_weighted,
                                       reference_start_year = 1850,
                                       reference_end_year = 1900
)

# Add column for adjusted_values in weighted matilda result.
matilda_weighted$adjusted_value <- data_adjusted$adjusted_value

# split by scenario for metric calculation
matilda_weighted_split <- split(matilda_weighted, matilda_weighted$scenario)

# 2 Define and compute metric results -------------------------------

# Define your metric of interest.
# This is long term (IPCC) 20 year average of global
# surface temperature anomaly (GMST).
longterm_metric <- new_metric(GMST(), years = 2081:2100, op = mean)

# Computes my metric values (longterm_metric) for each hector run across all SSPs
# in the weighted_hector list
metric_result <- lapply(matilda_weighted_split, metric_calc, longterm_metric)


# Merges the calculated metrics for each run and the weights for each run by the
# run_number.
metrics_weighted <- lapply(metric_result, function(df) {
  merge(df, use_weights, by = "run_number")}) 

# 3 Compute Probabilities -------------------------------------------------

# Establish bins for sorting probabilities
bins <- c(1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, Inf)

# Computing probabilities across the list of met_and_scores dfs
probabilities <- lapply(metrics_weighted, function(x) {
  prob_calc(
    metrics = x$metric_result,
    bins,
    scores = x$weights
  )
})

# Computing probabilities does not carry over scenario names (do we want to
# fix this in matilda?)
# Define the labels to be added to each probability data frame
scenarios <- c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0")

# Use Map to add the new column to each element of the list - this will be our
# scenario column
probability_list <- Map(function(df, col_name) {
  df$scenario <- col_name
  return(df)
}, probabilities, scenarios)

# merge the probability dfs in probability_list into a single df
# This gives the probability of warming ranges for each SSP scenario in
# a single df.
probability_results <- do.call(rbind, probability_list)
rownames(probability_results) <- NULL

# 4 Plotting probability results ------------------------------------------

# Change the colnames to make is easier to produce an aesthetic plot
colnames(probability_results) <- c(
  "Warming",
  "Score",
  "Probability",
  "Scenario"
)

# Use the probability_results df to plot results
probability_plot <- ggplot(
  probability_results,
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
      "2.0 to 2.5 C", # used this bin specifically for LTE seminar example
      "2.5 to 3.0 C",
      "3.0 to 3.5 C",
      "3.5 to 4 C",
      "4 to 4.5 C",
      ">4.5 C"
    )
  ) +
  coord_flip() +
  ggtitle("B)") +
  theme_light()
probability_plot

# save resulting figure
ggsave(
  "figures/figure_06_A.png",
  plot = probability_plot,
  device = "png",
  height = 12,
  width = 24,
  units = "cm",
  dpi = 300
)
