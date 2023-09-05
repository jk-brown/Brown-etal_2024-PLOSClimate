# 1 Load packages and matilda data ---------------------------------------------------------

library(matilda)
library(ggplot2)
library(ggpubr)
library(tidyverse)

# Only run if needed -- load matilda data 
m_result <- read.csv("data/weighted_score_bayesian_default.csv")

# 2 Functions for this script (may already be loaded) -----------------------------

# 2.1 Write function to normalize result data to reference period
normalize_temperature <- function(data, reference_start_year, reference_end_year) {
  
  # Filter data for the reference period
  reference_period <- subset(data,
                             year >= reference_start_year &
                               year <= reference_end_year)
  
  # Calculate the mean values of reference period
  mean_reference_period <- mean(reference_period$value)
  
  # Calculate normalized values for each year in the data set
  ## subtract the reference period mean by data values
  normalized_values <- data$value - mean_reference_period
  
  # Create a new data frame with the normalized data
  normalized_data <- data.frame(year = data$year, 
                                adjusted_value = normalized_values)
  
  return(normalized_data)
}

# 2.2 Write function for computing weighted qunatiles (may already be loaded)
weighted_quantile <- function(values, weights, probs) {
  if (length(values) != length(weights)) {
    stop("Length of 'values' and 'weights' vectors must be the same.")
  }
  
  if (any(weights < 0)) {
    stop("All 'weights' must be non-negative.")
  }
  
  if (any(probs < 0) || any(probs > 1)) {
    stop("All 'probs' must be between 0 and 1.")
  }
  
  # Order data values and weights  
  order_index <- order(values)
  ordered_values <- values[order_index]
  ordered_weights <- weights[order_index]
  
  # compute the sum of the weights - tells us the proportion of the total weight
  # that lies below each value
  cumulative_weight <- cumsum(ordered_weights)
  
  # sum total weight - for matilda, this will = 1 
  total_weight <- sum(weights)
  
  quantiles <- (cumulative_weight) / total_weight
  approx_quantile <- approxfun(quantiles, ordered_values, 
                               method = "constant", 
                               yleft = ordered_values[1], 
                               yright = ordered_values[length(ordered_values)])
  
  return(approx_quantile(probs))
}

# 3 Normalizing to 1850-1900 reference period -------------------------------

# Normalize full Matilda result to 1850-1900 reference period
data_adjusted <- normalize_temperature(m_result, 
                                       reference_start_year = 1850,
                                       reference_end_year = 1900)
m_result$value_adjusted = data_adjusted$adjusted_value

# 4 Computing df of weighted quantiles ------------------------------------

# median warming calculation
median_warming <- m_result %>%
  mutate(scenario = factor(scenario)) %>%
  group_by(scenario, year) %>%
  summarize(
    median_warming_wt = weighted_quantile(value_adjusted, weights, probs = 0.5),
    per_5 = weighted_quantile(value_adjusted, weights, probs = 0.05),
    per_95 = weighted_quantile(value_adjusted, weights, probs = 0.95))

head(median_warming)

# filter data for plotting
temp_data <- subset(median_warming, 
                    year >= 2023 &
                      year <= 2100)

head(temp_data)

# median warming calculation
median_warming_his <- m_result %>%
  mutate(scenario = factor(scenario)) %>%
  group_by(scenario, year) %>%
  summarize(
    median_warming_wt = weighted_quantile(value_adjusted, weights, probs = 0.5),
    per_5 = weighted_quantile(value_adjusted, weights, probs = 0.05),
    per_95 = weighted_quantile(value_adjusted, weights, probs = 0.95))

head(median_warming_his)

# Create a data set of modeled historical data to plot
hist_data <- subset(median_warming_his,
                    scenario == "SSP3-7.0" &
                    year >= 1849 &
                      year <= 2023)

head(hist_data)

# 5 Plotting warming projections (gmst) for each scenario -----------------

# Plot the data using ggplot2
gmst_projection <- ggplot() +
  geom_line(data = temp_data, aes(x = year, y = median_warming_wt, color = scenario),
            linewidth = 1) +
  geom_ribbon(data = temp_data, aes(x = year, ymin = per_5, ymax = per_95, 
                                    fill = scenario), alpha = 0.1) +
  scale_fill_manual(values = c("#00a9cf",
                               "#003466",
                               "#f69320",
                               "#df0000")) +
  scale_color_manual(
    values = c("#00a9cf",
               "#003466",
               "#f69320",
               "#df0000"),
    name = "Scenario"
  ) +
  labs(title = "Median Temperature Projections (5-95% percentile)",
       x = "Year",
       y = expression("Temperature Anomaly (" * degree * "C)")) +
  theme_light() +
  guides(fill = guide_legend(title = "Scenario"))  # Add legend for the CI
gmst_projection

# adding historical to plot
gmst_projection_with_historic <- gmst_projection +
  geom_line(
    data = hist_data,
    aes(x = year, y = median_warming_wt),
    color = "black",
    linewidth = 1.5) +
  geom_ribbon(data = hist_data,
              aes(x = year, ymin = per_5, ymax = per_95),
              alpha = 0.2)
gmst_projection_with_historic



