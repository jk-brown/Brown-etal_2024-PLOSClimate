# 1 Load packages and Matilda data ---------------------------------------------------------

library(matilda)
library(ggpubr)
library(ggplot2)

# 2 Processing Data -------------------------------------------------------

# Load weighted Matilda data
matilda_weighted <- read.csv("data/matilda_weighted.csv")
anyNA(matilda_weighted) # check for NAs

# Omit NAs from the weighted result 
matilda_weighted <- na.omit(matilda_weighted)

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

# convert value to adjust for reference period
matilda_weighted$value_adjusted <- data_adjusted$adjusted_value

# 3 Preparing data to plot - gmst 1950:2100 -------------------------------

# Subset to include gmst 1950:2100
gmst_1950_2100 <- subset(
  matilda_weighted,
  year >= 1950 &
    year <= 2100 &
    weights > 1e-6 # only include records with a weight > 1e-6
)

# Creates observed data frame - this  can be added as a layer to the plot
# But currently only includes data from 1950-2023
obs_dat <- data.frame(
  year = criterion_gmst_obs()$year,
  value_obs = criterion_gmst_obs()$obs_values
)


# 4 Plotting gmst projections - colored by weight -------------------------

# Plotting gmst values (adjusted for reference period)  and faceting by scenario
# Ensemble projections colored (and alpha-ed) by weight

plot_gmst <- ggplot(data = gmst_1950_2100) +
  geom_line(
    aes(
      x = year,
      y = value_adjusted,
      group = run_number,
      color = weights,
      alpha = weights
    ),
    linewidth = 0.5
  ) +
  scale_color_gradient(low = "lightblue", high = "dodgerblue4", name = "Weights") +
  scale_alpha_continuous(range(c(0, 1))) +
  labs (x = "Year", y = "Temperature (C)") +
  theme_light() +
  theme(axis.text.x = element_text(size = 8)) +
  guides(alpha = "none") +
  ggtitle("A)") +
  facet_wrap(~scenario)
plot_gmst

# save the resulting figure in the "figures" folder - no observed line
ggsave("figures/figure_06_A.tiff",
       plot_gmst,
       device = "tiff",
       height = 10,
       width = 13.2,
       units = "cm",
       dpi = 300
)

# Add observed CO2 values to aid visualization of most plausible models
plot_gmst_with_obs <- plot_gmst + geom_line(
  data = obs_dat, aes(x = year, y = value_obs),
  color = "red",
  linewidth = 1
)
plot_gmst_with_obs

# save the resulting figure in the "figures" folder
ggsave("figures/gmst_with_obs_data.tiff",
  plot_gmst_with_obs,
  device = "tiff",
  height = 10,
  width = 13.2,
  units = "cm",
  dpi = 300
)
