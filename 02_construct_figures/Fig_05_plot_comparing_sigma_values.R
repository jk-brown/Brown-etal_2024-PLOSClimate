# 1 Loading packages and data ---------------------------------------------
library(ggplot2)
library(ggpubr)
library(matilda)

# 2 Load data -------------------------------------------------------------

matilda_1k <- read.csv("data/matilda_1k.csv")
anyNA(matilda_1k) # check for NAs

# Omit NAs in the matilda_1k data frame
matilda_1k <- na.omit(matilda_1k)

# 3 Compute RMSE for SSP126 for Plotting ----------------------------------

# load function needed for computing RMSE data frame
source("functions/functions_for_analysis.R")

# Creating usable rmse_data to plot
rmse_data <- RMSE_df(matilda_1k, 1950, 2023)
rmse_data <- subset(rmse_data, scenario == "SSP1-1.9")

# 4 Weight PPE members w/ score_bayesian(sigma = default) -------------------------------------------------

# subset Matilda result to include only SSP119
matilda_subset <- subset(matilda_1k, scenario == "SSP1-1.9")

# Compute weights (posterior probabilities) across list of Hector results
# (m_result_split) using observed gmst and the score_bayesian algorithm with the
# default level of sigma.
weight1 <- score_runs(
  matilda_subset,
  criterion_gmst_obs(),
  score_bayesian
)

# Merge results so each Hector run in m_result_split is assigned its corresponding
# weight.
rmse_data_weighted_1 <- merge(rmse_data, weight1, by = "run_number")

# 5 Weight PPE members w/ score_bayesian(sigma = 2 * sd(RMSE)) -------------------------------------------------

# Compute 2 units of standard deviation of the RMSE values
sd2 <- 2 * sd(matilda:::adjusted_gmst_data$anomaly_C)

# Compute weights (posterior probabilities) across list of Hector results
# (m_result_split) using observed gmst and the score_bayesian algorithm with the
# default level of sigma.
weight2 <- score_runs(matilda_subset,
  criterion_gmst_obs(),
  score_bayesian,
  sigma = sd2
)

# Merge results so each Hector run in m_result_split is assigned its corresponding
# weight.
rmse_data_weighted_2 <- merge(rmse_data, weight2, by = "run_number")

# 6 Plotting likelihood decay comparison ----------------------------------

# 6.1 Plotting weight decay against RMSE - decay line in blue and models as points
# Create the first plot with y-axis limits and custom tick labels
weight_decay <- ggplot() +
  geom_line(
    data = rmse_data_weighted_2,
    aes(
      x = RMSE,
      y = weights
    ),
    color = "red",
    linewidth = 1
  ) +
  geom_point(
    data = rmse_data_weighted_2,
    aes(
      x = RMSE,
      y = weights
    ),
    size = 3,
    shape = 1
  ) +
  geom_line(
    data = rmse_data_weighted_1,
    aes(
      x = RMSE,
      y = weights
    ),
    color = "blue",
    linewidth = 1
  ) +
  geom_point(
    data = rmse_data_weighted_1,
    aes(
      x = RMSE,
      y = weights
    ),
    size = 3,
    shape = 1
  ) +
  labs(y = "Likelihood") +
  theme_light() +
  scale_y_continuous(breaks = seq(0.0002, 0.0015, by = 0.0001))
# Display the plot
weight_decay

# Save the plot
ggsave(
  "figures/figure_05.tiff",
  weight_decay,
  device = "tiff",
  height = 10,
  width = 10,
  units = "cm",
  dpi = 300
)
