# 1 Loading libraries -----------------------------------------------------
library(ggplot2)

# 2 Creating sample data for plotting -------------------------------------

set.seed(123)

# Define a range of RMSE values to plot
rmse_vals <- seq(0, 8, length.out = 100)

# Sensitivity Values
sensitivity_1 = 1
sensitivity_2 = 2
sensitivity_3 = 3
sensitivity_4 = 4

# Compute the likelihood functions for e = 1, 1.5, 2, 3, 5
likelihood_d1 <- exp(-0.5 * ((rmse_vals) / sensitivity_1 * sd(rmse_vals))^2)
likelihood_d2 <- exp(-0.5 * ((rmse_vals) / sensitivity_2 * sd(rmse_vals))^2)
likelihood_d3 <- exp(-0.5 * ((rmse_vals) / sensitivity_3 * sd(rmse_vals))^2)
likelihood_d4 <- exp(-0.5 * ((rmse_vals) / sensitivity_4 * sd(rmse_vals))^2)

# Combine the likelihood functions into a data frame
likelihood_df <- data.frame(
  rmse = rmse_vals,
  "Sensitivity = 1" = likelihood_d1,
  "Sensitivity = 2" = likelihood_d2,
  "Sensitivity = 3" = likelihood_d3,
  "Sensitivtiy = 4" = likelihood_d4
)

# Melt the data frame into long format for plotting
likelihood_df_long <- reshape2::melt(likelihood_df,
  id.vars = "rmse",
  variable.name = "Decay_Rate",
  value.name = "likelihood"
)


# 3 Plotting --------------------------------------------------------------

# Plot the likelihood functions
likelihood_decay <-
  ggplot(
    likelihood_df_long,
    aes(x = rmse, y = likelihood, color = Decay_Rate)
  ) +
  geom_line(linewidth = 1) +
  scale_color_discrete(
    name = "Decay Rate",
    labels = c("Sensitivity = 1", "Sensitivity = 2", "Sensitivity = 3", "Sensitivity = 4")
  ) +
  labs(x = "RMSE", y = "Likelihood") +
  xlim(0, 7) +
  theme_light()
likelihood_decay

# save figure 
ggsave("figures/figure_02.tiff",
  plot = likelihood_decay,
  device = "tiff",
  height = 10,
  width = 13.2,
  units = "cm",
  dpi = 300
)
