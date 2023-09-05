# 1 Loading libraries -----------------------------------------------------
library(ggplot2)

# 2 Creating sample data for plotting -------------------------------------

# Define a range of RMSE values to plot
rmse_vals <- seq(0, 15, length.out = 100)

# Compute the likelihood functions for e = 1, 1.5, 2, 3, 5
likelihood_d1 <- exp(-0.5 * ((rmse_vals) / 1)^2)
likelihood_d1.5 <- exp(-0.5 * ((rmse_vals) / 1.5)^2)
likelihood_d2 <- exp(-0.5 * ((rmse_vals) / 2)^2)
likelihood_d3 <- exp(-0.5 * ((rmse_vals) / 3)^2)
likelihood_d4 <- exp(-0.5 * ((rmse_vals) / 4)^2)

# Combine the likelihood functions into a data frame
likelihood_df <- data.frame(rmse = rmse_vals,
                            "sigma_1" = likelihood_d1,
                            "sigma_1.5" = likelihood_d1.5,
                            "sigma_2" = likelihood_d2,
                            "sigma_3" = likelihood_d3,
                            "sigma_4" = likelihood_d4)

# Melt the data frame into long format for plotting
likelihood_df_long <- reshape2::melt(likelihood_df,
                                     id.vars = "rmse",
                                     variable.name = "Decay_Rate",
                                     value.name = "likelihood")


# 3 Plotting --------------------------------------------------------------

# Plot the likelihood functions
likelihood_decay <-
  ggplot(likelihood_df_long,
         aes(x = rmse, y = likelihood, color = Decay_Rate)) +
  geom_line(linewidth = 1) +
  scale_color_discrete(
    name = "Decay Rate",
    labels = c("sigma = 1", "sigma = 1.5", "sigma = 2", "sigma = 3", "sigma = 4")) +
  labs(x = "RMSE", y = "Likelihood") +
  xlim(0, 15) +
  theme_light()
likelihood_decay

ggsave("figures/figure_02.tiff",
       plot = likelihood_decay,
       device = "tiff",
       height = 10,
       width = 13.2,
       units = "cm",
       dpi = 300)
