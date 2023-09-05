# 1 Loading packages ------------------------------------------------------

#libraries
library(ggplot2)
library(ggpubr)
library(matilda)

# 2 Run Matilda and weight results ------------------------------------------

# Use unweighted Matilda result to compute weights with each scoring algorithm. 
matilda_result_25 <- read.csv("data/matilda_result_25.csv")

# using score_ramp
weights_sr <- score_runs(matilda_result_25, criterion_co2_obs(), score_ramp, 0, 10)

# using score_bayesian
weights_sb <- score_runs(matilda_result_25, criterion_co2_obs(), score_bayesian)

# creating observed data frame for plotting
observed_data <- matilda:::observed_data_co2

# 3 Plot Weighted CO2 projections -----------------------------------------

# Merge weights with unweighted Matilda results
# Score ramp results df
matilda_weighted_sr <- merge(matilda_result_25, weights_sr, by = "run_number")
# Score bayesian results df
matilda_weighted_sb <- merge(matilda_result_25, weights_sb, by = "run_number")

# Plotting CO2 projections for score_ramped results
plot_score_ramp <- ggplot(subset(matilda_weighted_sr,
                          year > 1959 & year < 2100
                          & scenario == "SSP2-4.5")) +
  geom_line(aes(x = year, y = value,
                group = run_number,
                color = weights, 
                alpha = weights),
            linewidth = 0.5) +
  scale_color_gradient(high = "dodgerblue4", low = "lightblue1",
                       name = "Weight", limits = c(0, 0.11)) +
  scale_alpha_continuous(range = c(0.5, 1)) +
  geom_line(data = observed_data,
            aes(year, co2_ppm),
            color = "red",
            linewidth = 0.7) +
  ylab(expression(CO[2]~Concentration~(ppm))) +
  xlab("Year") +
  labs(title = "A) score_ramp()") + 
  theme_light() +
  theme(legend.position = "bottom") +
  guides(alpha = "none")
plot_score_ramp

# Savig score_ramp plot
ggsave(
  "figures/figure_04_A.png",
  plot_score_ramp,
  device = "png",
  height = 12,
  width = 16,
  units = "cm",
  dpi = 300)

# Plotting CO2 projections for score_bayesian results
plot_score_bayesian <- ggplot(subset(matilda_weighted_sb,
                              year > 1959 & year < 2100
                              & scenario == "SSP2-4.5")) +
  geom_line(aes(x = year, y = value,
                group = run_number,
                color = weights, 
                alpha = weights),
            linewidth = 0.5) +
  scale_color_gradient(high = "dodgerblue4", low = "lightblue1",
                       name = "Weight", limits = c(0, 0.11)) +
  scale_alpha_continuous(range = c(0.5, 1)) +
  geom_line(data = matilda:::observed_data_co2,
            aes(year, co2_ppm),
            color = "red",
            linewidth = 0.7) +
  ylab(NULL) +
  xlab("Year") +
  labs(title = "B) score_bayesian()") + 
  theme_light() +
  theme(legend.position = "none") +
  guides(alpha = "none")
plot_score_bayesian

# save score_bayesian results
ggsave(
  "figures/figure_04_B.png",
  plot_score_bayesian,
  device = "png",
  height = 12,
  width = 16,
  units = "cm",
  dpi = 300)


# 4 Creating paneled figure ----------------------------------------------

# Combine plots with ggarrange
figure_04 <- ggarrange(plot_score_ramp, plot_score_bayesian,
                          ncol = 2, nrow = 1, common.legend = T, legend = "right")

# save paneled figure 
ggsave(
  "figures/figure_04_paneled.tiff",
  figure_04,
  device = "tiff",
  height = 6.5,
  width = 13.2,
  units = "cm",
  dpi = 300)
