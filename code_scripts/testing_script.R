library(matilda)
library(ggplot2)

ini <- system.file("input/hector_ssp245.ini", package = "hector")
hcore <- newcore(ini)

params1 <- generate_params(hcore, 100)

results1 <- iterate_hector(hcore, params1)

score1 <- score_hruns(results1, criterion_tas_obs(), score_bayesian)

hector_score1 <- merge(results1, score, by = "run_number")

plot <- ggplot(subset(hector_score1,
                              year > 1850 & year < 2100
                              & variable == GLOBAL_TAS())) +
  geom_line(aes(x = year, y = value,
                group = run_number,
                color = weights, 
                alpha = weights),
            linewidth = 0.5) +
  scale_color_gradient(high = "dodgerblue4", low = "lightblue1",
                       name = "Weight") +
  scale_alpha_continuous(range = c(0.5, 1)) +
  geom_line(data = matilda:::metricdata_tas,
            aes(year, anomaly_C),
            color = "red",
            linewidth = 0.7) +
  ylab("temp") +
  xlab("Year") +
  theme_light() +
  guides(alpha = "none")
plot


metric_def <- new_metric(GLOBAL_TAS(), years = 2081:2100, mean)
met <- metric_calc(results1, metric_def)

bins = c(0, 1.5, 2.1, 2.5, 3.0, 3.5, 4, Inf)

prob1 <- prob_calc(met$metric_result, bins, scores = score1$weights)
prob1
