# Building figure to showcase score ramp

library(ggplot2)

# Producing random score values for the figure
set.seed(100)
data <- data.frame(x = runif(600, min = 1, max = 20),
                   y = runif(600, min = 1, max = 20))

# Writing a loop to compute scores based on socre_ramp() from matilda.
# Want to be able to produce this figure without loading the library.
for (i in data) {
  
  # compute abs differences for each index in `data`
  Differences <- abs(data$x - data$y)
  
  # define scores when differences are greater than or less than the cut off 
  # values of min = 5 and max = 10
  scores [Differences >= 10] <- 0
  scores [Differences <= 5] <- 1
  
  # define and compute how to deal with values between 5-10
  between_w1_w2 <- Differences > 5 & Differences < 10
  w1_w2_frac <- (Differences [between_w1_w2] - 5) / (10 - 5)

  # scores are one minus the fraction computed above.
  scores [between_w1_w2] <- 1 - w1_w2_frac
  
 return(scores)
  
  }

# add column of differences and a column of scores
data$Differences <- abs(data$x - data$y)
data$Score <- scores

# Figure
score_ramp <- ggplot(data = data, aes(x = Differences, y = Score)) +
  geom_line(color = "dodgerblue",
            linewidth = 1) +
  labs(x = "Absolute Difference") +
  theme_light()
score_ramp

ggsave("figures/figure_03.png",
       plot = score_ramp,
       device = "png",
       width = 10,
       height = 8,
       units = c("cm"),
       dpi = 300)
