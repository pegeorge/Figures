## create representative graph #######

#####           Set the working directory where the original dataset is located
setwd("C:/Users/pegeorg/OneDrive - Emory University/Papers and Projects/K application/")
list.files()

rm(list = ls())


# Load necessary libraries
library(ggplot2)

# Create a data frame with time (years before and after HU initiation) and effect sizes
treated_effect_size = c(0.01, 0.015, 0, -0.05, -0.1, -0.09, -0.14, -0.17, -0.19)
untreated_effect_size = c(0.02, -0.01, 0, -0.01, 0.01, 0.02, 0.015, -0.005, 0)

data <- data.frame(
  time = rep(-3:5, 2),  # years before and after HU initiation
  effect_size = c(treated_effect_size, untreated_effect_size),  # effect sizes
  group = rep(c("Treated with HU", "Not Treated with HU"), each = 9),  # group indicator
  se = rep(c(0.05, 0.05), each = 9)  # standard errors
)

# Create the event study graph
ggplot(data, aes(x = time, y = effect_size, color = group)) +
  geom_errorbar(aes(ymin = effect_size - se, ymax = effect_size + se), width = 0.2) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Years Before and After HU Initiation", y = "HU effect on hospital days per year",
       title = "Example Graph of Expected Outcomes", 
       subtitle = "Event Study Graph: Effect Sizes of HU Over Time") +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) + 
  theme(legend.position = c(0.26, 0.21))
