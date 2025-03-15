# Load necessary libraries
library(lme4)
library(lmerTest)
library(ggplot2)
library(lattice)  # Load the package


# set working directory
setwd("path_to_directory")

# Load the data
data <- read.csv("path_to_data")

# Remove measurement errors
data_clean <- data[data$outlier != "x" & data$sl >= 2000, ]

# Add square root-transformed Area
data_clean$sqrt_area <- sqrt(data_clean$area)

# Plot the data with linear regression lines fitted by group
plot <- ggplot(data_clean, aes(x = sl, y = sqrt_area, color = group)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "SL vs. sqrt(Area) with Linear Fits by Group",
    x = "SL (Standard Length)",
    y = "sqrt(Area)",
    color = "Group"
  ) +
  theme_minimal()

# Print the plot
print(plot)

# Fit the mixed-effects ANCOVA
# Fixed effects: SL and Group
# Random effect: Experiment
model <- lmer(sqrt_area ~ sl + group + (1 | experiment), data = data_clean)

# Summarize the model
summary(model)

# Optional: Test interaction between SL and Group
model_interaction <- lmer(sqrt_area ~ sl * group + (1 | experiment), data = data_clean)
summary(model_interaction)


#######################################
##### check SL ########################
#######################################

# Fit a mixed-effects model for SL
model_sl <- lmer(sl ~ group + (1 | experiment), data = data_clean)

# Summarize the model
summary(model_sl)

# Perform pairwise comparisons
# Install and load the emmeans package if not already installed
if (!requireNamespace("emmeans", quietly = TRUE)) install.packages("emmeans")
library(emmeans)

# Pairwise comparisons of SL between groups
pairwise_comparisons <- emmeans(model_sl, pairwise ~ group)
print(pairwise_comparisons)

#######################################
##### check normality #################
#######################################

# Plot residuals to check normality
plot(resid(model_sl))  # Residuals vs. fitted values
qqnorm(resid(model_sl))  # Q-Q plot
qqline(resid(model_sl))  # Add reference line for normality

# Perform a Shapiro-Wilk test for normality of residuals
shapiro.test(resid(model_sl))



# Residuals vs. fitted values plot
plot(fitted(model_sl), resid(model_sl),
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red")  # Add a horizontal reference line


