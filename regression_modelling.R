# Updated regression_modeling.R

# Load necessary libraries
library(dplyr)
library(car)

# Ensure outputs directory exists
if (!dir.exists("outputs")) dir.create("outputs")

# Step 1: Load the refined dataset
refined_data <- read.csv("refined_data.csv", stringsAsFactors = FALSE)

# Step 2: Simplify the regression model
model <- lm(
  Internet_Usage ~ 
    Log_Poverty_Rate + Mobile_Cellular_Subscriptions + Poverty_Gap +
    Communications_Imports + Poverty_Quartile,
  data = refined_data
)

# Step 3: Evaluate the updated model
model_summary <- summary(model)
print("Updated Model Summary:")
print(model_summary)

# Save the updated model summary
capture.output(model_summary, file = "outputs/updated_model_summary.txt")

# Step 4: Check multicollinearity
vif_values <- vif(model)
print("Updated VIF Values:")
print(vif_values)

# Save the updated VIF values
capture.output(vif_values, file = "outputs/updated_vif_values.txt")

# Step 5: Visualize residuals
par(mfrow = c(2, 2))  # Arrange plots in a grid
plot(model)

# Save residuals plot
png("outputs/updated_residuals_plot.png")
plot(model)
dev.off()

# Step 6: Save residuals for further analysis
residuals_data <- data.frame(
  Residuals = residuals(model),
  Fitted_Values = fitted(model)
)
write.csv(residuals_data, "outputs/updated_residuals_data.csv", row.names = FALSE)

# Step 7: Save Adjusted R-Squared and F-Statistic
adjusted_r_squared <- model_summary$adj.r.squared
f_statistic <- model_summary$fstatistic[1]
capture.output(
  list(Adjusted_R_Squared = adjusted_r_squared, F_Statistic = f_statistic),
  file = "outputs/updated_model_fit_metrics.txt"
)

print("Updated regression modeling complete. Outputs saved in the 'outputs' directory.")


# Add quadratic terms for potentially nonlinear relationships
refined_data <- refined_data %>%
  mutate(
    Log_Poverty_Rate_Squared = Log_Poverty_Rate^2,
    Mobile_Cellular_Subscriptions_Squared = Mobile_Cellular_Subscriptions^2
  )

# Update the model formula
model_v2 <- lm(
  Internet_Usage ~ 
    Log_Poverty_Rate + Log_Poverty_Rate_Squared +
    Mobile_Cellular_Subscriptions + Mobile_Cellular_Subscriptions_Squared +
    Poverty_Gap + Communications_Imports + Poverty_Quartile,
  data = refined_data
)

# Evaluate the new model
summary(model_v2)


# Refit model without the insignificant quadratic term
model_v3 <- lm(
  Internet_Usage ~ 
    Log_Poverty_Rate + 
    Mobile_Cellular_Subscriptions + Mobile_Cellular_Subscriptions_Squared +
    Poverty_Gap + Communications_Imports + Poverty_Quartile,
  data = refined_data
)

summary(model_v3)

# Residual diagnostics
par(mfrow = c(2, 2))  # Arrange plots
plot(model_v3)


refined_data$Internet_Usage_Adjusted <- ifelse(refined_data$Internet_Usage <= 0, 0.01, refined_data$Internet_Usage)
refined_data$Log_Internet_Usage <- log(refined_data$Internet_Usage_Adjusted)

model_v4 <- lm(
  Log_Internet_Usage ~ Log_Poverty_Rate + Mobile_Cellular_Subscriptions +
    Mobile_Cellular_Subscriptions_Squared + Poverty_Gap + Communications_Imports + 
    Poverty_Quartile,
  data = refined_data
)

summary(model_v4)
par(mfrow = c(2, 2))
plot(model_v4)

# Check for multicollinearity using VIF
library(car)
vif_values <- vif(model_v4)
print("Variance Inflation Factors (VIF):")
print(vif_values)

# Save VIF values to a file for reference
capture.output(vif_values, file = "outputs/vif_values_model_v4.txt")


# Center Mobile_Cellular_Subscriptions
refined_data$Mobile_Cellular_Subscriptions_Centered <- 
  scale(refined_data$Mobile_Cellular_Subscriptions, center = TRUE, scale = FALSE)

# Generate its squared term
refined_data$Mobile_Cellular_Subscriptions_Centered_Squared <- 
  refined_data$Mobile_Cellular_Subscriptions_Centered^2

# Refit the model with centered variables
model_v5 <- lm(
  Log_Internet_Usage ~ Log_Poverty_Rate + Mobile_Cellular_Subscriptions_Centered +
    Mobile_Cellular_Subscriptions_Centered_Squared + Poverty_Gap +
    Communications_Imports + Poverty_Quartile,
  data = refined_data
)

# Evaluate the new model
summary_v5 <- summary(model_v5)
print("Summary of the Model with Centered Variables:")
print(summary_v5)

# Save the summary
capture.output(summary_v5, file = "outputs/model_v5_summary.txt")

# Check VIF again after centering
vif_values_v5 <- vif(model_v5)
print("VIF Values after Centering:")
print(vif_values_v5)

# Save VIF values
capture.output(vif_values_v5, file = "outputs/vif_values_model_v5.txt")


# Calculate Cook's Distance
cooksd <- cooks.distance(model_v5)

# Identify influential points with Cook's Distance > 4/n
influential_points <- which(cooksd > (4 / nrow(refined_data)))
print("Influential Points (Cook's Distance > 4/n):")
print(influential_points)

# Highlight rows in the data
refined_data_influential <- refined_data[influential_points, ]
print("Data of Influential Points:")
print(refined_data_influential)

# Leverage Threshold (2*(number of predictors)/n)
leverage_threshold <- 2 * (length(model_v5$coefficients)) / nrow(refined_data)
high_leverage_points <- which(hatvalues(model_v5) > leverage_threshold)
print("High Leverage Points:")
print(high_leverage_points)

# Optional: Visualize leverage and Cook's distance
par(mfrow = c(1, 2))
plot(hatvalues(model_v5), main = "Leverage Values", ylab = "Leverage")
abline(h = leverage_threshold, col = "red", lty = 2)

plot(cooksd, main = "Cook's Distance", ylab = "Cook's Distance")
abline(h = 4 / nrow(refined_data), col = "red", lty = 2)

# Ensure influential points have been identified
print("Influential Points (High Cook's Distance):")
print(influential_points)

# Extract rows corresponding to influential points
influential_data <- refined_data[influential_points, ]
print("Details of Influential Points:")
print(influential_data)

# Remove influential points
refined_data_cleaned <- refined_data[-influential_points, ]

# Save the cleaned dataset
write.csv(refined_data_cleaned, "refined_data_cleaned.csv", row.names = FALSE)

# Build a new model without influential points
model_v6 <- lm(
  Log_Internet_Usage ~ Log_Poverty_Rate + Mobile_Cellular_Subscriptions_Centered +
    Mobile_Cellular_Subscriptions_Centered_Squared + Poverty_Gap +
    Communications_Imports + Poverty_Quartile,
  data = refined_data_cleaned
)

# Evaluate the new model
summary_v6 <- summary(model_v6)
print(summary_v6)

# Save the updated model summary
capture.output(summary_v6, file = "outputs/model_v6_summary.txt")

# Optional: Visualize diagnostics for the new model
par(mfrow = c(2, 2))
plot(model_v6)


model_v7 <- lm(
  Log_Internet_Usage ~ Log_Poverty_Rate + Mobile_Cellular_Subscriptions_Centered +
    Mobile_Cellular_Subscriptions_Centered_Squared + Poverty_Gap +
    Communications_Imports,
  data = refined_data_cleaned
)

summary_v7 <- summary(model_v7)
print(summary_v7)

# Save the summary
capture.output(summary_v7, file = "outputs/model_v7_summary.txt")

# Optional: Plot diagnostics
par(mfrow = c(2, 2))
plot(model_v7)


# Compare models using AIC and BIC
aic_v6 <- AIC(model_v6)
aic_v7 <- AIC(model_v7)
bic_v6 <- BIC(model_v6)
bic_v7 <- BIC(model_v7)

print(paste("AIC for model_v6:", aic_v6))
print(paste("AIC for model_v7:", aic_v7))
print(paste("BIC for model_v6:", bic_v6))
print(paste("BIC for model_v7:", bic_v7))



# Save the final model summary
capture.output(summary_v7, file = "outputs/final_model_v7_summary.txt")

# Save final diagnostics
png("outputs/final_model_v7_diagnostics.png", width = 1000, height = 800)
par(mfrow = c(2, 2))
plot(model_v7)
dev.off()

print("Final model outputs saved.")


