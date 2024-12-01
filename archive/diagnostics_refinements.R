# diagnostics_refinements.R

# Load necessary libraries
if (!requireNamespace("car", quietly = TRUE)) install.packages("car")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("lmtest", quietly = TRUE)) install.packages("lmtest")
library(dplyr)
library(car)
library(lmtest)

# Step 1: Load the refined dataset
refined_data <- read.csv("refined_data.csv", stringsAsFactors = FALSE)

# Step 2: Build the initial weighted regression model
initial_model <- lm(Internet_Usage ~ Access_Electricity + Poverty_Rate + Gini_Index +
                      Log_Poverty_Rate + Mobile_Cellular_Subscriptions + Poverty_Gap +
                      Communications_Imports + Access_Electricity_x_Gini +
                      Poverty_Gap_x_Electricity + Communications_x_Internet +
                      Internet_x_Electricity + Mobile_Cellular_x_Gini + Poverty_Quartile,
                    data = refined_data)

# Step 3: Adjust weights calculation
# Fix weights calculation to address zero or extreme values
adjusted_fitted_values <- pmax(abs(initial_model$fitted.values), 1e-6) # Ensure no negatives
weights <- 1 / (adjusted_fitted_values^2) # Stabilize weights

# Step 4: Build the adjusted weighted regression model
weighted_model <- lm(Internet_Usage ~ Access_Electricity + Poverty_Rate + Gini_Index +
                       Log_Poverty_Rate + Mobile_Cellular_Subscriptions + Poverty_Gap +
                       Communications_Imports + Access_Electricity_x_Gini +
                       Poverty_Gap_x_Electricity + Communications_x_Internet +
                       Internet_x_Electricity + Mobile_Cellular_x_Gini + Poverty_Quartile, 
                     data = refined_data, weights = weights)

# Step 5: Evaluate the weighted regression model
weighted_model_summary <- summary(weighted_model)
print(weighted_model_summary)

# Step 6: Save Weighted Regression Summary
if (!dir.exists("outputs")) dir.create("outputs") # Ensure outputs directory exists
capture.output(weighted_model_summary, file = "outputs/weighted_model_summary.txt")

# Step 7: Plot Weighted Residual Diagnostics
png("outputs/adjusted_weighted_residual_diagnostics.png", width = 1000, height = 800)
par(mfrow = c(2, 2))
plot(weighted_model)
dev.off()

# Step 8: Re-check weights and fitted values
weights_summary <- summary(weights)
fitted_values_summary <- summary(weighted_model$fitted.values)
print(weights_summary)
print(fitted_values_summary)

# Save weights and fitted values summaries for documentation
capture.output(weights_summary, file = "outputs/weights_summary.txt")
capture.output(fitted_values_summary, file = "outputs/fitted_values_summary.txt")

# Step 9: Breusch-Pagan Test for heteroscedasticity
bp_test <- bptest(weighted_model)
print(bp_test)
capture.output(bp_test, file = "outputs/bp_test_results.txt")

# Step 10: Re-check multicollinearity after adjustments
vif_values <- vif(weighted_model)
print(vif_values)
capture.output(vif_values, file = "outputs/vif_values_weighted.txt")

# Step 11: Save residuals and fitted values for further analysis (optional)
residuals_data <- data.frame(Residuals = residuals(weighted_model), 
                             Fitted_Values = fitted(weighted_model),
                             Weights = weights)
write.csv(residuals_data, "outputs/weighted_residuals_data.csv", row.names = FALSE)

print("Diagnostics and refinements complete. Outputs saved in the 'outputs' directory.")
