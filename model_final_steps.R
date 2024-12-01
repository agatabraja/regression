# Load necessary libraries
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("car", quietly = TRUE)) install.packages("car")
library(dplyr)
library(car)

# Step 1: Load the refined dataset
refined_data <- read.csv("outputs/refined_data_with_log_transformations.csv", stringsAsFactors = FALSE)

# Step 2: Recreate centered variables
refined_data$Mobile_Cellular_Subscriptions_Centered <- scale(refined_data$Mobile_Cellular_Subscriptions, center = TRUE, scale = FALSE)
refined_data$Mobile_Cellular_Subscriptions_Centered_Squared <- refined_data$Mobile_Cellular_Subscriptions_Centered^2

# Step 3: Identify Influential Points and High Leverage Points
cooksd <- cooks.distance(refined_model)
influential_points <- as.numeric(names(cooksd)[cooksd > 4 / nrow(refined_data)])
cat("Influential Points (High Cook's Distance):\n")
print(influential_points)

leverage <- hatvalues(refined_model)
high_leverage_points <- which(leverage > 2 * (length(coef(refined_model)) / nrow(refined_data)))
cat("High Leverage Points:\n")
print(high_leverage_points)

# Step 4: Save diagnostics information
if (!dir.exists("outputs")) dir.create("outputs")
capture.output(list(
  cooks_distance = cooksd,
  influential_points = influential_points,
  high_leverage_points = high_leverage_points
), file = "outputs/residual_analysis.txt")

# Step 5: Remove Influential Points
refined_data_no_outliers <- refined_data[-influential_points, ]

# Step 6: Fit the Final Model Without Outliers
final_model <- lm(
  Log_Internet_Usage ~ Log_Poverty_Rate +
    Mobile_Cellular_Subscriptions_Centered +
    Mobile_Cellular_Subscriptions_Centered_Squared +
    Poverty_Gap + Communications_Imports + Poverty_Quartile,
  data = refined_data_no_outliers
)

# Step 7: Evaluate the Final Model
final_model_summary <- summary(final_model)
print("Final Model Summary:\n")
print(final_model_summary)
capture.output(final_model_summary, file = "outputs/final_model_summary_updated.txt")

# Step 8: Diagnostic Plots for Final Model
png("outputs/final_model_diagnostics_updated.png", width = 1000, height = 800)
par(mfrow = c(2, 2))
plot(final_model)
dev.off()

# Step 9: Compare Models Using AIC and BIC
aic_final <- AIC(final_model)
bic_final <- BIC(final_model)

cat("AIC for Final Model:", aic_final, "\n")
cat("BIC for Final Model:", bic_final, "\n")

capture.output(list(
  AIC = aic_final,
  BIC = bic_final
), file = "outputs/model_comparison_metrics.txt")

# Optional: Save the Final Dataset Without Outliers
write.csv(refined_data_no_outliers, "outputs/refined_data_no_outliers.csv", row.names = FALSE)

cat("\nFinal model interpretation and diagnostics completed. Results saved to 'outputs' directory.\n")



colnames(refined_data)[which(colnames(refined_data) == "Mobile.cellular.subscriptions")] <- "Mobile_Cellular_Subscriptions"
colnames(refined_data)[which(colnames(refined_data) == "Poverty.gap.at..2.15.a.day..2017.PPP.....")] <- "Poverty_Gap"


refined_data$Mobile_Cellular_Subscriptions_Centered <- scale(
  refined_data$Mobile_Cellular_Subscriptions, center = TRUE, scale = FALSE
)

str(refined_data)

# Fit the final model
final_model <- lm(
  Log_Internet_Usage ~ Log_Poverty_Rate + Mobile_Cellular_Subscriptions_Centered +
    Poverty_Gap + Poverty_Quartile, 
  data = refined_data
)

# Summarize the final model
final_model_summary <- summary(final_model)
print(final_model_summary)

# Save the model summary to a file
capture.output(final_model_summary, file = "outputs/final_model_summary.txt")

# Optional: Plot diagnostics for the final model
png("outputs/final_model_diagnostics.png", width = 1000, height = 800)
par(mfrow = c(2, 2))
plot(final_model)
dev.off()





