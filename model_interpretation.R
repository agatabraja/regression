# Load necessary libraries
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("car", quietly = TRUE)) install.packages("car")

library(dplyr)
library(car)

# Step 1: Load the refined dataset
refined_data <- read.csv("outputs/refined_data_with_log_transformations.csv", stringsAsFactors = FALSE)

# Step 2: Load the final model
load("outputs/refined_model.RData")  # Ensure this RData file exists

# Step 3: Interpretation of Findings
cat("Interpretation of the Refined Model:\n\n")

cat("1. Log_Access_Electricity:\n")
cat("- Coefficient: ", coef(refined_model)["Log_Access_Electricity"], "\n")
cat("- Indicates that a 1% increase in access to electricity is associated with a",
    round(coef(refined_model)["Log_Access_Electricity"], 2), "increase in log-internet usage.\n\n")

cat("2. Log_Poverty_Rate:\n")
cat("- Coefficient: ", coef(refined_model)["Log_Poverty_Rate"], "\n")
cat("- Suggests that higher poverty levels negatively impact log-internet usage, highlighting disparities in access.\n\n")

cat("3. Poverty_Quartile:\n")
cat("- Coefficient: ", coef(refined_model)["Poverty_Quartile"], "\n")
cat("- Indicates potential stratification in access based on poverty levels.\n\n")

# Step 4: Residual Analysis
cat("Analyzing Potential Outliers:\n\n")
cooksd <- cooks.distance(refined_model)
cat("Top Influential Points:\n")
influential <- as.numeric(names(cooksd)[(cooksd > 4/(nrow(refined_data)))])
print(influential)

cat("\nHigh Leverage Points (> 2*(p/n)):\n")
leverage <- hatvalues(refined_model)
high_leverage <- which(leverage > 2*(length(coef(refined_model))/nrow(refined_data)))
print(high_leverage)

# Save residual diagnostics
capture.output(list(
  cooks_distance = cooksd,
  high_leverage_points = high_leverage
), file = "outputs/residual_analysis.txt")

# Step 5: Optional Final Refinement
refined_data_no_outliers <- refined_data[-influential, ]
final_model <- lm(Log_Internet_Usage ~ Log_Access_Electricity + Log_Poverty_Rate + Poverty_Quartile, 
                  data = refined_data_no_outliers)
final_model_summary <- summary(final_model)
capture.output(final_model_summary, file = "outputs/final_model_summary.txt")

# Step 6: Update Diagnostic Plots
png("outputs/final_model_diagnostics.png", width = 1000, height = 800)
par(mfrow = c(2, 2))
plot(final_model)
dev.off()
