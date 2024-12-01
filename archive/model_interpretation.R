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

# Interpretation for Log_Poverty_Rate
cat("1. Log_Poverty_Rate:\n")
cat("- Coefficient: ", coef(refined_model)["Log_Poverty_Rate"], "\n")
cat("- Interpretation: A 1% increase in poverty rate (log-transformed) is associated with a decrease in log-internet usage, emphasizing the negative impact of poverty on internet access and usage.\n\n")

# Interpretation for Mobile_Cellular_Subscriptions_Centered
cat("2. Mobile_Cellular_Subscriptions_Centered:\n")
cat("- Coefficient: ", coef(refined_model)["Mobile_Cellular_Subscriptions_Centered"], "\n")
cat("- Interpretation: An increase in mobile cellular subscriptions positively impacts internet usage, demonstrating the crucial role of mobile connectivity in internet access.\n\n")

# Interpretation for Poverty_Gap
cat("3. Poverty_Gap:\n")
cat("- Coefficient: ", coef(refined_model)["Poverty_Gap"], "\n")
cat("- Interpretation: A higher poverty gap is associated with a significant reduction in internet usage, indicating disparities based on economic inequality.\n\n")

# Interpretation for Communications_Imports
cat("4. Communications_Imports:\n")
cat("- Coefficient: ", coef(refined_model)["Communications_Imports"], "\n")
cat("- Interpretation: A positive coefficient indicates that higher imports of communication-related services correlate with increased internet usage, reflecting the importance of technological infrastructure investments.\n\n")

# Interpretation for Poverty_Quartile
cat("5. Poverty_Quartile:\n")
cat("- Coefficient: ", coef(refined_model)["Poverty_Quartile"], "\n")
cat("- Interpretation: Internet usage varies significantly across poverty quartiles, with lower poverty quartiles demonstrating higher usage, highlighting socio-economic stratification in access.\n\n")

# Step 4: Residual Analysis
cat("Analyzing Residuals and Outliers:\n\n")

# Cook's Distance for identifying influential points
cooksd <- cooks.distance(refined_model)
cat("Top Influential Points (based on Cook's Distance):\n")
influential_points <- as.numeric(names(cooksd)[(cooksd > 4 / nrow(refined_data))])
print(influential_points)

# High Leverage Points
cat("\nHigh Leverage Points:\n")
leverage <- hatvalues(refined_model)
high_leverage_points <- which(leverage > 2 * (length(coef(refined_model)) / nrow(refined_data)))
print(high_leverage_points)

# Save residual diagnostics
capture.output(list(
  cooks_distance = cooksd,
  high_leverage_points = high_leverage_points
), file = "outputs/residual_analysis_updated.txt")

# Step 5: Final Refinement (Optional)
refined_data_no_outliers <- refined_data[-influential_points, ]
final_model <- lm(Log_Internet_Usage ~ Log_Poverty_Rate + Mobile_Cellular_Subscriptions_Centered +
                    Poverty_Gap + Communications_Imports + Poverty_Quartile, 
                  data = refined_data_no_outliers)
final_model_summary <- summary(final_model)
capture.output(final_model_summary, file = "outputs/final_model_summary_updated.txt")

# Step 6: Update Diagnostic Plots
png("outputs/final_model_diagnostics_updated.png", width = 1000, height = 800)
par(mfrow = c(2, 2))
plot(final_model)
dev.off()

cat("\nFinal interpretation and diagnostics completed. Results saved to 'outputs' directory.\n")
