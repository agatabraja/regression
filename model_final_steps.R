# model_final_steps.R

# Load necessary libraries
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("car", quietly = TRUE)) install.packages("car")

library(dplyr)
library(car)

# Step 1: Load the refined dataset
refined_data <- read.csv("outputs/refined_data_with_log_transformations.csv", stringsAsFactors = FALSE)

# Step 2: Recreate the refined model (if needed)
# Recreate the model using the refined dataset
refined_model <- lm(
  Log_Internet_Usage ~ Log_Access_Electricity + Log_Poverty_Rate + Poverty_Quartile,
  data = refined_data
)

# Step 3: Save the recreated model
save(refined_model, file = "outputs/refined_model.RData")

# Step 4: Model evaluation
# Generate and save the summary of the refined model
refined_model_summary <- summary(refined_model)
capture.output(refined_model_summary, file = "outputs/refined_model_summary.txt")

# Step 5: Check multicollinearity
vif_values <- vif(refined_model)
capture.output(vif_values, file = "outputs/refined_vif_values.txt")

# Step 6: Residual diagnostics
# Save residual diagnostics plots
png("outputs/refined_residual_diagnostics.png", width = 1000, height = 800)
par(mfrow = c(2, 2))
plot(refined_model)
dev.off()

# Step 7: Save the refined data
write.csv(refined_data, "outputs/refined_data_with_log_transformations.csv", row.names = FALSE)

# End of script
