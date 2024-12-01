# model_refinements.R

# Load necessary libraries
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("car", quietly = TRUE)) install.packages("car")
if (!requireNamespace("MASS", quietly = TRUE)) install.packages("MASS")

library(dplyr)
library(car)
library(MASS)

# Step 1: Load the refined dataset
refined_data <- read.csv("refined_data.csv", stringsAsFactors = FALSE)

# Step 2: Log-transform variables to handle heteroscedasticity
refined_data <- refined_data %>%
  mutate(
    Log_Internet_Usage = log(Internet_Usage + 1),
    Log_Access_Electricity = log(Access_Electricity + 1),
    Log_Poverty_Rate = log(Poverty_Rate + 1)
  )

# Step 3: Drop highly collinear variables
refined_data <- refined_data %>%
  dplyr::select(-c(Access_Electricity_x_Gini, Gini_Index))

# Step 4: Build a new regression model
refined_model <- lm(
  Log_Internet_Usage ~ Log_Access_Electricity + Log_Poverty_Rate + Poverty_Quartile,
  data = refined_data
)

# Step 5: Evaluate the model
refined_model_summary <- summary(refined_model)
print(refined_model_summary)

# Step 6: Check for multicollinearity
refined_vif_values <- vif(refined_model)
print(refined_vif_values)

# Step 7: Save the updated model summary and VIF values to text files
if (!dir.exists("outputs")) dir.create("outputs")
capture.output(refined_model_summary, file = "outputs/refined_model_summary.txt")
capture.output(refined_vif_values, file = "outputs/refined_vif_values.txt")

# Step 8: Save residual diagnostics plots
png("outputs/refined_residual_diagnostics.png", width = 1000, height = 800)
par(mfrow = c(2, 2))
plot(refined_model)
dev.off()

# Step 9: Save the refined dataset (optional)
write.csv(refined_data, "outputs/refined_data_with_log_transformations.csv", row.names = FALSE)

# End of model_refinements.R
