# regression_modeling.R

# Install necessary libraries (if not installed)
if (!requireNamespace("car", quietly = TRUE)) install.packages("car")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")

# Load necessary libraries
library(dplyr)
library(car)

# Ensure the outputs directory exists
if (!dir.exists("outputs")) dir.create("outputs")

# Load necessary libraries
library(dplyr)
library(car)

# Step 1: Load the refined dataset
refined_data <- read.csv("refined_data.csv", stringsAsFactors = FALSE)

# Step 2: Build the regression model
model <- lm(Internet_Usage ~ Access_Electricity + Poverty_Rate + Gini_Index +
              Log_Poverty_Rate + Access_Electricity_x_Gini + Poverty_Quartile,
            data = refined_data)

# Step 3: Evaluate the model
model_summary <- summary(model)
print(model_summary)

# Step 4: Check for multicollinearity
vif_values <- vif(model)
print(vif_values)

# Step 5: Save the model summary and VIF values to text files
capture.output(model_summary, file = "outputs/model_summary.txt")
capture.output(vif_values, file = "outputs/vif_values.txt")

# Optional: Save VIF values
capture.output(vif_values, file = "outputs/vif_values.txt")

# Step 6: Visualize residuals (diagnostics)
par(mfrow = c(2, 2))  # Arrange plots in a grid
plot(model)

# Save residuals plot (optional)
png("outputs/residuals_plot.png")
plot(model)
dev.off()
