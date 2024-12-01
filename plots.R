# Load necessary libraries
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(ggplot2)
library(dplyr)

# Step 1: Create a data frame for the timeline of iterations
timeline_data <- data.frame(
  Step = c(
    "Initial Model",
    "Addressed Multicollinearity",
    "Removed Outliers",
    "Final Refined Model"
  ),
  Description = c(
    "Basic model with initial variables.",
    "Centered variables and calculated VIF values.",
    "Removed influential points and outliers based on Cook's distance and leverage values.",
    "Final model with transformed variables and improved diagnostics."
  ),
  Iteration = 1:4
)

# Plot a timeline
timeline_plot <- ggplot(timeline_data, aes(x = Iteration, y = 1, label = Step)) +
  geom_segment(aes(xend = Iteration, yend = 0), color = "blue", size = 1) +
  geom_point(size = 4, color = "darkblue") +
  geom_text(aes(y = 1.2), size = 4, hjust = 0.5) +
  labs(
    title = "Model Development Timeline",
    x = "Iteration",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

# Save the timeline plot
ggsave("outputs/model_timeline.png", timeline_plot, width = 10, height = 5)

# Step 2: Create a data frame for AIC/BIC comparison
model_comparison <- data.frame(
  Model = c("Initial Model", "Final Refined Model"),
  AIC = c(4545.37, 4543.96),  # Replace with actual AIC values from your models
  BIC = c(4591.97, 4584.72)   # Replace with actual BIC values from your models
)

# Plot a bar chart for AIC/BIC comparison
comparison_plot <- model_comparison %>%
  pivot_longer(cols = c(AIC, BIC), names_to = "Metric", values_to = "Value") %>%
  ggplot(aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = "Comparison of AIC and BIC Between Models",
    x = "Model",
    y = "Value"
  ) +
  theme_minimal()

# Save the AIC/BIC comparison plot
ggsave("outputs/aic_bic_comparison.png", comparison_plot, width = 8, height = 5)

# Outputs
cat("Timeline and AIC/BIC comparison plots saved in 'outputs' directory.")
