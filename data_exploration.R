# Load necessary libraries
library(dplyr)
library(ggplot2)
library(GGally)
library(tidyr)

# Load the cleaned dataset
data_cleaned <- read.csv("data_cleaned.csv", stringsAsFactors = FALSE)

# Rename columns for simplicity
data_cleaned <- data_cleaned %>%
  rename(
    Internet_Usage = `Individuals.using.the.Internet....of.population.`,
    Access_Electricity = `Access.to.electricity....of.population.`,
    Gini_Index = `Gini.index`,
    Poverty_Rate = `Poverty.headcount.ratio.at..2.15.a.day..2017.PPP.....of.population.`
  )

# Step 1: Summary Statistics
print("Dataset Structure:")
print(str(data_cleaned))

print("Summary Statistics:")
print(summary(data_cleaned))

# Step 2: Check for Missing Values
missing_summary <- data_cleaned %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Missing_Count")

print("Missing Value Summary:")
print(missing_summary)

# Step 3: Correlation Matrix
numeric_data <- data_cleaned %>% select_if(is.numeric)
correlation_matrix <- cor(numeric_data, use = "complete.obs")
print("Correlation Matrix:")
print(correlation_matrix)

# Plot 1: Correlation Matrix
correlation_plot <- ggcorr(correlation_matrix, label = TRUE) +
  ggtitle("Correlation Matrix of Numeric Variables")
ggsave("correlation_matrix.png", plot = correlation_plot, width = 8, height = 6)

# Step 4: Univariate Analysis
# Plot 2: Histogram of Internet Usage
internet_usage_plot <- ggplot(data_cleaned, aes(x = Internet_Usage)) +
  geom_histogram(fill = "blue", bins = 20, alpha = 0.7) +
  labs(
    title = "Distribution of Internet Usage",
    x = "Internet Usage (% of population)",
    y = "Count"
  )
ggsave("internet_usage_distribution.png", plot = internet_usage_plot, width = 8, height = 6)

# Step 5: Bivariate Analysis
# Plot 3: Scatterplot of Internet Usage vs Gini Index
internet_vs_gini_plot <- ggplot(data_cleaned, aes(x = Gini_Index, y = Internet_Usage)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Internet Usage vs Gini Index",
    x = "Gini Index",
    y = "Internet Usage (%)"
  )
ggsave("internet_vs_gini.png", plot = internet_vs_gini_plot, width = 8, height = 6)

# Plot 4: Scatterplot of Internet Usage vs Access to Electricity
internet_vs_electricity_plot <- ggplot(data_cleaned, aes(x = Access_Electricity, y = Internet_Usage)) +
  geom_point(color = "green", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Internet Usage vs Access to Electricity",
    x = "Access to Electricity (%)",
    y = "Internet Usage (%)"
  )
ggsave("internet_vs_electricity.png", plot = internet_vs_electricity_plot, width = 8, height = 6)

# Step 6: Boxplot Analysis
# Plot 5: Boxplot of Internet Usage by Poverty Quartiles
data_cleaned <- data_cleaned %>%
  mutate(Poverty_Quartile = ntile(Poverty_Rate, 4))

internet_by_poverty_plot <- ggplot(data_cleaned, aes(x = as.factor(Poverty_Quartile), y = Internet_Usage)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  labs(
    title = "Internet Usage by Poverty Quartiles",
    x = "Poverty Quartile",
    y = "Internet Usage (%)"
  )
ggsave("internet_by_poverty_quartiles.png", plot = internet_by_poverty_plot, width = 8, height = 6)

# End of Script
