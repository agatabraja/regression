# Load necessary libraries
library(dplyr)
library(ggplot2)
library(GGally)
library(tidyr)

# Load the cleaned dataset
data_cleaned <- read.csv("data_cleaned.csv", stringsAsFactors = FALSE)


# Read the processed data and top countries list
final_data <- read.csv("outputs/final_cleaned_data.csv")
top_countries <- read.csv("outputs/top_countries.csv")

# Filter data for only the top countries
exploration_data <- final_data %>%
  filter(Country.Name %in% top_countries$x)  # Using 'x' as that's the column name in top_countries.csv

# Now you can proceed with your exploration using exploration_data
# You might want to verify the filtering worked:
print("Number of unique countries in filtered dataset:")
print(length(unique(exploration_data$Country.Name)))

# Example explorations you could do:
# 1. Basic summary statistics for key metrics
# 2. Time series analysis for different indicators
# 3. Cross-country comparisons
# 4. Correlation analysis between different variables

# First, let's check what we have in top_countries
print("Column names in top_countries:")
print(colnames(top_countries))

print("First few rows of top_countries:")
print(head(top_countries))

print("First few countries in final_data:")
print(unique(head(final_data$Country.Name)))

# Now fix the filtering
exploration_data <- final_data %>%
  filter(Country.Name %in% top_countries$X)  # Note: It might be 'X' instead of 'x' due to R's auto-naming

# Verify the filtering
print("Number of unique countries in filtered dataset:")
print(length(unique(exploration_data$Country.Name)))

# If we still have issues, we could try this alternative:
# Clean country names to ensure matching
exploration_data <- final_data %>%
  filter(trimws(Country.Name) %in% trimws(top_countries$X))

# Check the content of final_data
print("Structure of final_data:")
str(final_data)

# Check the content of top_countries
print("\nStructure of top_countries:")
str(top_countries)

print("\nFirst few rows of top_countries:")
head(top_countries)

# Let's see if we have any data in final_data at all
print("\nNumber of rows in final_data:")
nrow(final_data)

# Check if the file exists first
print("Does file exist:")
file.exists("outputs/final_cleaned_data.csv")

# Try reading with different options
final_data <- read.csv("outputs/final_cleaned_data.csv", 
                       header = TRUE, 
                       check.names = FALSE,
                       na.strings = "NA",
                       stringsAsFactors = FALSE)

# If that doesn't work, let's try reading with readLines to see the actual content
raw_lines <- readLines("outputs/final_cleaned_data.csv", n = 5)
print("First few lines of the CSV file:")
print(raw_lines)

# Alternative reading method using data.table
# library(data.table)
# final_data <- fread("outputs/final_cleaned_data.csv")

# Check for exact column names
colnames(data_cleaned)


# Adjust renaming based on exact names
data_cleaned <- data_cleaned %>%
  rename(
    Internet_Usage = `Individuals.using.the.Internet....of.population.`,
    Access_Electricity = `Access.to.electricity....of.population.`,
    Gini_Index = `Gini.index`,
    Poverty_Rate = `Poverty.headcount.ratio.at..2.15.a.day..2017.PPP.....of.population.`,
    Mobile_Cellular_Subscriptions = `Mobile.cellular.subscriptions..per.100.people.`,
    Poverty_Gap = `Poverty.gap.at..2.15.a.day..2017.PPP.....`,
    Communications_Computers_Imports = `Communications..computer..etc.....of.service.imports..BoP.`
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

# Step 3: Updated Correlation Matrix
# Include all numeric variables, including the new ones
numeric_data <- data_cleaned %>% select_if(is.numeric)
correlation_matrix <- cor(numeric_data, use = "complete.obs")

# Print and visualize the correlation matrix
print("Correlation Matrix:")
print(correlation_matrix)

# Save the updated correlation matrix plot
correlation_plot <- ggcorr(correlation_matrix, label = TRUE) +
  ggtitle("Updated Correlation Matrix Including New Variables")
ggsave("updated_correlation_matrix.png", plot = correlation_plot, width = 8, height = 6)

# Step 4: Univariate Analysis
# Plot 1: Histogram of Internet Usage
internet_usage_plot <- ggplot(data_cleaned, aes(x = Internet_Usage)) +
  geom_histogram(fill = "blue", bins = 20, alpha = 0.7) +
  labs(
    title = "Distribution of Internet Usage",
    x = "Internet Usage (% of population)",
    y = "Count"
  )
ggsave("internet_usage_distribution.png", plot = internet_usage_plot, width = 8, height = 6)

# Step 5: Bivariate Analysis
# Plot 2: Scatterplot of Internet Usage vs Gini Index
internet_vs_gini_plot <- ggplot(data_cleaned, aes(x = Gini_Index, y = Internet_Usage)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Internet Usage vs Gini Index",
    x = "Gini Index",
    y = "Internet Usage (%)"
  )
ggsave("internet_vs_gini.png", plot = internet_vs_gini_plot, width = 8, height = 6)

# Plot 3: Scatterplot of Internet Usage vs Access to Electricity
internet_vs_electricity_plot <- ggplot(data_cleaned, aes(x = Access_Electricity, y = Internet_Usage)) +
  geom_point(color = "green", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Internet Usage vs Access to Electricity",
    x = "Access to Electricity (%)",
    y = "Internet Usage (%)"
  )
ggsave("internet_vs_electricity.png", plot = internet_vs_electricity_plot, width = 8, height = 6)

# Plot 4: Internet Usage vs Poverty Gap
internet_vs_poverty_gap_plot <- ggplot(data_cleaned, aes(x = Poverty_Gap, y = Internet_Usage)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Internet Usage vs Poverty Gap",
    x = "Poverty Gap at $2.15/day (2017 PPP)",
    y = "Internet Usage (%)"
  )
ggsave("internet_vs_poverty_gap.png", plot = internet_vs_poverty_gap_plot, width = 8, height = 6)

# Plot 5: Internet Usage vs Mobile Cellular Subscriptions
internet_vs_mobile_plot <- ggplot(data_cleaned, aes(x = Mobile_Cellular_Subscriptions, y = Internet_Usage)) +
  geom_point(color = "green", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Internet Usage vs Mobile Cellular Subscriptions",
    x = "Mobile Cellular Subscriptions (per 100 people)",
    y = "Internet Usage (%)"
  )
ggsave("internet_vs_mobile.png", plot = internet_vs_mobile_plot, width = 8, height = 6)

# Plot 6: Internet Usage vs Communications and Computers Imports
internet_vs_communications_plot <- ggplot(data_cleaned, aes(x = Communications_Computers_Imports, y = Internet_Usage)) +
  geom_point(color = "purple", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Internet Usage vs Communications and Computers Imports",
    x = "Communications, Computers, etc. (% of Service Imports)",
    y = "Internet Usage (%)"
  )
ggsave("internet_vs_communications.png", plot = internet_vs_communications_plot, width = 8, height = 6)

# Step 6: Boxplot Analysis
# Plot 7: Boxplot of Internet Usage by Poverty Quartiles
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
