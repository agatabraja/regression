# Load necessary libraries
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
library(dplyr)
library(tidyr)
library(stringr)

# Step 1: Load the datasets
data_pivoted <- read.csv("data_pivoted_CO_47+2.csv", stringsAsFactors = FALSE)
print("Initial data dimensions:")
print(dim(data_pivoted))

# Step 2: Extract Year and Metric from the `Attribute` column
data_pivoted <- data_pivoted %>%
  mutate(
    Year = str_extract(Attribute, "\\[YR[0-9]{4}\\]") %>%
      str_remove_all("\\[YR|\\]") %>%
      as.numeric(),
    Metric = str_remove(Attribute, "\\[YR[0-9]{4}\\]") %>%
      str_trim()
  )

# Step 3: Filter for GINI data completeness
gini_completeness <- data_pivoted %>%
  group_by(Country.Name) %>%
  summarise(
    Years_With_Data = sum(!is.na(Gini.index)),
    Total_Years = n()
  ) %>%
  arrange(desc(Years_With_Data))

print("Gini completeness summary:")
print(head(gini_completeness))

# Step 4: Select top 70 countries based on GINI completeness
top_countries <- gini_completeness %>%
  head(70) %>%
  pull(Country.Name)

print("Number of top countries:")
print(length(top_countries))

# Step 5: Filter the dataset for top countries
filtered_data <- data_pivoted %>%
  filter(Country.Name %in% top_countries)

print("Filtered data dimensions:")
print(dim(filtered_data))

# After getting filtered_data
variable_completeness <- filtered_data %>%
  select_at(vars(-Country.Name, -Attribute, -Year, -Metric)) %>%
  summarise(across(everything(),
                   ~mean(!is.na(.)),
                   .names = "{.col}"
  )) %>%
  pivot_longer(
    everything(),
    names_to = "Metric",
    values_to = "Completeness"
  ) %>%
  filter(Completeness >= 0.7) %>%
  arrange(desc(Completeness))

# Create final dataset using vars() and select_at()
final_data <- filtered_data %>%
  select_at(vars("Country.Name", "Year", all_of(variable_completeness$Metric))) %>%
  arrange(Country.Name, Year)



# Step 8: Save outputs
if (!dir.exists("outputs")) dir.create("outputs")

# Save with explicit row.names=FALSE and handling of NA values
write.csv(final_data, "outputs/final_cleaned_data.csv", row.names = FALSE, na = "")
write.csv(gini_completeness, "outputs/gini_completeness.csv", row.names = FALSE)
write.csv(variable_completeness, "outputs/variable_completeness.csv", row.names = FALSE)
write.csv(data.frame(x = top_countries), "outputs/top_countries.csv", row.names = FALSE)

print("Data cleaning process completed successfully. Outputs saved in the 'outputs' directory.")