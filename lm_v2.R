# Load necessary libraries
library(dplyr)

# Load the dataset
data <- read.csv("data_pivoted_CO_47+2.csv", stringsAsFactors = FALSE)

# Drop columns with 100% missing values
data_cleaned <- data %>% select_if(~sum(!is.na(.)) > 0)

# Inspect column names for reference
print("Column names in the dataset:")
print(colnames(data_cleaned))

# Dynamically match columns using partial matches or regex
selected_columns <- grep(
  pattern = "electricity|urban population|Internet|cellular|Gini|poverty",
  colnames(data_cleaned),
  value = TRUE,
  ignore.case = TRUE
)

# Ensure matching columns are found
if (length(selected_columns) == 0) {
  stop("No matching columns found. Please verify the column names.")
}

# Filter the dataset to include only the selected columns
data_cleaned <- data_cleaned %>% select(all_of(selected_columns))

# Handle missing values
data_cleaned <- data_cleaned %>%
  mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Save the cleaned dataset for further analysis
write.csv(data_cleaned, "data_cleaned.csv", row.names = FALSE)

# View the cleaned dataset
print("Cleaned dataset preview:")
print(head(data_cleaned))
