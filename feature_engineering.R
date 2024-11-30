# Transform skewed variables
data_cleaned <- data_cleaned %>%
  mutate(
    Log_Poverty_Rate = log1p(Poverty_Rate),
    Log_Gini_Index = log1p(Gini_Index)
  )

# Scale variables
data_cleaned <- data_cleaned %>%
  mutate(
    Scaled_Access_Electricity = scale(Access_Electricity),
    Scaled_Gini_Index = scale(Gini_Index),
    Scaled_Poverty_Rate = scale(Poverty_Rate)
  )

# Create interaction terms
data_cleaned <- data_cleaned %>%
  mutate(
    Access_Electricity_x_Poverty = Access_Electricity * Poverty_Rate,
    Access_Electricity_x_Gini = Access_Electricity * Gini_Index
  )

# Bin continuous variables
data_cleaned <- data_cleaned %>%
  mutate(
    Poverty_Category = case_when(
      Poverty_Rate < 10 ~ "Low",
      Poverty_Rate >= 10 & Poverty_Rate < 30 ~ "Medium",
      Poverty_Rate >= 30 ~ "High"
    )
  )

# Encode quartiles
data_cleaned <- data_cleaned %>%
  mutate(
    Gini_Quartile = ntile(Gini_Index, 4),
    Poverty_Quartile = ntile(Poverty_Rate, 4)
  )

# Correlation analysis
correlation_matrix <- cor(data_cleaned %>% select_if(is.numeric), use = "complete.obs")
print("Correlation Matrix:")
print(correlation_matrix)

refined_data <- data_cleaned
write.csv(refined_data, "refined_data.csv", row.names = FALSE)
