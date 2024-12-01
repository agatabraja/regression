# feature_engineering.R
# Install necessary libraries (if not installed)
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")

# Load necessary libraries
library(dplyr)
library(tidyr)

# Load the cleaned dataset
data_cleaned <- read.csv("outputs/final_cleaned_data.csv", stringsAsFactors = FALSE)

# Rename columns for simplicity
data_cleaned <- data_cleaned %>%
  rename(
    Internet_Usage = Individuals.using.the.Internet....of.population.,
    Access_Electricity = Access.to.electricity....of.population.,
    Gini_Index = Gini.index,
    Poverty_Rate = Poverty.headcount.ratio.at..2.15.a.day..2017.PPP.....of.population.,
    Mobile_Cellular = Mobile.cellular.subscriptions,
    Mobile_Cellular_Per100 = Mobile.cellular.subscriptions..per.100.people.,
    Poverty_Gap = Poverty.gap.at..2.15.a.day..2017.PPP.....,
    Communications_Imports = Communications..computer..etc.....of.service.imports..BoP.,
    Internet_Female = Individuals.using.the.Internet..female....of.female.population.,
    Internet_Male = Individuals.using.the.Internet..male....of.male.population.,
    Fixed_Broadband = Fixed.broadband.subscriptions..per.100.people.
  )



# Step 1: Create technology access indicators
data_cleaned <- data_cleaned %>%
  mutate(
    # Gender gap in internet usage
    Internet_Gender_Gap = Internet_Male - Internet_Female,
    
    # Mobile penetration ratio (subscriptions vs population)
    Mobile_Penetration_Ratio = Mobile_Cellular_Per100 / 100,
    
    # Combined connectivity score
    Connectivity_Score = (Internet_Usage + Mobile_Cellular_Per100 + Fixed_Broadband) / 3
  )

# Step 2: Transform skewed variables
data_cleaned <- data_cleaned %>%
  mutate(
    Log_Internet_Usage = log(Internet_Usage + 1),
    Log_Mobile_Cellular = log(Mobile_Cellular + 1),
    Log_Poverty_Gap = log(Poverty_Gap + 1),
    Log_Communications_Imports = log(Communications_Imports + 1),
    Log_Fixed_Broadband = log(Fixed_Broadband + 1)
  )

# Step 3: Create categorical features
data_cleaned <- data_cleaned %>%
  mutate(
    # Poverty levels
    Poverty_Quartile = ntile(Poverty_Rate, 4),
    
    # Internet adoption categories
    Internet_Level = case_when(
      Internet_Usage < 25 ~ "Low",
      Internet_Usage < 50 ~ "Medium",
      Internet_Usage < 75 ~ "High",
      TRUE ~ "Very High"
    ),
    
    # Digital divide indicator (based on mean internet usage)
    Digital_Divide = ifelse(Internet_Usage > mean(Internet_Usage, na.rm = TRUE), 
                            "Above Average", "Below Average")
  )

# Step 4: Create interaction terms
data_cleaned <- data_cleaned %>%
  mutate(
    # Technology interactions
    Mobile_Internet_Interaction = Mobile_Penetration_Ratio * Internet_Usage,
    Broadband_Mobile_Interaction = Fixed_Broadband * Mobile_Cellular_Per100,
    
    # Socioeconomic interactions
    Poverty_Internet_Impact = Internet_Usage * (1 - Poverty_Gap/100),
    Gini_Internet_Impact = Internet_Usage * (1 - Gini_Index/100)
  )


# Additional feature engineering
data_cleaned <- data_cleaned %>%
  mutate(
    # Gender gap categories
    Internet_Gender_Gap_Cat = case_when(
      is.na(Internet_Gender_Gap) ~ "Unknown",
      Internet_Gender_Gap < 0 ~ "Female_Higher",
      Internet_Gender_Gap < 3 ~ "Low_Gap",
      Internet_Gender_Gap < 6 ~ "Medium_Gap",
      TRUE ~ "High_Gap"
    ),
    
    # Mobile penetration categories
    Mobile_Penetration_Level = case_when(
      Mobile_Penetration_Ratio < 1 ~ "Under_Penetrated",
      Mobile_Penetration_Ratio < 1.5 ~ "Adequately_Penetrated",
      TRUE ~ "Over_Penetrated"
    ),
    
    # Connectivity quartiles
    Connectivity_Level = ntile(Connectivity_Score, 4),
    
    # Combined poverty-connectivity indicator
    Poverty_Connectivity_Group = case_when(
      Poverty_Quartile <= 2 & Connectivity_Level >= 3 ~ "High_Access_Low_Poverty",
      Poverty_Quartile >= 3 & Connectivity_Level <= 2 ~ "Low_Access_High_Poverty",
      TRUE ~ "Medium_Balance"
    )
  )


# Step 5: Scale numeric features for modeling
numeric_cols <- c("Internet_Usage", "Mobile_Cellular_Per100", "Fixed_Broadband", 
                  "Poverty_Gap", "Gini_Index", "Communications_Imports")

data_cleaned <- data_cleaned %>%
  mutate(across(all_of(numeric_cols), 
                list(scaled = ~scale(.)), 
                .names = "Scaled_{.col}"))

# Save the refined dataset
write.csv(data_cleaned, "outputs/refined_data.csv", row.names = FALSE)

# Print summary of new features
print("Summary of engineered features:")
summary(data_cleaned[c("Internet_Gender_Gap", "Mobile_Penetration_Ratio", 
                       "Connectivity_Score", "Poverty_Quartile", "Internet_Level")])