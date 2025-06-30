library(dplyr)
library(readxl)

# Set the working directory
setwd("C:/Users/Saba Akram/Documents/Assignment")

# Load education data
education_data <- read.table("rawdata_369.txt", header = FALSE, skip = 2, sep = "", fill = TRUE, stringsAsFactors = FALSE)
names(education_data) <- c("country", "education_expenditure", "year")
education_data <- education_data[, c("country", "education_expenditure")]

# Load unemployment data from Excel
unemployment_data <- read_excel("rawdata 373.xls")
names(unemployment_data) <- c("country", "youth_unemployment_rate")

# Load migration data
migration_data <- read.table("rawdata_347.txt", header = FALSE, sep = "", fill = TRUE, stringsAsFactors = FALSE)
names(migration_data) <- c("country", "net_migration_rate", "year")
migration_data <- migration_data[, c("country", "net_migration_rate")]

# Convert country columns to character
education_data$country <- as.character(education_data$country)
unemployment_data$country <- as.character(unemployment_data$country)
migration_data$country <- as.character(migration_data$country)

# Merge datasets using full join
merged_data <- full_join(education_data, unemployment_data, by = "country")
merged_data <- full_join(merged_data, migration_data, by = "country")

# Check dimensions and preview data
print(dim(merged_data))
head(merged_data)


# Part C

# Load necessary libraries
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
library(readxl)

if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

# Load income classification data
income_data <- read_excel("C:/Users/Saba Akram/Documents/Assignment/CLASS.xlsx")

# Rename columns for consistency
names(income_data) <- c("country", "iso_code", "region", "income_group", "lending_category")

# Clean and standardize country names
merged_data$country <- trimws(tolower(gsub("[^[:alnum:] ]", "", as.character(merged_data$country))))
income_data$country <- trimws(tolower(gsub("[^[:alnum:] ]", "", as.character(income_data$country))))
 
# Merge datasets using the 'country' column
enriched_data <- left_join(merged_data, income_data, by = "country")

# Check for unmatched countries
unmatched_countries <- merged_data$country[!merged_data$country %in% income_data$country]
if (length(unmatched_countries) > 0) {
  print("Unmatched Countries:")
  print(unique(unmatched_countries))
}

# Check the result
print(head(enriched_data))

# View the enriched data
View(enriched_data)

#Part D

# Load necessary libraries
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
library(readxl)

# Load your existing dataset
df_vars <- read_excel("C:/Users/Saba Akram/Documents/Assignment/Existing_dataset.xls")

# Load the continent dataset
continent_data <- read.csv("C:/Users/Saba Akram/Documents/Assignment/continents2.csv")

# Clean and standardize country names
df_vars$country <- trimws(tolower(gsub("[^[:alnum:] ]", "", as.character(df_vars$country))))
continent_data$country <- trimws(tolower(gsub("[^[:alnum:] ]", "", as.character(continent_data$country))))

# Merge the datasets
df_vars <- left_join(df_vars, continent_data, by = "country")

# Check the result
print(head(df_vars))

# View the enriched data
View(df_vars)

#Part E

# Load necessary libraries
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
library(readxl)

# Load your existing dataset
df_vars <- read_excel("C:/Users/Saba Akram/Documents/Assignment/Existing_dataset.xls")

# Load the continent dataset
continent_data <- read.csv("C:/Users/Saba Akram/Documents/Assignment/continents2.csv")

# Rename 'region' to 'continent' in continent_data
names(continent_data)[names(continent_data) == "region"] <- "continent"

# Clean and standardize country names
df_vars$country <- trimws(tolower(gsub("[^[:alnum:] ]", "", as.character(df_vars$country))))
continent_data$country <- trimws(tolower(gsub("[^[:alnum:] ]", "", as.character(continent_data$country))))

# Merge datasets
df_vars <- left_join(df_vars, continent_data, by = "country")

# Remove duplicates
df_vars <- df_vars %>% distinct()

# Check for missing values
df_vars <- df_vars %>% filter(!is.na(income_group), !is.na(continent))

# Check the structure
str(df_vars)

# Frequency table for income status
income_status_freq <- table(df_vars$income_group)
print(income_status_freq)

# Interpretation
cat("The frequency table shows the distribution of countries across different income groups, highlighting which income status is most common.\n")

# Absolute frequencies across continents
income_continent_abs <- table(df_vars$continent, df_vars$income_group)
print(income_continent_abs)

# Relative frequencies across continents
income_continent_rel <- prop.table(income_continent_abs, 1)
print(income_continent_rel)

# Comment on findings
cat("The distribution of income status across continents shows how economic classifications vary regionally. Some continents may have more countries in certain income groups.\n")

# Identify unique countries in their income group across continents
unique_countries <- df_vars %>%
  group_by(continent, income_group) %>%
  filter(n() == 1) %>%
  select(country, continent, income_group)

print(unique_countries)

# Discussion
cat("Countries that are the only ones in their income group across their continent could indicate unique economic conditions or classifications.\n")

#Part F

library(dplyr)
library(readxl)
library(tidyr)

# Load your existing dataset
df_vars <- read_excel("C:/Users/Saba Akram/Documents/Assignment/Existing_dataset.xls")

# Load the continent dataset
continent_data <- read.csv("C:/Users/Saba Akram/Documents/Assignment/continents2.csv")

# Rename 'region' to 'continent' in continent_data
names(continent_data)[names(continent_data) == "region"] <- "continent"

# Clean and standardize country names
df_vars$country <- trimws(tolower(gsub("[^[:alnum:] ]", "", as.character(df_vars$country))))
continent_data$country <- trimws(tolower(gsub("[^[:alnum:] ]", "", as.character(continent_data$country))))

# Merge datasets
df_vars <- left_join(df_vars, continent_data, by = "country")

# Convert necessary columns to numeric
df_vars$education_expenditure <- as.numeric(df_vars$education_expenditure)
df_vars$youth_unemployment_rate <- as.numeric(df_vars$youth_unemployment_rate)
df_vars$net_migration_rate <- as.numeric(df_vars$net_migration_rate)

# Keep all data and only filter for analysis
filtered_df_vars <- df_vars %>% filter(!is.na(income_group), !is.na(continent), !is.na(education_expenditure), !is.na(youth_unemployment_rate), !is.na(net_migration_rate))

# Calculate mean and median values
average_stats <- filtered_df_vars %>%
  group_by(income_group) %>%
  summarise(
    mean_expenditure = mean(education_expenditure, na.rm = TRUE),
    median_expenditure = median(education_expenditure, na.rm = TRUE),
    mean_youth_unemployment = mean(youth_unemployment_rate, na.rm = TRUE),
    median_youth_unemployment = median(youth_unemployment_rate, na.rm = TRUE),
    mean_net_migration = mean(net_migration_rate, na.rm = TRUE),
    median_net_migration = median(net_migration_rate, na.rm = TRUE)
  ) %>%
  arrange(factor(income_group, levels = c("L", "LM", "UM", "H")))

print(average_stats)

# Calculate standard deviation and IQR
variation_stats <- filtered_df_vars %>%
  group_by(income_group) %>%
  summarise(
    sd_expenditure = sd(education_expenditure, na.rm = TRUE),
    iqr_expenditure = IQR(education_expenditure, na.rm = TRUE),
    sd_youth_unemployment = sd(youth_unemployment_rate, na.rm = TRUE),
    iqr_youth_unemployment = IQR(youth_unemployment_rate, na.rm = TRUE),
    sd_net_migration = sd(net_migration_rate, na.rm = TRUE),
    iqr_net_migration = IQR(net_migration_rate, na.rm = TRUE)
  ) %>%
  arrange(factor(income_group, levels = c("L", "LM", "UM", "H")))

print(variation_stats)

# Extend analysis by income status and continent
extended_stats <- filtered_df_vars %>%
  group_by(continent, income_group) %>%
  summarise(
    median_expenditure = median(education_expenditure, na.rm = TRUE),
    iqr_expenditure = IQR(education_expenditure, na.rm = TRUE),
    median_youth_unemployment = median(youth_unemployment_rate, na.rm = TRUE),
    iqr_youth_unemployment = IQR(youth_unemployment_rate, na.rm = TRUE),
    median_net_migration = median(net_migration_rate, na.rm = TRUE),
    iqr_net_migration = IQR(net_migration_rate, na.rm = TRUE)
  )

# Display different views
extended_stats_long <- pivot_longer(extended_stats, cols = -c(continent, income_group), names_to = "statistic", values_to = "value")
print(extended_stats_long)

extended_stats_wide <- pivot_wider(extended_stats, names_from = income_group, values_from = c(median_expenditure, iqr_expenditure, median_youth_unemployment, iqr_youth_unemployment, median_net_migration, iqr_net_migration))
print(extended_stats_wide)

# Identify well-performing countries
well_performing_countries <- filtered_df_vars %>%
  group_by(continent) %>%
  filter(
    net_migration_rate > quantile(net_migration_rate, 0.75, na.rm = TRUE) &
      youth_unemployment_rate < quantile(youth_unemployment_rate, 0.25, na.rm = TRUE)
  ) %>%
  select(country, continent, net_migration_rate, youth_unemployment_rate)

print(well_performing_countries)


# Part G

library(dplyr)
library(readxl)

# Assuming merged_data is created from previous parts and contains the necessary columns

# Load income classification data
income_data <- read_excel("C:/Users/Saba Akram/Documents/Assignment/CLASS.xlsx")

# Rename columns for consistency
names(income_data) <- c("country", "iso_code", "region", "income_group", "lending_category")

# Clean and standardize country names
merged_data$country <- trimws(tolower(gsub("[^[:alnum:] ]", "", as.character(merged_data$country))))
income_data$country <- trimws(tolower(gsub("[^[:alnum:] ]", "", as.character(income_data$country))))

# Merge datasets using the 'country' column
merged_data <- left_join(merged_data, income_data, by = "country")

# Check if the merge was successful
if ("income_group" %in% colnames(merged_data)) {
  print("Income group column is present.")
} else {
  stop("Income group column is missing.")
}

# Load the continent dataset
continent_data <- read.csv("C:/Users/Saba Akram/Documents/Assignment/continents2.csv")

# Clean and standardize country names
continent_data$country <- trimws(tolower(gsub("[^[:alnum:] ]", "", as.character(continent_data$country))))

# Merge datasets
merged_data <- left_join(merged_data, continent_data, by = "country")

# Calculate prior probability of high income
prior_high_income <- merged_data %>%
  summarise(prior_probability = mean(income_group == "high", na.rm = TRUE))

print(prior_high_income)

# Calculate conditional probability of high income for European countries
conditional_high_income_europe <- merged_data %>%
  filter(continent == "Europe") %>%
  summarise(conditional_probability = mean(income_group == "high", na.rm = TRUE))

print(conditional_high_income_europe)

# Calculate conditional probability of negative net migration given high youth unemployment
conditional_negative_migration <- merged_data %>%
  filter(youth_unemployment_rate > 25) %>%
  summarise(conditional_probability = mean(net_migration_rate < 0, na.rm = TRUE))

print(conditional_negative_migration)

#Part H

library(dplyr)
library(readxl)

# Load continent data
continent_data <- read.csv("C:/Users/Saba Akram/Documents/Assignment/continents2.csv")

# Check available column names in continent_data
print("Columns in continent_data:")
print(colnames(continent_data))

# Clean and standardize country names
continent_data$country <- trimws(tolower(gsub("[^[:alnum:] ]", "", as.character(continent_data$country))))
merged_data$country <- trimws(tolower(gsub("[^[:alnum:] ]", "", as.character(merged_data$country))))

# Use the correct column name for continent or region
# If 'region' is the correct column, use it directly
# No renaming needed if 'continent' is not present
if ("region" %in% colnames(continent_data)) {
  names(continent_data)[names(continent_data) == "region"] <- "continent"
} else {
  stop("Neither 'region' nor 'continent' column found in continent_data.")
}

# Merge continent data with merged_data
merged_data <- left_join(merged_data, continent_data, by = "country")

# Verify if the continent column exists
if ("continent" %in% colnames(merged_data)) {
  print("Continent column is present.")
} else {
  stop("Continent column is missing.")
}

# Calculate average youth unemployment rates by income group and continent
continent_unemployment <- merged_data %>%
  group_by(continent, income_group) %>%
  summarise(continent_average_unemployment = mean(youth_unemployment_rate, na.rm = TRUE), .groups = 'drop')

print("Average Youth Unemployment Rates by Income Group and Continent:")
print(continent_unemployment)

# Part I ( Exporting Final Dataset)

library(dplyr)
library(readxl)

# Set the working directory
setwd("C:/Users/Saba Akram/Documents/Assignment")

# Load education data
education_data <- read.table("rawdata_369.txt", header = FALSE, skip = 2, sep = "", fill = TRUE, stringsAsFactors = FALSE)
names(education_data) <- c("country", "education_expenditure", "year")
education_data <- education_data[, c("country", "education_expenditure")]

# Load unemployment data from Excel
unemployment_data <- read_excel("rawdata 373.xls")
names(unemployment_data) <- c("country", "youth_unemployment_rate")

# Load migration data
migration_data <- read.table("rawdata_347.txt", header = FALSE, sep = "", fill = TRUE, stringsAsFactors = FALSE)
names(migration_data) <- c("country", "net_migration_rate", "year")
migration_data <- migration_data[, c("country", "net_migration_rate")]

# Convert country columns to character
education_data$country <- as.character(education_data$country)
unemployment_data$country <- as.character(unemployment_data$country)
migration_data$country <- as.character(migration_data$country)

# Load income classification data
income_data <- read_excel("CLASS.xlsx")
names(income_data) <- c("country", "iso_code", "region", "income_group", "lending_category")

# Load the continent dataset
continent_data <- read.csv("continents2.csv")
names(continent_data)[names(continent_data) == "region"] <- "continent"

# Clean and standardize country names
standardize_country_names <- function(df) {
  df$country <- trimws(tolower(gsub("[^[:alnum:] ]", "", as.character(df$country))))
  return(df)
}

education_data <- standardize_country_names(education_data)
unemployment_data <- standardize_country_names(unemployment_data)
migration_data <- standardize_country_names(migration_data)
income_data <- standardize_country_names(income_data)
continent_data <- standardize_country_names(continent_data)

# Merge datasets using full join
merged_data <- full_join(education_data, unemployment_data, by = "country")
merged_data <- full_join(merged_data, migration_data, by = "country")
merged_data <- left_join(merged_data, income_data, by = "country")
merged_data <- left_join(merged_data, continent_data, by = "country")

# Remove duplicates and filter missing values
final_data <- merged_data %>% distinct()

# Export the final tidy dataset as a CSV with specific formatting
write.table(final_data, file = "final_dataset.csv", sep = ";", na = ".", row.names = FALSE, quote = FALSE)

print("Data exported successfully.")
