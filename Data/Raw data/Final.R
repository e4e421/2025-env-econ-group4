# -----------------------------------------------------------------------------
# 1. SETUP AND PACKAGE LOADING
# -----------------------------------------------------------------------------

# Install packages if not already installed
install.packages("tidyverse")
install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("ggplot2")
install.packages("scales")

# Load required libraries
library(tidyverse)    # For data manipulation and visualization
library(readr)        # For reading CSV files
library(dplyr)        # For data transformation
library(tidyr)        # For data reshaping
library(stringr)      # For string operations
library(ggplot2)      # For data visualization
library(scales)       # For axis formatting

# Set seed for reproducibility 
set.seed(123)

# -----------------------------------------------------------------------------
# 2. DATA LOADING AND INITIAL EXPLORATION
# -----------------------------------------------------------------------------

# Load emissions data
emissions_raw <- read_csv("EDGAR_2024_GHG_booklet_2024.csv", 
                          show_col_types = FALSE)

# Load GDP data
gdp_raw <- read_csv("GDP.csv", 
                    show_col_types = FALSE)

# View column names and types for emissions data
print(colnames(emissions_raw))
print(head(emissions_raw, 3))

# View column names and types for GDP data
print(colnames(gdp_raw))
print(head(gdp_raw, 3))

# -----------------------------------------------------------------------------
# 3. DATA VALIDATION AND QUALITY CHECKS
# -----------------------------------------------------------------------------

# Check for missing values in key columns in emissions data
missing_emissions <- colSums(is.na(emissions_raw))
print(missing_emissions[missing_emissions > 0])

# Check for missing values in key columns in gdp data
missing_gdp <- colSums(is.na(gdp_raw))
print(missing_gdp[missing_gdp > 0])

# Check for duplicate rows
sum(duplicated(emissions_raw))
sum(duplicated(gdp_raw))

# Check unique values in key categorical columns
print(unique(emissions_raw$Substance))
print(unique(emissions_raw$Sector))

# -----------------------------------------------------------------------------
# 4. EMISSIONS DATA PROCESSING
# -----------------------------------------------------------------------------

# Define target countries and substance
target_countries <- c("China", "India", "Japan", "South Korea", "Indonesia")
target_substance <- "CO2"

# 4.1 Filter and select relevant data
emissions_processed <- emissions_raw %>%
  # Filter for target countries and CO2 only
  filter(Country %in% target_countries,
         Substance == target_substance) %>%
  # Select relevant columns: Country, Sector, and year columns (1973-2023)
  select(Country, Sector, matches("^(19[7-9]|20[0-2])"))

# 4.2 Convert from wide to long format
emissions_long <- emissions_processed %>%
  pivot_longer(
    cols = -c(Country, Sector),
    names_to = "Year",
    values_to = "Emissions_MtCO2eq"
  ) %>%
  mutate(
    Year = as.numeric(Year)
  ) %>%
  filter(Year >= 1973 & Year <= 2023)

# 4.3 Check for missing emission values
missing_co2_check <- emissions_long %>%
  filter(is.na(Emissions_MtCO2eq)) %>%
  nrow()

# 4.4 Aggregate to country-year totals (summing across sectors)
country_year_emissions <- emissions_long %>%
  group_by(Country, Year) %>%
  summarise(
    Total_CO2_Mt = sum(Emissions_MtCO2eq, na.rm = TRUE),
    .groups = 'drop'
  )

# 4.5 Create sector-level dataset for later analysis
country_sector_emissions <- emissions_long %>%
  group_by(Country, Sector, Year) %>%
  summarise(
    Sector_CO2_Mt = sum(Emissions_MtCO2eq, na.rm = TRUE),
    .groups = 'drop'
  )

# -----------------------------------------------------------------------------
# 5. GDP DATA PROCESSING
# -----------------------------------------------------------------------------

# 5.1 Select relevant columns and filter for target countries
gdp_processed <- gdp_raw %>%
  # Select only Country and year columns (1973-2023)
  select(Country, matches("^(19[7-9]|20[0-2])")) %>%
  # Filter for target countries
  filter(Country %in% target_countries)

# Check country matching
print(unique(gdp_processed$Country))

# 5.2 Convert from wide to long format
gdp_long <- gdp_processed %>%
  pivot_longer(
    cols = -Country,                    
    names_to = "Year",                   
    values_to = "GDP_USD"              
  ) %>%
  mutate(
    Year = as.numeric(Year)              
  ) %>%
  # Filter for years 1973-2023
  filter(Year >= 1973 & Year <= 2023)

# 5.3 Check for missing GDP values
missing_gdp_check <- gdp_long %>%
  filter(is.na(GDP_USD)) %>%
  nrow()

# -----------------------------------------------------------------------------
# 6. DATA MERGING AND VALIDATION
# -----------------------------------------------------------------------------

# 6.1 Check country name consistency between datasets
emissions_countries <- unique(country_year_emissions$Country)
gdp_countries <- unique(gdp_long$Country)

# 6.2 Merge datasets
decoupling_data <- country_year_emissions %>%
  inner_join(gdp_long, by = c("Country", "Year"))

# 6.3 Check for missing combinations (countries/years that didn't merge)
missing_combinations <- country_year_emissions %>%
  anti_join(gdp_long, by = c("Country", "Year"))

if (nrow(missing_combinations) > 0) {
  cat("\n6.4 WARNING: Some country-year combinations not merged:\n")
  print(missing_combinations)
} else {
  cat("\n6.4 ✓ All country-year combinations successfully merged\n")
}

# 6.4 Sort for time-series analysis
decoupling_data <- decoupling_data %>%
  arrange(Country, Year)

# -----------------------------------------------------------------------------
# 7. DATA QUALITY ASSESSMENT
# -----------------------------------------------------------------------------

# 7.1 Summary statistics by country
summary_stats <- decoupling_data %>%
  group_by(Country) %>%
  summarise(
    Years = n(),
    CO2_First = round(Total_CO2_Mt[Year == min(Year)], 1),
    CO2_Last = round(Total_CO2_Mt[Year == max(Year)], 1),
    CO2_Growth = round((CO2_Last / CO2_First - 1) * 100, 1),
    GDP_First = round(GDP_USD[Year == min(Year)] / 1e9, 1),  # In billions
    GDP_Last = round(GDP_USD[Year == max(Year)] / 1e9, 1),   # In billions
    GDP_Growth = round((GDP_Last / GDP_First - 1) * 100, 1),
    .groups = 'drop'
  )

print(summary_stats)

# 7.2 Check for zero or negative values
zero_co2 <- sum(decoupling_data$Total_CO2_Mt <= 0, na.rm = TRUE)
zero_gdp <- sum(decoupling_data$GDP_USD <= 0, na.rm = TRUE)

# -----------------------------------------------------------------------------
# 8. DATA EXPORT
# -----------------------------------------------------------------------------

# 8.1 Export main decoupling dataset
write_csv(decoupling_data, "decoupling_data_1973_2023.csv")

# 8.2 Export sector-level emissions data
write_csv(country_sector_emissions, "sector_emissions_1973_2023.csv")

# 8.3 Export GDP data
write_csv(gdp_long, "gdp_data_1973_2023.csv")


