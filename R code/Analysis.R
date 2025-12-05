### Data Cleaning

## Install & load data transformation function package 
install.packages("tidyverse")
library(tidyverse)

## Step 1:Load Data
raw_data <- read_csv("EDGAR_emiss_on_UCDB_v2024.csv")
gdp_data <- read_csv("GDP.csv")

# View the structure to confirm column names
glimpse(raw_data)
head(raw_data)

## Step 2:Filter target countries
target_countries <- c("China", "India", "Japan", "South Korea", "Indonesia")

cleaned_data <- raw_data %>%
  # Filter target countries
  filter(UC_country %in% target_countries) %>%
  # Select all EMI columns for CO2 across all years and sectors
  select(UC_name, UC_country, matches("EMI_CO2_.*_(1975|1990|2000|2005|2010|2015|2020|2022)"))

# Reshape GDP data from wide to long format
gdp_long <- gdp_data %>%
  select(Country, `1975`, `1990`, `2000`, `2005`, `2010`, `2015`, `2020`, `2022`) %>%
  pivot_longer(
    cols = -Country,
    names_to = "Year",
    values_to = "GDP"
  ) %>%
  mutate(Year = as.numeric(Year))


### Step 3: Pivot to long format to extract Year and Sector from column names
long_data <- cleaned_data %>%
  pivot_longer(
    cols = starts_with("EMI_"),
    names_to = "Emission_Variable",
    values_to = "Emission_Value"
  ) %>%
  ## Extract Year, Sector, and Pollutant from the column name
  mutate(
    # Extract year (last 4 digits)
    Year = str_extract(Emission_Variable, "\\d{4}$"),
    # Extract sector (between CO2_ and _Year)
    Sector = str_extract(Emission_Variable, "CO2_(.*?)_\\d{4}"),
    Sector = str_remove(Sector, "CO2_"),
    Sector = str_remove(Sector, "_\\d{4}"),
    Pollutant = "CO2"
  ) %>%
  select(-Emission_Variable) %>%
  mutate(Year = as.numeric(Year))

## Step 4: Aggregate from city level to country level
# For total CO2 by country and year (for trend analysis)
country_year_totals <- long_data %>%
  group_by(UC_country, Year) %>%
  summarise(
    Total_CO2 = sum(Emission_Value, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  rename(Country = UC_country)

# For sector-level data (for sector contribution analysis)
country_sector_year <- long_data %>%
  group_by(UC_country, Sector, Year) %>%
  summarise(
    Sector_CO2 = sum(Emission_Value, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  rename(Country = UC_country)

## STEP 6: Merge emissions with GDP data
final_trend_data <- country_year_totals %>%
  inner_join(gdp_long, by = c("Country", "Year")) %>%
  mutate(Carbon_Intensity = Total_CO2 / GDP)

## STEP 7: Save cleaned datasets
write_csv(final_trend_data, "country_co2_gdp_trends.csv")
write_csv(country_sector_year, "country_sector_co2.csv")

### Data Analysis

# Package for axis formatting
install.packages("scales")

library(scales)

# Load your cleaned data
final_trend_data <- read_csv("country_co2_gdp_trends.csv")

## Plot 1: Carbon Intensity Trends 
carbon_intensity_plot <- ggplot(final_trend_data, 
                                aes(x = Year, y = Carbon_Intensity, color = Country)) +
  
  # Thicker, more prominent lines
  geom_line(linewidth = 1.5, alpha = 0.8) +
  geom_point(size = 3, alpha = 0.8) +
  
  # Labels and title
  labs(
    title = "Carbon Intensity Trends in Major Asian Economies",
    x = "Year",
    y = expression("Carbon Intensity (ton CO"[2]~"/ USD GDP)"),
    color = "Country",
  ) +
  
  # Theme
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, lineheight = 1.2),
    plot.caption = element_text(face = "italic", color = "gray50"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  
  # Color scheme (colorblind friendly)
  scale_color_brewer(palette = "Set1") +
  
  # Axis formatting
  scale_x_continuous(breaks = unique(final_trend_data$Year)) +
  scale_y_continuous(labels = comma_format())

# Display the plot
print(carbon_intensity_plot)

## Plot 2: Percentage Change from Baseline (1975)
# Calculate percentage change in Carbon Intensity for each country (1975 baseline)
carbon_intensity_change <- final_trend_data %>%
  arrange(Country, Year) %>%
  group_by(Country) %>%
  mutate(
    pct_change = (Carbon_Intensity / first(Carbon_Intensity) - 1) * 100,
    abs_change = Carbon_Intensity - first(Carbon_Intensity)
  ) %>%
  ungroup()

# plot 
pct_change_plot <- ggplot(carbon_intensity_change, 
                          aes(x = Year, y = pct_change, color = Country)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2.5) +
  labs(
    title = "Carbon Intensity Change From 1975",
    x = "Year",
    y = "Percentage Change (%)",
    color = "Country",
    fill = "Country"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(breaks = seq(1975, 2022, by = 5)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  
  geom_text(data = carbon_intensity_change %>% 
              group_by(Country) %>% 
              filter(Year == max(Year)),
            aes(label = paste0(round(pct_change, 1), "%")),
            nudge_x = 1, nudge_y = 2, size = 3.5, show.legend = FALSE)

# Display the plot
print(pct_change_plot)
