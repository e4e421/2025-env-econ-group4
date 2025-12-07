### Data Cleaning

## Install & load data transformation function package 
install.packages("tidyverse")
library(tidyverse)

## Install & load data visualisation packages
install.packages("patchwork")
library(patchwork)
install.packages("ggplot2")
library(ggplot2)

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

# Load cleaned data
final_trend_data <- read_csv("country_co2_gdp_trends.csv")
# Calculate CO₂ as percentage of GDP
ratio_data <- final_trend_data %>%
  mutate(CO2_as_pct_GDP = (Total_CO2 / GDP) * 100) %>%
  select(Country, Year, CO2_as_pct_GDP)

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
ggsave("Carbon Intensity over time.png", carbon_intensity_plot, 
       width = 16, height = 12, dpi = 400, bg = "white")

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
    title = "Percentage Change in Carbon Intensity Change From 1975",
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
ggsave("Carbon Intensity Percentage change.png", pct_change_plot, 
       width = 16, height = 12, dpi = 400, bg = "white")

## Plot 3: GDP & CO2 Percentage change from 1975
# Calculate percentage change for both GDP and CO2 from 1975 baseline
co2_gdp_change <- final_trend_data %>%
  arrange(Country, Year) %>%
  group_by(Country) %>%
  mutate(
    gdp_pct_change = (GDP / first(GDP) - 1) * 100,
    co2_pct_change = (Total_CO2 / first(Total_CO2) - 1) * 100
  ) %>%
  ungroup()

# Convert to long format for all countries
all_countries_long <- co2_gdp_change %>%
  select(Country, Year, gdp_pct_change, co2_pct_change) %>%
  pivot_longer(cols = c(gdp_pct_change, co2_pct_change), 
               names_to = "Variable", values_to = "Percentage_Change") %>%
  mutate(Variable = case_when(
    Variable == "gdp_pct_change" ~ "GDP",
    Variable == "co2_pct_change" ~ "CO2 Emissions"
  ))

# Small multiples for all countries
small_multiples_dual <- ggplot(all_countries_long, aes(x = Year, y = Percentage_Change, color = Variable)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5, alpha = 0.7) +
  facet_wrap(~ Country, ncol = 2) +
  labs(
    title = "Percentage Change in GDP vs CO2 Emissions by Country (1975-2022)",
    x = "Year",
    y = "Percentage Change from 1975 (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    strip.background = element_rect(fill = "#2c3e50"),
    strip.text = element_text(face = "bold", color = "white"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(breaks = seq(1975, 2022, by = 10)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_color_manual(values = c("GDP" = "#2E8B57", "CO2 Emissions" = "#E41A1C"))

print(small_multiples_dual)
ggsave("GDP & CO2 Percentage change.png", small_multiples_dual, 
       width = 16, height = 12, dpi = 400, bg = "white")


## Decoupling Analysis 
# Calculate Tapio Decoupling Indicators
tapio_decoupling <- final_trend_data %>%
  arrange(Country, Year) %>%
  group_by(Country) %>%
  mutate(
    # Calculate changes relative to baseline (1975 or your chosen baseline year)
    delta_EI = (Total_CO2 - lag(Total_CO2)) / lag(Total_CO2),
    delta_GDP = (GDP - lag(GDP)) / lag(GDP),
    # Tapio Decoupling Index (DI)
    DI = delta_EI / delta_GDP
  ) %>%
  # Remove the first year (no lag values)
  filter(!is.na(DI)) %>%
  # Classify decoupling types
  mutate(
    Decoupling_Type = case_when(
      DI < 0 & delta_EI < 0 & delta_GDP > 0 ~ "Absolute Decoupling",
      DI > 0 & DI < 1 & delta_EI > 0 & delta_GDP > 0 ~ "Relative Decoupling",
      DI > 1 & delta_EI > 0 & delta_GDP > 0 ~ "Non-Decoupling",
      abs(delta_EI) < 0.1 & delta_GDP <= 0 ~ "Stagnant",
      TRUE ~ "Other"
    ),
    # Create a factor with meaningful ordering
    Decoupling_Type = factor(Decoupling_Type,
                             levels = c("Absolute Decoupling", 
                                        "Relative Decoupling", 
                                        "Non-Decoupling", 
                                        "Stagnant",
                                        "Other"))
  ) %>%
  ungroup()

# View the classification summary
decoupling_summary <- tapio_decoupling %>%
  count(Country, Decoupling_Type) %>%
  pivot_wider(names_from = Decoupling_Type, values_from = n, values_fill = 0)

print(decoupling_summary)
# Save the summary table (counts by decoupling type)
write_csv(decoupling_summary, "decoupling_type_summary.csv")


# Plot 1: Decoupling Trajectories Over Time 
decoupling_trajectory_faceted <- ggplot(tapio_decoupling, 
                                        aes(x = Year, y = DI)) +
  # Reference lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  
  
  # Data points and lines
  geom_line(linewidth = 1.2) +
  geom_point(aes(shape = Decoupling_Type), size = 3) +
  
  # Facet by country
  facet_wrap(~ Country, ncol = 2, scales = "free_y") +
  
  # Labels and titles
  labs(
    title = "Tapio Decoupling Analysis: Environmental-Economic Dynamics by Country (1975-2022)",
    x = "Year",
    y = "Decoupling Index (DI)",
    shape = "Decoupling Type"
  ) +
  
  # Theme and formatting
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 15)),
    legend.position = "bottom",
    legend.box = "vertical",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#2c3e50"),
    strip.text = element_text(face = "bold", color = "white", size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  
  # Scales
  scale_x_continuous(breaks = seq(1975, 2022, by = 10)) +
  scale_color_brewer(palette = "Set1") +
  scale_shape_manual(values = c(16, 17, 15, 8, 4))

print(decoupling_trajectory_faceted)
ggsave("decoupling_trajectory_faceted.png", decoupling_trajectory_faceted, 
       width = 16, height = 12, dpi = 400, bg = "white")

## Table 
# Create a summary table
decoupling_performance <- tapio_decoupling %>%
  group_by(Country) %>%
  summarise(
    Years_Absolute_Decoupling = sum(Decoupling_Type == "Absolute Decoupling", na.rm = TRUE),
    Years_Relative_Decoupling = sum(Decoupling_Type == "Relative Decoupling", na.rm = TRUE),
    Years_Non_Decoupling = sum(Decoupling_Type == "Non-Decoupling", na.rm = TRUE),
    Years_Stagnant = sum(Decoupling_Type == "Stagnant", na.rm = TRUE),
    Percent_Absolute = Years_Absolute_Decoupling / n() * 100,
    Avg_DI = mean(DI, na.rm = TRUE),
    Final_State = last(Decoupling_Type),
    .groups = 'drop'
  ) %>%
  arrange(desc(Percent_Absolute))

# Print formatted table
library(knitr)
kable(decoupling_performance, 
      caption = "Decoupling Performance Summary (1975-2022)",
      digits = 2,
      col.names = c("Country", "Abs. Dec.", "Rel. Dec.", "Non-Dec.", 
                    "Stagnant", "% Abs.", "Avg DI", "Final State"))
#Save table
write_csv(decoupling_performance, "decoupling performance.csv")


library(tidyverse)

# 1. Basic table (countries as columns, years as rows)
decoupling_basic <- tapio_decoupling %>%
  mutate(Decoupling_Type = as.character(Decoupling_Type)) %>%
  select(Year, Country, Decoupling_Type) %>%
  pivot_wider(
    names_from = Country,
    values_from = Decoupling_Type
  ) %>%
  arrange(Year)

print("Basic Decoupling Status Table:")
print(decoupling_basic)
#Save table
write_csv(decoupling_basic, "decoupling basic.csv")

# 6. Create a summary of most common state per country
decoupling_summary <- tapio_decoupling %>%
  mutate(Decoupling_Type = as.character(Decoupling_Type)) %>%
  group_by(Country) %>%
  summarise(
    `Most Common State` = names(which.max(table(Decoupling_Type))),
    `% Absolute Decoupling` = round(sum(Decoupling_Type == "Absolute Decoupling") / n() * 100, 1),
    `% Relative Decoupling` = round(sum(Decoupling_Type == "Relative Decoupling") / n() * 100, 1),
    `% Non-Decoupling` = round(sum(Decoupling_Type == "Non-Decoupling") / n() * 100, 1),
    .groups = 'drop'
  )

print("Summary Statistics:")
print(decoupling_summary)




### Sector Emissions
## Plot 1: Emissions Stacked Line Chart 
sector_stacked_enhanced <- ggplot(country_sector_year, 
                                  aes(x = Year, y = Sector_CO2, fill = Sector)) +
  
  # Stacked area with outline
  geom_area(alpha = 0.85, linewidth = 0.2, color = "white", 
            position = position_stack(reverse = TRUE)) +
  
  # Facet by country with clean layout
  facet_wrap(~ Country, scales = "free_y", ncol = 2) +
  
  # Labels and titles
  labs(
    title = expression("Evolution of CO"[2]~" Emissions by Economic Sector (1975-2022)"),
    x = "Year",
    y = expression("Annual CO"[2]~" Emissions (tons)"),
    fill = "Economic Sector",
    caption = "Source: Environmental Database | Note: Y-axis scales vary by country"
  ) +
  
  # Professional theme
  theme_minimal(base_size = 13) +
  theme(
    # Title styling
    plot.title = element_text(
      face = "bold", 
      size = 18, 
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = 14, 
      hjust = 0.5, 
      color = "gray40",
      margin = margin(b = 20)
    ),
    plot.caption = element_text(
      size = 10, 
      color = "gray50",
      margin = margin(t = 15)
    ),
    
    # Facet styling
    strip.background = element_rect(
      fill = "#1e3a5f", 
      color = "#1e3a5f",
      linewidth = 0.5
    ),
    strip.text = element_text(
      face = "bold", 
      color = "white", 
      size = 12,
      margin = margin(5, 0, 5, 0)
    ),
    
    # Axis styling
    axis.title = element_text(
      face = "bold", 
      size = 12
    ),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text.x = element_text(
      angle = 45, 
      hjust = 1,
      size = 10
    ),
    axis.text.y = element_text(size = 10),
    
    # Legend styling
    legend.position = "bottom",
    legend.title = element_text(
      face = "bold", 
      size = 12,
      margin = margin(b = 5)
    ),
    legend.text = element_text(size = 11),
    legend.box = "horizontal",
    legend.box.just = "left",
    legend.margin = margin(t = 10, b = 10),
    
    # Grid and panel styling
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      color = "gray90", 
      linewidth = 0.3
    ),
    panel.spacing = unit(1, "lines"),
    
    # Plot margins
    plot.margin = margin(1, 1.5, 1, 1.5, "cm"),
    
    # Background
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  
  # Color palette
  scale_fill_manual(
    values = c(
      "Energy" = "#1f77b4",      # Blue
      "Industry" = "#ff7f0e",    # Orange
      "Transport" = "#2ca02c",   # Green
      "Residential" = "#d62728", # Red
      "Agriculture" = "#9467bd", # Purple
      "Waste" = "#8c564b"        # Brown
    )
  ) +
  
  # Axis scaling
  scale_x_continuous(
    breaks = seq(1975, 2022, by = 10),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      scale = 1e-6,  # Convert to millions
      suffix = "M",
      accuracy = 0.1
    ),
    expand = expansion(mult = c(0, 0.1))
  ) 
  


# Display the plot
print(sector_stacked_enhanced)

# Save high-resolution version
ggsave("sector_co2_stacked_area_enhanced.png", sector_stacked_enhanced, 
       width = 16, height = 12, dpi = 400, bg = "white")

##Plot 2: Sectoral analysis 2022
# Analyze sector contributions for 2022
sector_2022 <- country_sector_year %>%
  filter(Year == 2022) %>%
  group_by(Country) %>%
  mutate(
    Total_CO2_2022 = sum(Sector_CO2, na.rm = TRUE),
    Sector_Share_2022 = Sector_CO2 / Total_CO2_2022 * 100,
    Rank = rank(-Sector_CO2, ties.method = "first")  # Rank sectors by emissions
  ) %>%
  ungroup() %>%
  arrange(Country, Rank)

# Identify top contributing sector for each country in 2022
top_sectors_2022_enhanced <- sector_2022 %>%
  filter(Rank == 1) %>%
  select(Country, 
         `Dominant Sector` = Sector, 
         `Sector Emissions (tons)` = Sector_CO2,
         `Share of Total (%)` = Sector_Share_2022) %>%
  # Format to 2 decimal places
  mutate(
    `Sector Emissions (tons)` = round(`Sector Emissions (tons)`, 2),
    `Share of Total (%)` = round(`Share of Total (%)`, 2)  # Convert to percentage and round
  ) %>%
  arrange(desc(`Share of Total (%)`))
  arrange(desc(`Share of Total (%)`))
print("Table 1: Dominant CO₂ Emission Sectors by Country (2022)")
print(top_sectors_2022_enhanced)
# Save to CSV
write_csv(top_sectors_2022_enhanced, "dominant_sectors_2022.csv")
