# =============================================================================
# DECOUPLING ANALYSIS SCRIPT
# Project: Decoupling Analysis of Asian Economies (1973-2023)
# Description: Comprehensive decoupling analysis using Tapio method with
#              sectoral driver analysis and statistical testing.
# =============================================================================

# -----------------------------------------------------------------------------
# 1. SETUP AND DATA LOADING
# -----------------------------------------------------------------------------

# Load required libraries
library(tidyverse)
library(scales)
library(knitr)
library(broom)        # For tidy regression output
library(car)          # For ANOVA diagnostics
library(patchwork)    # For combining plots
library(ggrepel)      # For better labels
library(kableExtra)   # For tables
library(webshot2)     # For tables

# Load cleaned datasets from previous processing
decoupling_data <- read_csv("decoupling_data_1973_2023.csv")
sector_data <- read_csv("sector_emissions_1973_2023.csv")


# -----------------------------------------------------------------------------
# 2. GDP AND CO2 PERCENTAGE CHANGE VISUALIZATION
# -----------------------------------------------------------------------------

# 2.1 Calculate percentage change from 1973 baseline
co2_gdp_pct_change <- decoupling_data %>%
  arrange(Country, Year) %>%
  group_by(Country) %>%
  mutate(
    gdp_pct_change = (GDP_USD / first(GDP_USD) - 1) * 100,
    co2_pct_change = (Total_CO2_Mt / first(Total_CO2_Mt) - 1) * 100
  ) %>%
  ungroup()

# 2.2 Plot for a specific country
Indonesia <- co2_gdp_pct_change %>%
  filter(Country == "Indonesia") 

# Create the dual axis plot with proper scaling
Indo <- ggplot(Indonesia, aes(x = Year)) +
  # GDP line (primary y-axis)
  geom_line(aes(y = gdp_pct_change, color = "GDP"), size = 1.2) +
  # CO2 line (secondary y-axis)
  geom_line(aes(y = co2_pct_change, color = "CO2 Emissions"), size = 1.2, linetype = "dashed") +
  
  # Scale for primary y-axis (GDP)
  scale_y_continuous(
    name = "GDP Change (%)",
    labels = scales::percent_format(scale = 1),
    sec.axis = sec_axis(~ .,
      name = "CO2 Emissions Change (%)",
      labels = scales::percent_format(scale = 1)
    )
  ) +
  
  scale_color_manual(
    name = "Indicators",
    values = c("GDP" = "steelblue", "CO2 Emissions" = "darkred")
  ) +
  
  labs(
    title = paste("Indonesia"),
    x = "Year"
  ) +
  
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(face = "bold")
  )

# Plot for a specific country
India <- co2_gdp_pct_change %>%
  filter(Country == "India") 

# Create the dual axis plot with proper scaling
Ind <- ggplot(India, aes(x = Year)) +
  # GDP line (primary y-axis)
  geom_line(aes(y = gdp_pct_change, color = "GDP"), size = 1.2) +
  # CO2 line (secondary y-axis)
  geom_line(aes(y = co2_pct_change, color = "CO2 Emissions"), size = 1.2, linetype = "dashed") +
  
  # Scale for primary y-axis (GDP)
  scale_y_continuous(
    name = "GDP Change (%)",
    labels = scales::percent_format(scale = 1),
    sec.axis = sec_axis(~ .,
                        name = "CO2 Emissions Change (%)",
                        labels = scales::percent_format(scale = 1)
    )
  ) +
  
  scale_color_manual(
    name = "Indicators",
    values = c("GDP" = "steelblue", "CO2 Emissions" = "darkred")
  ) +
  
  labs(
    title = paste("India"),
    x = "Year"
  ) +
  
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(face = "bold")
  )

# Plot for a specific country
China <- co2_gdp_pct_change %>%
  filter(Country == "China") 

# Create the dual axis plot with proper scaling
Chi <- ggplot(China, aes(x = Year)) +
  # GDP line (primary y-axis)
  geom_line(aes(y = gdp_pct_change, color = "GDP"), size = 1.2) +
  # CO2 line (secondary y-axis)
  geom_line(aes(y = co2_pct_change, color = "CO2 Emissions"), size = 1.2, linetype = "dashed") +
  
  # Scale for primary y-axis (GDP)
  scale_y_continuous(
    name = "GDP Change (%)",
    labels = scales::percent_format(scale = 1),
    sec.axis = sec_axis(~ .,
                        name = "CO2 Emissions Change (%)",
                        labels = scales::percent_format(scale = 1)
    )
  ) +
  
  scale_color_manual(
    name = "Indicators",
    values = c("GDP" = "steelblue", "CO2 Emissions" = "darkred")
  ) +
  
  labs(
    title = paste("China"),
    x = "Year"
  ) +
  
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(face = "bold")
  )

# Plot for a specific country
South_Korea <- co2_gdp_pct_change %>%
  filter(Country == "South Korea") 

# Create the dual axis plot with proper scaling
Kor <- ggplot(South_Korea, aes(x = Year)) +
  # GDP line (primary y-axis)
  geom_line(aes(y = gdp_pct_change, color = "GDP"), size = 1.2) +
  # CO2 line (secondary y-axis)
  geom_line(aes(y = co2_pct_change, color = "CO2 Emissions"), size = 1.2, linetype = "dashed") +
  
  # Scale for primary y-axis (GDP)
  scale_y_continuous(
    name = "GDP Change (%)",
    labels = scales::percent_format(scale = 1),
    sec.axis = sec_axis(~ .,
                        name = "CO2 Emissions Change (%)",
                        labels = scales::percent_format(scale = 1)
    )
  ) +
  
  scale_color_manual(
    name = "Indicators",
    values = c("GDP" = "steelblue", "CO2 Emissions" = "darkred")
  ) +
  
  labs(
    title = paste("South Korea"),
    x = "Year"
  ) +
  
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(face = "bold")
  )

# Plot for a specific country
Japan <- co2_gdp_pct_change %>%
  filter(Country == "Japan") 

# Create the dual axis plot with proper scaling
Jap <- ggplot(Japan, aes(x = Year)) +
  # GDP line (primary y-axis)
  geom_line(aes(y = gdp_pct_change, color = "GDP"), size = 1.2) +
  # CO2 line (secondary y-axis)
  geom_line(aes(y = co2_pct_change, color = "CO2 Emissions"), size = 1.2, linetype = "dashed") +
  
  # Scale for primary y-axis (GDP)
  scale_y_continuous(
    name = "GDP Change (%)",
    labels = scales::percent_format(scale = 1),
    sec.axis = sec_axis(~ .,
                        name = "CO2 Emissions Change (%)",
                        labels = scales::percent_format(scale = 1)
    )
  ) +
  
  scale_color_manual(
    name = "Indicators",
    values = c("GDP" = "steelblue", "CO2 Emissions" = "darkred")
  ) +
  
  labs(
    title = paste("Japan"),
    x = "Year"
  ) +
  
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(face = "bold")
  )


# 2.3 Combine plots 
combined_patchwork <- (Indo | Ind | Chi) / 
  (Kor | Jap | plot_spacer()) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Add main title above all plots
combined_patchwork <- combined_patchwork + 
  plot_annotation(
    title = "Percentage Change in CO2 Emissions and GDP from 1973 to 2023",
    subtitle = "Baseline year: 1973 (0%) - CO2 emissions (dashed) vs GDP (solid)",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18, margin = margin(b = 10)),
      plot.subtitle = element_text(hjust = 0.5, size = 14, margin = margin(b = 20))
    )
  )

print(combined_patchwork)

# Save
ggsave("combined_plot_with_country_titles.png", combined_patchwork, 
       width = 16, height = 12, dpi = 300)

# 2.4 Table
baseline_summary <- co2_gdp_pct_change %>%
  group_by(Country) %>%
  summarise(
    gdp_change_1973_2023 = last(gdp_pct_change),
    co2_change_1973_2023 = last(co2_pct_change),
    .groups = "drop"
  )

baseline_summary %>%
  kbl(
    digits   = 1,
    col.names = c(
      "Country",
      "GDP change 1973–2023 (%)",
      "CO2 change 1973–2023 (%)"
    ),
    caption = "Total percentage change in GDP and CO2 emissions relative to 1973"
  ) %>%
  kable_styling(full_width = FALSE) %>%
  save_kable(
    file   = "table_baseline_change_1973_2023.png",
    zoom   = 2,
    vwidth = 1200
  )

# -----------------------------------------------------------------------------
# 3. TAPIO DECOUPLING ANALYSIS
# -----------------------------------------------------------------------------


# 3.1 Define Tapio Decoupling Function
# Tapio decoupling index DI is elasticitiy:
# DI = (ΔC% / ΔGDP%) = [(C_T - C_B) / C_B] / [(GDP_T - GDP_B) / GDP_B].
# Here, C is Total_CO2_Mt and GDP is GDP_USD; B and T are consecutive years.

# Compute percentage change (ΔX / X_B) for consecutive years
calc_pct_change <- function(x) {
  # Returns NA for first observation (no previous year)
  c(NA, diff(x) / head(x, -1))
}

# Classify decoupling state according to Tapio’s 8 categories.
classify_tapio <- function(delta_c, delta_gdp, di) {
  # Handle NA or zero GDP change defensively
  if (is.na(delta_c) || is.na(delta_gdp) || is.na(di) || delta_gdp == 0) {
    return(NA_character_)
  }
  
  # Thresholds follow the Macao paper using DI cut-offs 0, 0.8, 1.2.[file:1]
  if (delta_c > 0 && delta_gdp > 0) {
    if (di > 1.2) {
      "expansive negative decoupling"
    } else if (di > 0.8 && di < 1.2) {
      "expansive coupling"
    } else if (di > 0 && di < 0.8) {
      "weak decoupling"
    } else {
      NA_character_
    }
  } else if (delta_c < 0 && delta_gdp > 0) {
    if (di < 0) {
      "strong decoupling"
    } else {
      NA_character_
    }
  } else if (delta_c < 0 && delta_gdp < 0) {
    if (di > 1.2) {
      "recessive decoupling"
    } else if (di > 0.8 && di < 1.2) {
      "recessive coupling"
    } else if (di > 0 && di < 0.8) {
      "weak negative decoupling"
    } else {
      NA_character_
    }
  } else if (delta_c > 0 && delta_gdp < 0) {
    if (di < 0) {
      "strong negative decoupling"
    } else {
      NA_character_
    }
  } else {
    NA_character_
  }
}

# 3.2 Compute year to year Tapio Indices
# Computing decoupling between consecutive years gives a detailed time path of DI for each country.

decoupling_results <- decoupling_data %>%
  group_by(Country) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(
    # Level changes (T - B)
    delta_CO2  = Total_CO2_Mt - lag(Total_CO2_Mt),
    delta_GDP  = GDP_USD - lag(GDP_USD),
    
    # Relative changes (ΔX / X_B)
    rel_CO2    = calc_pct_change(Total_CO2_Mt),
    rel_GDP    = calc_pct_change(GDP_USD),
    
    # Tapio elasticity DI = rel_CO2 / rel_GDP
    DI         = rel_CO2 / rel_GDP,
    
    # Human-readable category using Tapio states
    decoupling_state = pmap_chr(
      list(delta_c = rel_CO2, delta_gdp = rel_GDP, di = DI),
      ~ classify_tapio(..1, ..2, ..3)
    )
  ) %>%
  ungroup()

# Inspect results table (one row per country-year)
head(decoupling_results)


# 3.3 Summarise decoupling states 
# Produce a tidy summary of how often each decoupling category appears
# for each country (year-to-year version).

state_summary <- decoupling_results %>%
  filter(!is.na(decoupling_state)) %>%
  count(Country, decoupling_state, name = "n_years") %>%
  group_by(Country) %>%
  mutate(
    share_years = n_years / sum(n_years)
  ) %>%
  ungroup()

# View summary in console
state_summary

# 3.4 Table 
state_summary_table <- decoupling_results %>%
  filter(!is.na(decoupling_state)) %>%
  count(Country, decoupling_state, name = "n_years") %>%
  group_by(Country) %>%
  mutate(
    share_years   = n_years / sum(n_years),
    share_percent = round(share_years * 100, 1)
  ) %>%
  ungroup() %>%
  arrange(Country, decoupling_state)

state_summary_table %>%
  kbl(
    digits  = 2,
      col.names = c(
        "Country",
        "Decoupling State",
        "No. of Years",
        "Proportion of Total Years ",
        "Percentage of Total Years "
      ),
    caption = "Frequency of Tapio decoupling states by country"
  ) %>%
  kable_styling(full_width = FALSE) %>%
  save_kable(
    file   = "table_decoupling_state_summary.png",
    zoom   = 2,
    vwidth = 1200
  )


# -----------------------------------------------------------------------------
# 4. VISUALIZATION: TIME SERIES OF DI AND STATES
# -----------------------------------------------------------------------------

# Decoupling states as a categorical ribbon/point plot

p_states <- decoupling_results %>%
  filter(!is.na(decoupling_state)) %>%
  ggplot(aes(x = Year, y = Country, fill = decoupling_state)) +
  geom_tile(color = "white") +
  scale_fill_brewer(palette = "Set3", na.value = "grey90") +
  labs(
    title = "Decoupling States",
    subtitle = "Tapio categories for CO2–GDP relationship",
    x = "Year",
    y = "Country",
    fill = "Decoupling state"
  )

 print(p_states)
 
 # Save
 ggsave("tapio_decoupling_states.png", 
        p_states, width = 14, height = 12, dpi = 300, bg = "white")
 
 # -----------------------------------------------------------------------------
 # 5. SECTORAL ANALYSIS
 # -----------------------------------------------------------------------------
 
 # Check the sectors in your new dataset
 print(unique(sector_data$Sector))
 
 
 # 5.1 Sector Emissions Stacked Area Chart
 sector_stacked_enhanced <- ggplot(sector_data, 
                                   aes(x = Year, y = Sector_CO2_Mt, fill = Sector)) +
   
   # Stacked area with outline
   geom_area(alpha = 0.85, linewidth = 0.2, color = "white", 
             position = position_stack(reverse = TRUE)) +
   
   # Facet by country with clean layout
   facet_wrap(~ Country, scales = "free_y", ncol = 2) +
   
   # Labels and titles
   labs(
     title = expression("Evolution of CO"[2]~" Emissions by Economic Sector (1973-2023)"),
     x = "Year",
     y = expression("Annual CO"[2]~" Emissions (Mt CO"[2]~"eq)"),
     fill = "Economic Sector"
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
   
   # Color palette for EDGAR sectors (colorblind friendly)
   scale_fill_manual(
     name = "Sectors",
     values = c(
       "Agriculture" = "#0072B2",           # Blue
       "Buildings" = "#E69F00",             # Orange
       "Fuel Exploitation" = "#009E73",     # Green
       "Industrial Combustion" = "#D55E00", # Vermillion
       "Power Industry" = "#56B4E9",        # Sky Blue
       "Processes" = "#F0E442",             # Yellow
       "Transport" = "#CC79A7",             # Pink
       "Waste" = "#999999"                  # Gray
     )
   ) +
   
   # Axis scaling
   scale_x_continuous(
     breaks = seq(1973, 2023, by = 10),
     expand = expansion(mult = c(0.02, 0.02))
   ) +
   scale_y_continuous(
     labels = scales::comma_format(accuracy = 0.1),
     expand = expansion(mult = c(0, 0.1))
   )
 
 # Display the plot
 print(sector_stacked_enhanced)
 
 # Save high-resolution version
 ggsave("sector_co2_stacked_area_EDGAR.png", sector_stacked_enhanced, 
        width = 16, height = 12, dpi = 400, bg = "white")
 



 # =============================================================================
 # 6. SECTORAL DRIVERS OF DECOUPLING (FINAL SECTION)
 # =============================================================================
 # Goal:
 # - Link Tapio decoupling index (DI) and states to sectoral CO2 composition.
 # - Identify which sectors are associated with better/worse decoupling.
 # - Provide both descriptive comparisons and simple regression evidence.
 # This section builds on objects created earlier:
 #   - decoupling_results (Country–Year DI and decoupling_state)
 #   - sector_data (Country–Year–Sector emissions)
 # =============================================================================
 
 # -----------------------------------------------------------------------------
 # 6.1 Prepare sector shares and merge with decoupling results
 # -----------------------------------------------------------------------------
 # We work with sector shares of total national CO2 for each Country–Year.
 # This allows comparison across countries and over time, independent of scale.
 
 # 6.1.1 Compute sector shares within each Country–Year
 sector_shares <- sector_data %>%
   # Ensure expected column names and types
   rename(
     Country      = Country,
     Year         = Year,
     Sector       = Sector,
     Sector_CO2_Mt = Sector_CO2_Mt
   ) %>%
   select(Country, Year, Sector, Sector_CO2_Mt) %>%
   arrange(Country, Year, Sector) %>%
   group_by(Country, Year) %>%
   mutate(
     total_CO2_year = sum(Sector_CO2_Mt, na.rm = TRUE),
     sector_share   = Sector_CO2_Mt / total_CO2_year
   ) %>%
   ungroup()
 
 # 6.1.2 Aggregate power-sector variables to a Country–Year panel
 # (optional convenience for regression / plotting)
 sector_panel <- sector_shares %>%
   select(Country, Year, Sector, Sector_CO2_Mt, sector_share)
 
 # 6.1.3 Merge sector panel with Tapio DI and decoupling states
 # Result: panel with one row per Country–Year–Sector, including DI and state.
 decoupling_sector_panel <- sector_panel %>%
   left_join(
     decoupling_results %>%
       select(Country, Year, DI, decoupling_state),
     by = c("Country", "Year")
   ) %>%
   # Drop years where DI/state is NA (first year or missing)
   filter(!is.na(DI), !is.na(decoupling_state))
 
 # Quick check (optional)
 # head(decoupling_sector_panel)
 # count(decoupling_sector_panel, Country, Sector)
 
 # -----------------------------------------------------------------------------
 # 6.2 Descriptive sector comparison: strong vs negative decoupling
 # -----------------------------------------------------------------------------
 # We compare average sector shares in:
 #   - "good" decoupling years (strong/weak decoupling)
 #   - "bad" decoupling years (expansive negative / strong negative decoupling)
 # This helps identify which sectors are relatively larger in good vs bad years.
 
 # Define "good" and "bad" state groups explicitly
 good_states <- c("strong decoupling", "weak decoupling")
 bad_states  <- c("expansive negative decoupling", "strong negative decoupling")
 
 # 6.2.1 Compute mean sector shares by Country, Sector, and decoupling group
 sector_decoupling_summary <- decoupling_sector_panel %>%
   mutate(
     decoupling_group = case_when(
       decoupling_state %in% good_states ~ "Good decoupling (strong/weak)",
       decoupling_state %in% bad_states  ~ "Negative decoupling (expansive/strong)",
       TRUE                              ~ NA_character_
     )
   ) %>%
   filter(!is.na(decoupling_group)) %>%
   group_by(Country, Sector, decoupling_group) %>%
   summarise(
     mean_sector_share = mean(sector_share, na.rm = TRUE),
     n_years           = n(),  # number of country–year observations in this group
     .groups           = "drop"
   )
 
 # Optional: save a PNG table summarising sector shares by decoupling group
 sector_decoupling_summary %>%
   arrange(Country, Sector, decoupling_group) %>%
   kbl(
     digits = 3,
     col.names = c(
       "Country",
       "Sector",
       "Decoupling Group",
       "Mean Share of CO2",
       "No. of Years"
     ),
     caption = "Average sector CO2 shares in good vs negative decoupling years"
   ) %>%
   kable_styling(full_width = FALSE) %>%
   save_kable(
     file   = "table_sector_shares_by_decoupling_group.png",
     zoom   = 2,
     vwidth = 1400
   )
 
 # 6.2.2 Visual comparison: bar plot of sector shares in good vs negative years
 sector_decoupling_plot <- sector_decoupling_summary %>%
   ggplot(aes(x = Sector,
              y = mean_sector_share,
              fill = decoupling_group)) +
   geom_col(position = position_dodge(width = 0.8)) +
   facet_wrap(~ Country, scales = "free_y") +
   scale_y_continuous(
     labels = scales::percent_format(accuracy = 1)
   ) +
   scale_fill_brewer(palette = "Set2") +
   labs(
     title    = "Average sectoral CO\u2082 shares in good vs negative decoupling years",
     subtitle = "Good decoupling = strong/weak decoupling; Negative decoupling = expansive/strong negative decoupling",
     x        = "Sector",
     y        = "Mean share of total CO\u2082 emissions",
     fill     = "Decoupling group"
   ) +
   theme_minimal(base_size = 12) +
   theme(
     axis.text.x  = element_text(angle = 45, hjust = 1),
     strip.text   = element_text(face = "bold"),
     plot.title   = element_text(face = "bold", hjust = 0.5),
     legend.position = "bottom"
   )
 
 print(sector_decoupling_plot)
 
 ggsave(
   "sector_shares_good_vs_negative_decoupling.png",
   sector_decoupling_plot,
   width  = 14,
   height = 10,
   dpi    = 300,
   bg     = "white"
 )
 
 # -----------------------------------------------------------------------------
 # 6.3 Simple regression: sector shares as predictors of DI
 # -----------------------------------------------------------------------------
 # Purpose:
 #   Explore whether variation in the Tapio decoupling index (DI)
 #   is statistically associated with sector composition.
 #   This is exploratory, not causal.
 
 # 6.3.1 Reshape sector shares to wide Country–Year format
 # We create one row per Country–Year, with one column per sector's share.
 sector_shares_wide <- sector_shares %>%
   select(Country, Year, Sector, sector_share) %>%
   pivot_wider(
     names_from  = Sector,
     values_from = sector_share,
     values_fill = 0
   )
 
 # Merge with DI at Country–Year level
 di_sector_wide <- decoupling_results %>%
   select(Country, Year, DI) %>%
   filter(!is.na(DI)) %>%
   left_join(sector_shares_wide, by = c("Country", "Year"))
 
 # Optional: inspect available sector columns
 # glimpse(di_sector_wide)
 
 # 6.3.2 Example: pooled regression DI ~ sector shares
 # (You can refine this per country or with fixed effects as needed.)
 # Here we fit a simple linear model for illustration.
 
 sector_cols <- setdiff(names(di_sector_wide), c("Country", "Year", "DI"))
 
 # Build formula DI ~ all sector share columns
 reg_formula <- as.formula(
   paste("DI ~", paste(sector_cols, collapse = " + "))
 )
 
 # Fit model
 di_sector_lm <- lm(reg_formula, data = di_sector_wide)
 
 # Tidy summary (for interpretation in report)
 di_sector_lm_tidy <- broom::tidy(di_sector_lm)
 
 # Optional: save regression results as PNG table
 di_sector_lm_tidy %>%
   kbl(
     digits = 3,
     caption = "Regression of Tapio decoupling index on sectoral CO\u2082 shares (pooled panel)"
   ) %>%
   kable_styling(full_width = FALSE) %>%
   save_kable(
     file   = "table_regression_DI_sector_shares.png",
     zoom   = 2,
     vwidth = 1600
   )
 
 # -----------------------------------------------------------------------------
 # 6.4 Optional scatter plots: DI vs key sector shares
 # -----------------------------------------------------------------------------
 # These plots help visualise associations for selected sectors (e.g. Power, Industry).
 
 # Choose a few key sectors you want to highlight (must match names in sector_data$Sector)
 key_sectors <- c("Power Industry", "Industrial Combustion", "Transport")
 
 # Loop-style approach (no saving inside loop to keep it explicit and reproducible)
 for (s in key_sectors) {
   if (s %in% colnames(di_sector_wide)) {
     p <- di_sector_wide %>%
       ggplot(aes_string(x = s, y = "DI", color = "Country")) +
       geom_point(alpha = 0.6) +
       geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
       scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
       labs(
         title = paste("Tapio decoupling index vs", s, "CO\u2082 share"),
         x     = paste(s, "share of total CO\u2082 emissions"),
         y     = "Tapio decoupling index (DI)",
         color = "Country"
       ) +
       theme_minimal(base_size = 12) +
       theme(
         plot.title = element_text(face = "bold", hjust = 0.5),
         legend.position = "bottom"
       )
     
     print(p)
     
     ggsave(
       filename = paste0("scatter_DI_vs_", gsub(" ", "_", s), "_share.png"),
       plot     = p,
       width    = 10,
       height   = 7,
       dpi      = 300,
       bg       = "white"
     )
   }
 }

