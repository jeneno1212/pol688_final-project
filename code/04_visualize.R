# =============================================================================
# Script 5: Visualize DEI Word Counts in 990 Text Corpus
# Project: DEI Language Change in Nonprofits (2016-2022)
# Author: Jonathan Espinosa
#
# What this script does:
#   1. Loads dei_scores.csv from Script 4
#   2. Produces three publication-ready visualizations:
#        Plot 1 - Line chart by org, faceted by NTEE sector
#        Plot 2 - Grouped bar chart of sector averages by year
#        Plot 3 - HRC Foundation vs. Civil Rights sector average
#   3. Saves all plots as PNG files to the Output folder
#
# Outputs:
#   - Output/plot1_org_trends_by_sector.png
#   - Output/plot2_sector_averages_by_year.png
#   - Output/plot3_hrc_vs_civil_rights.png
# =============================================================================

setwd("C:/Users/jenen/JE/2026/Arizona/Spring 2026/POL 688/Final Project/Data")

library(tidyverse)
library(ggplot2)

# --- Configuration -----------------------------------------------------------

data_folder   <- "C:/Users/jenen/JE/2026/Arizona/Spring 2026/POL 688/Final Project/Data/Data"
output_folder <- "C:/Users/jenen/JE/2026/Arizona/Spring 2026/POL 688/Final Project/Output"

if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

# --- Load data ---------------------------------------------------------------

dei <- read_csv(file.path(data_folder, "dei_scores.csv"), show_col_types = FALSE)

# Ensure year is numeric
dei <- dei %>% mutate(year = as.integer(year))

# --- Shared theme ------------------------------------------------------------

base_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 15, hjust = 0),
    plot.subtitle = element_text(size = 11, hjust = 0, color = "grey40"),
    plot.caption  = element_text(size = 9, color = "grey50", hjust = 0),
    axis.title    = element_text(size = 11),
    axis.text     = element_text(size = 10),
    legend.title  = element_text(size = 10, face = "bold"),
    legend.text   = element_text(size = 9),
    strip.text    = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    plot.margin   = margin(15, 15, 10, 15)
  )

# =============================================================================
# Plot 1: Org-level trend lines, faceted by NTEE sector
# =============================================================================

# Color palette — one color per org within each facet
org_colors <- c(
  # Arts & Culture
  "Met Museum of Art" = "#1b7837",
  "Lincoln Center"    = "#5aae61",
  "Kennedy Center"    = "#a6dba0",
  # Education
  "NAACP LDF"         = "#2166ac",
  "UNCF"              = "#6baed6",
  "Teach For America" = "#08519c",
  # Civil Rights
  "ACLU Foundation"   = "#d6604d",
  "National Urban League" = "#f4a582",
  "HRC Foundation"    = "#b2182b",
  # Human Services
  "United Way"        = "#762a83",
  "Goodwill Intl"     = "#af8dc3",
  "American Red Cross"= "#e7d4e8"
)

p1 <- ggplot(dei, aes(x = year, y = dei_word_count,
                       color = org_name, group = org_name)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  facet_wrap(~ ntee_label, ncol = 2, scales = "free_y") +
  scale_color_manual(values = org_colors, name = "Organization") +
  scale_x_continuous(breaks = c(2016, 2018, 2020, 2022)) +
  labs(
    title    = "DEI Language in Nonprofit 990 Filings by Sector (2016–2022)",
    subtitle = "Total count of DEI lexicon terms per filing year",
    x        = "Filing Year",
    y        = "DEI Word Count (Raw)",
    caption  = "Sources: IRS Form 990 filings via ProPublica. DEI lexicon from Cornell University DEI Glossary\nand NSF leaked trigger word list (2025). Note: some org-years missing due to unavailable PDFs."
  ) +
  base_theme +
  theme(legend.position = "right")

ggsave(file.path(output_folder, "plot1_org_trends_by_sector.png"),
       plot = p1, width = 11, height = 8, dpi = 300)
cat("Saved Plot 1\n")

# =============================================================================
# Plot 2: Grouped bar chart — sector averages by year
# =============================================================================

sector_avg <- dei %>%
  group_by(ntee_label, year) %>%
  summarise(avg_dei = round(mean(dei_word_count), 1), .groups = "drop")

sector_colors <- c(
  "Arts & Culture"  = "#5aae61",
  "Education"       = "#2166ac",
  "Civil Rights"    = "#d6604d",
  "Human Services"  = "#762a83"
)

p2 <- ggplot(sector_avg, aes(x = factor(year), y = avg_dei,
                              fill = ntee_label)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(aes(label = avg_dei),
            position = position_dodge(width = 0.75),
            vjust = -0.5, size = 3.2, fontface = "bold") +
  scale_fill_manual(values = sector_colors, name = "NTEE Sector") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Average DEI Word Count by Sector and Year (2016–2022)",
    subtitle = "Mean number of DEI lexicon terms per 990 filing, averaged within each sector",
    x        = "Filing Year",
    y        = "Average DEI Word Count",
    caption  = "Sources: IRS Form 990 filings via ProPublica. DEI lexicon from Cornell University DEI Glossary\nand NSF leaked trigger word list (2025)."
  ) +
  base_theme +
  theme(legend.position = "bottom")

ggsave(file.path(output_folder, "plot2_sector_averages_by_year.png"),
       plot = p2, width = 10, height = 6.5, dpi = 300)
cat("Saved Plot 2\n")

# =============================================================================
# Plot 3: HRC Foundation vs. Civil Rights sector average
# =============================================================================

civil_rights_avg <- dei %>%
  filter(ntee_label == "Civil Rights") %>%
  group_by(year) %>%
  summarise(avg_dei = mean(dei_word_count), .groups = "drop") %>%
  mutate(org_name = "Civil Rights Sector Average")

hrc <- dei %>%
  filter(org_name == "HRC Foundation") %>%
  select(year, avg_dei = dei_word_count, org_name)

hrc_plot_data <- bind_rows(hrc, civil_rights_avg)

p3 <- ggplot(hrc_plot_data, aes(x = year, y = avg_dei,
                                  color = org_name, group = org_name)) +
  geom_line(aes(linetype = org_name), linewidth = 1.4) +
  geom_point(size = 3) +
  geom_text(aes(label = round(avg_dei, 1)),
            vjust = -1, size = 3.5, show.legend = FALSE) +
  scale_color_manual(
    values = c("HRC Foundation" = "#b2182b",
               "Civil Rights Sector Average" = "#999999"),
    name = ""
  ) +
  scale_linetype_manual(
    values = c("HRC Foundation" = "solid",
               "Civil Rights Sector Average" = "dashed"),
    name = ""
  ) +
  scale_x_continuous(breaks = c(2016, 2018, 2020, 2022)) +
  scale_y_continuous(limits = c(0, 240)) +
  labs(
    title    = "HRC Foundation vs. Civil Rights Sector Average DEI Word Count (2016–2022)",
    subtitle = "HRC Foundation's DEI language grew sharply while sector peers remained relatively stable",
    x        = "Filing Year",
    y        = "DEI Word Count (Raw)",
    caption  = "Sources: IRS Form 990 filings via ProPublica. DEI lexicon from Cornell University DEI Glossary\nand NSF leaked trigger word list (2025). Sector average includes ACLU Foundation and National Urban League."
  ) +
  base_theme +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10))

ggsave(file.path(output_folder, "plot3_hrc_vs_civil_rights.png"),
       plot = p3, width = 9, height = 6, dpi = 300)
cat("Saved Plot 3\n")

cat("\n--- Script 5 complete. Check your Output folder for all three plots. ---\n")
