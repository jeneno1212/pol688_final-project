# =============================================================================
# Script 2: Build PDF URL Log from ProPublica Nonprofit Explorer API
# Project: DEI Language Change in Nonprofits (2016-2022)
# Author: Jonathan Espinosa
#
# What this script does:
#   1. Hits the ProPublica Nonprofit Explorer API for each of 12 organizations
#   2. Identifies the PDF URL for each target year (2016, 2018, 2020, 2022)
#   3. Writes a URL log (pdf_url_log.csv) used to download PDFs manually
#
# Why manual download:
#   Automated download attempts (RSelenium, chromote, httr direct download)
#   all failed against ProPublica. RSelenium required a missing Java dependency,
#   chromote sessions timed out, and direct httr requests returned HTTP 403
#   because ProPublica requires real browser session cookies. The reliable path
#   was to use this script to harvest the URLs, then download PDFs by hand from
#   a logged-in browser session.
#
# Outputs:
#   - Data/pdf_url_log.csv : org_name, ein, ntee, year, pdf_url, save_as
#
# Required packages:
#   httr, jsonlite, tidyverse
# =============================================================================

# --- Packages ----------------------------------------------------------------
# install.packages(c("httr", "jsonlite", "tidyverse"))
library(httr)
library(jsonlite)
library(tidyverse)

# --- Configuration -----------------------------------------------------------

# 12 organizations across 4 NTEE sectors:
#   A = Arts & Culture, B = Education, P = Human Services, R = Civil Rights
orgs <- data.frame(
  org_name = c("NAACP LDF", "UNCF", "Teach For America",
               "United Way", "Goodwill Intl", "American Red Cross",
               "ACLU Foundation", "National Urban League", "HRC Foundation",
               "Lincoln Center", "Kennedy Center", "Met Museum of Art"),
  ein = c("131655255", "131624241", "133541913",
          "131635294", "530196517", "530196605",
          "136213516", "131840489", "521481896",
          "131847137", "530245017", "131624086"),
  ntee = c("B", "B", "B",
           "P", "P", "P",
           "R", "R", "R",
           "A", "A", "A"),
  stringsAsFactors = FALSE
)

# Target tax years (every other year 2016-2022)
target_years <- c(2016, 2018, 2020, 2022)

# Create output directory if needed
if (!dir.exists("Data")) dir.create("Data")

# --- Harvest URLs from ProPublica API ----------------------------------------

url_log <- data.frame()

cat("Building PDF URL log for", nrow(orgs), "orgs x",
    length(target_years), "years...\n\n")

for (i in 1:nrow(orgs)) {

  org  <- orgs$org_name[i]
  ein  <- orgs$ein[i]
  ntee <- orgs$ntee[i]

  cat("Fetching filing history for:", org, "(EIN:", ein, ")\n")

  api_url  <- paste0("https://projects.propublica.org/nonprofits/api/v2/organizations/",
                     ein, ".json")
  response <- tryCatch(
    GET(api_url, timeout(30)),
    error = function(e) { cat("  API error:", e$message, "\n"); return(NULL) }
  )

  if (is.null(response) || status_code(response) != 200) {
    cat("  Could not reach API for", org, "\n")
    next
  }

  d <- fromJSON(content(response, as = "text", encoding = "UTF-8"))

  # Combine filings with and without data — both contain pdf_url and tax_prd_yr
  filings <- bind_rows(
    if (!is.null(d$filings_with_data) && nrow(d$filings_with_data) > 0)
      d$filings_with_data %>% select(tax_prd_yr, pdf_url)
    else data.frame(),
    if (!is.null(d$filings_without_data) && nrow(d$filings_without_data) > 0)
      d$filings_without_data %>% select(tax_prd_yr, pdf_url)
    else data.frame()
  )

  # For each target year, log either the URL or "NOT AVAILABLE"
  for (yr in target_years) {

    match <- filings %>% filter(tax_prd_yr == yr)

    pdf_url_value <- if (nrow(match) == 0 ||
                         is.na(match$pdf_url[1]) ||
                         match$pdf_url[1] == "") {
      "NOT AVAILABLE"
    } else {
      as.character(match$pdf_url[1])
    }

    url_log <- bind_rows(url_log, data.frame(
      org_name = org,
      ein      = ein,
      ntee     = ntee,
      year     = yr,
      pdf_url  = pdf_url_value,
      save_as  = paste0(ein, "_", yr, ".pdf"),
      stringsAsFactors = FALSE
    ))

    cat("  ", yr, ":", ifelse(pdf_url_value == "NOT AVAILABLE",
                              "NOT AVAILABLE", "URL captured"), "\n")
  }

  cat("\n")
  Sys.sleep(0.5)  # polite pause between API calls
}

# --- Save the URL log --------------------------------------------------------

write_csv(url_log, "Data/pdf_url_log.csv")

# --- Summary -----------------------------------------------------------------

cat("=== URL Log Summary ===\n")
cat("URLs captured:", sum(url_log$pdf_url != "NOT AVAILABLE"), "\n")
cat("Not available:", sum(url_log$pdf_url == "NOT AVAILABLE"), "\n")
cat("\nLog saved to Data/pdf_url_log.csv\n")
cat("\n--- Next step: Open the log, click each URL, save PDFs to Data/PDFs/ ---\n")
cat("--- using the save_as filename. Then run Script 3 to extract text.    ---\n")
