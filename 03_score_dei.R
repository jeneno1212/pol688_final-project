# =============================================================================
# Script 4.1: Count DEI Lexicon Terms in 990 Text Corpus (Cleaned Lexicon)
# Project: DEI Language Change in Nonprofits (2016-2022)
# Author: Jonathan Espinosa
#
# What this script does:
#   1. Loads the text corpus from Script 3
#   2. Counts DEI lexicon terms using a CLEANED lexicon
#   3. Overwrites dei_scores.csv and dei_term_counts.csv for use in Script 5
#
# Changes from Script 4:
#   The following 6 terms were removed from the lexicon after reviewing
#   dei_term_counts.csv. Each was flagged as a likely false positive —
#   high-frequency terms appearing in non-DEI contexts in nonprofit 990 language:
#
#     - "ability"      : overwhelmingly used as "ability to" (boilerplate)
#     - "respect"      : overwhelmingly used as "with respect to" (boilerplate)
#     - "liberation"   : concentrated in Arts sector, likely artistic programming
#     - "excluded"     : likely financial/legal usage ("excluded from")
#     - "historically" : likely temporal usage ("historically, the org...")
#     - "culture"      : concentrated in Arts sector, refers to artistic culture
#
# Outputs:
#   - Data/dei_scores.csv      : one row per org-year with total DEI word count
#   - Data/dei_term_counts.csv : long-format term-level counts per document
# =============================================================================

setwd("C:/Users/jenen/JE/2026/Arizona/Spring 2026/POL 688/Final Project/Data")

library(tidyverse)
library(stringr)

# --- Configuration -----------------------------------------------------------

data_folder <- "C:/Users/jenen/JE/2026/Arizona/Spring 2026/POL 688/Final Project/Data/Data"

# --- Cleaned DEI Lexicon -----------------------------------------------------
# Removed: ability, respect, liberation, excluded, historically, culture

dei_lexicon <- unique(tolower(trimws(c(
  # Cornell DEI Glossary terms
  "acknowledgement", "active listening", "advocacy", "allyship",
  "antiracism", "antiracist", "belonging", "bias", "bias incident",
  "binary", "bipoc", "bystander effect", "calling in", "calling out",
  "coalescing", "cycle of liberation", "cycle of socialization",
  "dehumanization", "dialogue", "discrimination", "diversity",
  "emotional labor", "empathy", "ethnicity", "equity",
  "gender identity", "growth mindset", "impact", "implicit bias",
  "inclusion", "inclusive", "indigenous", "individual racism",
  "institutionalized racism", "intersection of identities",
  "latinx", "leadership", "macroaggressions", "marginalized",
  "marginalized identities", "micro-affirmations", "microaggressions",
  "mindfulness", "neurodiversity", "oppression", "oppressor",
  "person of color", "people of color", "power dynamics", "prejudice",
  "privileged identities", "psychological safety", "race", "racial",
  "racism", "racist", "religious identity", "resilience",
  "sexual orientation", "social class", "social identities", "socialization",
  "solidarity", "stereotype", "stereotypes", "structural inequality",
  "systemic racism", "transparency",

  # NSF Leaked DEI Trigger Words
  "activism", "activists", "advocate", "barrier", "barriers",
  "biased", "black and latinx", "community diversity", "community equity",
  "cultural differences", "cultural heritage", "culturally responsive",
  "disabilities", "discriminatory", "diversified", "diversify",
  "equal opportunity", "equality", "equitable", "excluded", "female",
  "fostering", "gender", "hate speech", "hispanic minority",
  "inequalities", "inequities", "lgbtq", "marginalize", "minorities",
  "multicultural", "polarization", "privileges", "promoting", "justice",
  "sense of belonging", "social justice", "sociocultural", "socioeconomic",
  "underappreciated", "underrepresented", "underserved", "victim", "women"
))))

cat("Cleaned DEI lexicon:", length(dei_lexicon), "unique terms\n")
cat("(Removed: ability, respect, liberation, excluded, historically, culture)\n\n")

# --- Load corpus -------------------------------------------------------------

corpus <- readRDS(file.path(data_folder, "text_corpus.rds"))
cat("Documents loaded:", nrow(corpus), "\n\n")

# --- Count DEI terms per document --------------------------------------------

scores_list      <- list()
term_counts_list <- list()

for (i in 1:nrow(corpus)) {

  org_name   <- corpus$org_name[i]
  ein        <- corpus$ein[i]
  ntee       <- corpus$ntee[i]
  ntee_label <- corpus$ntee_label[i]
  year       <- corpus$year[i]
  text       <- tolower(corpus$text[i])

  # Count each term
  counts <- sapply(dei_lexicon, function(term) str_count(text, fixed(term)))

  total_dei_words <- sum(counts)

  cat("Scored:", org_name, year, "| DEI word count:", total_dei_words, "\n")

  # Summary row
  scores_list[[i]] <- data.frame(
    org_name       = org_name,
    ein            = ein,
    ntee           = ntee,
    ntee_label     = ntee_label,
    year           = year,
    word_count     = corpus$word_count[i],
    dei_word_count = total_dei_words,
    stringsAsFactors = FALSE
  )

  # Term-level rows (only terms that appear at least once)
  term_counts_list[[i]] <- data.frame(
    org_name = org_name,
    ein      = ein,
    ntee     = ntee,
    year     = year,
    term     = names(counts),
    count    = as.integer(counts),
    stringsAsFactors = FALSE
  ) %>% filter(count > 0)
}

# --- Save outputs (overwrites Script 4 files) --------------------------------

dei_scores      <- bind_rows(scores_list)
dei_term_counts <- bind_rows(term_counts_list)

write_csv(dei_scores,      file.path(data_folder, "dei_scores.csv"))
write_csv(dei_term_counts, file.path(data_folder, "dei_term_counts.csv"))

# --- Summary -----------------------------------------------------------------

cat("\n=== Summary ===\n")
cat("Documents scored:", nrow(dei_scores), "\n")
cat("Avg DEI word count:", round(mean(dei_scores$dei_word_count), 1), "\n\n")

cat("DEI word count by NTEE sector:\n")
dei_scores %>%
  group_by(ntee_label) %>%
  summarise(avg = round(mean(dei_word_count), 1), .groups = "drop") %>%
  print()

cat("\nDEI word count by year:\n")
dei_scores %>%
  group_by(year) %>%
  summarise(avg = round(mean(dei_word_count), 1), .groups = "drop") %>%
  print()

cat("\nTop 10 most frequent DEI terms across all documents:\n")
dei_term_counts %>%
  group_by(term) %>%
  summarise(total = sum(count), .groups = "drop") %>%
  arrange(desc(total)) %>%
  slice_head(n = 10) %>%
  print()

cat("\n--- Script 4.1 complete. Run Script 5 (unchanged) to visualize results ---\n")
