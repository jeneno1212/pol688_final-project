# =============================================================================
# Script 3: Extract Text from Downloaded 990 PDFs (OCR version)
# =============================================================================

setwd("C:/Users/jenen/JE/2026/Arizona/Spring 2026/POL 688/Final Project/Data")


install.packages(c("pdftools", "tesseract", "tidyverse", "stringr", "png"))
library(pdftools)
library(tesseract)
library(tidyverse)
library(stringr)
library(png)

# --- Configuration -----------------------------------------------------------

pdf_folder  <- "C:/Users/jenen/JE/2026/Arizona/Spring 2026/POL 688/Final Project/Data/Data/PDFs"
text_folder <- "C:/Users/jenen/JE/2026/Arizona/Spring 2026/POL 688/Final Project/Data/Data/Text"
data_folder <- "C:/Users/jenen/JE/2026/Arizona/Spring 2026/POL 688/Final Project/Data/Data"

if (!dir.exists(text_folder)) dir.create(text_folder, recursive = TRUE)

eng <- tesseract("eng")

# --- Org lookup table --------------------------------------------------------

orgs <- data.frame(
  org_name = c("NAACP LDF", "UNCF", "Teach For America",
               "United Way", "Goodwill Intl", "American Red Cross",
               "ACLU Foundation", "National Urban League", "HRC Foundation",
               "Lincoln Center", "Kennedy Center", "Met Museum of Art"),
  ein      = c("131655255", "131624241", "133541913",
               "131635294", "530196517", "530196605",
               "136213516", "131840489", "521481896",
               "131847137", "530245017", "131624086"),
  ntee     = c("B","B","B","P","P","P","R","R","R","A","A","A"),
  ntee_label = c("Education","Education","Education",
                 "Human Services","Human Services","Human Services",
                 "Civil Rights","Civil Rights","Civil Rights",
                 "Arts & Culture","Arts & Culture","Arts & Culture"),
  stringsAsFactors = FALSE
)

# --- Find PDFs ---------------------------------------------------------------

pdf_files <- list.files(pdf_folder, pattern = "\\.pdf$", full.names = TRUE)
cat("Found", length(pdf_files), "PDF files\n\n")

# --- Extraction loop ---------------------------------------------------------

corpus <- data.frame()

for (pdf_path in pdf_files) {
  
  file_name <- basename(pdf_path)
  parts     <- str_match(file_name, "^(\\d+)_(\\d{4})\\.pdf$")
  
  if (is.na(parts[1])) {
    cat("Skipping - bad filename:", file_name, "\n")
    next
  }
  
  ein  <- parts[2]
  year <- as.integer(parts[3])
  
  org_info <- orgs[orgs$ein == ein, ]
  if (nrow(org_info) == 0) {
    cat("Skipping - EIN not in org list:", ein, "\n")
    next
  }
  
  org_name   <- org_info$org_name
  ntee       <- org_info$ntee
  ntee_label <- org_info$ntee_label
  
  cat("Processing:", org_name, year, "...\n")
  
  # Render each page to an image, then OCR it
  raw_text <- tryCatch({
    num_pages <- pdf_info(pdf_path)$pages
    page_texts <- c()
    
    for (i in 1:num_pages) {
      # Render page as a raw raster image (300 DPI)
      img_raw <- pdf_render_page(pdf_path, page = i, dpi = 300, numeric = FALSE)
      
      # Save to temp PNG
      img_path <- file.path(tempdir(), paste0(ein, "_", year, "_p", i, ".png"))
      writePNG(img_raw, img_path)
      
      # OCR the image
      page_text <- ocr(img_path, engine = eng)
      page_texts <- c(page_texts, page_text)
    }
    
    paste(page_texts, collapse = "\n")
    
  }, error = function(e) {
    cat("  ERROR:", e$message, "\n")
    return(NA_character_)
  })
  
  if (is.na(raw_text)) next
  
  clean_text <- raw_text %>%
    str_replace_all("\\s+", " ") %>%
    str_to_lower() %>%
    str_trim()
  
  word_count <- str_count(clean_text, "\\S+")
  cat("  Pages:", pdf_info(pdf_path)$pages, "| Words:", word_count, "\n")
  
  txt_path <- file.path(text_folder, paste0(ein, "_", year, ".txt"))
  writeLines(clean_text, txt_path)
  
  corpus <- bind_rows(corpus, data.frame(
    org_name, ein, ntee, ntee_label, year,
    word_count, text = clean_text,
    stringsAsFactors = FALSE
  ))
}

# --- Save outputs ------------------------------------------------------------

saveRDS(corpus, file.path(data_folder, "text_corpus.rds"))
corpus %>%
  select(org_name, ein, ntee, ntee_label, year, word_count) %>%
  write_csv(file.path(data_folder, "text_corpus_index.csv"))

cat("\n=== Done ===\n")
cat("Documents processed:", nrow(corpus), "\n")
cat("Avg word count:", round(mean(corpus$word_count, na.rm = TRUE)), "\n")
cat("\n--- Script 3 complete. Run Script 4 to score DEI language ---\n")
