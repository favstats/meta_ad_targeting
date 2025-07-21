# Load necessary libraries
pacman::p_load(tidyverse, arrow, piggyback, httr)

# Source your helper functions (for get_page_insights and pb_upload)
# Make sure this file exists and is correctly configured.
source("utils.R")

#' @title Scrape and Upload Targeting Data for a Specific Date
#' @description Downloads a daily ad report, scrapes targeting data for all pages in it, and uploads the result to a GitHub release.
#' @param country_code A two-letter ISO code for the country (e.g., "US").
#' @param tf The timeframe in days (e.g., "7", "30").
#' @param report_date The specific date of the report to process, as a string in "YYYY-MM-DD" format.
#' @return A message indicating the outcome.

scrape_by_date <- function(country_code, tf, report_date) {
  
  # --- 1. Define URLs and File Paths ---
  
  message(paste0("▶️ Starting process for ", country_code, " | Timeframe: ", tf, " days | Date: ", report_date))
  
  report_repo <- "favstats/meta_ad_reports2"
  targeting_repo <- "favstats/meta_ad_targeting"
  
  report_tag <- paste0(country_code, "-last_", tf, "_days")
  report_filename <- paste0(report_date, ".rds")
  report_url <- paste0("https://github.com/favstats/", report_repo, "/releases/download/", report_tag, "/", report_filename)
  
  # --- 2. Download the Daily Ad Report ---
  
  message(paste("Downloading daily report from:", report_url))
  
  response <- httr::GET(report_url, write_disk("daily_report.rds", overwrite = TRUE), progress())
  
  if (httr::http_error(response)) {
    stop("❌ ERROR: Could not download the daily report. Please check if a report exists for the specified country, timeframe, and date.")
  }
  
  daily_report <- readRDS("daily_report.rds")
  file.remove("daily_report.rds")
  
  page_ids <- unique(daily_report$page_id)
  message(paste("✅ Report downloaded successfully. Found", length(page_ids), "unique pages to scrape."))
  
  # --- 3. Scrape Targeting Data for Each Page ---
  
  message("Scraping targeting data for each page... (This may take a while)")
  
  # The `possibly` function makes sure that if one page fails, the whole process doesn't stop.
  safe_scraper <- possibly(get_page_insights, otherwise = NULL)
  
  scraped_data <- map_dfr(page_ids, ~ safe_scraper(
    page_id = .x,
    timeframe = paste0("LAST_", tf, "_DAYS"),
    include_info = "targeting_info"
  ))
  
  if (nrow(scraped_data) == 0) {
    stop("❌ ERROR: Scraping produced no data. Halting process.")
  }
  
  message(paste("✅ Scraping complete. Collected data for", nrow(scraped_data), "ads."))
  
  # --- 4. Save and Upload to GitHub ---
  
  output_filename <- paste0(report_date, ".parquet")
  arrow::write_parquet(scraped_data, output_filename)
  
  message(paste0("⬆️ Uploading ", output_filename, " to GitHub release."))
  
  # Assumes `pb_upload` is your helper function from piggyback
  pb_upload(
    file = output_filename,
    repo = targeting_repo,
    tag = report_tag # The tag is the same (e.g., "US-last_7_days")
  )
  
  file.remove(output_filename)
  
  message(paste0("✅ DONE! Pipeline finished successfully for ", report_date, "."))
  
  return(invisible(TRUE))
}


# --- EXAMPLE USAGE ---

# Scrape data for the United States for the last 7 days, using the report from July 4, 2025
# scrape_by_date(country_code = "US", tf = "7", report_date = "2025-07-04")

# Scrape data for Mexico for the last 30 days, using the report from June 1, 2025
# scrape_by_date(country_code = "MX", tf = "30", report_date = "2025-06-01")