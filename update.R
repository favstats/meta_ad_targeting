library(tidyverse)
library(httr)
library(rvest)

source("helpers.R")
source("utils.R")

sets <- list()
sets$cntry <- "OO"

# jsonlite::read_json("settings.json")

full_cntry_list <-
  read_rds("https://github.com/favstats/meta_ad_reports/raw/main/cntry_list.rds") %>%
  rename(iso2c = iso2,
         country = cntry) %>%
  sample_n(n()) %>% 
  # mutate(iso2c = fct_relevel(iso2c, eu_countries)) %>% 
  arrange(iso2c) %>% 
  sample_n(n())

out <- full_cntry_list$iso2c %>%
  map( ~ {
    .x %>%
      paste0(c("-last_7_days", "-last_30_days",
               "-last_90_days"))
  }) %>%
  unlist() %>%
  # keep( ~ str_detect(.x, tf)) %>%
  # .[100:120] %>%
  map_dfr_progress( ~ {
    the_assets <-
      httr::GET(
        paste0(
          "https://github.com/favstats/meta_ad_targeting/releases/expanded_assets/",
          .x
        )
      )
    
    the_assets %>% httr::content() %>%
      html_elements(".Box-row") %>%
      html_text()  %>%
      tibble(raw = .)   %>%
      # Split the raw column into separate lines
      mutate(raw = strsplit(as.character(raw), "\n")) %>%
      # Extract the relevant lines for filename, file size, and timestamp
      transmute(
        filename = sapply(raw, function(x)
          trimws(x[3])),
        file_size = sapply(raw, function(x)
          trimws(x[6])),
        timestamp = sapply(raw, function(x)
          trimws(x[7]))
      ) %>%
      filter(filename != "Source code") %>%
      mutate(release = .x) %>%
      mutate_all(as.character)
  })

thosearethere <- out %>%
  rename(tag = release,
         file_name = filename) %>%
  arrange(desc(tag)) %>%
  separate(
    tag,
    into = c("cntry", "tframe"),
    remove = F,
    sep = "-"
  ) %>%
  mutate(date  = str_remove(file_name, "\\.rds|\\.zip|\\.parquet") %>% lubridate::ymd()) %>%
  distinct(cntry, date, tframe) %>%
  drop_na(date) %>%
  arrange(desc(date))

retrieve_em <- function(iso2c, date, tf = "30") {
  
  read_rds(glue::glue("https://github.com/favstats/wtm_{iso2c}/raw/main/historic/{date}/{tf}.rds")) %>% 
    mutate(cntry = iso2c) %>% 
    arrow::write_parquet(glue::glue("{date}.parquet"))
  
  Sys.sleep(1)
  print(file.exists(glue::glue("{date}.parquet")))
  
  # if(!(identical(latest_elex, election_dat))){
  releases <- readRDS("C:/Users/fabio/Dropbox/postdoc/meta_ad_targeting/data/releases.rds")    
  
    # print("###########C")
    the_tag <- glue::glue("{iso2c}-last_{tf}_days")
    print(the_tag)
    try({
      # print(paste0(the_date, ".rds"))
      # print(the_tag)
      # debugonce(pb_upload_file_fr)
      # debugonce(pb_upload_file_fr)
      pb_upload_file_fr(
        glue::glue("{date}.parquet"),
        repo = "favstats/meta_ad_targeting",
        tag = the_tag,
        releases = releases, skip = T
      )
      # pb_upload_file_fr(paste0(the_date, ".zip"), repo = "favstats/meta_ad_reports", tag = the_tag, releases = full_repos)
      
    })
    
    file.remove(glue::glue("{date}.parquet"))
}

retrieve_em2 <- possibly(retrieve_em, otherwise = NULL, quiet = F)



some_audiences <- full_cntry_list$iso2c %>% 
  expand_grid(cntry = . ,date = seq.Date(as.Date("2023-12-01"), as.Date("2024-03-06"), "day")) %>% 
  expand_grid(tframe = c("last_7_days", "last_30_days", "last_90_days")) %>% 
  anti_join(thosearethere) %>% 
  split(1:nrow(.)) %>% 
  walk_progress(~{retrieve_em2(.x$cntry, .x$date)})
