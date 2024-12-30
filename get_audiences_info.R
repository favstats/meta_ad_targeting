# Get command-line arguments
# tf <- commandArgs(trailingOnly = TRUE)
# rate_limit <<- F
try({
  
  outcome <- commandArgs(trailingOnly = TRUE)
  
  sets <- list()
  tf <- outcome[1]
  sets$cntry <- outcome[2]
  # here::i_am("wtm_mx.Rproj")
  
  print(outcome)
  
  # setwd("template")
  # getwd()
  source("utils.R")
  # ?get_targeting
  # get_targeting("41459763029", timeframe = "LAST_90_DAYS")
  # debugonce(get_targeting)
  
  library(httr)
  library(httr2)
  library(tidyverse)
  library(lubridate)
  library(rvest)
  library(piggyback)
  # library(metatargetr)
  
  try({
    
    # Step 3: Decrypt the file and read content
    decrypted_content <- decrypt_file("data/ips-targeting.enc")
    ips_targeting <- str_split(decrypted_content, "\n", simplify = F)
    
    ips_targeting <- unlist(ips_targeting)[-1:-2]
  })
  
  # sets <- jsonlite::fromJSON("settings.json")
  #
  # title_txt <- read_lines("_site/_quarto.yml")
  # title_txt[which(str_detect(title_txt, "title"))[1]] <-  glue::glue('  title: "{sets$dashboard}"')
  # write_lines(title_txt, "_site/_quarto.yml")
  
  
  eu_countries <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", 
                    "FR", "GR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", 
                    "NL", "PL", "PT", "RO", "SE", "SI", "SK", "US", "MX", "NZ", 
                    "CA", "AU")
  
  full_cntry_list <-
    read_rds("https://github.com/favstats/meta_ad_reports/raw/main/cntry_list.rds") %>%
    rename(iso2c = iso2,
           country = cntry) %>%
    sample_n(n()) %>% 
    mutate(iso2c = fct_relevel(iso2c, eu_countries)) %>% 
    arrange(iso2c) 
  # filter(iso2c %in% c("MT", "NP", "AM", "FR", "XK"))
  # slice(1:5)
  
  # full_cntry_list$iso2c %>% dput()
  
  if (Sys.info()[["effective_user"]] %in% c("fabio", "favstats")) {
    ### CHANGE ME WHEN LOCAL!
    tf <- "30"
    sets$cntry <- "HR"
    print(paste0("TF: ", tf))
    print(paste0("cntry: ", sets))
    
  }
  
  targeting <- F

  
  unlink("targeting", recursive = T, force = T)
  unlink("historic", recursive = T, force = T)
  unlink("info", recursive = T, force = T)
  
  tstamp <- Sys.time()
  
  write_lines(lubridate::as_date(tstamp), "tstamp.txt")
  
  country_codes <- c(
    "AD", "AL", "AM", "AR", "AT", "AU", "BA",
    "BE", "BG", "BR", "CA", "CH", "CL", "CO", 
    "CY", "CZ", "DE", "DK", "EC", "EE", "ES", 
    "FI", "FR", "GB", "GR", "GT", "HR", "HU",
    "IE", "IN", "IS", "IT", "LI", "LT", "LU",
    "LV", "MD", "ME", "MK", "MT", "MX", "NL", 
    "NO", "NZ", "PL", "PT", "RO", "RS", "SE",
    "SI", "SK", "SM", "TR", "UA", "US", "VE",
    "ZA"
  )
  
  print("################ WTM DATA ################")
  
  
  # try({
  #   download.file(
  #     paste0(
  #       "https://data-api.whotargets.me/advertisers-export-csv?countries.alpha2=",
  #       str_to_lower(sets$cntry)
  #     ),
  #     destfile = "data/wtm_advertisers.csv"
  #   )
  #   
  #   thedat <- read_csv("data/wtm_advertisers.csv")
  #   
  # })
  
  if (!exists("thedat")) {
    thedat <- tibble(no_data = NULL)
  }
  
  
  if (sets$cntry %in% country_codes & nrow(thedat) != 0) {
    wtm_data <- read_csv("data/wtm_advertisers.csv") %>% #names
      select(page_id = advertisers_platforms.advertiser_platform_ref,
             page_name = name,
             party = entities.short_name)  %>%
      mutate(page_id = as.character(page_id)) %>%
      mutate(sources = "wtm")
    
  } else {
    wtm_data <-  tibble(no_data = T)
  }
  
  polsample <- readRDS("data/polsample.rds")
  
  tep_dat <- polsample %>%
    filter(cntry %in% sets$cntry) %>%
    mutate(sources = "tep") %>%
    rename(party = name_short)
  
  print("################ CHECK LATEST REPORT ################")
  
  
  try({
    out <- sets$cntry %>%
      map( ~ {
        .x %>%
          paste0(c(
            "-yesterday",
            "-last_7_days",
            "-last_30_days",
            "-last_90_days"
          ))
      }) %>%
      unlist() %>%
      .[str_detect(., "last_90_days")] %>%
      # .[100:120] %>%
      map_dfr( ~ {
        the_assets <-
          httr::GET(
            paste0(
              "https://github.com/favstats/meta_ad_reports/releases/expanded_assets/",
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
    
    
    latest <- out  %>%
      rename(tag = release,
             file_name = filename) %>%
      arrange(desc(tag)) %>%
      separate(
        tag,
        into = c("country", "timeframe"),
        remove = F,
        sep = "-"
      ) %>%
      filter(str_detect(file_name, "rds")) %>%
      mutate(day  = str_remove(file_name, "\\.rds|\\.zip|\\.parquet") %>% lubridate::ymd()) %>%
      arrange(desc(day)) %>%
      group_by(country) %>%
      slice(1) %>%
      ungroup()
    
    
    download.file(
      paste0(
        "https://github.com/favstats/meta_ad_reports/releases/download/",
        sets$cntry,
        "-last_90_days/",
        latest$file_name
      ),
      destfile = "report.rds"
    )
    
    last7 <- readRDS("report.rds") %>%
      mutate(sources = "report") %>%
      mutate(party = "unknown")
    
    file.remove("report.rds")
  })
  
  if (!exists("last7")) {
    last7 <- tibble()
  }
  
  
  all_dat <- #read_csv("nl_advertisers.csv") %>%
    # mutate(page_id = as.character(page_id)) %>%
    # bind_rows(internal_page_ids) %>%
    bind_rows(wtm_data) %>%
    bind_rows(tep_dat) %>%
    bind_rows(last7) %>%
    # bind_rows(rep) %>%
    # bind_rows(more_data %>% mutate(sources = "new")) %>%
    # bind_rows(groenams) %>%
    distinct(page_id, .keep_all = T) %>%
    add_count(page_name, sort  = T) %>%
    mutate(remove_em = n >= 2 & str_ends(page_id, "0")) %>%
    filter(!remove_em) %>%
    # filter(n >= 2) %>%
    # filter(n >= 2 & str_ends(page_id, "0", negate = T)) %>%
    select(-n,-contains("no_data"))
  
  
  # all_dat %>% filter(str_detect(page_name, "GroenLinks-PvdA"))
  
  saveRDS(all_dat, "data/all_dat.rds")
  
  # all_dat <- readRDS("data/all_dat.rds")
  
  # source("cntry.R")
  
  # all_dat %>% filter(page_id == "492150400807824")
  
  
  
  scraper <- function(.x, time = tf, cntry = "US") {
    # print(paste0(.x$page_name,": ", round(which(internal_page_ids$page_id == .x$page_id)/nrow(internal_page_ids)*100, 2)))
    # .x <- list()
    # .x$page_id <- "7860876103"
    # time <- "90"
    # get_targeting(.x$page_id, timeframe = glue::glue("LAST_{time}_DAYS"))
    
    if(nrow(.x)==0) return(tibble(targeting_info = NULL, page_info = NULL))
    
    raw_resp <- get_page_insights(.x$page_id, include_info = "page_info") 
    
    fin2 <- raw_resp %>%
      mutate(tstamp = tstamp)
          # }
    
    if (nrow(fin2) != 0) {
      if (!dir.exists(glue::glue("info"))) {
        dir.create(glue::glue("info"), recursive = T)
      }
      
      path <-
        paste0(glue::glue("info/"), .x$page_id, ".rds")
      
      
      saveRDS(fin2, file = path)
      # }
    } else {
      fin2 <- tibble(internal_id = .x$page_id, no_data = T) %>%
        mutate(tstamp = tstamp) 
    }
    
    
    return(list(page_info = fin2))
    
  }
  
  scraper <- possibly(scraper, otherwise = NULL, quiet = F)
  
  
  print("################ RETRIEVE PAGE INFO ################")
  try({
    old_info <- arrow::read_parquet(glue::glue("https://github.com/favstats/meta_ad_targeting/releases/download/PageInfo/{sets$cntry}-page_info.parquet"))
  })
  if(!exists("old_info")){
    old_info <- tibble()
  }

  
  try({
    
    current_date <-
      paste0("historic/",
             as.character(new_ds),
             "/",
             "last_",
             tf,
             "_days")
    
      ### save seperately
      benny <- all_dat %>%
        arrange(page_id) %>%
        # sample_n(5) %>%
        split(1:nrow(.)) %>%
        map_progress(scraper)
  
    
  })
  # saveRDS(election_dat, paste0("data/election_dat", tf, ".rds"))
  
  # f
  
  # election_dat <- arrow::read_parquet("historic/2024-03-05/30.parquet")
  
  # sources("start.R")
  
  the_tag <- paste0(sets$cntry, "-", "last_", tf, "_days")

  # full_repos
  
  
  
  # print(unique(all_dat$cntry))
  # glimpse(all_dat)
  
  # glimpse(old_info)
  infodat <- benny %>% map_dfr(~pluck(.x, "page_info")) %>% as_tibble()
  
  
  try({
    infodat <- infodat %>% 
      bind_rows(old_info) %>% 
      distinct(page_name, about, likes, .keep_all = T) %>% 
      drop_na(page_id)
  })
  
  
  # infodat %>% View()
  # glimpse(infodat)
  
  ### TODO: get previous info
  arrow::write_parquet(infodat, paste0(sets$cntry,"-page_info", ".parquet"))
  # reeeleases <- get_full_release()
  # releeasee <- get_full_release()
  # releasess <- get_full_release()
  # releasesss <- piggyback::pb_releases()
  # saveRDS(releasesss, file = "data/releases.rds")
  releases <- readRDS("data/releases.rds")
  
  cntry_name <- full_cntry_list %>%
    filter(iso2c == sets$cntry) %>%
    pull(country)
  
  if(nrow(infodat)!=0){
    if(!(identical(old_info, infodat))){
      
      print("################ UPLOAD INFO FILE ################")
      
      try({
        
        # debugonce(pb_upload_file_fr)
        # debugonce(delete_asset_by_filename)
        pb_upload_file_fr(
          paste0(sets$cntry,"-page_info", ".parquet"),
          repo = "favstats/meta_ad_targeting",
          tag = "PageInfo",
          releases = releases
        )
        # pb_upload_file_fr(paste0(the_date, ".zip"), repo = "favstats/meta_ad_reports", tag = the_tag, releases = full_repos)
        
      })
      
      print(paste0("################ UPLOADED INFO FILE ################: ", sets$cntry))
      
      
    } else {
      print("Info File is identical, will not be uploaded")
    }      
  }
  
  
  
  
  file.remove(paste0(sets$cntry,"-page_info", ".parquet"))
  rm(old_info)
  
  gc()
  
  
  ### END FOR LOOP
  # }
  # # .[1:7] %>%
  # walk_progress( ~ {
  #
  #
  # })
  
  # unzip("report/TN/2023-11-28.zip", exdir = "extracted", overwrite = T)
  
  # unzip(dir(paste0("report/",cntry_str), full.names = T, recursive = T), exdir = "extracted")
  
  
  
  unlink("targeting", recursive = T, force = T)
  unlink("historic", recursive = T, force = T)
  unlink("info", recursive = T, force = T)
  
  print("################ FIN ################")
  
  # }
  
  # unlink("node_modules", recursive = T, force = T)
  # unlink("out", recursive = T, force = T)
  
  
})


print("################ VERY END ################")

# arrow::read_parquet("https://github.com/favstats/meta_ad_targeting/releases/download/PageInfo/DK-page_info.parquet")
# 
# arrow::read_parquet("https://github.com/favstats/meta_ad_targeting/releases/download/PageInfo/FI-page_info.parquet") %>% View()
