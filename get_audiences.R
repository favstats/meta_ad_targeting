# Get command-line arguments
# tf <- commandArgs(trailingOnly = TRUE)
# rate_limit <<- F
bind_rows_chr <- function(...) {
  dfs <- list(...)
  if (length(dfs) == 1L && is.list(dfs[[1L]]) && !inherits(dfs[[1L]], "data.frame")) {
    dfs <- dfs[[1L]]          # support a single list argument
  }
  
  dfs_chr <- lapply(
    dfs,
    \(df) dplyr::mutate(across(everything(), as.character), .data = df)
  )
  
  dplyr::bind_rows(dfs_chr)
}

try({
  
  if (!(Sys.info()[["effective_user"]] %in% c("fabio", "favstats"))) {
    remove.packages("arrow")
  }
  
  
  outcome <- commandArgs(trailingOnly = TRUE)
  
  sets <- list()
  tf <- outcome[1]
  the_cntry <- outcome[2]
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
  library(openssl)
  library(jsonlite)
  
  
  
  
  Sys.setenv(LIBARROW_MINIMAL = "false")
  Sys.setenv("NOT_CRAN" = "true")
  
  print("##### please install arrow #####")
  
  options(
    HTTPUserAgent =
      sprintf(
        "R/%s R (%s)",
        getRversion(),
        paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"])
      )
  )
  if (!(Sys.info()[["effective_user"]] %in% c("fabio", "favstats"))) {
    install.packages("arrow", repos = "https://packagemanager.rstudio.com/all/__linux__/focal/latest")
    arrow::install_arrow(verbose = F) # verbose output to debug install errors
  }
  # print(arrow::arrow_info())
  # print("##### did you install arrow? #####")
  
  # pacman::p_load(arrow)
  
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
    tf <- "7"
    the_cntry <- "NO"
    print(paste0("TF: ", tf))
    print(paste0("cntry: ", sets))
    
  }
  
  
  # for (cntryy in full_cntry_list$iso2c) {
  #   the_cntry <-  cntryy
  #   print(the_cntry)
  #   
  #   if(!exists("rate_limit")){
  #     rate_limit <<- F
  #   } else {
  #     
  #     if(length(rate_limit)==0)  rate_limit <<- F
  #     
  #   }
  
  #   # Telegram bot setup
  #   TELEGRAM_BOT_ID <- Sys.getenv("TELEGRAM_BOT_ID")
  #   TELEGRAM_GROUP_ID <- Sys.getenv("TELEGRAM_GROUP_ID")
  #   
  #   # Function to send a Telegram message
  #   send_telegram_message <- function(message) {
  #     url <- paste0("https://api.telegram.org/bot", TELEGRAM_BOT_ID, "/sendMessage")
  #     httr::POST(url, body = list(chat_id = TELEGRAM_GROUP_ID, text = message), encode = "form")
  #   }
  #   
  #   # Function to log updates with Telegram integration
  #   log_update <- function(stage, tf, cntry, details = "") {
  #     message <- glue::glue(
  #       "
  # 🔹 *Update: {stage}* 🔹
  # 🌍 Country: {cntry}
  # ⏳ Timeframe: {tf}
  # 🕒 Time: {Sys.time()}
  # {details}
  #   "
  #     )
  #     send_telegram_message(message)
  #   }
  #   
  #   log_update <- possibly(log_update, otherwise = NULL, quiet = F)
  #   
  #   log_update("Script Started", tf,   the_cntry, details = "Initializing data processing...")
  #   
  
  # if(rate_limit){
  #   break
  # }
  
  unlink("targeting", recursive = T, force = T)
  unlink("historic", recursive = T, force = T)
  
  try({
    
    # Step 3: Decrypt the file and read content
    decrypted_content <- decrypt_file("data/ips-targeting.enc")
    ips_targeting <- str_split(decrypted_content, "\n", simplify = F)
    
    ips_targeting <- unlist(ips_targeting)[-1:-2]
  })
  # 
  print("################ CHECK LATEST REPORT ################")
  
  
  try({
    out <- the_cntry %>%
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
              "https://github.com/favstats/meta_ad_reports2/releases/expanded_assets/",
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
        "https://github.com/favstats/meta_ad_reports2/releases/download/",
        the_cntry,
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
  
  
  togetstuff <- last7 %>% select(page_id , contains("amount")) %>% 
    set_names("page_id", "spend") %>% 
    mutate(spend = parse_number(spend)) %>% 
    arrange(desc(spend))
  
  for (i in 1:length(togetstuff$page_id)) {
    # Get insights for the current page ID
    jb <- get_page_insights(
      togetstuff$page_id[i], 
      timeframe = glue::glue("LAST_90_DAYS"), 
      include_info = "targeting_info"
    )
    
    # Check if `jb` is not NULL
    if (!is.null(jb)) {
      # print("is not null")
      if(nrow(jb) == 0){
        # print("but is zero")
        next
      } else {
        # Extract the `new_ds` value
        new_ds <- jb %>% 
          arrange(ds) %>% 
          slice(1) %>% 
          pull(ds)
        
        # Break the loop if `new_ds` is successfully assigned
        if (!is.null(new_ds)) {
          # message("New `ds` found, breaking the loop.")
          break
        }
        
      }
    } 
  }
  
  to_get <- latest %>%
    filter(day == new_ds) %>%
    filter(str_detect(timeframe, tf))
  
  if (nrow(to_get) != 0) {
    download.file(
      paste0(
        "https://github.com/favstats/meta_ad_reports2/releases/download/",
        the_cntry,
        "-",
        to_get$timeframe,
        "/",
        to_get$file_name
      ),
      destfile = "report.rds"
    )
    
    last7 <- readRDS("report.rds") %>%
      mutate(sources = "report") %>%
      mutate(party = "unknown")
    
    file.remove("report.rds")
    
    togetstuff <-
      last7 %>% select(page_id , contains("amount")) %>%
      set_names("page_id", "spend") %>%
      mutate(spend = parse_number(spend)) %>%
      arrange(desc(spend))
    
    report_matched = T
  } else {
    report_matched = F
    
  }
  
  print("################ LATEST TARGETING DATA ################")
  
  try({
    # latest_elex <- readRDS(paste0("data/election_dat", tf, ".rds"))
    
    out <- the_cntry %>%
      map( ~ {
        .x %>%
          paste0(c("-last_7_days", "-last_30_days",
                   "-last_90_days"))
      }) %>%
      unlist() %>%
      keep( ~ str_detect(.x, tf)) %>%
      # .[100:120] %>%
      map_dfr( ~ {
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
      mutate(ds  = str_remove(file_name, "\\.rds|\\.zip|\\.parquet")) %>%
      distinct(cntry, ds, tframe) %>%
      drop_na(ds) %>%
      arrange(desc(ds))
    
    try({
      latest_elex <-
        arrow::read_parquet(
          paste0(
            "https://github.com/favstats/meta_ad_targeting/releases/download/",
            the_cntry,
            "-last_",
            tf,
            "_days/",
            thosearethere$ds[1],
            ".parquet"
          )
        )
    })
    
    if (!exists("latest_elex")) {
      latest_elex <- tibble()
    }
    
    if (!("ds" %in% names(latest_elex))) {
      latest_elex <- latest_elex %>% mutate(ds = "")
    }
    
    latest_ds <- thosearethere$ds[1]
    
  })
  
  
  if (!exists("latest_ds")) {
    latest_ds <- "2023-01-01"
  } else if (is.na(latest_ds)) {
    latest_ds <- "2023-01-01"
  }
  
  
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
  #       str_to_lower(the_cntry)
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
  
  
  if (the_cntry %in% country_codes & nrow(thedat) != 0) {
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
    filter(cntry %in% the_cntry) %>%
    mutate(sources = "tep") %>%
    rename(party = name_short)
  
  
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
    select(-n,-contains("no_data"))  %>% 
    mutate(total_n = n()) %>% 
    filter(page_id != 0) 
  
  
  the_amount <- all_dat %>% names() %>% keep(~str_detect(.x, "amount_spent")) %>% .[1]
  
  
  
  all_dat <- all_dat %>% 
    mutate(amount_spent = parse_number(as.character(all_dat[[the_amount]]))) %>% 
    arrange(desc(amount_spent))
  
  djt_page <- all_dat %>% 
    filter(page_id == "153080620724")
  
  all_dat <- djt_page %>% 
    bind_rows(all_dat) %>%
    distinct(page_id, .keep_all = T)
  
  # all_dat %>% filter(str_detect(page_name, "GroenLinks-PvdA"))
  
  saveRDS(all_dat, "data/all_dat.rds")
  
  scrape_dat <- all_dat
  # source("cntry.R")
  
  # all_dat %>% filter(page_id == "492150400807824")
  
  
  fin <<- tibble(no_data = T)
  
  scraper <- function(internal, time = tf) {
    
    if((which(scrape_dat$page_id == internal$page_id) %% round(nrow(scrape_dat)/4, -1)) == 0){
      
      print(paste0(internal$page_name,": ", round(which(scrape_dat$page_id == internal$page_id)/nrow(scrape_dat)*100, 2)))
      
    }
    
    
    # if(is.null(fin$error)){
    
    fin <<-
      # get_targeting(internal$page_id, timeframe = glue::glue("LAST_{time}_DAYS")) %>%
      get_page_insights(internal$page_id, timeframe = glue::glue("LAST_{time}_DAYS"), include_info = "targeting_info", iso2c = the_cntry) %>% 
      mutate(tstamp = tstamp)
    
    if (nrow(fin) != 0) {
      if (!dir.exists(glue::glue("targeting/{time}"))) {
        dir.create(glue::glue("targeting/{time}"), recursive = T)
      }
      
      path <-
        paste0(glue::glue("targeting/{time}/"), internal$page_id, ".rds")
      # if(file.exists(path)){
      #   ol <- read_rds(path)
      #
      #   saveRDS(fin %>% bind_rows(ol), file = path)
      # } else {
      
      saveRDS(fin, file = path)
      # }
    } else {
      fin <- tibble(internal_id = internal$page_id, no_data = T) %>%
        mutate(tstamp = tstamp)
    }
    
    # print(nrow(fin))
    # })
    return(fin)
    
    # }
    
  }
  
  scraper <- possibly(scraper, otherwise = NULL, quiet = F)
  
  
  print("################ RETRIEVE AUDIENCES ################")
  # if(F){
  #     # dir("provincies/7", full.names
  # }
  # da30 <- readRDS("data/election_dat30.rds")
  # da7 <- readRDS("data/election_dat7.rds")
  try({
    
    current_date <-
      paste0("historic/",
             as.character(new_ds),
             "/",
             "last_",
             tf,
             "_days")
    
    if (new_ds == latest_ds) {
      print(glue::glue("New DS: {new_ds}: Old DS: {latest_ds}"))
      
      scrape_dat <- all_dat %>%
        # arrange(page_id) %>%
        # slice(1:150) %>%
        filter(!(page_id %in% latest_elex$page_id))  %>%
        filter(page_id %in% last7$page_id) %>% 
        mutate(total_n = n())
      
      the_rows_to_be_checked <- nrow(scrape_dat)
      
      print(paste0("Number of remaining pages to check: ", nrow(scrape_dat)))
      
      ### save seperately
      enddat <-  scrape_dat %>%
        split(1:nrow(.)) %>%
        map_dfr(scraper)
      
      if (nrow(enddat) == 0) {
        
        print("same length! will just save the same parquet!")
        
        election_dat <- latest_elex
        
        dir.create(paste0("historic/",  as.character(new_ds)), recursive = T)
        
        
        arrow::write_parquet(election_dat, paste0(current_date, ".parquet"))
        
      } else {
        
        print("new data to be uploaded")
        
        if(is.null(enddat$page_id)){
          enddat$page_id <- enddat$internal_id
        }
        
        new_elex <- enddat
        
        election_dat  <- enddat %>%
          mutate_at(vars(contains("total_spend_formatted")), ~ parse_number(as.character(.x))) %>%
          # rename(page_id = internal_id) %>%
          left_join(all_dat) %>%
          bind_rows_chr(latest_elex %>% filter(!(page_id %in% enddat$page_id))) %>%
          distinct()
        
        dir.create(paste0("historic/",  as.character(new_ds)), recursive = T)
        
        
        arrow::write_parquet(election_dat, paste0(current_date, ".parquet"))
        
        # arrow::read_parquet(paste0(current_date, ".parquet")) %>% View()
        
        # saveRDS(election_dat, file = paste0(current_date, ".rds"))
      }
      
      
    } else {
      
      print(glue::glue("Complete new Data. New DS: {new_ds}: Old DS: {latest_ds} 2"))
      
      print(paste0("Number of pages to check: ", nrow(scrape_dat)))
      
      # debugonce(scraper)
      ### save seperately
      election_dat <- all_dat %>%
        # arrange(page_id) %>%
        # slice(1:2) %>%
        split(1:nrow(.)) %>%
        map_dfr(scraper)  %>%
        mutate_at(vars(contains("total_spend_formatted")), ~ parse_number(as.character(.x))) 
      
      if(is.null(election_dat$page_id)){
        election_dat$page_id <- election_dat$internal_id
      }
      
      election_dat <- election_dat %>% 
        left_join(all_dat)
      
      dir.create(paste0("historic/",  as.character(new_ds)), recursive = T)
      
      
      arrow::write_parquet(election_dat, paste0(current_date, ".parquet"))
      
      
    }
  })
  # saveRDS(election_dat, paste0("data/election_dat", tf, ".rds"))
  
  # f
  
  # election_dat <- arrow::read_parquet("historic/2024-03-05/30.parquet")
  
  # sources("start.R")
  
  the_tag <- paste0(the_cntry, "-", "last_", tf, "_days")
  the_date <- new_ds
  
  # full_repos
  
  # cntry_name
  
  # reeeleases <- get_full_release()
  # releeasee <- get_full_release()
  
  # saveRDS(releeasee %>% drop_na(), file = "data/releeasee.rds")
  releases <- readRDS("data/releases.rds")
  
  cntry_name <- full_cntry_list %>%
    filter(iso2c == the_cntry) %>%
    pull(country)
  
  if(!(the_tag %in% releases$tag_name)){
    try({
      pb_release_create_fr(
        repo = "favstats/meta_ad_targeting",
        tag = the_tag,
        body = paste0(
          "This release includes ",
          cntry_name , " '", "last_", tf, "_days" , "' Meta ad target audiences."
        ),
        releases = releases
      )    # Sys.sleep(5)
    })
  }
  
  
  file.copy(paste0(current_date, ".parquet"),
            paste0(the_date, ".parquet"),
            overwrite = T)
  
  print(file.exists(paste0(the_date, ".parquet")))
  
  
  if("no_data" %in% names(election_dat)){
    election_dat <- election_dat %>% filter(is.na(no_data))
  }
  
  
  if(!(identical(latest_elex, election_dat))){
    
    print("################ UPLOAD FILE ################")
    
    try({
      # print(paste0(the_date, ".rds"))
      # print(the_tag)
      # debugonce(pb_upload_file_fr)
      rsd <- pb_upload_file_fr(
        paste0(the_date, ".parquet"),
        repo = "favstats/meta_ad_targeting",
        tag = the_tag,
        releases = releases
      )
      
      try({
        the_status_code <- httr::status_code(rsd)
      })
      # pb_upload_file_fr(paste0(the_date, ".zip"), repo = "favstats/meta_ad_reports", tag = the_tag, releases = full_repos)
      
    })
    
    print(paste0("################ UPLOADED FILE ################: ", the_cntry))
    
    
  } else {
    print("File is identical, will not be uploaded")
  }
  
  file.remove(paste0(the_date, ".parquet"))
  
  
  gc()
  
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
  
  print("################ FIN ################")
  
  # }
  
  # unlink("node_modules", recursive = T, force = T)
  # unlink("out", recursive = T, force = T)
  
  
})


if(!exists("new_elex")){
  new_elex <- tibble()
} else {
  if("no_data" %in% names(new_elex)){
    new_elex <- new_elex %>% filter(is.na(no_data))
  }
}

if(!exists("the_rows_to_be_checked")){
  the_rows_to_be_checked <- tibble()
} 

if(!exists("the_status_code")){
  the_status_code <- "no status code"
} 




# Telegram bot setup
TELEGRAM_BOT_ID <- Sys.getenv("TELEGRAM_BOT_ID")
TELEGRAM_GROUP_ID <- Sys.getenv("TELEGRAM_GROUP_ID")

# Function to log final statistics with Telegram integration
log_final_statistics <- function(stage, tf, cntry, new_ds, latest_ds,
                                 the_rows_to_be_checked, election_dat, new_elex,
                                 pushed_successfully, togetstuff, report_matched) {
  # Check if ds was already present
  ds_present <- ifelse(new_ds == latest_ds, "Yes", "No")
  
  # Calculate statistics
  total_rows <- length(unique(election_dat$page_id))
  new_rows <- length(unique(new_elex$page_id))
  lag_days <- as.numeric(Sys.Date() - lubridate::ymd(new_ds))
  
  # Spending coverage statistics
  page_ids_in_togetstuff <- sum(togetstuff$page_id %in% election_dat$page_id)
  total_spend_in_togetstuff <- sum(togetstuff$spend, na.rm = TRUE)
  election_dat <- distinct(election_dat, page_id, .keep_all = T)
  covered_spend <- sum(election_dat$amount_spent[election_dat$page_id %in% togetstuff$page_id], na.rm = TRUE)
  
  spend_coverage_pct <- round((covered_spend / total_spend_in_togetstuff) * 100)
  coverage_status <- ifelse(spend_coverage_pct == 100, "✅", "❌")
  
  # Check GitHub push status
  push_status <- ifelse(pushed_successfully, "✅ Yes", "❌ No")
  report_status <- ifelse(report_matched, "✅ Yes", "❌ No")
  
  # Construct details message
  details <- glue::glue(
    "   \t\t📌 *Newest DS:* {new_ds}\n",
    "   \t\t📌 *Latest DS:* {latest_ds}\n",
    "   \t\t📌 *DS Already Present:* {ds_present}\n",
    "   \t\t🔋 *Page IDs Checked:* {the_rows_to_be_checked}\n",
    "   \t\t📊 *Total Page IDs:* {total_rows}\n",
    "   \t\t➕ *New Page IDs Added:* {new_rows}\n",
    "   \t\t🕒 *Days Lagging:* {lag_days} days\n",
    "   \t\t🚀 *GitHub Push Successful:* {push_status}\n",
    "   \t\t😎 *Report Matched:* {report_status}\n",
    "   \t\t🔍 *Page IDs Present (of Report):* {page_ids_in_togetstuff}/{nrow(togetstuff)}\n",
    "   \t\t💰 *Spending Coverage:* {covered_spend}/{total_spend_in_togetstuff} ({spend_coverage_pct}% {coverage_status})",
    "   \t\t💰 *Source:* meta_ad_targeting"
    
  )
  
  # Construct the full message
  the_message <- glue::glue(
    "🔹 *{stage}* 🔹\n",
    "🌍 *Country:* {cntry} {tf}\n",
    "⏳ *Timeframe:* {tf}\n",
    "🕒 *Time:* {Sys.time()}\n",
    "{details}"
  )
  
  print(the_message)
  
  # Send the message to Telegram
  url <- paste0("https://api.telegram.org/bot", Sys.getenv("TELEGRAM_BOT_ID"), "/sendMessage")
  out <<- httr::POST(url, body = list(chat_id = Sys.getenv("TELEGRAM_GROUP_ID"), text = the_message, parse_mode = "Markdown"), encode = "form")
  if (httr::http_error(out)) {
    print(httr::content(out))
    print(httr::headers(out))
  }
}

try({
  # Example integration (call this after processing):
  log_final_statistics(
    stage = "Process Complete",
    tf = tf,
    cntry = the_cntry,
    new_ds = new_ds,
    latest_ds = latest_ds,
    the_rows_to_be_checked = the_rows_to_be_checked,
    election_dat = election_dat,
    new_elex = new_elex,
    pushed_successfully = the_status_code,
    togetstuff = togetstuff,
    report_matched = report_matched
  )
})



print("################ VERY END ################")