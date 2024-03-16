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
  
  if (Sys.info()[["sysname"]] == "Windows") {
    ### CHANGE ME WHEN LOCAL!
    tf <- "30"
    sets$cntry <- "LU"
    print(paste0("TF: ", tf))
    print(paste0("cntry: ", sets))
    
  }
  
  # for (cntryy in full_cntry_list$iso2c) {
  #   sets$cntry <-  cntryy
  #   print(sets$cntry)
  # #   
  #   if(!exists("rate_limit")){
  #     rate_limit <<- F
  #   } else {
  #     
  #     if(length(rate_limit)==0)  rate_limit <<- F
  #     
  #   }
    
    
    
    # if(rate_limit){
    #   break
    # }
    
    unlink("targeting", recursive = T, force = T)
    unlink("historic", recursive = T, force = T)
    unlink("info", recursive = T, force = T)
    
    # if()
    
    jb <-
      get_targeting("7860876103", timeframe = glue::glue("LAST_90_DAYS"))
    
    new_ds <- jb %>% arrange(ds) %>% slice(1) %>% pull(ds)
    # new_ds <- "2023-01-01"
    
    print("################ LATEST TARGETING DATA ################")
    
    try({
      # latest_elex <- readRDS(paste0("data/election_dat", tf, ".rds"))
      
      out <- sets$cntry %>%
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
              sets$cntry,
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
    
    
    try({
      download.file(
        paste0(
          "https://data-api.whotargets.me/advertisers-export-csv?countries.alpha2=",
          str_to_lower(sets$cntry)
        ),
        destfile = "data/wtm_advertisers.csv"
      )
      
      thedat <- read_csv("data/wtm_advertisers.csv")
      
    })
    
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
      
      raw_resp <- get_page_insights(.x$page_id, timeframe = glue::glue("LAST_{time}_DAYS"), iso2c = sets$cntry, join_info = F) 
      
      fin <- raw_resp %>%
        .[[2]] %>% 
        mutate(tstamp = tstamp) %>% 
        rename(internal_id = page_id)
      
      fin2 <- raw_resp %>%
        .[[1]] %>% 
        mutate(tstamp = tstamp)
      
      if (nrow(fin) != 0) {
        if (!dir.exists(glue::glue("targeting/{time}"))) {
          dir.create(glue::glue("targeting/{time}"), recursive = T)
        }
        
        path <-
          paste0(glue::glue("targeting/{time}/"), .x$page_id, ".rds")
        # if(file.exists(path)){
        #   ol <- read_rds(path)
        #
        #   saveRDS(fin %>% bind_rows(ol), file = path)
        # } else {
        
        saveRDS(fin, file = path)
        # }
      } else {
        fin <- tibble(internal_id = .x$page_id, no_data = T) %>%
          mutate(tstamp = tstamp)
      }
      
      if (nrow(fin2) != 0) {
        if (!dir.exists(glue::glue("info"))) {
          dir.create(glue::glue("info"), recursive = T)
        }
        
        path <-
          paste0(glue::glue("info/"), .x$page_id, ".rds")
        # if(file.exists(path)){
        #   ol <- read_rds(path)
        #
        #   saveRDS(fin %>% bind_rows(ol), file = path)
        # } else {
        
        saveRDS(fin2, file = path)
        # }
      } else {
        fin2 <- tibble(internal_id = .x$page_id, no_data = T) %>%
          mutate(tstamp = tstamp) 
      }
      
      
      c("page_info", "targeting_info")
      # print(nrow(fin))
      # })
      return(list(page_info = fin2, targeting_info = fin))
      
    }
    
    scraper <- possibly(scraper, otherwise = NULL, quiet = F)
    
    
    print("################ RETRIEVE AUDIENCES ################")
    try({
      old_info <- arrow::read_parquet(glue::glue("https://github.com/favstats/meta_ad_targeting/releases/download/PageInfo/{sets$cntry}-page_info.parquet"))
    })
    if(!exists("old_info")){
      old_info <- tibble()
    }
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
      
      # all_dat <- all_dat %>% slice(1)
      
      if (new_ds == latest_ds) {
        ### if we are still in the same period
        print(glue::glue("New DS: {new_ds}: Old DS: {latest_ds}"))
        # debugonce(get_page_insights)
        
        ### save seperately
        benny <- all_dat %>%
          arrange(page_id) %>%
          # slice(1:10) %>%
          filter(!(page_id %in% latest_elex$page_id))  %>%
          filter(page_id %in% last7$page_id) %>%
          split(1:nrow(.)) %>%
          map(scraper)
        
        enddat <- benny %>% map_dfr(~pluck(.x, "targeting_info")) %>% as_tibble()
        
        
        if (nrow(enddat) == 0) {
          ## no new data
          election_dat <- latest_elex
          
          dir.create(paste0("historic/",  as.character(new_ds)), recursive = T)
          
          
          arrow::write_parquet(election_dat, paste0(current_date, ".parquet"))
          
          
        } else {
          ## new data so we need to bind it
          print(glue::glue("New DS: {new_ds}: Old DS: {latest_ds} 2"))
          
          
          election_dat  <- enddat %>%
            mutate_at(vars(contains("total_spend_formatted")), ~ parse_number(as.character(.x))) %>%
            rename(page_id = internal_id) %>%
            left_join(all_dat) %>%
            bind_rows(latest_elex) %>%
            distinct()
          
          dir.create(paste0("historic/",  as.character(new_ds)), recursive = T)

          
          arrow::write_parquet(election_dat, paste0(current_date, ".parquet"))

          # arrow::read_parquet(paste0(current_date, ".parquet")) %>% View()
          
          # saveRDS(election_dat, file = paste0(current_date, ".rds"))
        }
        
        
      } else {
        
        ### save seperately
        benny <- all_dat %>%
          arrange(page_id) %>%
          split(1:nrow(.)) %>%
          map(scraper)
        
        enddat <- benny %>% map_dfr(~pluck(.x, "targeting_info")) %>% as_tibble()
        
        # infodat <- benny %>% map_dfr(~pluck(.x, "page_info")) %>% as_tibble()
        
        
        election_dat <- enddat %>%
          mutate_at(vars(contains("total_spend_formatted")), ~ parse_number(as.character(.x))) %>%
          rename(page_id = internal_id)  %>%
          left_join(all_dat)
        
        dir.create(paste0("historic/",  as.character(new_ds)), recursive = T)

        
        arrow::write_parquet(election_dat, paste0(current_date, ".parquet"))
        
        # arrow::write_parquet(infodat, paste0(sets$cntry,"-page_info", ".parquet"))
      }
    })
    # saveRDS(election_dat, paste0("data/election_dat", tf, ".rds"))
    
    # f
    
    # election_dat <- arrow::read_parquet("historic/2024-03-05/30.parquet")
    
    # sources("start.R")
    
    the_tag <- paste0(sets$cntry, "-", "last_", tf, "_days")
    the_date <- new_ds
    
    # full_repos
    
    # cntry_name
    ### save seperately
    # benny <- all_dat %>%
    #   arrange(page_id) %>%
    #   sample_n(30) %>%
    #   split(1:nrow(.)) %>%
    #   map_progress(scraper)
    
    # print(unique(all_dat$cntry))
    # glimpse(all_dat)
    
    # glimpse(old_info)
    infodat <- benny %>% map_dfr(~pluck(.x, "page_info")) %>% as_tibble()
    
    
    try({
      infodat <- infodat %>% 
        bind_rows(old_info) %>% 
        distinct(page_name, about, likes, .keep_all = T)      
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
    
    if(!(identical(latest_elex, election_dat))){
      
      print("################ UPLOAD TARGETING FILE ################")
      
      try({
        # print(paste0(the_date, ".rds"))
        # print(the_tag)
        # debugonce(pb_upload_file_fr)
        # debugonce(pb_upload_file_fr)
        pb_upload_file_fr(
          paste0(the_date, ".parquet"),
          repo = "favstats/meta_ad_targeting",
          tag = the_tag,
          releases = releases
        )
        
        # pb_upload_file_fr(paste0(the_date, ".zip"), repo = "favstats/meta_ad_reports", tag = the_tag, releases = full_repos)
        
      })
      
      print(paste0("################ UPLOADED TARGETING FILE ################: ", sets$cntry))
      
      
    } else {
      print("Targeting File is identical, will not be uploaded")
    }
    
    if(nrow(infodat)!=0){
      if(!(identical(old_info, infodat))){
        
        print("################ UPLOAD INFO FILE ################")
        
        try({
          
          # debugonce(pb_upload_file_fr)
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

    
    

    file.remove(paste0(the_date, ".parquet"))
    file.remove(paste0(sets$cntry,"-page_info", ".parquet"))
    rm(old_info)
    
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
