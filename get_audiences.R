# Get command-line arguments
# tf <- commandArgs(trailingOnly = TRUE)

unlink("targeting", recursive = T, force = T)
unlink("historic", recursive = T, force = T)

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
library(tidyverse)
library(lubridate)
library(rvest)
library(piggyback)

# sets <- jsonlite::fromJSON("settings.json")
# 
# title_txt <- read_lines("_site/_quarto.yml")
# title_txt[which(str_detect(title_txt, "title"))[1]] <-  glue::glue('  title: "{sets$dashboard}"')
# write_lines(title_txt, "_site/_quarto.yml")

full_cntry_list <- read_rds("https://github.com/favstats/meta_ad_reports/raw/main/cntry_list.rds") %>% 
  rename(iso2c = iso2,
         country = cntry) %>% 
  sample_n(n())

# full_cntry_list$iso2c %>% dput()

if(Sys.info()[["sysname"]]=="Windows"){
  ### CHANGE ME WHEN LOCAL!
  tf <- "30"
  sets$cntry <- "LU"
  print(paste0("TF: ", tf))
  print(paste0("cntry: ", sets))
  
}

# for (cntryy in full_cntry_list$iso2c) {
#  sets$cntry <-  cntryy
#  print(sets$cntry)

# if()

jb <- get_targeting("7860876103", timeframe = glue::glue("LAST_90_DAYS"))

new_ds <- jb %>% arrange(ds) %>% slice(1) %>% pull(ds)
# new_ds <- "2023-01-01"

try({
  # latest_elex <- readRDS(paste0("data/election_dat", tf, ".rds"))
  
  out <- sets$cntry %>% 
    map(~{
      .x %>% 
        paste0(c("-last_7_days", "-last_30_days", 
                 "-last_90_days"))
    }) %>% 
    unlist() %>% 
    keep(~str_detect(.x, tf)) %>% 
    # .[100:120] %>% 
    map_dfr_progress(~{
      the_assets <- httr::GET(paste0("https://github.com/favstats/meta_ad_targeting/releases/expanded_assets/", .x))
      
      the_assets %>% httr::content() %>% 
        html_elements(".Box-row") %>% 
        html_text()  %>%
        tibble(raw = .)   %>%
        # Split the raw column into separate lines
        mutate(raw = strsplit(as.character(raw), "\n")) %>%
        # Extract the relevant lines for filename, file size, and timestamp
        transmute(
          filename = sapply(raw, function(x) trimws(x[3])),
          file_size = sapply(raw, function(x) trimws(x[6])),
          timestamp = sapply(raw, function(x) trimws(x[7]))
        ) %>% 
        filter(filename != "Source code") %>% 
        mutate(release = .x) %>% 
        mutate_all(as.character)
    })
  
  thosearethere <- out %>% 
    rename(tag = release,
           file_name = filename) %>% 
    arrange(desc(tag)) %>% 
    separate(tag, into = c("cntry", "tframe"), remove = F, sep = "-") %>% 
    mutate(ds  = str_remove(file_name, "\\.rds|\\.zip|\\.parquet")) %>% 
    distinct(cntry, ds, tframe) %>% 
    drop_na(ds) %>% 
    arrange(desc(ds))
  
  try({
    latest_elex <- arrow::read_parquet(paste0("https://github.com/favstats/meta_ad_targeting/releases/download/", sets$cntry, "-last_", tf,"_days/", thosearethere$ds[1], ".parquet"))
  })
  
  if(!exists("latest_elex")){
    latest_elex <- tibble()
  }
  
  if(!("ds" %in% names(latest_elex))){
    latest_elex <- latest_elex %>% mutate(ds = "")
  }
  
  latest_ds <- thosearethere$ds[1]
  
})


if(!exists("latest_ds")){
  latest_ds <- "2023-01-01"
} else if(is.na(latest_ds)){
  latest_ds <- "2023-01-01"
}


tstamp <- Sys.time()

write_lines(lubridate::as_date(tstamp), "tstamp.txt")

# - name: Set timeframe 
# run: |
#   echo "::set-env name=TIMEFRAME::30 Timeframe"




# tstamp <- Sys.time()

country_codes <- c("AD", "AL", "AM", "AR", "AT", 
                   "AU", "BA", "BE", "BG", "BR", 
                   "CA", "CH", "CL", "CO", "CY", 
                   "CZ", "DE", "DK", "EC", "EE", 
                   "ES", "FI", "FR", "GB", "GR", 
                   "GT", "HR", "HU", "IE", "IN", 
                   "IS", "IT", "LI", "LT", "LU", 
                   "LV", "MD", "ME", "MK", "MT",
                   "MX", "NL", "NO", "NZ", "PL", 
                   "PT", "RO", "RS", "SE", "SI", 
                   "SK", "SM", "TR", "UA", "US", 
                   "VE", "ZA")

try({
  download.file(paste0("https://data-api.whotargets.me/advertisers-export-csv?countries.alpha2=", str_to_lower(sets$cntry)), destfile = "data/wtm_advertisers.csv")

  thedat <- read_csv("data/wtm_advertisers.csv")

})

if(!exists("thedat")){
  thedat <- tibble(no_data = NULL)
}


if(sets$cntry %in% country_codes & nrow(thedat)!=0){

  wtm_data <- read_csv("data/wtm_advertisers.csv") %>% #names
    select(page_id = advertisers_platforms.advertiser_platform_ref,
           page_name = name, party = entities.short_name)  %>%
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

try({

  out <- sets$cntry %>% 
    map(~{
      .x %>% 
        paste0(c("-yesterday", "-last_7_days", "-last_30_days", 
                 "-last_90_days"))
    }) %>% 
    unlist() %>% 
    .[str_detect(., "last_90_days")] %>%
    # .[100:120] %>% 
    map_dfr_progress(~{
      the_assets <- httr::GET(paste0("https://github.com/favstats/meta_ad_reports/releases/expanded_assets/", .x))

      the_assets %>% httr::content() %>% 
        html_elements(".Box-row") %>% 
        html_text()  %>%
        tibble(raw = .)   %>%
        # Split the raw column into separate lines
        mutate(raw = strsplit(as.character(raw), "\n")) %>%
        # Extract the relevant lines for filename, file size, and timestamp
        transmute(
          filename = sapply(raw, function(x) trimws(x[3])),
          file_size = sapply(raw, function(x) trimws(x[6])),
          timestamp = sapply(raw, function(x) trimws(x[7]))
        ) %>% 
        filter(filename != "Source code") %>% 
        mutate(release = .x) %>% 
        mutate_all(as.character)
    })


  latest <- out  %>% 
    rename(tag = release,
           file_name = filename) %>% 
    arrange(desc(tag)) %>% 
    separate(tag, into = c("country", "timeframe"), remove = F, sep = "-") %>% 
    filter(str_detect(file_name, "rds")) %>% 
    mutate(day  = str_remove(file_name, "\\.rds|\\.zip") %>% lubridate::ymd()) %>% 
    arrange(desc(day)) %>% 
    group_by(country) %>% 
    slice(1) %>% 
    ungroup() 


  download.file(paste0("https://github.com/favstats/meta_ad_reports/releases/download/", sets$cntry,"-last_90_days/", latest$file_name), 
                destfile = "report.rds"
  )

  last7 <- readRDS("report.rds")%>% 
    mutate(sources = "report") %>% 
    mutate(party = "unknown")

  file.remove("report.rds")
})

if(!exists("last7")){
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
  add_count(page_name, sort  =T) %>%
  mutate(remove_em = n >= 2 & str_ends(page_id, "0")) %>%
  filter(!remove_em) %>%
  # filter(n >= 2) %>%
  # filter(n >= 2 & str_ends(page_id, "0", negate = T)) %>%
  select(-n, -contains("no_data")) 


# all_dat %>% filter(str_detect(page_name, "GroenLinks-PvdA"))

saveRDS(all_dat, "data/all_dat.rds")

# source("cntry.R")

# all_dat %>% filter(page_id == "492150400807824")



scraper <- function(.x, time = tf) {

  # print(paste0(.x$page_name,": ", round(which(internal_page_ids$page_id == .x$page_id)/nrow(internal_page_ids)*100, 2)))

  fin <- get_targeting(.x$page_id, timeframe = glue::glue("LAST_{time}_DAYS")) %>%
    mutate(tstamp = tstamp)
  
  if(nrow(fin)!=0){
    if(!dir.exists(glue::glue("targeting/{time}"))){
      dir.create(glue::glue("targeting/{time}"), recursive = T)
    }
    
    path <- paste0(glue::glue("targeting/{time}/"),.x$page_id, ".rds")
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

  # print(nrow(fin))
  # })
  return(fin)

}

scraper <- possibly(scraper, otherwise = NULL, quiet = F)


# if(F){
#     # dir("provincies/7", full.names
# }
# da30 <- readRDS("data/election_dat30.rds")
# da7 <- readRDS("data/election_dat7.rds")

if(new_ds == latest_ds){
  print(glue::glue("New DS: {new_ds}: Old DS: {latest_ds}"))

  ### save seperately
  enddat <- all_dat %>% 
    arrange(page_id) %>%
    # slice(1:150) %>% 
    filter(!(page_id %in% latest_elex$page_id))  %>% 
    filter(page_id %in% last7$page_id) %>% 
    split(1:nrow(.)) %>%
    map_dfr_progress(scraper) 

  if(nrow(enddat)==0){
    election_dat <- latest_elex
  } else {

    print(glue::glue("New DS: {new_ds}: Old DS: {latest_ds} 2"))


    election_dat  <- enddat %>%
      mutate_at(vars(contains("total_spend_formatted")), ~parse_number(as.character(.x))) %>% 
      rename(page_id = internal_id) %>%
      left_join(all_dat) %>% 
      bind_rows(latest_elex) %>% 
      distinct()

    dir.create(paste0("historic/",  as.character(new_ds)), recursive = T)
    current_date <- paste0("historic/",  as.character(new_ds), "/", "last_",tf,"_days")
    
    arrow::write_parquet(election_dat, paste0(current_date, ".parquet"))
    
    # arrow::read_parquet(paste0(current_date, ".parquet")) %>% View()

    # saveRDS(election_dat, file = paste0(current_date, ".rds"))
  }


} else {

  ### save seperately
  election_dat <- all_dat %>% 
    arrange(page_id) %>%
    # slice(1:50) %>%
    split(1:nrow(.)) %>%
    map_dfr_progress(scraper)  %>%
    mutate_at(vars(contains("total_spend_formatted")), ~parse_number(as.character(.x))) %>% 
    rename(page_id = internal_id)  %>%
    left_join(all_dat) 

  dir.create(paste0("historic/",  as.character(new_ds)), recursive = T)
  current_date <- paste0("historic/",  as.character(new_ds), "/", "last_",tf,"_days")
  
  arrow::write_parquet(election_dat, paste0(current_date, ".parquet"))
  

}

# saveRDS(election_dat, paste0("data/election_dat", tf, ".rds"))

# f

# election_dat <- arrow::read_parquet("historic/2024-03-05/30.parquet")

# sources("start.R")

the_tag <- paste0(sets$cntry, "-", "last_",tf,"_days")
the_date <- new_ds

# full_repos

# cntry_name

# reeeleases <- get_full_release()
# releeasee <- get_full_release()

# saveRDS(releeasee %>% drop_na(), file = "data/releeasee.rds")
releeasee <- readRDS("data/releeasee.rds")

cntry_name <- full_cntry_list %>% 
  filter(iso2c == sets$cntry) %>% 
  pull(country)

try({
  ## TODO: needs to change once you got all countries
  
  # if(!(the_tag %in% release_names)){
  pb_release_create_fr(repo = "favstats/meta_ad_targeting", 
                       tag = the_tag,
                       body = paste0("This release includes ", cntry_name ," '", "last_",tf,"_days" ,"' Meta ad target audiences."), 
                       releases = releeasee)    # Sys.sleep(5)
  # }
  
})

file.copy(paste0(current_date, ".parquet"), paste0(the_date, ".parquet"), overwrite = T)



try({
  ## TODO: needs to change once you got all countries
  releeasee <- get_full_release()
  
  # print(paste0(the_date, ".rds"))
  # print(the_tag)
  # debugonce(pb_upload_file_fr)
  # debugonce(pb_upload_file_fr)
  pb_upload_file_fr(paste0(the_date, ".parquet"), repo = "favstats/meta_ad_targeting", tag = the_tag, releases = releeasee)
  # pb_upload_file_fr(paste0(the_date, ".zip"), repo = "favstats/meta_ad_reports", tag = the_tag, releases = full_repos)
  
  
})

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

print("NL UNZIPPED")



unlink("targeting", recursive = T, force = T)
unlink("historic", recursive = T, force = T)

print("################6")

# }

# unlink("node_modules", recursive = T, force = T)
# unlink("out", recursive = T, force = T)
