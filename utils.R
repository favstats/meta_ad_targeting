calc_targeting <- function(only_tags, exclude = NULL) {
  
  if(sets$cntry=="TW"){
    age_limit <- 46
  } else {
    age_limit <- 48
  }
  
  # only_tags <- election_dat30 %>%
  # mutate(total_spend = total_spend_formatted) %>%
  #   filter(main_currency == the_currency)
  # only_tags <- election_dat30  %>%
  #   # left_join(all_dat) %>%
  #   # rename(internal_id = page_id) %>%
  #   filter(party != "And")  %>%
  #   filter(is.na(no_data)) %>%
  #   mutate(party = ifelse(party %in% c("GroenLinks", "PvdA"), "GroenLinks-PvdA", party)) %>%
  #   mutate(total_spend = total_spend_formatted) %>%
  #   filter(is.na(no_data)) %>%
  #   filter(page_name == "Partij voor de Dieren Gemeente Groningen")
  # filter(party == "Volt Nederland")
  
  total_sppppeen <- only_tags %>%
    distinct(internal_id, .keep_all = T)  %>%
    # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
    mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
    select(internal_id, total_spend, total_num_ads) %>%
    arrange(desc(total_spend)) %>%
    summarize(total_spend = sum(total_spend),
              num_ads = sum(total_num_ads))
  
  if(!is.null(exclude)){
    if(exclude){
      only_tags <- only_tags %>% filter(is_exclusion)
    } else if(!exclude){
      only_tags <- only_tags %>% filter(!is_exclusion)      
    }
  }
  
  
  howmuchisinterest <- only_tags %>%
    filter(type == "detailed") %>%
    group_by(internal_id) %>%
    filter(total_spend_pct == max(total_spend_pct)) %>%
    slice(1) %>%
    ungroup() %>%
    # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
    mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
    mutate(spend_per = total_spend * total_spend_pct) %>%
    select(internal_id, spend_per, num_ads) %>%
    arrange(desc(spend_per)) %>%
    summarize(spend_per = sum(spend_per),
              ads_per = sum(num_ads)) %>%
    mutate(target = "interest")
  
  howmuchislocation <- only_tags %>%
    filter(type == "location") %>%
    group_by(internal_id, location_type) %>%
    filter(total_spend_pct == max(total_spend_pct)) %>%
    slice(1) %>%
    ungroup() %>%
    # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
    mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
    mutate(spend_per = total_spend * total_spend_pct) %>%
    select(internal_id, spend_per, location_type, num_ads) %>%
    arrange(desc(spend_per)) %>%
    group_by(location_type) %>%
    summarize(spend_per = sum(spend_per),
              ads_per = sum(num_ads)) %>%
    rename(target = location_type)
  
  # only_tags <- only_tags %>% filter(party == "Volt Nederland")
  howmuchisage <- only_tags %>%
    filter(type == "age") %>%
    filter(total_spend_pct != 0) %>%
    group_by(internal_id) %>%
    mutate(n_ages = n()) %>% #count(n_ages, sort = T)
    ungroup() %>%
    mutate(spending_age = sum(total_spend_pct)) 
  
  if(nrow(howmuchisage)==0){
    howmuchisage <- tibble(spend_per = 0, ads_per = 0,  target = "age")
  } else if(howmuchisage %>% slice(1) %>% pull(spending_age) >= age_limit){
    howmuchisage <- tibble(spend_per = 0, ads_per = 0,  target = "age")
  } else if (nrow(howmuchisage)<age_limit) {
    
    howmuchisage <- howmuchisage %>% mutate(spend_per = total_spend, ads_per = total_num_ads, target = "age") %>% select(spend_per, target, ads_per) %>% slice(1)
    
    
    
    ## TODO: BUT WHYYYYY?
  } else if (!all(howmuchisage$total_spend_pct==1)){
    howmuchisage <- howmuchisage %>% 
      # filter(n_ages <= 47) %>%
      group_by(internal_id) %>%
      filter(total_spend_pct == min(total_spend_pct)) %>%
      slice(1) %>%
      ungroup() %>%
      # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
      mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
      mutate(spend_per = total_spend * (1-total_spend_pct)) %>%
      select(internal_id, spend_per, num_ads) %>%
      summarize(spend_per = sum(spend_per),
                ads_per = sum(num_ads)) %>%
      mutate(ads_per = total_sppppeen$num_ads-ads_per) %>% 
      mutate(target = "age")
  } else if (all(howmuchisage$total_spend_pct==1)){
    
    howmuchisage <- howmuchisage %>% mutate(spend_per = total_spend, ads_per = total_num_ads, target = "age") %>% select(spend_per, target, ads_per) %>% slice(1)
    
  } else {
    
    howmuchisage <- tibble(spend_per = 0, ads_per = 0, target = "age")
    
  }
  
  
  
  
  
  # howmuchisgender <- only_tags %>%
  #     filter(type == "gender") %>%
  #     filter(total_spend_pct != 0) %>%
  #     filter(value != "All") %>%
  #     # group_by(internal_id) %>%
  #     # summarize()
  #     # # filter(total_spend_pct == max(total_spend_pct)) %>%
  #     # slice(1) %>%
  #     # ungroup() %>%
  #     # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
  #     mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
  #     mutate(spend_per = total_spend * total_spend_pct) %>%
  #     select(internal_id, spend_per) %>%
  #     summarize(spend_per = sum(spend_per))  %>%
  #     mutate(target = "gender")
  
  howmuchisgender <- only_tags %>%
    filter(type == "gender") %>%
    filter(value != "All") %>%
    group_by(internal_id, value) %>%
    filter(total_spend_pct == max(total_spend_pct)) %>%
    slice(1) %>%
    ungroup() %>%
    # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
    mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
    mutate(spend_per = total_spend * total_spend_pct) %>%
    select(internal_id, spend_per, value, num_ads) %>%
    arrange(desc(spend_per)) %>%
    group_by(value) %>%
    summarize(spend_per = sum(spend_per),
              ads_per = sum(num_ads)) %>%
    ungroup() %>% 
    mutate(target = paste0("Gender: ", value)) %>% 
    select(-value)
  
  howmuchcustom <- only_tags %>%
    filter(type == "custom_audience") %>%
    filter(total_spend_pct != 0) %>%
    # filter(value != "All") %>%
    group_by(internal_id) %>%
    filter(total_spend_pct == max(total_spend_pct)) %>%
    slice(1) %>%
    ungroup() %>%
    # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
    mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
    mutate(spend_per = total_spend * total_spend_pct) %>%
    select(internal_id, spend_per, num_ads) %>%
    summarize(spend_per = sum(spend_per),
              ads_per = sum(num_ads)) %>%
    mutate(target = "custom_audience")
  
  
  howmuchlookalike <- only_tags %>%
    filter(type == "lookalike_audience") %>%
    filter(total_spend_pct != 0) %>%
    # filter(value != "All") %>%
    group_by(internal_id) %>%
    filter(total_spend_pct == max(total_spend_pct)) %>%
    slice(1) %>%
    ungroup() %>%
    # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
    mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
    mutate(spend_per = total_spend * total_spend_pct) %>%
    select(internal_id, spend_per, num_ads) %>%
    summarize(spend_per = sum(spend_per),
              ads_per = sum(num_ads)) %>%    
    mutate(target = "lookalike_audience")
  
  howmuchlanguage <- only_tags %>%
    filter(type == "language") %>%
    filter(total_spend_pct != 0) %>%
    drop_na(value) %>%
    # filter(value != "All") %>%
    group_by(internal_id) %>%
    filter(total_spend_pct == max(total_spend_pct)) %>%
    slice(1) %>%
    ungroup() %>%
    # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
    mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
    mutate(spend_per = total_spend * total_spend_pct) %>%
    select(internal_id, spend_per, num_ads) %>%
    summarize(spend_per = sum(spend_per),
              ads_per = sum(num_ads)) %>%
    mutate(target = "language")
  
  targeting_on_each <- howmuchisinterest %>%
    bind_rows(howmuchislocation) %>%
    bind_rows(howmuchisage) %>%
    bind_rows(howmuchisgender) %>%
    bind_rows(howmuchcustom) %>%
    bind_rows(howmuchlookalike) %>%
    bind_rows(howmuchlanguage) %>%
    mutate(total = total_sppppeen$total_spend) %>%
    mutate(total_ads = total_sppppeen$num_ads) %>%
    mutate(perc = spend_per/total*100) %>%
    mutate(perc_ads = ads_per/total_ads*100) %>%
    arrange(desc(perc))
  
  return(targeting_on_each)
}

relationshipstuff <- "Widowed|Recently moved|Away|[r|R]elationship|Parents|Partner|Separated|Divorced|Single|Complicated|Married|Engaged|Newlywed|Civil Union|Unspecified|Newly engaged"


add_ribbons <- function(x, adv, col) {
  x %>%
    tab_options(table.width = pct(100)) %>%
    tab_style(
      style = cell_borders(
        sides = c("left"),
        color = col,
        weight = px(18.5),
        style = "solid"
      ),
      locations = cells_body(
        columns = `Number of Advertisers`,
        rows = adv
      ))
}




get_targeting <- function(id, timeframe = "LAST_30_DAYS") {
  
  url <- "https://www.facebook.com/api/graphql/"
  
  heads_up <- httr::add_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:93.0) Gecko/20100101 Firefox/93.0",
                                Accept = "*/*",
                                `Accept-Language` = 'en-GB,en;q=0.5',
                                `X-FB-Friendly-Name` = "AdLibraryPageAudienceTabQuery",
                                `X-FB-LSD`= "AVrNiQCSUnA",
                                `Alt-Used`= "www.facebook.com",
                                `Sec-Fetch-Dest`= "empty",
                                `Sec-Fetch-Mode`= "cors",
                                `Sec-Fetch-Site`= "same-origin",
                                # `Accept-Encoding` = "gzip, deflate, br",
                                `Content-Type` = "application/x-www-form-urlencoded",
                                Connection = "keep-alive"
  )
  
  
  if(timeframe == "LAST_30_DAYS"){
    
    # audienceTimeframe <- "%7B%22audienceTimeframe%22%3A%22LAST_30_DAYS%22%2C%22"
    da_body <- glue::glue("av=0&__user=0&__a=1&__dyn=7xeUmxa3-Q8zo5ObwKBWobVo9E4a2i5U4e1FxebzEdF8ixy7EiwvoWdwJwCwAwgU2lxS6Ehwem0nCqbwgE3awbG78b87C1xwEwgolzUO0n2US2G3i1ywa-2l0Fwwwi831wnFokwyx2cw8WfK6E5i3e4U3mxOu2S2W2K7o725U4q0HUkyE9E11EbodEGdw46wbLwiU8U6C2-&__csr=&__req=m&__hs=19237.BP%3ADEFAULT.2.0.0.0.0&dpr=1&__ccg=EXCELLENT&__rev=1006139712&__s=ll61s1%3Axn89ey%3Admpplc&__hsi=7138774996758193009&__comet_req=0&lsd=AVrNiQCSYrc&jazoest=2981&__spin_r=1006139712&__spin_b=trunk&__spin_t=1662125577&__jssesw=1&fb_api_caller_class=RelayModern&fb_api_req_friendly_name=AdLibraryPageAudienceTabQuery&variables=%7B%22audienceTimeframe%22%3A%22LAST_30_DAYS%22%2C%22viewAllPageID%22%3A%22{id}%22%7D&server_timestamps=true&doc_id=4756112137823411") %>% as.character()
    
  } else if (timeframe == "LAST_7_DAYS"){
    
    # audienceTimeframe <- "%7B%22"
    da_body <- glue::glue("av=0&__user=0&__a=1&__dyn=7xeUmxa3-Q8zo5ObwKBWobVo9E4a2i5U4e1FxebzEdF8aUuxa1ZzES2S2q2i13w9m7oqx60Vo1upEK12wcG0KEswIwuo662y11xmfz81sbzoaEd86a0HU9k2C2218wc61uBxi2a48O0zE-Uqwl8cUjwdq79UbobEaUtws8nwhE2LxiawCw46wJwSyES0gq0K-1bwzwqobU&__csr=&__req=f&__hs=19245.BP%3ADEFAULT.2.0.0.0.0&dpr=1&__ccg=EXCELLENT&__rev=1006179750&__s=njkc5w%3A6o847a%3A9gcoa8&__hsi=7141736891942848978&__comet_req=0&lsd=AVrbeuAiHJg&jazoest=21000&__spin_r=1006179750&__spin_b=trunk&__spin_t=1662815197&__jssesw=1&fb_api_caller_class=RelayModern&fb_api_req_friendly_name=AdLibraryPageAudienceTabQuery&variables=%7B%22audienceTimeframe%22%3A%22LAST_7_DAYS%22%2C%22viewAllPageID%22%3A%22{id}%22%7D&server_timestamps=true&doc_id=4756112137823411") %>% as.character()
    
  } else if (timeframe == "LAST_90_DAYS"){
    
    da_body <- glue::glue("av=0&__user=0&__a=1&__dyn=7xeUmxa3-Q8zo5ObwKBWobVo9E4a2i5U4e1FxebzEdF8aUuxa1ZzES2S2q2i13w9m7oqx60Vo1upEK12wcG0KEswIwuo662y11xmfz81sbzoaEd86a0HU9k2C2218wc61uBxi2a48O3u1mzXxG1kwPxe3C0D8sDwJwKwHxS1Mxu16wa-58G2q0gq2S3qazo11E2XU4K2e1FwLw8O2i&__csr=&__req=h&__hs=19301.BP%3ADEFAULT.2.0.0.0.0&dpr=1&__ccg=EXCELLENT&__rev=1006553893&__s=20shv5%3A62a2bj%3A6goj90&__hsi=7162612241770415577&__comet_req=0&lsd=AVohzhTn68E&jazoest=2965&__spin_r=1006553893&__spin_b=trunk&__spin_t=1667675618&__jssesw=1&fb_api_caller_class=RelayModern&fb_api_req_friendly_name=AdLibraryPageAudienceTabQuery&variables=%7B%22audienceTimeframe%22%3A%22LAST_90_DAYS%22%2C%22viewAllPageID%22%3A%22{id}%22%7D&server_timestamps=true&doc_id=4756112137823411") %>% as.character()
    
    url <- "https://www.facebook.com/api/graphql/"
    
    heads_up <- httr::add_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:93.0) Gecko/20100101 Firefox/93.0",
                                  Accept = "*/*",
                                  `Accept-Language` = 'en-US,en;q=0.5',
                                  `X-FB-Friendly-Name` = "AdLibraryPageAudienceTabQuery",
                                  `X-FB-LSD`= "AVrNiQCSUnA",
                                  `Alt-Used`= "www.facebook.com",
                                  `Sec-Fetch-Dest`= "empty",
                                  `Sec-Fetch-Mode`= "cors",
                                  `Sec-Fetch-Site`= "same-origin",
                                  # `Accept-Encoding` = "gzip, deflate, br",
                                  `Content-Type` = "application/x-www-form-urlencoded",
                                  Connection = "keep-alive"
    )
    
  }
  
  
  
  
  
  posted = httr::POST(url, heads_up, body = da_body)
  
  contentwise <- httr::content(posted)
  
  rate_limit <<- str_detect(as.character(contentwise), "Rate limit exceeded")
  if(rate_limit){
    stop(as.character(contentwise))
  }
  
  
  
  out_raw <- contentwise %>%
    rvest::html_nodes("body") %>%
    rvest::html_nodes("p") %>%
    as.character() %>% str_remove_all("</p>|<p>") %>%
    jsonlite::fromJSON()  %>%
    purrr::pluck("data") %>%
    purrr::pluck("page") %>%
    purrr::pluck("ad_library_page_targeting_insight")
  
  
  summary_dat <- out_raw %>%
    purrr::pluck("ad_library_page_targeting_summary") %>%
    dplyr::bind_rows()
  
  if(nrow(summary_dat) > 1){
    
    summary_dat <- summary_dat %>%
      dplyr::slice(which(summary_dat$detailed_spend$currency == summary_dat$main_currency)) %>%
      dplyr::select(-detailed_spend)
    
  }
  
  targeting_details_raw <- out_raw[!(names(out_raw) %in% c("ad_library_page_targeting_summary", "ad_library_page_has_siep_ads"))]
  
  # names(targeting_details_raw)
  
  res <- targeting_details_raw %>%
    purrr::discard(purrr::is_empty) %>%
    purrr::imap_dfr(~{.x %>% dplyr::mutate(type = .y %>% stringr::str_remove("ad_library_page_targeting_"))}) %>%
    dplyr::bind_cols(summary_dat) %>%
    dplyr::mutate(internal_id = id)
  
  return(res)
  
}

get_targeting <- suppressWarnings(get_targeting)

append_date_suffix <- function(dates){
  dayy <- lubridate::day(dates)
  suff <- case_when(dayy %in% c(11,12,13) ~ "th",
                    dayy %% 10 == 1 ~ 'st',
                    dayy %% 10 == 2 ~ 'nd',
                    dayy %% 10 == 3 ~'rd',
                    TRUE ~ "th")
  paste0(dayy, suff)
}

create_date <- function(x) {
  the_date <- format(x, "%b %d")
  the_date <- ifelse(str_detect(the_date, " 0"),
                     str_remove(the_date, "0"),
                     the_date)
  str_replace(the_date, 
              as.character(lubridate::day(x)), 
              append_date_suffix(x))
}


scale_fill_parties <- function(...){
  ggplot2:::manual_scale(
    'fill', 
    values = setNames(color_dat$colors, color_dat$party), 
    ...
  )
}
scale_color_parties <- function(...){
  ggplot2:::manual_scale(
    'color', 
    values = setNames(color_dat$colors, color_dat$party), 
    ...
  )
}


walk_progress <- function(.x, .f, ...) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(
    total = length(.x), 
    format = " (:spin) [:bar] :percent | :current / :total | eta: :eta",
    # format = " downloading [:bar] :percent eta: :eta",
    force = TRUE)
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::walk(.x, f, ...)
}

map_progress <- function(.x, .f, ...) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(
    total = length(.x), 
    format = " (:spin) [:bar] :percent | :current / :total | eta: :eta",
    # format = " downloading [:bar] :percent eta: :eta",
    force = TRUE)
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::map(.x, f, ...)
}

map_dfr_progress <- function(.x, .f, ...) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(
    total = length(.x), 
    format = glue::glue(" (:spin) [:bar] :percent | :current / :total | eta: :eta | {sets$cntry}"),
    # format = " downloading [:bar] :percent eta: :eta",
    force = TRUE)
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::map_dfr(.x, f, ...)
}

map_chr_progress <- function(.x, .f, ...) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(
    total = length(.x), 
    format = " (:spin) [:bar] :percent | :current / :total | eta: :eta",
    # format = " downloading [:bar] :percent eta: :eta",
    force = TRUE)
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::map_chr(.x, f, ...)
}


library(gh)

# Function to delete a release asset by filename
delete_asset_by_filename <- function(owner, repo, release_id, filename, .token  = gh::gh_token()) {
  # Retrieve all assets for the specified release
  assets <- gh::gh("GET /repos/:owner/:repo/releases/:release_id/assets",
               owner = owner, repo = repo, release_id = release_id, .token = .token)
  
  print(assets)
  
  # Find the asset by filename
  asset <- purrr::keep(assets, ~ .x$name == filename)
  
  print(glimpse(asset))
  
  # Check if the asset was found
  if (length(asset) == 1) {
    # Extract the asset ID
    asset_id <- asset[[1]]$id
    
    # Delete the asset
    gh::gh("DELETE /repos/:owner/:repo/releases/assets/:id",
       owner = owner, repo = repo, id = asset_id, .token = .token)
    
    message("Asset deleted successfully.")
  } else {
    stop("Asset not found or multiple assets with the same name exist.")
  }
}

# Example usage
# Note: Replace 'owner', 'repo', 'release_id', 'filename', and '.token' with actual values
# delete_asset_by_filename(owner = "your_username", repo = "your_repo", release_id = "your_release_id", filename = "your_filename", .token = "your_token")

# df <- releases[releases$tag == "DE-last_90_days",]
# 
# assets <- gh::gh("GET /repos/:owner/:repo/releases/:release_id/assets",
#                  owner = "favstats", repo = "meta_ad_targeting", release_id = df$release_id)
# 
# asset <- purrr::keep(assets, ~ .x$name == "2024-03-16.parquet")


pb_upload_file_fr <- function (file, repo, tag, .token = gh::gh_token(), releases, dir = NULL, skip  = F) {
  # Construct the file path
  file_path <- do.call(file.path, compact(list(dir, file)))
  
  # Check if the file exists
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  # full_repos$file_name
  # Obtain release information
  # releases <- pb_releases(repo = repo, .token = .token)
  upload_url <- releases$upload_url[releases$tag == tag][1]
  # print(upload_url)
  
  if(is.na(upload_url)){
    return(NULL)
  }
  # Set up the request
  rsd <- httr::POST(
    # "POST",
    url = sub("\\{.+$", "", upload_url),
    query = list(name = basename(file_path)),
    httr::add_headers(Authorization = paste("token", .token)),
    body = httr::upload_file(file_path)
  )
  
  r <- piggyback:::parse_repo(repo)
  
  if(!is.null(httr::content(rsd)$errors[[1]]$code)){
    # tag <- "EE-last_7_days"
    print(httr::content(rsd)$errors[[1]])
    
    df <- releases[releases$tag == tag,]
    
    if(!skip){
      
      delete_asset_by_filename(owner = r[1], repo = r[2], release_id = df$release_id, filename = file_path)
      
      # df <- piggyback:::pb_info(repo = repo, tag = tag, .token = .token)
      # i <- which(stringr::str_detect(dfs$file_name, str_remove(file_path, ".parquet")))
      # if (length(i) > 0) {
      # if (use_timestamps) {
      #   local_timestamp <- fs::file_info(file_path)$modification_time
      #   no_update <- local_timestamp <= df[i, "timestamp"]
      #   if (no_update) {
      #     cli::cli_warn("Matching or more recent version of {.file {file_path}} found on GH, not uploading.")
      #     return(invisible(NULL))
      #   }
      # }
      # if (overwrite) {
      # gh::gh("DELETE /repos/:owner/:repo/releases/assets/:id", 
      #        owner = r[1], repo = r[2], 
      #        id = df$id[i], .token = .token)
      # }
      # else {
      #   cli::cli_warn("Skipping upload of {.file {df$file_name[i]}} as file exists on GitHub and {.code overwrite = FALSE}")
      #   return(invisible(NULL))
      # }
      # }
      rsd <- httr::POST(
        # "POST",
        url = sub("\\{.+$", "", upload_url),
        query = list(name = basename(file_path)),
        httr::add_headers(Authorization = paste("token", .token)),
        body = httr::upload_file(file_path)
      )
      
      # } else {
      #   print("already there so we skip")
      # }
      
      httr::warn_for_status(rsd)
      # invisible(rsd)
      
      return(NULL)
      
    }
    

    
    
  }
  
  print(paste0("Status CODE: ", httr::status_code(rsd)))
  # Handle response
  httr::warn_for_status(rsd)
  invisible(rsd)
}



pb_release_create_fr <- function (repo, tag, commit = NULL, name = tag, 
                                  body = "Data release", draft = FALSE, prerelease = FALSE, releases,
                                  .token = gh::gh_token()) {
  if(is.null(releases)){
    releases <- pb_releases(repo = repo, .token = .token, verbose = FALSE)
  }
  
  if (nrow(releases) > 0 && tag %in% releases$tag) {
    cli::cli_warn("Failed to create release: {.val {tag}} already exists!")
    return(invisible(releases[tag %in% releases$tag, 
    ]))
  }
  r <- piggyback:::parse_repo(repo)
  payload <- compact(list(tag_name = tag, target_commitish = commit, 
                          name = name, body = body, draft = draft, prerelease = prerelease))
  resp <- httr::RETRY(verb = "POST", url = glue::glue("https://api.github.com/repos/{r[[1]]}/{r[[2]]}/releases"), 
                      httr::add_headers(Authorization = paste("token", .token)), 
                      body = jsonlite::toJSON(payload, auto_unbox = TRUE), 
                      terminate_on = c(400, 401, 403, 404, 422))
  if (httr::http_error(resp)) {
    cli::cli_warn(c(`!` = "Failed to create release: HTTP error {.val {httr::status_code(resp)}}.", 
                    "See returned error messages for more details"))
    # httr::content(resp)$errors[[1]]$code=="already_exists"
    return(httr::content(resp))
  }
  piggyback:::.pb_cache_clear()
  release <- httr::content(resp)
  cli::cli_alert_success("Created new release {.val {name}}.")
  return(invisible(release))
}




pb_info_fr <- function(repo = guess_repo(),
                       tag = NULL,
                       .token = gh::gh_token()) {
  
  r <- piggyback:::parse_repo(repo)
  
  # get all releases
  releases <- piggyback::pb_releases(repo = repo, .token = .token, verbose = FALSE)
  
  # if no releases return empty df
  if(nrow(releases) == 0) {
    return(
      data.frame(
        file_name = "",
        size = 0L,
        timestamp = .as_datetime(0),
        tag = "",
        owner = r[[1]],
        repo = r[[2]],
        upload_url = "",
        browser_download_url = "",
        api_download_url = "",
        id = "",
        state = "",
        stringsAsFactors = FALSE
      ))
  }
  
  # if tag is "latest" (and no tag is literally named "latest"), set tag to
  # GitHub's idea of latest release tag
  if(identical(tag, "latest") && !"latest" %in% releases$tag_name) {
    tag <- releases$tag_name[releases$latest]
  }
  
  # if tag is present, filter the releases to search to just the tags requested
  if(!is.null(tag)) releases <- releases[releases$tag_name %in% tag,]
  
  # get release assets and metadata for each release
  info <- piggyback:::get_release_assets(releases = releases, r = r, .token = .token)  %>% 
    bind_rows(releases %>% 
                select(tag = release_name,
                       id = release_id,
                       upload_url)) %>% 
    distinct(tag, id, upload_url, .keep_all = T)
  
  return(info)
}


# Define a function to perform the operation
get_full_release <- function() {
  # tryCatch({
    # Your original operation
    full_repos <- pb_info_fr("favstats/meta_ad_targeting") %>% as_tibble()
    
    return(full_repos)  # return the result
  # }, error = function(e) {
  #   # Print the error message
  #   print(paste("Error occurred: ", e$message))
  #   
  #   # Wait for an hour (3600 seconds)
  #   # print("Waiting for 1 hour before retrying...")
  #   # Sys.sleep(3600)
  #   
  #   # Retry the operation
  #   return(get_full_release())
  # })
}



source("https://raw.githubusercontent.com/favstats/appendornot/master/R/save.R")





#' Get Page Insights
#'
#' Retrieves insights for a given Facebook page within a specified timeframe, language, and country. 
#' It allows for fetching specific types of information and optionally joining page info with targeting info.
#'
#' @param pageid A string specifying the unique identifier of the Facebook page.
#' @param timeframe A string indicating the timeframe for the insights. Valid options include predefined 
#'        timeframes such as "LAST_30_DAYS". The default value is "LAST_30_DAYS".
#' @param lang A string representing the language locale to use for the request, formatted as language 
#'        code followed by country code (e.g., "en-GB" for English, United Kingdom). The default is "en-GB".
#' @param iso2c A string specifying the ISO-3166-1 alpha-2 country code for which insights are requested. 
#'        The default is "US".
#' @param include_info A character vector specifying the types of information to include in the output. 
#'        Possible values are "page_info" and "targeting_info". By default, both types of information are included.
#' @param join_info A logical value indicating whether to join page info and targeting info into a single 
#'        data frame (if TRUE) or return them as separate elements in a list (if FALSE). The default is TRUE.
#'
#' @return If \code{join_info} is TRUE, returns a data frame combining page and targeting information for 
#'         the specified Facebook page. If \code{join_info} is FALSE, returns a list with two elements: 
#'         \code{page_info} and \code{targeting_info}, each containing the respective data as a data frame.
#'         In case of errors or no data available, the function may return a simplified data frame or list 
#'         indicating the absence of data.
#'
#' @examples
#' insights <- get_page_insights(pageid="123456789", timeframe="LAST_30_DAYS", lang="en-GB", iso2c="US", 
#'                               include_info=c("page_info", "targeting_info"), join_info=TRUE)
#'
#' @export
#'
#' @importFrom httr2 request req_headers req_body_raw req_perform
#' @importFrom jsonlite fromJSON
#' @importFrom rvest html_element html_text
#' @importFrom dplyr mutate_all select bind_cols left_join slice
#' @importFrom purrr set_names flatten discard imap_dfr is_empty
#' @importFrom stringr str_split str_remove
#' @importFrom tibble as_tibble

get_page_insights <- function (pageid, timeframe = "LAST_30_DAYS", lang = "en-GB",
                               iso2c = "US", include_info = c("page_info", "targeting_info"),
                               join_info = T)
{
  ua_list <- c("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3",
               "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.82 Safari/537.36",
               "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.77 Safari/537.36")
  ua <- sample(ua_list, 1)
  resp <- request("https://www.facebook.com/api/graphql/") %>%
    req_headers(`Accept-Language` = paste0(lang, ",", stringr::str_split(lang,
                                                                         "-") %>% unlist() %>% .[1], ";q=0.5"), `sec-fetch-site` = "same-origin",
                `user-agent` = ua) %>% 
    req_body_raw(glue::glue("av=0&_aaid=0&user=0&a=1&req=3&hs=19797.BP%3ADEFAULT.2.0..0.0&dpr=1&ccg=EXCELLENT&rev=1012093869&s=sbbnic%3Awquopy%3A7r1j3c&hsi=7346737420686302672&dyn=7xe6Eiw_K9zo5ObwKBAgc9o2exu13wqojyUW3qi4EoxW4E7SewXwCwfW7oqx60Vo1upEK12wvk1bwbG78b87C2m3K2y11wBw5Zx62G3i1ywdl0Fw4Hwp8kwyx2cU8EmwoHwrUcUjwVw9O7bK2S2W2K4EG1Mxu16wciaw4JwJwSyES0gq0K-1LwqobU2cwmo6O1Fw44wt8&csr=&lsd=AVo6-wl7l1Q&jazoest=2881&spin_r=1012093869&spin_b=trunk&spin_t=1710545602&_jssesw=1&fb_api_caller_class=RelayModern&fb_api_req_friendly_name=AdLibraryMobileFocusedStateProviderQuery&variables=%7B%22adType%22%3A%22POLITICAL_AND_ISSUE_ADS%22%2C%22audienceTimeframe%22%3A%22{timeframe}%22%2C%22country%22%3A%22{iso2c}%22%2C%22viewAllPageID%22%3A%22{pageid}%22%2C%22fetchPageInfo%22%3Atrue%2C%22fetchSharedDisclaimers%22%3Atrue%2C%22active_status%22%3A%22ALL%22%2C%22ad_type%22%3A%22POLITICAL_AND_ISSUE_ADS%22%2C%22bylines%22%3A%5B%5D%2C%22collation_token%22%3A%227ca3912f-0148-43ce-83e4-9a68ef656e4d%22%2C%22content_languages%22%3A%5B%5D%2C%22count%22%3A30%2C%22countries%22%3A%5B%22{iso2c}%22%5D%2C%22excluded_ids%22%3A%5B%5D%2C%22full_text_search_field%22%3A%22ALL%22%2C%22group_by_modes%22%3A%5B%5D%2C%22image_id%22%3Anull%2C%22location%22%3Anull%2C%22media_type%22%3A%22ALL%22%2C%22page_ids%22%3A%5B%5D%2C%22pagination_mode%22%3Anull%2C%22potential_reach_input%22%3Anull%2C%22publisher_platforms%22%3A%5B%5D%2C%22query_string%22%3A%22%22%2C%22regions%22%3A%5B%5D%2C%22search_type%22%3A%22PAGE%22%2C%22session_id%22%3A%221678877b-700b-485a-abb0-60efcb6b4019%22%2C%22sort_data%22%3A%7B%22mode%22%3A%22SORT_BY_RELEVANCY_MONTHLY_GROUPED%22%2C%22direction%22%3A%22ASCENDING%22%7D%2C%22source%22%3Anull%2C%22start_date%22%3Anull%2C%22view_all_page_id%22%3A%22{pageid}%22%7D&server_timestamps=true&doc_id=7193625857423421"),
                                                    "application/x-www-form-urlencoded") %>% req_perform()
  out <- resp %>% httr2::resp_body_html() %>% rvest::html_element("p") %>%
    rvest::html_text() %>% str_split_1("(?<=\\})\\s*(?=\\{)") %>%
    map(jsonlite::fromJSON)
  if (is.null(out[[1]][["data"]][["page"]][["ad_library_page_targeting_insight"]])) {
    message(out[[1]][["errors"]][["description"]])
    rate_limit <- stringr::str_detect(as.character(out[[1]][["errors"]]), "Rate limit exceeded")
    if(rate_limit){
      return(tibble(error = T))
    }
  }
  if ("page_info" %in% include_info) {
    page_info1 <- out[[1]][["data"]][["ad_library_page_info"]][["page_info"]]
    if (is.null(page_info1)) {
      if ("page_info" %in% include_info & "targeting_info" %in%
          include_info) {
        if (join_info) {
          return(tibble(page_id = pageid, no_data = T))
        }
        else {
          return(list(page_info = tibble(page_id = pageid,
                                         no_data = T), targeting_info = tibble(page_id = pageid,
                                                                               no_data = T)))
        }
      }
      else {
        return(tibble(page_id = pageid, no_data = T))
      }
    }
    my_dataframe <- as.data.frame(t(unlist(page_info1)),
                                  stringsAsFactors = FALSE) %>% dplyr::mutate_all(as.character)
    page_info2_raw <- out[[2]][["data"]][["page"]][["shared_disclaimer_info"]][["shared_disclaimer_pages"]][["page_info"]]
    if (!is.null(page_info2_raw)) {
      page_info2 <- page_info2_raw %>% tibble::as_tibble() %>%
        dplyr::mutate_all(as.character) %>% dplyr::mutate(shared_disclaimer_info = pageid[1])
    }
    else {
      page_info2 <- tibble(no_shared_disclaimer = T)
    }
    creat_times <- out[[1]][["data"]][["page"]][["pages_transparency_info"]][["history_items"]] %>%
      dplyr::mutate(event = paste0(item_type, ": ", as.POSIXct(event_time,
                                                               origin = "1970-01-01", tz = "UTC"))) %>% dplyr::select(event) %>%
      unlist() %>% t() %>% as.data.frame()
    about_text <- out[[1]][["data"]][["page"]][["about"]] %>%
      purrr::set_names("about")
    address_raw <- out[[1]][["data"]][["page"]][["confirmed_page_owner"]][["information"]]
    if (!is.null(address_raw)) {
      address <- address_raw %>% purrr::flatten()
    }
    else {
      address <- tibble(no_address = T)
    }
    sdis_raw <- out[[2]][["data"]][["page"]][["shared_disclaimer_info"]][["shared_disclaimer_pages"]][["page_info"]]
    if (!is.null(sdis_raw)) {
      sdis <- sdis_raw %>% dplyr::mutate_all(as.character) %>%
        dplyr::mutate(shared_disclaimer_page_id = pageid[1]) %>%
        jsonlite::toJSON() %>% as.character()
    }
    else {
      sdis <- "[]"
    }
    page_info <- my_dataframe %>% dplyr::mutate(shared_disclaimer_info = sdis) %>%
      dplyr::bind_cols(about_text) %>% dplyr::bind_cols(creat_times) %>%
      dplyr::bind_cols(address)
  }
  if ("targeting_info" %in% include_info) {
    out_raw <- out[[1]][["data"]][["page"]][["ad_library_page_targeting_insight"]]
    summary_dat <- out_raw %>% purrr::pluck("ad_library_page_targeting_summary") %>%
      dplyr::bind_rows()
    if (nrow(summary_dat) > 1) {
      summary_dat <- summary_dat %>% dplyr::slice(which(summary_dat$detailed_spend$currency ==
                                                          summary_dat$main_currency)) %>% dplyr::select(-detailed_spend)
    }
    targeting_details_raw <- out_raw[!(names(out_raw) %in%
                                         c("ad_library_page_targeting_summary", "ad_library_page_has_siep_ads"))]
    targeting_info <- targeting_details_raw %>% purrr::discard(purrr::is_empty) %>%
      purrr::imap_dfr(~{
        .x %>% dplyr::mutate(type = .y %>% stringr::str_remove("ad_library_page_targeting_"))
      }) %>% dplyr::bind_cols(summary_dat) %>% dplyr::mutate(page_id = pageid)
  }
  if ("page_info" %in% include_info & "targeting_info" %in%
      include_info) {
    if (join_info) {
      fin <- page_info %>% left_join(targeting_info, by = "page_id")
    }
    else {
      fin <- list(page_info, targeting_info)
    }
  }
  else if ("page_info" %in% include_info) {
    return(page_info)
  }
  else if ("targeting_info" %in% include_info) {
    return(targeting_info)
  }
  return(fin)
}

