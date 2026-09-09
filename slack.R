library(tidyverse)
library(rvest)
library(httr)
library(glue)
library(dplyr)

source("release_assets.R")

out <- "US" %>%
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
  # .[str_detect(., "last_90_days")] %>%
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
      parse_release_assets() %>%
      filter(filename != "Source code") %>%
      mutate(release = .x) %>%
      mutate_all(as.character)
    
    
  })

timelag_data <- out %>%
  rename(tag = release,
         file_name = filename) %>%
  arrange(desc(tag)) %>%
  separate(
    tag,
    into = c("country", "timeframe"),
    remove = F,
    sep = "-"
  ) %>%
  filter(str_detect(file_name, "parquet")) %>%
  mutate(day  = str_remove(file_name, "\\.rds|\\.zip|\\.parquet") %>% lubridate::ymd()) %>%
  arrange(desc(day)) %>%
  group_by(timeframe) %>%
  slice(1) %>%
  ungroup() %>% 
  mutate(time_lag = lubridate::today()-day) 





post_data_to_slack <- function(data, webhook_url) {
  
  # Format the data into a readable message
  formatted_data <- data %>%
    mutate(
      file_info = glue("{file_name} ({file_size}) - {country}, {timeframe}, uploaded on {day}, lag: {time_lag} days")
    ) %>%
    pull(file_info) %>%
    paste(collapse = "\n")
  
  # Create the final message for Slack
  message <- glue("Recent File Updates:\n{formatted_data}")
  
  # Title of the message
  msg_title <- glue("Meta Targeting Data Lags Behind")
  
  # Send the POST request to Slack webhook
  request <- POST(webhook_url,
                  body = paste(
                    '{"attachments": [{',
                    '"pretext": "', msg_title, '",',
                    '"text": "', message, '", "color": "#36a64f"',
                    '}]}',
                    sep = ''
                  ),
                  encode = "json",
                  content_type_json()
  )
  
  # Check if the message was posted successfully
  if (status_code(request) == 200) {
    print("Message successfully posted to Slack!")
  } else {
    print("Failed to post the message to Slack.")
  }
}

thebot <- Sys.getenv("slackbot")
if(thebot==""){
  thebot <- Sys.getenv("SLACKBOT")
}

the_timelag <- timelag_data %>% 
  filter(timeframe == "last_30_days") %>% 
  pull(time_lag) %>% 
  as.numeric()

# An unset webhook (POST("") is a malformed URL) or an empty time_lag (if() on a
# zero-length value is an error) used to halt this script, which marked all 204
# matrix jobs failed over a notification. A job that is red every day carries no
# signal, and it hid that the data steps above had actually succeeded. Report and
# carry on instead.
if (length(the_timelag) != 1 || is.na(the_timelag)) {
  message("No last_30_days time lag available; skipping Slack notification.")
} else if (!nzchar(thebot)) {
  message("SLACKBOT is not set; skipping Slack notification (time lag: ", the_timelag, " days).")
} else if (the_timelag > 3) {
  try(post_data_to_slack(timelag_data, thebot))
}
