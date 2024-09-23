library(tidyverse)
library(rvest)
library(httr)
library(glue)
library(dplyr)

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

if(the_timelag>3){
  
  post_data_to_slack(timelag_data, thebot)
  
}
