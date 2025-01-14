library(jsonlite)
library(slickR)
library(httr)
library(lubridate)
library(hoopR)

# ui_module.R
slickUI <- function(id) {
  ns <- NS(id)  # Unique namespace for the module
  
  slickROutput(ns(outputId, width = "100%", height = "400px"))
  
}

# server_module.R
slickServer <- function(input, output, session) {
  
  parse_clean_data <- function(url) {
    
    call <- GET(toString(url))
    games <- content(call, as = "text")
    cleaned_games <- fromJSON(games)[["events"]]
    
    extract <- sub(".*(/)([^/]+)(?=/[^/]*$).*", "\\2", url, perl = TRUE)
    cleaned_games$sport <- extract
    cleaned_games$date <- ymd_hms(cleaned_games$date)
    
    # Get current date and time
    current_time <- Sys.time()
    
    # Filter out games that are not today or have already passed
    games_today <- games[as.Date(games$game_time) == as.Date(current_time) & games$game_time > current_time, ]
    
    
    return()
  }
  
  nba_todays_scoreboard()
  
  
  
  
  test <- get_live_games()
  
  live_games <- slickR(obj = )
  
  output$slick <-   renderSlickR(l
                         autoplay = TRUE, 
                         autoplaySpeed = 500,
                         )
  
}
test <- "https://site.api.espn.com/apis/site/v2/sports/basketball/nba/scoreboard"
fromjson(content(get(test),as = "text"))

 
  renderSlickR(expr, env = parent.frame(), quoted = FALSE)
