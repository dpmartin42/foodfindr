#############################################################
# create_table:
# function to create a table of restaurants

library(DBI)
library(dplyr)
library(RCurl)
library(SDMTools)
library(jsonlite)

# Drop first column of row numbers after pulling from database

con <- dbConnect(RMySQL::MySQL(), db = "food_db")
restaurant_data <- dbGetQuery(con, "SELECT * FROM food_tb") %>%
  .[, -1]
dbDisconnect(con)

create_table <- function(input_address, input_distance, input_price){
  
  address_call <- paste0("https://maps.googleapis.com/maps/api/geocode/json?address=",
                         input_address,
                         ",+Boston,+MA")
  
  address_data <- URLencode(address_call) %>%
    getURL() %>%
    fromJSON()
  
  address_lat <- address_data$results$geometry$location$lat
  address_long <- address_data$results$geometry$location$lng
  
  METERS_TO_MILES <- 0.000621371192
  
  # Backwards due to coding error (NEED TO FIX)
  
  distances <- distance(lat1 = rep(address_lat, time = nrow(restaurant_data)),
                        lon1 = rep(address_long, time = nrow(restaurant_data)),
                        lat2 = restaurant_data$latitude,
                        lon2 = restaurant_data$longitude)
  
  restaurant_data$distance <- round(distances$distance * METERS_TO_MILES, 2)
  
  restaurant_data$health_color <- factor(restaurant_data$health_color, levels = c("green", "yellow", "red"))
  
  restaurant_sub <- select(restaurant_data, names, addresses, price, health_color, distance, longitude, latitude, links) %>%
    filter(price %in% input_price, distance < input_distance) %>%
    arrange(health_color, distance)
  
  return(list(c(address_long, address_lat), restaurant_sub))
  
}
