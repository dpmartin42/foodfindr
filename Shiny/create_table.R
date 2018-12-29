#############################################################
# create_table:
# function to create a table of restaurants
# @param input_address address text input by the user
# @param input_distance maximum distance input by the user
# @param input_price price filter input by the user
# @param input_restrictions dietary restrictions input by the user

library(readr)
library(dplyr)
library(RCurl)
library(jsonlite)
library(SDMTools)

restaurant_data <- read_csv("data/restaurant_ratings.csv") %>% 
  mutate(special_diet = if_else(is.na(special_diet), "", special_diet))

API_KEY <- read_csv("data/opencage_apikey.txt") %>% 
  names()

METERS_TO_MILES <- 0.000621371192
BOSTON_COORD <- data_frame(lat = 42.36025, lng = -71.05829)

create_table <- function(input_address, input_distance, input_price, input_restrictions){
  
  # Clean input address if it's the default
  input_address <- if_else(input_address == "e.g., 50 Milk Street, Boston MA",
                           "50 Milk Street, Boston MA",
                           input_address)
  
  address_call <- paste0("https://api.opencagedata.com/geocode/v1/json?q=",
                         input_address,
                         "&key=",
                         API_KEY,
                         "&pretty=1")
  
  address_data <- address_call %>%
    URLencode() %>% 
    getURL() %>%
    fromJSON()
  
  if(address_data$total_results > 0){
    
    address_data_clean <- address_data$results %>% 
      .[[c("components")]] %>% 
      select(state_code) %>% 
      bind_cols(address_data$results[["geometry"]],
                data_frame(confidence = address_data$results[["confidence"]])) %>% 
      mutate(center_lat = BOSTON_COORD$lat,
             center_lng = BOSTON_COORD$lng) %>% 
      mutate(distance = distance(lat1 = lat, lon1 = lng, lat2 = center_lat, lon2 = center_lng)$distance) %>% 
      filter(state_code == "MA" & confidence > 8 & distance * METERS_TO_MILES < 7) %>% 
      arrange(desc(confidence), distance)
    
    address_lat <- address_data_clean$lat[1]
    address_long <- address_data_clean$lng[1]
  
  }
  
  if(address_data$total_results == 0){
    
    restaurant_sub <- data_frame(Name = character(),
                                 Address = character(),
                                 Price = character(),
                                 health_color = character(),
                                 Distance = double(),
                                 special_diet = character())
    
    address_lat <- NA
    address_long <- NA
  
  } else if(nrow(address_data_clean) == 0){
    
    restaurant_sub <- data_frame(Name = character(),
                                 Address = character(),
                                 Price = character(),
                                 health_color = character(),
                                 Distance = double(),
                                 special_diet = character())
    
  } else{
    
    distances <- distance(lat1 = rep(address_lat, time = nrow(restaurant_data)),
                          lon1 = rep(address_long, time = nrow(restaurant_data)),
                          lat2 = restaurant_data$latitude,
                          lon2 = restaurant_data$longitude)
    
    restaurant_data$distance <- round(distances$distance * METERS_TO_MILES, 2)
    restaurant_data$health_color <- factor(restaurant_data$health_color, levels = c("green", "yellow", "red"))
    
    restaurant_sub <- restaurant_data %>% 
      select(name, address, price, distance, longitude, latitude, link, health_color, special_diet) %>%
      filter(price %in% input_price,
             distance < input_distance, 
             grepl(paste(input_restrictions, collapse = "|"), special_diet)) %>%
      arrange(health_color, distance)
    
    names(restaurant_sub)[1:4] <- c("Name", "Address", "Price", "Distance")
    
  }

  return(list(c(address_long, address_lat), restaurant_sub))

}
