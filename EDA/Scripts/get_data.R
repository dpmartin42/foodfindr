###################
# Scrape menu data

rm(list = ls())

# will need to save as a json so I can read it into python as a dict

library(dplyr)
library(rvest)
library(tm)

# Just do R for now and I can convert if needed

restaurants <- readLines("http://boston.menupages.com/restaurants/downtown-north-end/financial-district/all-cuisines/1")

links_names <- restaurants[grep("<a class=\"link\" href='/restaurants/.*'", restaurants)]

names <- gsub(".*</span>", "", links_names) %>%
  gsub ("</a>.*", "", .) %>%
  gsub("\\(.*\\)", "", .)

links <- gsub(".* href='", "", links_names) %>%
  gsub("'><span.*", "", .)

addresses <- gsub('.*</a>', '', links_names) %>%
  gsub(' \\|.*', '', .)
  
locations <- restaurants[grep("\\['longitude'\\] = |\\['latitude'\\] = ", restaurants)]

locations_exact <- gsub('.*= \"', '', locations) %>%
  gsub('\";', '', .) %>%
  as.numeric()

prices <- restaurants[grep('<td class="price">', restaurants)]

prices_exact <- gsub('.*"price[0-9]\">', '', prices) %>%
  gsub('</span>.*', '', .)

restaurant_data <- data.frame(names,
                              links,
                              addresses,
                              longitude = locations_exact[seq(from = 1, to = length(locations_exact), by = 2)],
                              latitude = locations_exact[seq(from = 2, to = length(locations_exact), by = 2)],
                              price = prices_exact)

# To-do eventually:
# Fix apostrophe's so they actually show up

base_url <- "http://boston.menupages.com"

food_list <- list(NA)

for(i in 1:nrow(restaurant_data)){
  
  menu_page <- readLines(paste0(base_url, links[i], "menu"))
  
  the_food <- menu_page[grep("<tr><th>.*</td><td>", menu_page)]
  
  # Need to build in within-item normalization to try and fix weighting
  
  foods <- gsub("</?[^>]*>", " ", the_food) %>%
    gsub("&nbsp;|&#[0-9][0-9];|&amp;", " ", .) %>%
    strsplit(" ") %>%
    unlist() %>%
    gsub("[^A-z]", "", .) %>%
    .[. != ""] %>%
    tolower()
  
  food_list[[i]] <- foods
  
  print(i)

}

# about 5 minutes for 100 restaurants

total_words <- unlist(food_list)

plot(sort(table(total_words)))

final_words <- names(table(total_words)[table(total_words) > 50]) %>%
  .[!(. %in% stopwords())]

satfat_per_cal <- c(NA)

for(i in 1:length(final_words)){
  
  cal_html <- readLines(paste0("http://www.fatsecret.com/calories-nutrition/search?q=", final_words[i]))
  
  search_results <- grep('<a class="prominent" href="/calories-nutrition', cal_html)
  
  if(length(search_results) > 0){
    
    the_link <- cal_html[search_results[1]] %>%
      gsub('\">.*', '', .) %>%
      gsub('.*href=\"|', '', .) %>%
      paste0("http://www.fatsecret.com", .) %>%
      readLines()
    
      sat_fat <- the_link[grep("Cholesterol", the_link) + 1] %>%
          gsub("\t\t\t\t|mg", "", .) %>%
          as.numeric()
    
    total_cal <- the_link[grep("\t\t\t\t<b>Calories</b>", the_link)] %>%
      gsub("\t\t\t\t<b>Calories</b> ", "", .) %>%
      as.numeric()
    
    satfat_per_cal[i] <- sat_fat/total_cal
    
  } else{
    
    satfat_per_cal[i] <- NA
    
  }
  
  print(i)
  
}

satfat_per_cal[is.infinite(satfat_per_cal)] <- 0

food_data <- data.frame(food_name = final_words,
                        health_score = satfat_per_cal)

merge_data <- list(NA)

for(i in 1:nrow(restaurant_data)){
  
  the_data <- food_list[[i]]
  merge_data[[i]] <- restaurant_data[rep(i, length(the_data)), ]

  merge_data[[i]]$food_name <- food_list[[i]]
  
}

total_data <- do.call("rbind", merge_data) %>%
  merge(., food_data, by = "food_name", all.x = TRUE) %>%
  na.omit()

head(total_data, n = 100)

agg_data <- group_by(total_data, links) %>%
  summarise(names = unique(names),
            addresses = unique(addresses),
            latitude = unique(latitude),
            longitude = unique(longitude),
            price = unique(price),
            my_mean = mean(health_score),
            my_N = n(),
            my_median = median(health_score)) %>%
  filter(my_N > 50) %>%
  mutate(health_score = my_mean * -1) %>%
  mutate(health_score = round(percent_rank(health_score), 2) * 100) %>%
  select(names, addresses, price, health_score)

arrange(agg_data, desc(health_score))

write.csv(agg_data, "Documents/Insight Health Data Science/Project/EDA/Data/food_data.csv", row.names = FALSE)

# To-do:
# Get top 10 and bottom 10 and record with current code for N = 100 (on first page)
# Add in within-item normalization and re-check results
# Extract more nurtitional features
# Run a PCA to see dimension reduction
# If it doesn't look great, try scraping GrubHub for training data for these features
# -> after checking if grubhub healthy tag makes sense






