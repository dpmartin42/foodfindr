library(shiny)
library(leaflet)

source("create_table.R")
input_address = "50 Milk St"; input_distance = 1; input_price = c("$", "$$", "$$$")
output_table <- create_table("50 Milk St", 1, c("$", "$$", "$$$"))[[2]] %>%
  select(names, addresses, price, health_color, distance) %>%
  rename("health rating" = health_color)

shinyServer(function(input, output) {
  
  output$restaurants <- renderDataTable({
    
    output_table <- create_table(input$address, input$distance, input$price)[[2]] %>%
      select(names, addresses, price, health_color, distance) %>%
      rename("health rating" = health_color)
    
    shiny::validate(
      need(nrow(output_table) > 0, "I'm sorry, there are no restaurants that match your current search criteria. Please expand your search distance or try another address in the Boston area.")
    )
    
    output_table
    
  },

  options = list(pageLength = 10)
  
  )
  
  output$mymap <- renderLeaflet({
    
    output_data <- create_table(input$address, input$distance, input$price)
    
    output_location <- output_data[[1]]
    output_table <- output_data[[2]]
    
    if(nrow(output_table) == 0){
      
      leaflet() %>% 
        setView(lng = output_location[1], lat = output_location[2], zoom = 16) %>%
        addProviderTiles("CartoDB.Positron")

    } else{
      
      output_table$links <- gsub("/$", "", output_table$links)
      
      output_table$content <- paste0("<b><a href='http://boston.menupages.com", output_table$links, "' target='_blank'>", output_table$names, "</a></b>") %>%
        paste("<center>", ., output_table$addresses, output_table$price, "</center>", sep = "<br/>")
      
      pal <- colorFactor(c("green", "yellow", "red"), levels = c("green", "yellow", "red"))
      
      leaflet(output_table) %>% 
        setView(lng = output_location[1], lat = output_location[2], zoom = 16) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addCircleMarkers(~longitude, ~latitude, popup = ~content, color = ~pal(health_color),
                         radius = 5,
                         stroke = FALSE,
                         fillOpacity = 0.5)

    }
    
  })
  
})
