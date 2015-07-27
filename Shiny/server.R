library(shiny)
library(leaflet)

source("create_table.R")

shinyServer(function(input, output) {
  
  output$restaurants <- renderDataTable({
    
    create_table(input$address, input$distance, input$price)[[2]] %>%
      select(names, addresses, price, health_color, distance) %>%
      rename("health rating" = health_color)
    
  },
  
  options = list(pageLength = 10)
  
  )
  
  output$mymap <- renderLeaflet({
    
    output_data <- create_table(input$address, input$distance, input$price)
    
    output_location <- output_data[[1]]
    output_table <- output_data[[2]]
    
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
    
  })
  
})
