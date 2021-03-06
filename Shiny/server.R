library(shiny)
library(leaflet)

source("create_table.R")

shinyServer(function(input, output, session) {
  
  output$restaurants <- renderDataTable({
    
    output_table <- create_table(input$address, input$distance, input$price, input$restrictions)[[2]] %>%
      select(Name, Address, Price, health_color, Distance, special_diet) %>%
      rename("Health Rating" = health_color,
             "Dietary Restriction" = special_diet)
    
    shiny::validate(
      need(nrow(output_table) > 0,
      "I'm sorry, there are no restaurants that match your current search criteria.
      Please expand your search distance or try another address in the Greater Boston area.
      Be sure to include city and state (e.g., Boston, MA or Cambridge, MA) to help 
      identify the correct location.")
    )
    
    output_table

  },

  options = list(pageLength = 10)
  
  )
 
  output$mymap <- renderLeaflet({
    
    output_data <- create_table(input$address, input$distance, input$price, input$restrictions)
    
    output_location <- output_data[[1]]
    output_table <- output_data[[2]]
    
    shiny::validate(
      need(nrow(output_table) > 0,
           "\n\tI'm sorry, there are no restaurants that match your current search criteria.
           Please expand your search distance or try another address in the Greater Boston area.
           Be sure to include city and state (e.g., Boston, MA or Cambridge, MA) to help 
           identify the correct location.")
      )
    
    output_table$link <- gsub("/$", "", output_table$link)
      
    output_table$content <- paste0("<b>", output_table$Name, "</b>") %>%
      paste("<center>", ., output_table$Address, output_table$Price, output_table$special_diet, "</center>", sep = "<br/>")
      
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
