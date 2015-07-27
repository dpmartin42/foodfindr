library(shiny)
library(leaflet)

shinyUI(fluidPage(
  titlePanel("Food Findr"),
  
  sidebarLayout(
    sidebarPanel(
      
      helpText("Your guide to local, healthy dining options in Boston, MA."),
      
      tags$hr(),
      
      textInput("address", label = "Enter your address", 
                value = "e.g., 50 Milk Street"),
  
      tags$hr(),
      
      sliderInput("distance", label = "Max distance (in mi.)", min = 0.1, 
                  max = 3.0, value = 0.5),
      
      tags$hr(),
      
      checkboxGroupInput("price", 
                         label = "Price preference", 
                         choices = list("$" = "$", 
                                        "$$" = "$$",
                                        "$$$" = "$$$",
                                        "$$$$" = "$$$$",
                                        "$$$$$" = "$$$$$"),
                         selected = c("$", "$$", "$$$", "$$$$", "$$$$$")),
      
      tags$hr(),
      
      submitButton("Update View")

    ),
    
    mainPanel(tabsetPanel(
      tabPanel("Map", leafletOutput("mymap", width = 800, height = 550)), 
      tabPanel("Table", dataTableOutput("restaurants"))
      )
    )

  )
))

