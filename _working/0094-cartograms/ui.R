library(shiny)
library(leaflet)
load("variables.rda")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Stuff"),
  
  sidebarLayout(
    sidebarPanel(
       selectInput("variable", "Proportion of population - variable for colour", choices = variables),
       selectInput("colour_scheme", "Colour Scheme",
                   choices = c("Reds", "Blues", "Greys", 
                               "Spectral", "YlOrRd", "RdYlBu", "RdGy"),
                   selected = "YlOrRd"),
       radioButtons("colour_order", "Colour order?",
                    choices = c("Original", "Reverse"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       leafletOutput('myMap', width = 600, height = 600)
    )
  )
))
