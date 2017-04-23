library(shiny)
library(leaflet)

load("comb_data.rda")
load("reg_cart_simpl.rda")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   var <- reactive({
      var_name <- paste0("Prop", input$variable, "2013")
      value <- comb_data[ , var_name]
      return(value)
   })
   
   the_data <- reactive({
      tmp <- reg_cart_simpl
      tmp@data[ ,"value"] <- var()
      return(tmp)
   })   
   
   colour_reverse <- reactive({
      return(input$colour_order == "Reverse")
   })
   
   
   map <- reactive({reg_cart_simpl %>%
      leaflet() %>%
      addPolygons(data = the_data(),
         color = "#444444", weight = 1,
         fillOpacity = 1,
         fillColor = ~colorQuantile(input$colour_scheme, value, reverse = colour_reverse())(value),
         highlightOptions = highlightOptions(color = "steelblue", weight = 2,
                                             bringToFront = TRUE))})
   output$myMap <- renderLeaflet(map())
   
   
})
