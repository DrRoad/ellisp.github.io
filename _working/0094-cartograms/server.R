library(shiny)
library(leaflet)

camel_to_english <- function(camelCase){
   return(gsub("([a-z])([A-Z])", "\\1 \\L\\2", camelCase, perl = TRUE))
}

load("comb_data.rda")
load("reg_cart.rda")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   var <- reactive({
      var_name <- paste0("Prop", input$variable, "2013")
      value <- comb_data[ , var_name]
      return(value)
   })
   
   the_data <- reactive({
      tmp <- reg_cart
      tmp@data[ ,"value"] <- var()
      tmp@data[ , "label"] <-paste0(tmp@data$Name, " ", round(tmp@data$value * 100, 1), "%")
      return(tmp)
   })   
   
   colour_reverse <- reactive({
      return(input$colour_order == "Reverse")
   })
   
   map <- reactive({
      the_data() %>%
      leaflet() %>%
      addPolygons(color = "#444444", weight = 1,
         fillOpacity = 1,
         fillColor = ~colorNumeric(input$colour_scheme, range(var()), reverse = colour_reverse())(value),
         label = ~label,
         highlightOptions = highlightOptions(color = "black", weight = 2,
                                             bringToFront = TRUE)) %>%
         addLegend(pal = colorNumeric(input$colour_scheme, range(var()), reverse = colour_reverse()), 
                   values = range(var()), bins = 5, opacity = 1, 
                   title = "Percentage",
                   labFormat = labelFormat(
                      transform = function(x){x * 100}, 
                      suffix = "%")
         )
      }) 
   output$myMap <- renderLeaflet(map())
   
   the_title <- reactive({
      tmp <- paste("Proportion", tolower(camel_to_english(input$variable)), "in 2013 Census")
      tmp <- gsub("_", " ", tmp)
      return(tmp)
   })
   
   output$the_title <- renderText(the_title())


      
   
})
