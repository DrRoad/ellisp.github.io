library(shiny)
library(h2o)
library(ggvis)
library(scales)
h2o.init()

load("mod.rda")
load("nzes_imp.rda")

nzes_h2o <- as.h2o(nzes_imp)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   the_probs <- reactive({
      
      x <- nzes_h2o[1, ]
      
      x$dage <- input$dage
      x$NotEuropean <- 1 - 1 * input$European
      x$Maori <- 1 * input$Maori
      x$HHIncome <- input$HHIncome
      x$OwnHouseOrFlat <- 1 * input$OwnHouseOrFlat
      x$HHMemberTradeUnion <- 1 * input$HHMemberTradeUnion
      x$HHMemberProfAssoc <- 1 * input$HHMemberProfAssoc
      x$NZBorn <- 1 * input$NZBorn
      x$Religion <- input$Religion
      x$Marital <- 1 * input$Marital
      x$HighestQual <- input$HighestQual
      x$IdentifyWorkingClass <- 1 * input$IdentifyWorkingClass
      x$WorkStatus <- input$WorkStatus
      x$Student <- 1 * input$Student
      x$SuperviseAnyone <- 1 * input$SuperviseAnyone
      x$City <- 1 * input$City
      x$Male <- 1 * input$Male
      
      
      tmp <- as.vector(predict(mod, newdata = x))[-1]
      tmp2 <- data.frame(
         party = c("Did not vote",  "Green", "Labour", "NZ First", "National", "Other"),
         prob = as.numeric(as.character(tmp))
      )
      return(tmp2)
   })
   
   # for some reason the scale_ordinal doesn't work with shiny, only in interactive mode
   the_probs %>%
      ggvis(x = ~party, y = ~prob, fill = ~party) %>%
      layer_bars(opacity := 0.5) %>%
      scale_ordinal('fill', range = c("lightgrey", "green", "red",  "blue", "black", "lightgrey")) %>%
      bind_shiny("distPlot")

})
