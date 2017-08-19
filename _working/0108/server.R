library(shiny)
library(DT)
library(dplyr)
library(tidyr)

load("nzes.rda")
load("vars.rda")

shinyServer(function(input, output, session) {
  the_var <- reactive({
    names(vars)[as.character(vars) == input$variable]
  }) 
  
  the_value_var <- reactive({
    if(input$value == "Sample size"){
      tmp <- "unweighted"
    } else if(input$value != "Margin of error"){
      tmp <- "weighted"
    } else {
      tmp <- "rme"
    }
    return(tmp)
  })
  
   my_table <- reactive({
     tab <- nzes %>%
       group_by_(the_var(), "dpartyvote") %>%
       summarise(weighted = sum(dwtfin),
                 unweighted = n()) %>%
       ungroup()
     
     if(input$value == "Percentage"){
       if(input$percent_type == "Columns"){
         tab <- tab %>%
           group_by_(the_var()) %>%
           mutate(weighted = weighted / sum(weighted) * 100)
           
       } else {
         tab <- tab %>%
           group_by_("dpartyvote") %>%
           mutate(weighted = weighted / sum(weighted) * 100)
       }
     }
     
     if(input$value == "Margin of error"){
       tab <- tab %>%
         ungroup() %>%
         mutate(p = pmax(0.05, weighted / sum(weighted)),
                rme = round(1.96 * sqrt(p * (1 - p) / pmax(1, unweighted)) / p * 100))
         
     }
     
     tab <- tab %>%
       mutate(weighted = round(weighted)) %>%
       select_(the_var(), "dpartyvote", the_value_var()) %>%
       spread_(the_var(), the_value_var(), fill = 0) %>%
       rename_("Party vote" = "dpartyvote")
     
     return(tab)
   })
  
   output$the_table <- renderDataTable(my_table(), 
                                       options = list(
                                         dom = 't',
                                         autoWidth = TRUE, 
                                         pageLength = 20))
   
   output$the_heading <- renderText(paste0("<h2>", input$variable, "</h2>"))
  
  
})
