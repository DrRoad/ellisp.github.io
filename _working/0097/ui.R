library(shiny)
load("nzes_imp.rda")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Predict your election vote..."),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       selectInput("WorkStatus",
                   "Working status",
                   unique(nzes_imp$WorkStatus),
                   selected = "Full time"),
       checkboxInput("SuperviseAnyone",
                     "Supervise someone in the workplace"),
       checkboxInput("HHMemberTradeUnion",
                     "You or household member belong to a trade union"),
       checkboxInput("HHMemberProfAssoc",
                     "You or household member belong to a professional association"),
       checkboxInput("IdentifyWorkingClass",
                     "Identify as working class"),
       checkboxInput("Student",
                     "Currently a student"),
       selectInput("HighestQual",
                   "Highest education / training qualification",
                   unique(nzes_imp$HighestQual),
                   selected = "University"),
       selectInput("Religion",
                   "Religion",
                   unique(nzes_imp$Religion),
                   selected = "No religion"),
       checkboxInput("City",
                     "Live in a city",
                     TRUE),
       checkboxInput("Male",
                     "Male"),
       checkboxInput("Marital",
                     "Married or de facto"),
       checkboxInput("European",
                    "European ethnicity",
                    value = TRUE),
       checkboxInput("Maori",
                     "Maori ethnicity"),
       checkboxInput("NZBorn",
                     "Born in New Zealand",
                     TRUE)
       
       
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       ggvisOutput("distPlot"),
       sliderInput("dage",
                   "Age:",
                   min = 18,
                   max = 99,
                   value = 45),
       selectInput("HHIncome",
                   "Household income",
                   unique(nzes_imp$HHIncome),
                   selected = "Lower"),
       checkboxInput("OwnHouseOrFlat",
                     "Own the house or flat you live in")
       
       
    )
  )
))
