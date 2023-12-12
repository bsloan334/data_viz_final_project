#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("superhero"),
    
    # Application title
    titlePanel("South Bend in Focus: Addressing the City’s Concerns and Code Violations"),
    

    # Main panel for displaying outputs ----
    mainPanel(
        
        # Output: Tabset w/ plot, summary, and table ----
        tabsetPanel(type = "tabs",
                    tabPanel("Violations By The Numbers", plotOutput("plot")),
                    tabPanel("Looking at Violations Spatially", verbatimTextOutput("summary")),
                    tabPanel("South Bend’s Concerns", tableOutput("table")),
                    tabPanel("What Comes Next? Our Strategies and Recommendations", tableOutput("table"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
