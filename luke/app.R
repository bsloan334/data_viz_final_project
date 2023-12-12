#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#setwd("~/Notre Dame/Data Science/Semester 4/Advanced Data Visualization/Final Project")

library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(plotly)

# Load the 311_Phone_Call_Log_Mod data
#call_data <- read.csv("311_Phone_Call_Log_Mod.csv", check.names = F, sep = ",")

#omitting NAs
call_data <- na.omit(call_data)

unique_departments <- unique(call_data$Department)
cat("Unique Departments:", paste(unique_departments, collapse = ", "), "\n")



# Define UI for application
ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("311 Phone Call Log Data"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("durationRange", "Select Duration Range for 'Call Duration' Histogram:",
                  min = 0, max = max(call_data$duration_Seconds),
                  value = c(0, max(call_data$duration_Seconds))),
      sliderInput("topN", "Select Number of 'Call Types' to Display", value = 10, min = 1, max = 50)
    ),
    
    mainPanel(
      plotOutput("distPlot"),
      plotlyOutput("avgTimeByCalledAboutPlot"),
      plotlyOutput("avgTimeByDepartmentPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Histogram plot
  output$distPlot <- renderPlot({
    # Filter data based on the selected duration range
    filtered_data <- call_data %>%
      filter(duration_Seconds >= input$durationRange[1] & duration_Seconds <= input$durationRange[2])
    
    bins <- 50
    
    hist(filtered_data$duration_Seconds, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Duration (in seconds)',
         main = 'Call Duration', las = 2)
  })
  
  # Interactive bar plot of average call time by Called_About
  output$avgTimeByCalledAboutPlot <- renderPlotly({
    avg_time_by_called_about <- call_data %>%
      group_by(Called_About) %>%
      summarize(Avg_Duration = mean(duration_Seconds))
    
    # select top N categories
    top_n_categories <- input$topN
    top_called_about <- avg_time_by_called_about %>%
      top_n(top_n_categories, wt = Avg_Duration)
    
    plot_ly(x = ~top_called_about$Called_About,
            y = ~top_called_about$Avg_Duration,
            type = 'bar', marker = list(color = 'skyblue')) %>%
      layout(title = 'Call Duration by Call Type',
             xaxis = list(title = 'Called Type'),
             yaxis = list(title = 'Average Duration (seconds)'),
             showlegend = FALSE)
  })
  
  # Interactive bar plot of average call time by Department
  output$avgTimeByDepartmentPlot <- renderPlotly({
    avg_time_by_dept <- call_data %>%
      group_by(Department) %>%
      summarize(Avg_Duration = mean(duration_Seconds))
    
    
    plot_ly(x = ~avg_time_by_dept$Department,
            y = ~avg_time_by_dept$Avg_Duration,
            type = 'bar', marker = list(color = 'lightcoral')) %>%
      layout(title = 'Average Call Time by Department',
             xaxis = list(title = 'Department'),
             yaxis = list(title = 'Average Duration (seconds)'),
             showlegend = FALSE)
  })
}


# Run the application
shinyApp(ui = ui, server = server)