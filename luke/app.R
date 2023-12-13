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

#create base path and then create a few paths that reference the specific CSVs

#omitting NAs
call_data <- na.omit(call_data)


# Define UI for application
ui <- fluidPage(
  theme = shinytheme("superhero"),
  h3("Tab Overview"),
  p("This tab looks at 311 call data to understand what the community is concerned about. 
    It also looks at the calls the residents of South Bend spend the most time on and what departments might be the most strained. 
    It also sheds some light on calls South Bend get the most of, as time on the call doesn't necessarily represent a higher volume of issues.
    
    The histogram here shows how our call duration frequency is skewed right. There are a few significant outliers with some calls reaching over 50 minutes.
    Overall, most the calls are between 1-100 seconds.
    
    The Call Duration by Call Type graph shows a large number of categories of calls. There are two call types that are within the top 5 average durations that directly concern the Mayor.
    
    The average Call Time by Department helps us identify smaller call times that might be quicker questions to answer with an automated messaging system.
    Longer call durations for departments might be indicative of areas that might be strained with time spent on calls. There also could be areas of efficiency improvement."),
  
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
             xaxis = list(title = 'Call Type'),
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

#Plot Takeaways

# The histogram here shows how our call duration frequency is skewed right. 
# The 