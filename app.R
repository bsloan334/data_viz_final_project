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
library(lubridate)
library(plotly)
library(sf)

# using RData 
# load("example.RData")

# Base file path
# basepath <- "./Data-Viz-2023-Fall/FinalProject/"

##### INDIVIDUAL TAB SETTINGS

### INTRO TAB

# UI with custom CSS to increase font size
ui_intro <- fluidPage(
  # Include the logo image
  imageOutput("image", height = "150px"),
  
  # Descriptive paragraphs for the dashboard
  h3("Dashboard Overview"),
  p("This tool tracks and analyzes violation trends in South Bend, Indiana. Data sources come from the City of South Bend's 311 Calls, Code Enforcement Cases, Census Data, and Public Facilities datasets to provide insights into city complaint and violation patterns, identify areas of concern, and inform violation prevention strategies."),
  
)

# Wrap the second application in a tabPanel
intro <- tabPanel("Introduction", ui_intro)


### TAB 1: "Violations by the Numbers"

# Read the dataset
code_enforcement <- read.csv("code_enforcement.csv", stringsAsFactors = FALSE)

# Convert 'Date_of_Last_Compliance' to Date object and handle other cleaning
code_enforcement$Date_of_Last_Compliance <- as.Date(sub(" .*", "", code_enforcement$Date_of_Last_Compliance))

# Keep records even if some of the other columns have NAs,
# Selectively remove NAs only from 'Date_of_Last_Compliance'
code_enforcement <- code_enforcement[!is.na(code_enforcement$Date_of_Last_Compliance), ]

# Select the required columns
code_enforcement_cleaned <- code_enforcement %>%
  select(County_ID, Date_of_Last_Compliance, Last_Compliance_Type, Last_Compliance_Completed_By)

# Replace NA values in 'Last_Compliance_Type' and 'Last_Compliance_Completed_By' with "Unknown"
code_enforcement_cleaned$Last_Compliance_Type[is.na(code_enforcement_cleaned$Last_Compliance_Type)] <- "Unknown"
code_enforcement_cleaned$Last_Compliance_Completed_By[is.na(code_enforcement_cleaned$Last_Compliance_Completed_By)] <- "Unknown"

# Group the data by Date_of_Last_Compliance and count the number of County_IDs (violations)
violations_by_date <- code_enforcement_cleaned %>%
  group_by(Date_of_Last_Compliance) %>%
  summarise(count = n())

# UI with custom CSS to increase font size
ui_tab1 <- fluidPage(
  theme = shinytheme("superhero"), # Apply the Superhero theme
  
  h3("Tab Overview"),
  p("This tab shows a variety of charts and graphs for property violation trends over time and by type of property violation. Code violation outcomes and trends visualize resource allocation requirements."),
  
  # Selector for a three-year block of data
  selectInput("yearBlock", "Select Year Block:",
              choices = seq(from = 2013, to = max(format(as.Date(code_enforcement_cleaned$Date_of_Last_Compliance), "%Y")), by = 3)),
  
  # Plot for Violations Over Time
  plotOutput("violations_over_time"),
  
  # Plots for Violations by Type and by Entity with headers
  h3("Violations by Type"),
  plotOutput("violations_by_type"),
  
  h3("Violations by Entity Completing the Compliance"),
  plotOutput("violations_by_completed_by")
)

# Wrap the second application in a tabPanel
tab1 <- tabPanel("Violations by the Numbers", ui_tab1)

### TAB 2: "Looking at Violations Spatially"

# Reading in the datasets
parks <- read.csv("Parks_Locations_and_Features.csv")
pub_fac <- read.csv("Public_Facilities.csv")
code_enf <- read.csv("code_enforcement.csv")

# Converting into spatial data
parks_sp <- parks %>% st_as_sf(coords = c('Lon', 'Lat')) %>% st_set_crs(value = 4326)
pub_fac_sp <- pub_fac %>% st_as_sf(coords = c('Lon', 'Lat')) %>% st_set_crs(value = 4326)
code_enf_sp <- code_enf %>% drop_na() %>% st_as_sf(coords = c('X', 'Y')) %>% st_set_crs(value = 4326)

# Add color palette
pal <- colorFactor(palette = 'PuOr', domain = code_enf_sp$Status)

# Popup info
parks_sp$popup = paste("<b>",parks_sp$Park_Name,"</b><br>",
                       "Type: ",parks_sp$Park_Type,"<br>",
                       "Address: ",parks_sp$Address,sep ="")
pub_fac_sp$popup = paste("<b>",pub_fac_sp$POPL_NAME,"</b><br>",
                         "Type: ",pub_fac_sp$POPL_TYPE,"<br>",
                         "Address: ",pub_fac_sp$POPL_ADDR1,sep ="")
code_enf_sp$popup = paste("<b>",code_enf_sp$Full_Address,"</b><br>",
                          "Status: ",code_enf_sp$Status,"<br>",
                          "Code Enforcement Order: ",code_enf_sp$Code_Enforcement_Order,sep ="")

# Define UI elements
ui_tab2 <- fluidPage(

  h3("Tab Overview"),
  p("This tab looks spatially at property code violations and nearby local landmarks through mapping and proximity."),
  
  # Leaflet output placeholder
  leafletOutput("map")
)

# Wrap the third application in a tabPanel
tab2 <- tabPanel("Looking at Violations Spatially", ui_tab2)

### TAB 3: "South Bend's Concerns"

# Load the 311_Phone_Call_Log_Mod data
call_data <- read.csv("311_Phone_Call_Log_Mod.csv", check.names = F, sep = ",")

#omitting NAs
call_data <- na.omit(call_data)

unique_departments <- unique(call_data$Department)

# Define UI for application
ui_tab3 <- fluidPage(
  theme = shinytheme("superhero"),

  h3("Tab Overview"),
  p("This tab looks at 311 call data to understand what the community is concerned about. It also looks at the calls the residents of South Bend spend the most time on and what departments might be the most strained. It also sheds some light on calls South Bend get the most of, as time on the call doesn't necessarily represent a higher volume of issues."),
  
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

# Wrap the first application in a tabPanel
tab3 <- tabPanel("South Bend's Concerns", ui_tab3)

### TAB 4: "What Comes Next? Our Strategies and Recommendations"

##### GLOBAL APPLICATION SETTINGS

ui <- fluidPage(
    theme = shinytheme("superhero"),
    
    # Application title
    titlePanel("South Bend in Focus: Addressing the City’s Concerns and Code Violations"),
    
    tags$head(tags$style(HTML("
    body, label, input, button, select, p, h1, h2, h3, h4, h5 {
      font-size: 18px !important;
    }
    .shiny-output-error { font-size: 18px !important; }
    .shiny-output-error:before { content: none !important; }
    .shiny-plot-output text { font-size: 18px !important; }
    h2 { font-size: 32px !important; } /* Increase the size for the title */
    p {
    background-color: rgba(0, 0, 0, 0.5); /* Semi-transparent white background */
    padding: 10px; /* Adjust padding as needed */
    border-radius: 10px; /* Optional: Add rounded corners */
    }
  "))),
    
    # Create tabs with the tabsetPanel function
    tabsetPanel(
      intro,
      tab1,
      tab2,
      tab3
    )
    
)

# Define server logic 
server <- function(input, output) {
  
  ##### INTRO
  
  # Render the image dynamically using renderImage
  output$image <- renderImage({
    list(src = "advbrand.png",
         alt = "South Bend in Focus")
  }, deleteFile = FALSE)
  
  ###### TAB 1
  
  # Filtered data based on the selected year block
  filtered_data <- reactive({
    startYear <- as.numeric(input$yearBlock)
    endYear <- startYear + 2
    code_enforcement_cleaned %>%
      filter(year(Date_of_Last_Compliance) >= startYear & year(Date_of_Last_Compliance) <= endYear) %>%
      mutate(Month = floor_date(Date_of_Last_Compliance, "month"))  # Add a Month column floored to the month
  })
  
  # Plot for Violations Over Time with filtered data
  output$violations_over_time <- renderPlot({
    ggplot(filtered_data(), aes(x = Month)) +
      geom_histogram(stat = "count", bins = 36) + # Set bins to 36 for 3 years of monthly data
      theme_minimal() +
      theme(text = element_text(size = 18)) + # Increase text size for plot elements
      labs(title = "Violation Counts Over Time", x = "Date of Last Compliance", y = "Number of Violations")
  })
  
  # Plot for Violations by Type with cleaned names
  output$violations_by_type <- renderPlot({
    ggplot(filtered_data(), aes(x = Last_Compliance_Type, fill = Last_Compliance_Type)) +
      geom_bar() +
      scale_fill_manual(values = c("Demolished" = "cyan", "Repaired" = "magenta", "Deconstructed" = "green", "Unknown" = "grey"),
                        labels = c("Demolished", "Repaired", "Deconstructed", "Unknown")) +
      theme_minimal() +
      theme(legend.title = element_text(size = 18), legend.text = element_text(size = 16)) + # Increase legend text size
      labs(title = "Violations by Type", x = "Type of Compliance", y = "Count")
  })
  
  # Plot for Violations by Entity Completing the Compliance with cleaned names
  output$violations_by_completed_by <- renderPlot({
    ggplot(filtered_data(), aes(x = Last_Compliance_Completed_By, fill = Last_Compliance_Completed_By)) +
      geom_bar() +
      scale_fill_manual(values = c("City" = "green", "Private" = "blue", "Unknown" = "grey"),
                        labels = c("City", "Private", "Unknown")) +
      theme_minimal() +
      theme(legend.title = element_text(size = 18), legend.text = element_text(size = 16)) + # Increase legend text size
      labs(title = "Violations by Entity Completing the Compliance", x = "Entity", y = "Count")
  })
  
  ##### TAB 2
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = parks_sp, popup = ~popup) %>%
      addMarkers(data = pub_fac_sp, popup = ~popup) %>% 
      addCircleMarkers(data = code_enf_sp, popup = ~popup, color = ~pal(Status), stroke = 0, fillOpacity = 1, radius = 4) %>%
      addLegend("bottomright", pal = pal, values = code_enf_sp$Status, title = "Status")
  })
  
  ##### TAB 3
  
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
