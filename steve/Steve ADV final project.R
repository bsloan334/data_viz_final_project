library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

# Set the path to the dataset
data_path <- "C:/Users/samex/Desktop/ND Grad School/ADV Fall 23/code_enforcement.csv"

# Read the dataset
code_enforcement <- read.csv(data_path, stringsAsFactors = FALSE)

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
ui <- fluidPage(
  theme = shinytheme("superhero"), # Apply the Superhero theme
  tags$head(tags$style(HTML("
    body, label, input, button, select, p, h1, h2, h3, h4, h5 {
      font-size: 18px !important;
    }
    .shiny-output-error { font-size: 18px !important; }
    .shiny-output-error:before { content: none !important; }
    .shiny-plot-output text { font-size: 18px !important; }
    h2 { font-size: 32px !important; } /* Increase the size for the title */
  "))),
  
  # Title
  
  titlePanel("South Bend in Focus", windowTitle = "South Bend in Focus"), # Page title 
  
  # Descriptive paragraphs for the dashboard
  h3("Overview"),
 
  p("Violations By The Numbers: This display shows a variety of data visualizations for property violation trends in South Bend, Indiana. Data sources come from the City of South Bend's Code Enforcement Cases over time and by type of property violation. Code violation outcomes and trends visualize resource allocation requirements. The data presented allows for the identification of patterns and peak periods of code violations, which can inform strategic planning for enforcement and preventive measures."), 
    
  p("By analyzing the breakdown of resolutions carried out by the city versus private entities, decision-makers can infer the effectiveness of current policies and community engagement. Allocation of resources can be optimized based on these insights to target high-violation areas or times of the year, ensuring that efforts are proactive rather than reactive. Understanding these trends is crucial for city planning, budgeting for enforcement activities, and fostering public-private partnerships that address the underlying causes of property violations."),
  
  
  # Selector for a three-year block of data
  selectInput("yearBlock", "Historical Blocks & Current Year View:",
              choices = seq(from = 2013, to = max(format(as.Date(code_enforcement_cleaned$Date_of_Last_Compliance), "%Y")), by = 3)),
  
  # Plot for Violations Over Time
  plotOutput("violations_over_time"),
  
  # Plots for Violations by Type and by Entity with headers
  h3("Violations by Type"),
  plotOutput("violations_by_type"),
  
  h3("Violations by Entity Completing the Compliance"),
  plotOutput("violations_by_completed_by")
)

# Server function to create the plots
server <- function(input, output) {
  
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
      scale_fill_manual(values = c("Demolished" = "skyblue", "Repaired" = "lightcoral", "Deconstructed" = "plum", "Unknown" = "grey"),
                        labels = c("Demolished", "Repaired", "Deconstructed", "Unknown")) +
      theme_minimal() +
      theme(legend.title = element_text(size = 18), legend.text = element_text(size = 16)) + # Increase legend text size
      labs(title = "Violations by Type", x = "Type of Compliance", y = "Count")
  })
  
  # Plot for Violations by Entity Completing the Compliance with cleaned names
  output$violations_by_completed_by <- renderPlot({
    ggplot(filtered_data(), aes(x = Last_Compliance_Completed_By, fill = Last_Compliance_Completed_By)) +
      geom_bar() +
      scale_fill_manual(values = c("City" = "lightcoral", "Private" = "skyblue", "Unknown" = "grey"),
                        labels = c("City", "Private", "Unknown")) +
      theme_minimal() +
      theme(legend.title = element_text(size = 18), legend.text = element_text(size = 16)) + # Increase legend text size
      labs(title = "Violations by Entity Completing the Compliance", x = "Entity", y = "Count")
  })
  
}

# Run the application
shinyApp(ui = ui, server = server) 
