#
# SOUTH BEND IN FOCUS
# Addressing the City's Concerns and Code Violations
#
# Advanced Data Visualization
# Section West 3 Final Project
# Luke Bohenek, Bert Sloan, Steve Stull, Gabe Tauro
# 12/13/2023
#
# https://gtauro.shinyapps.io/ADV_Final_West3/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(plotly)
library(leaflet)
library(sf)

#### INDIVIDUAL TAB SETTINGS ####

#### INTRO TAB ####

# UI with custom CSS to increase font size
ui_intro <- fluidPage(
  # Include the logo image
  imageOutput("image", height = "150px"),
  
  # Descriptive paragraphs for the dashboard
  h3("Dashboard Overview"),
  p("This tool tracks and analyzes violation trends in South Bend, Indiana. \
    Data sources come from the City of South Bend's 311 Calls, Code Enforcement Cases, \
    City Council Data, and Public Facilities datasets to provide insights into city complaint \
    and violation patterns, identify areas of concern, and inform violation prevention \
    strategies.",
    br(), br(),
    "• ", strong("Building Code Violations"), ": These could involve issues with the structural integrity of buildings, unsafe conditions, failure to comply with construction permits, or not adhering to the local building codes and regulations.",
    br(), br(),
    "• ", strong("Health and Safety Violations"), ": Properties may be cited for violations that pose health and safety risks to occupants or the public, such as poor sanitation, presence of hazardous materials, or pest infestations.",
    br(), br(),
    "• ", strong("Maintenance Violations"), ": These might include neglected property maintenance, such as broken windows, dilapidated structures, overgrown yards, or accumulation of trash which could also be indicative of vacancy or abandonment.",
    br(), br(),
    "• ", strong("Zoning and Land Use Violations"), ": This could encompass unauthorized uses of property, such as operating a business in a residentially zoned area, or making modifications to land or structures without proper approvals.",
    br(), br(),
    "• ", strong("Environmental Violations"), ": Issues that affect the environment, such as improper disposal of waste, illegal dumping, or other actions that could harm the ecosystem.")
  
)

# Wrap the second application in a tabPanel
intro <- tabPanel("Introduction", ui_intro)

#### STEVE | TAB 1: "Violations by the Numbers" ####

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
  
  h3("Overview"),
  
  p("This tab shows a variety of data visualizations for property violation trends in South Bend, Indiana. Data sources come from the City of South Bend's Code Enforcement Cases over time and by type of property violation. Code violation outcomes and trends visualize resource allocation requirements. The data presented allows for the identification of patterns and peak periods of code violations, which can inform strategic planning for enforcement and preventive measures.",
    br(),
    br(),
    "By analyzing the breakdown of resolutions carried out by the city versus private entities, decision-makers can infer the effectiveness of current policies and community engagement. Allocation of resources can be optimized based on these insights to target high-violation areas or times of the year, ensuring that efforts are proactive rather than reactive. Understanding these trends is crucial for city planning, budgeting for enforcement activities, and fostering public-private partnerships that address the underlying causes of property violations."), 
  
  sidebarLayout(
    
    sidebarPanel(
      # Selector for a three-year block of data
      selectInput("yearBlock", "Historical Blocks & Current Year View:",
                  choices = setNames(seq(from = 2013, to = max(format(as.Date(code_enforcement_cleaned$Date_of_Last_Compliance), "%Y")), by = 3),
                                     c("2013-2015", "2016-2018", "2019-2021", "2022"))),
    ),
    
    mainPanel(
      # Plot for Violations Over Time
      h3("Violations Over Time"),
      plotOutput("violations_over_time"),
      
      # Plots for Violations by Type and by Entity with headers
      h3("Violations by Type"),
      plotOutput("violations_by_type"),
      
      h3("Violations by Entity Completing the Compliance"),
      plotOutput("violations_by_completed_by")
    )
  )
  
)

# Wrap the second application in a tabPanel
tab1 <- tabPanel("Violations by the Numbers", ui_tab1)

#### BERT | TAB 2: "Looking at Violations Spatially" ####

# Reading in the datasets
parks <- read.csv("Parks_Locations_and_Features.csv")
pub_fac <- read.csv("Public_Facilities.csv")
code_enf <- read.csv("code_enforcement.csv")

# Converting into spatial data
parks_sp <- parks %>% st_as_sf(coords = c('Lon', 'Lat')) %>% st_set_crs(value = 4326)
pub_fac_sp <- pub_fac %>% st_as_sf(coords = c('Lon', 'Lat')) %>% st_set_crs(value = 4326)
code_enf_sp <- code_enf %>% drop_na() %>% st_as_sf(coords = c('X', 'Y')) %>% st_set_crs(value = 4326)

# Add color palette
# pal <- colorFactor(palette = 'BrBG', domain = code_enf_sp$Status)
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

# Misc cleanup section
# pub_fac_sp <- pub_fac_sp %>% unite('address' , c(POPL_ADDR1, POPL_CITY, POPL_STATE, POPL_ZIP), remove = FALSE, sep = ', ')


# Define UI elements
ui_tab2 <- fluidPage(

  h3("Overview"),
  p("This tab looks spatially at property code violations and nearby local landmarks in the city of South Bend \
    through mapping and proximity.",
    br(), br(),
    "Parks, landmarks, and various public facilities are denoted by blue markers, and Active and Closed property compliance cases are \
    marked with orange and purple dots respectively."),
  
  titlePanel("Code Enforcement Records near Public Landmarks"),
  
  # Leaflet output placeholder
  leafletOutput("map")
)

# Wrap the third application in a tabPanel
tab2 <- tabPanel("Looking at Violations Spatially", ui_tab2)

#### LUKE | TAB 3: "South Bend's Concerns" ####

# Load the 311_Phone_Call_Log_Mod data
call_data <- read.csv("311_Phone_Call_Log_Mod.csv", check.names = F, sep = ",")

#omitting NAs
call_data <- na.omit(call_data)

unique_departments <- unique(call_data$Department)

# Define UI for application
ui_tab3 <- fluidPage(
  theme = shinytheme("superhero"),

  h3("Overview"),
  p("This tab looks at 311 call data to understand what the community is concerned about. 
    It also looks at the calls the residents of South Bend spend the most time on and what departments might be the most strained. 
    It also sheds some light on calls South Bend get the most of, as time on the call doesn't necessarily represent a higher volume of issues.",
    
    br(),
    br(),
    
    "The histogram here shows how our call duration frequency is skewed right. There are a few significant outliers with some calls reaching over 50 minutes.
    Overall, most the calls are between 1-100 seconds.",
    
    br(),
    br(),
    
    "The Call Duration by Call Type graph shows a large number of categories of calls. There are two call types that are within the top 5 average durations that directly concern the Mayor.",
    
    br(),
    br(),
    
    "The average Call Time by Department helps us identify smaller call times that might be quicker questions to answer with an automated messaging system.
    Longer call durations for departments might be indicative of areas that might be strained with time spent on calls. There also could be areas of efficiency improvement."),
  
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

# Wrap the third application in a tabPanel
tab3 <- tabPanel("South Bend's Concerns", ui_tab3)

#### GABE | TAB 4: "What Comes Next? Our Strategies and Recommendations" ####

# Read in council districts
districts <- sf::st_read("City_Council_Districts/City_Council_Districts.shp",
                         layer = "City_Council_Districts")

# Make popups for districts
districts$popup = paste("District: <b>",districts$Dist,"</b><br> Council Member: <b>",
                        districts$Council_Me,"</b>",sep ="")

# Subset only district 3
dist3 <- subset(districts, districts$Dist %in% c("1303"))

# Identify majority ZIP codes in district 3
zips3 <- c("46601", "46613", "46615")

# Subset data from all mapping tab into distict 3
code_enf_sp3 <- subset(code_enf_sp, code_enf_sp$Zip %in% zips3)
parks_sp3 <- subset(parks_sp, parks_sp$Zip_Code %in% zips3)
pub_fac_sp3 <- subset(pub_fac_sp, pub_fac_sp$POPL_ZIP %in% zips3)

# Generate code enforcement data from only 2019-2021
code_enf_20192022 <- code_enforcement_cleaned %>%
  filter(year(Date_of_Last_Compliance) >= 2019 & year(Date_of_Last_Compliance) < 2022) %>%
  mutate(Month = floor_date(Date_of_Last_Compliance, "month"))  # Add a Month column floored to the month

# Define UI for application
ui_tab4 <- fluidPage(
  theme = shinytheme("superhero"),
  
  h3("Overview"),
  p("What comes next? This tab provides our recommendations to help address the mounting concerns and property violations of the city of South Bend. \
  It takes into account the datasets from all tabs in this tool provides potential next steps for the city to consider in keeping the vibrant \
    South Bend community vibrant yet well-maintained."),
  

  # Generate tabs for all 3 recommendations
      tabsetPanel(
        tabPanel("Recommendation 1", 
                 h2("Evaluate city sentiment of citizens in southwest District 3 of South Bend (130312)."),
                 leafletOutput("rec1"),
                 p("Through investigating the various public facilities and facility code violations on a map, \
                 we are able to see that, at the southwest corner of South Bend's District 3, there is a large \
                 grouping of compliance-related property cases (a handful of which are still active). This would \
                 indicate that not only are there several historic cases of compliance code violations, but there \
                 are many ongoing ones as well in this sector of the city.",
                   br(), br(),
                   "As such, it would be potentially fruitful to look into the sentiment of citizens in this voting district \
                   to consider their opinions of the area, why they believe there are so many compliance code violations, \
                   and whether or not they believe anything should be done about it.",
                   br(), br(),
                   "It is also worthwhile to note that this grouping of code violations in District 3 does not have too many \
                   public landmarks surrounding it in the dataset. While this could just be based on labeling methodology, \
                   it could point to a need for leisure or safety. These would be important variables to consider in \
                   retrieving the sentiment of citizens.")),
        
        tabPanel("Recommendation 2", 
                 h2("Investigate the efficiency of Parks-Maintenance and Public Works Calls."),
                 plotlyOutput("rec2"),
                 p("In examining the average call duration categories received by South Bend's 311 lines, it is \
                 readily apparent that two categories stand out above the rest - Parks-Maintenance and Public Works.",
                   br(), br(),
                   "To ensure that these calls are both meaningful and efficient, it would be worthwhile to further investigate \
                   what types of requests/concerns are being voiced, how frequently each concern is brought up, and what \
                   pathways are identified to go from call to action.")),
        
        tabPanel("Recommendation 3", 
                 h2("Further examine the ratio of Demolished to Repaired properties when evaluating compliance code violations."),
                 plotOutput("rec3"),
                 p("When considering the next steps for a property compliance code violation, one typically needs to make \
                 a decision as to whether or not the property should be repaired or demolished. This can come down to several \
                 factors - with economic/monetary value and building integrity being perhaps some of the biggest.",
                   br(), br(),
                   "To better understand this, it would be a good idea to further examine the ratio of properties year over year \
                   that are demolished versus repaired. Which private and/or public entities are being put in charge of these \
                   changes? How much say do voters get? And how often do voters get a say?",
                   br(), br(),
                   "While money is certainly important in maintaining a city's well-being, history, tradition, and culture often \
                   are important in maintaining a city's soul."))
      )


)

# Wrap the fourth application in a tabPanel
tab4 <- tabPanel("Our Strategies and Recommendations", ui_tab4)

#### GLOBAL APPLICATION SETTINGS ####

#### GLOBAL UI ####
ui <- navbarPage(
    theme = shinytheme("superhero"),
    
    # Application title
    title = "South Bend in Focus: Addressing the City’s Concerns and Code Violations",
    
    # Create tabs with the for navbar
    intro,
    tab1,
    tab2,
    tab3,
    tab4,
    
    footer = tags$head(tags$style(HTML("
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

    
)

#### GLOBAL SERVER ####

# Define server logic 
server <- function(input, output) {
  
  ##### INTRO
  
  # Render the image dynamically using renderImage
  output$image <- renderImage({
    list(src = "advbrand.png",
         alt = "South Bend in Focus",
         height = 150)
  }, deleteFile = FALSE)
  
  ##### STEVE | TAB 1 ####
  
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
      scale_fill_manual(values = c("Demolished" = "skyblue", "Repaired" = "lightcoral", "Deconstructed" = "plum", "Unknown" = "grey")) +
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
  
  #### BERT | TAB 2 ####
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = parks_sp, popup = ~popup) %>%
      addMarkers(data = pub_fac_sp, popup = ~popup) %>% 
      addCircleMarkers(data = code_enf_sp, popup = ~popup, color = ~pal(Status), stroke = 0, fillOpacity = 1, radius = 4) %>%
      addLegend("bottomright", pal = pal, values = code_enf_sp$Status, title = "Status")
  })
  #### LUKE | TAB 3 ####
  
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
  
  #### GABE | TAB 4 ####
  
  # Generate recommendation 1 output
  # Leaflet displaying district 3 landmarks and violations
  output$rec1 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = dist3, color = "lightcoral", dashArray = '5,10', 
                  weight = 3, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = "lightcoral",
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  popup = ~popup)%>%
      addMarkers(data = parks_sp3, popup = ~popup) %>%
      addMarkers(data = pub_fac_sp3, popup = ~popup) %>% 
      addCircleMarkers(data = code_enf_sp3, popup = ~popup, color = ~pal(Status), stroke = 0, fillOpacity = 1, radius = 4) %>%
      addLegend("bottomright", pal = pal, values = code_enf_sp$Status, title = "Status")
  })
  
  # Generate recommendation 2 output
  # Highlights top two average call times
  output$rec2 <- renderPlotly({
    avg_time_by_dept <- call_data %>%
      group_by(Department) %>%
      summarize(Avg_Duration = mean(duration_Seconds))
    
    
    avg_time_by_dept$color_code = ifelse(avg_time_by_dept$Avg_Duration > 110, 'lightcoral', 'gray')
    
    plot_ly(x = ~avg_time_by_dept$Department,
            y = ~avg_time_by_dept$Avg_Duration,
            type = 'bar', marker = list(color = avg_time_by_dept$color_code)) %>%
      layout(title = 'Average Call Time by Department',
             xaxis = list(title = 'Department'),
             yaxis = list(title = 'Average Duration (seconds)'),
             showlegend = FALSE)
  })
  
  # Generate recommendation 3 output
  # Looks at ratio of Demolished to Repaired
  output$rec3 <- renderPlot({
    ggplot(code_enf_20192022, aes(x = Last_Compliance_Type, fill = Last_Compliance_Type)) +
      geom_bar() +
      scale_fill_manual(values = c("Demolished" = "skyblue", "Repaired" = "lightcoral", "Deconstructed" = "plum", "Unknown" = "grey")) +
      theme_minimal() +
      theme(legend.title = element_text(size = 18), legend.text = element_text(size = 16)) + # Increase legend text size
      labs(title = "Violations by Type", x = "Type of Compliance", y = "Count")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
