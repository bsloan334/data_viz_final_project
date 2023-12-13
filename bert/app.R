# Load the necessary libraries
library(shiny)
library(leaflet)
library(tidyverse)
library(sf)

# using RData 
# load("example.RData")

# Base file path
basepath <- "./Data-Viz-2023-Fall/FinalProject/"

# Reading in the datasets
parks <- read.csv(paste0(basepath,"Parks_Locations_and_Features.csv"))
pub_fac <- read.csv(paste0(basepath,"Public_Facilities.csv"))
code_enf <- read.csv(paste0(basepath,"code_enforcement.csv"))

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

# Define UI elements
ui <- fluidPage(
  theme = shinythemes::shinytheme("superhero"), # Applying the superhero theme
  titlePanel("Code enforcement records near public landmarks"),
  
  # Leaflet output placeholder
  leafletOutput("map")
)

# Define server logic
server <- function(input, output) {
  # Render Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = parks_sp, popup = ~popup) %>%
      addMarkers(data = pub_fac_sp, popup = ~popup) %>% 
      addCircleMarkers(data = code_enf_sp, popup = ~popup, color = ~pal(Status), stroke = 0, fillOpacity = 1, radius = 4) %>%
      addLegend("bottomright", pal = pal, values = code_enf_sp$Status, title = "Status")
  })
}

# Create Shiny app object
shinyApp(ui = ui, server = server)
