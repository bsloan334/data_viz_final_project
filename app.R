# Load the necessary libraries
library(shiny)
library(leaflet)
library(tidyverse)
library(sf)

# Base file path
basepath <- "./Data-Viz-2023-Fall/FinalProject/"

# Reading in the datasets
parks <- read.csv(paste0(basepath,"Parks_Locations_and_Features.csv"))
pub_fac <- read.csv(paste0(basepath,"Public_Facilities.csv"))

# Converting into spatial data
parks_sp <- parks %>% st_as_sf(coords = c('Lon', 'Lat')) %>% st_set_crs(value = 4326)
pub_fac_sp <- pub_fac %>% st_as_sf(coords = c('Lon', 'Lat')) %>% st_set_crs(value = 4326)

# Misc cleanup section
# pub_fac_sp <- pub_fac_sp %>% unite('address' , c(POPL_ADDR1, POPL_CITY, POPL_STATE, POPL_ZIP), remove = FALSE, sep = ', ')

# Popup info
parks_sp$popup = paste("<b>",parks_sp$Park_Name,"</b><br>",
                       "Type: ",parks_sp$Park_Type,"<br>",
                       "Address: ",parks_sp$Address,sep ="")
pub_fac_sp$popup = paste("<b>",pub_fac_sp$POPL_NAME,"</b><br>",
                       "Type: ",pub_fac_sp$POPL_TYPE,"<br>",
                       "Address: ",pub_fac_sp$POPL_ADDR1,sep ="")

# Define UI elements
ui <- fluidPage(
  theme = shinythemes::shinytheme("superhero"), # Applying the superhero theme
  titlePanel("Basic Shiny App"),
  
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
      addMarkers(data = pub_fac_sp, popup = ~popup)
      # setView(lng = 0, lat = 0, zoom = 2) # Set initial view coordinates and zoom level
  })
}

# Create Shiny app object
shinyApp(ui = ui, server = server)
