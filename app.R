library(shiny)
library(tidyverse)
library(data.table)
library(plotly)
library(sf)
library(leaflet)
library(leafem)
library(leaflet.extras)
library(bslib)
library(shinybusy)
library(shinyWidgets)
library(DT)
library(formattable)
library(gt)
library(readxl)
library(bsicons)
library(shinyalert)


# UI ---------------------------------------------------------------------------
ui <- page_sidebar(
  title = "Los Bronces Water & Load Balance Model",
  
  ## Sidebar ------------------
  sidebar = sidebar(
    a(href = 'http://www.rgc.ca',
      img(src = 'logo.png',
          title = "", height = "70px"),
      style = "padding-top:0px; padding-bottom:0px;"),
    helpText("Author: Hamed Sharif", br(), "Contact: hsharif@robertsongeo.com"),
    ### `get_data` ----
    span(
      actionButton("get_data", "Fetch Data",
                   icon = icon("database", class = "fa-light"), width = "90%"),
      div(style = "display:inline-block;",
          title = "Please push this button to read and prepare simulated discharge, concentrations and loads.",
          icon("info-circle")
      )
    ),
    ### `date_range` ----
    dateRangeInput(inputId = "date_range", 
                   label = "Date Interval of Interest (1988-2023):",
                   start = "1988-01-01", end = "2023-12-31", min = "1988-01-01",
                   max = "2023-12-31"),
    ### `scenario` ----
    checkboxGroupInput(
      inputId = "scenario", 
      label = "Select Scenarios:",
      choiceNames = c("Base case: No remediation",
                      "Scenario A: Fixing Confluencia Load Source (100%)",
                      "Scenario B: Fixing Perez Caldera (100%)",
                      "Scenario C: Applying both Scenario A and B"),
      choiceValues = c("Base", "A", "B", "C"),
      selected = c("Base")
    ),
    ### `outlet_fromlist` ----
    selectInput(
      inputId = "outlet_fromlist",
      label = "Point of interest:",
      choices = c(
        "El Plomo", "Dolores", "Tranque Plomito", "Planta Molienda Bronces",
        "Planta Molienda Confluencia", "Muro Cortafugas", "Confluence Area",
        "SF-VTO", "Upstream SF Tunnel", "Valenzuela Intake", 
        "Perez Caldera Tailings", "SF Tunnel", "Los Pitches Intake",
        "SF2400 Dren Sump", "Hotel Intake", "SF-PM", "Estero Yerba Loca",
        "Rio Molina", "Rio Mapocho"
      ),
      selected = "Rio Mapocho",
      multiple = FALSE
    ),
    ### Sidebar Settings 
    width = 350,
    open = "always"
  ),
  
  ## Navigation bar -----------
  navset_pill(
    ### Map page ----------
    nav_panel(
      title = "Watersheds",
      icon = icon("earth-americas", class = "fa-regular"),
      #### `wshed_map` ----
      tags$style(type = "text/css",
                 "#wshed_map {height: calc(100vh - 135px) !important;}"),
      leafletOutput("wshed_map")
    ),
    
    ### Discharge page ----------
    nav_panel(
      title = "Discharge",
      icon = bs_icon("water")
      
      
      
    ),
    
    ### Concentration page ------
    nav_panel(
      title = "Concentration",
      icon = icon("vial", class = "fa-light")
        
    ),
    
    ### Load page ---------------
    nav_panel(
      title = "Load",
      icon = icon("weight-scale", class = "fa-regular")
      
    ),
    
    ### Dark/Light mode switch
    nav_spacer(),
    nav_item(
      input_dark_mode(id = "mode")
    )
    
  )
  
)







# Server -----------------------------------------------------------------------
server <- function(input, output) {
  shinyalert(
    title = 'Welcome to the Dashboard!',
    text = "This tool presents the results of the coupled GR6J and WLBM model for the Los Bronces mine, covering baseline condition and three remediation scenarios. You can easily compare these scenarios to support informed decision-making by exploring discharge, concentrations, and loads of key constituents (SO4, Cu, and Mn) across different locations and scenarios.",
    type = "",
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE
  )
  
  # reading spatial data to prepare the map ------------------------------------
  # Name and id fields
  nodes <- st_read("spatial_data/model_nodes.shp") %>%
    st_transform(4326)
  snet <- st_read("spatial_data/Stream Network.shp") %>%
    st_transform(4326) %>%
    mutate(
      strahlr_weight = case_when(grid_code == 1 ~ 1,
                                 grid_code %in% c(2, 3) ~ 1.8,
                                 grid_code %in% c(4, 5) ~ 2.5,
                                 grid_code == 6 ~ 3.5)
    )
  wsheds <- st_read("spatial_data/Watersheds.shp") %>%
    st_transform(4326)
  
  output$wshed_map <- renderLeaflet({
    leaflet() %>%
      setView(lng = -70.3455308, lat = -33.3358337, zoom = 12) %>%
      addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",
               group = 'Google Map',
               options = tileOptions(opacity = 0.95)) %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       group = 'ERSI World Imagery',
                       options = tileOptions(opacity = 0.95)) %>%
      addScaleBar(position = "bottomleft") %>%
      addFullscreenControl(position = "bottomleft") %>%
      addLayersControl(
        baseGroups = c("Google Sattelite", "ERSI World Imagery"),
        overlayGroups = c("Stream Network", "Watersheds", 
                          "Model Nodes"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addPolygons(data = wsheds,
                  group = "Watersheds",
                  fill = FALSE,
                  color = "#212120",
                  weight = 2,
                  opacity = 1) %>%
      addPolylines(data = snet,
                   weight = ~ strahlr_weight,
                   color = "#1191ed",
                   opacity = 0.9,
                   group = "Stream Network") %>%
      addMarkers(data = nodes,
                 group = "Model Nodes",
                 label = ~Name,
                 icon = icon("box-open", class = "fa-light")) %>%
      addMiniMap() %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Zoom to Mining Area",
        onClick=JS("function(btn, map){map.setView([-33.3358337, -70.3455308], 12); }"))) %>%
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479")
  })
  
    
}

shinyApp(ui, server)











