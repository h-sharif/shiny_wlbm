library(shiny)
library(tidyverse)
library(data.table)
library(sf)
library(leaflet)
library(leafem)
library(leaflet.extras)
library(bslib)
library(shinybusy)
library(shinyWidgets)
library(bsicons)
library(shinyalert)
library(xts)
library(dygraphs)
theme_set(theme_bw())

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
    # need to ensure at least one is selected
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
    ### Light experimential version: only five key outlets ----
    selectInput(
      inputId = "outlet_fromlist",
      label = "Point of interest:",
      choices = c(
        "SF-VTO", "SF-PM", "Estero Yerba Loca",
        "Rio Molina", "Rio Mapocho"
      ),
      selected = "Mapocho",
      multiple = FALSE
    ),
    ### `constituent` ------
    # need to ensure at least one is selected
    checkboxGroupButtons(
      inputId = "constituent", label = "Constituent:", selected = "SO4",
      choices = c("SO4", "Mn", "Cu"),
      checkIcon = list(
        yes = icon("ok", 
                   lib = "glyphicon"),
        no = icon("remove",
                  lib = "glyphicon")),
      status = "danger"
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
      #### `wshedmap` ----
      tags$style(type = "text/css",
                 "#wshedmap {height: calc(100vh - 150px) !important;}"),
      br(),
      leafletOutput("wshedmap")
    ),
    
    ### Discharge page ----------
    nav_panel(
      title = "Discharge",
      icon = bs_icon("water"),
      br(),
      layout_columns(
        #### `which_year_q` ----
        #### `monthly_q` ----
        card(
          card_header("Monthly Mean Series"),
          full_screen = TRUE,
          uiOutput("which_year_q"),
          plotOutput("monthly_q", height = "auto")
        ),
        #### `longterm_monthly_q` ----
        card(
          card_header("Long-term Mean Monthly Totals"),
          full_screen = TRUE,
          plotOutput("longterm_monthly_q")
        ),
        #### `annual_q` ----
        card(
          card_header("Annual Totals"),
          full_screen = TRUE,
          plotOutput("annual_q", height = "auto")
        ),
        col_widths = c(4, 4, 4),
        height = "calc(50vh - 150px)"
      ),
      #### `daily_q` ----
      card(
        card_header("Daily"),
        full_screen = TRUE,
        dygraphOutput("daily_q"),
        max_height = "calc(50vh - 30px)"
      )
    ),
    
    ### Concentration page ------
    nav_panel(
      title = "Concentration",
      icon = icon("vial", class = "fa-light"),
      br(),
      layout_columns(
        #### `which_year_conc` ----
        #### `monthly_conc` ----
        card(
          card_header("Monthly Mean Series"),
          uiOutput("which_year_conc"),
          full_screen = TRUE,
          plotOutput("monthly_conc")
        ),
        #### `longterm_monthly_conc` ----
        card(
          card_header("Long-term Monthly Mean"),
          full_screen = TRUE,
          plotOutput("longterm_monthly_conc")
        ),
        #### `annual_conc` ----
        card(
          card_header("Annual Mean"),
          full_screen = TRUE,
          plotOutput("annual_conc")
        ),
        col_widths = c(4, 4, 4),
        height = "calc(50vh - 150px)"
      ),
      #### `daily_conc` ----
      card(
        card_header("Daily"),
        full_screen = TRUE,
        dygraphOutput("daily_conc"),
        max_height = "calc(50vh - 30px)"
      )
        
    ),
    
    ### Load page ---------------
    nav_panel(
      title = "Load",
      icon = icon("weight-scale", class = "fa-regular"),
      br(),
      layout_columns(
        #### `which_year_load` ----
        #### `monthly_load` ----
        card(
          card_header("Monthly Total Series"),
          uiOutput("which_year_load"),
          full_screen = TRUE,
          plotOutput("monthly_load")
        ),
        #### `longterm_monthly_load` ----
        card(
          card_header("Long-term Mean Monthly Totals"),
          full_screen = TRUE,
          plotOutput("longterm_monthly_load")
        ),
        #### `annual_load` ----
        card(
          card_header("Annual Totals"),
          full_screen = TRUE,
          plotOutput("annual_load")
        ),
        col_widths = c(4, 4, 4),
        height = "calc(50vh -  150px)"
      ),
      #### `daily_load` ----
      card(
        card_header("Daily"),
        full_screen = TRUE,
        dygraphOutput("daily_load"),
        max_height = "calc(50vh - 30px)"
      )
      
    ),
    
    ### Dark/Light mode switch
    nav_spacer(),
    nav_menu(
      title = "Links",
      nav_item(
        a(icon("laptop-code"), "Model",
          href = "https://robertsongeoconsultants-my.sharepoint.com/:f:/g/personal/hsharif_robertsongeo_com/EprIH6RRI-FHrD-OnapmoMoBNIT1fM9EzIayX0pvBdG6hQ?e=4Rrdo9", 
          target = "_blank")
      ),
      nav_item(
        a(icon("database", class = "fa-light"), "Input Data",
          href = "https://robertsongeoconsultants-my.sharepoint.com/:f:/g/personal/hsharif_robertsongeo_com/Erjgk6VDr1VBmFLa0qtbhAABUE0LgdMZzeYvmOBlIrHdtA?e=KhltHQ",
          target = "_blank")
      ),
      nav_item(
        a(icon("cloud-sun", class = "fa-light"), "Climate & Hydrometric Data Report",
          href = "https://robertsongeoconsultants-my.sharepoint.com/:u:/g/personal/hsharif_robertsongeo_com/EVfIbMGGTslFmQ1mSt8AW8IBatPhMG9OWDRLoW2zIAnlcQ?e=IjtKuS",
          target = "_blank")
      ),
      nav_item(
        a(icon("html5", class = "fa-light"), "GR6J Performance Report",
          href = "https://robertsongeoconsultants-my.sharepoint.com/:u:/g/personal/hsharif_robertsongeo_com/Edg-45J71l9HuUnNIDNxSTEBpdXN1BGk5T6Rg9GtJ5ldhw?e=RjR0eY",
          target = "_blank")
      ),
      nav_item(
        a(icon("html5", class = "fa-light"), "WLBM Performance Report",
          href = "https://robertsongeoconsultants-my.sharepoint.com/:u:/g/personal/hsharif_robertsongeo_com/EZjlzttUZpZGr-3YKlU2MjkBQKkUNorMUMg82rypUzN5yg?e=rSFfSh",
          target = "_blank")
      )
    ),
    nav_item(
      input_dark_mode(id = "mode")
    )
  )
  
)


# Server -----------------------------------------------------------------------
server <- function(input, output, session) {
  source("tidy_data_extractor.R")
  shinyalert(
    title = 'Welcome to the Dashboard!',
    text = "This tool presents the results of the coupled GR6J and WLBM model for the Los Bronces mine, covering baseline condition and three remediation scenarios. You can easily compare these scenarios to support informed decision-making by exploring discharge, concentrations, and loads of key constituents (SO4, Cu, and Mn) across different locations and scenarios.",
    type = "",
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE
  )
  
  ## Map & Spatial data --------------------------------------------------------
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
    st_transform(4326) %>%
    arrange(SubbasinID) %>%
    mutate(
      outlet_name = c(
        "El Plomo", "Dolores", "Planta Molienda Bronces", 
        "Planta Molienda Confluencia", "Muro Cortafugas",
        "Confluence Area", "SF-VTO", "Valenzuela Intake",
        "Upstream SF Tunnel", "Los Pitches Intake", "Perez Caldera Tailings",
        "SF2400 Dren Sump", "Hotel Intake", "SF-PM", "Rio San Francisco",
        "Estero Yerba Loca", "Rio Molina", "Rio Mapocho"
      )
    )
  labels_wsheds <- sprintf(
    "<strong>Sub-Catchment Name</strong>: %s<br/><strong>Area (km<sup>2</sup>)</strong>: %.1f<br/>",
    wsheds$outlet_name, wsheds$area_km2) %>%
    lapply(htmltools::HTML)
  df_id <- wsheds %>%
    st_drop_geometry() %>%
    dplyr::select(c(SubbasinID, outlet_name))

  output$wshedmap <- renderLeaflet({
    leaflet() %>%
      setView(lng = -70.3455308, lat = -33.3358337, zoom = 12) %>%
      addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",
               group = 'Google Map',
               options = tileOptions(opacity = 0.95)) %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       group = 'ESRI World Imagery',
                       options = tileOptions(opacity = 0.95)) %>%
      addScaleBar(position = "bottomleft") %>%
      addFullscreenControl(position = "bottomleft") %>%
      addMiniMap() %>%
      addLayersControl(
        baseGroups = c("Google Sattelite", "ESRI World Imagery"),
        overlayGroups = c("Stream Network", "Watersheds", 
                          "Model Nodes"),
        options = layersControlOptions(collapsed = FALSE),
        position = "bottomright"
      ) %>%
      addPolygons(data = wsheds,
                  group = "Watersheds",
                  label = labels_wsheds,
                  popup = labels_wsheds,
                  color = "#13007b",
                  fillColor = "#8dc0f1",
                  fillOpacity = 0.1,
                  weight = 2,
                  opacity = 1,
                  highlightOptions = highlightOptions(
                    weight = 2,
                    color = "#e9f50c",
                    opacity = 1,
                    bringToFront = TRUE,
                  )) %>%
      addPolylines(data = snet,
                   weight = ~ strahlr_weight,
                   color = "#1191ed",
                   opacity = 0.9,
                   group = "Stream Network") %>%
      addMarkers(data = nodes,
                 group = "Model Nodes",
                 label = ~Name,
                 icon = icon("box-open", class = "fa-light")) %>%
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
  
  # Need to make sure that user pushed the Fetch Data button
  # 01-Reactive ----
  fetched_data <- reactiveValues(df = NULL)
  
  # 01-ObserveEvent: Getting data ----
  observeEvent(input$get_data, {
    fetched_data$df <- df_retriver()
    if (is.null(fetched_data)) {
      showNotification("Data doesn't exist here, sorry!")
    }
  }, priority = 0)
  
  # 02-Reactive ----
  # only related to concentrations and loads
  processed_data <- reactiveValues(df = NULL)
  
  # 01-Observer: Adjusting data based on dashboard input ----
  observe({
    req(fetched_data$df, input$scenario, input$constituent)
    processed_data$df <- fetched_data$df %>%
      dplyr::filter(Station == input$outlet_fromlist,
                    Date >= input$date_range[1],
                    Date <= input$date_range[2]) %>%
      dplyr::filter(Scenario %in% input$scenario,
                    Constituent %in% input$constituent)
  }, priority = 1)
  
  # Discharge plots ------------------------------------------------------------
  output$which_year_q <- renderUI({
    sliderInput("selected_year_q", "Year:", 
                min = year(input$date_range[1]),
                max = year(input$date_range[2]),
                round = TRUE, step = 1, width = "100%",
                value = round(mean(year(input$date_range[1]),
                                   year(input$date_range[2]))))
  })
  
  observe({
    req(fetched_data$df, input$selected_year_q)
    df_q_list <- df_calc_discharge(
      fetched_data$df %>%
        dplyr::filter(Station == input$outlet_fromlist,
                      Date >= input$date_range[1],
                      Date <= input$date_range[2])
    )

    output$longterm_monthly_q <- renderPlot({
      df_q_list$longterm_monthly %>%
        ggplot(aes(x = Month, y = Monthly_Mean_Total)) +
          geom_col(fill = "blue3", color = "black", linewidth = 0.4) +
          labs(x = "", y = "Q (mm/month)")
    }, res = 100)
    
    output$monthly_q <- renderPlot({
      df_q_list$monthly %>%
        dplyr::filter(Year == input$selected_year_q) %>%
        mutate(Month = ordered(Month, levels = month.abb)) %>%
        ggplot(aes(x = Month, y = Monthly_Mean)) +
        geom_col(fill = "blue3", color = "black", linewidth = 0.4) +
        labs(x = "",
             y = expression("Q"~"("*m^3*"/s)"))
      
    }, res = 100)
    
    output$annual_q <- renderPlot({
      df_q_list$annual %>%
        ggplot(aes(x = Year, y = Annual_Total)) +
        geom_point(color = "royalblue", shape = 16, size = 1.5) +
        geom_line(color = "black", linewidth = 0.3) +
        labs(x = "",
             y = "Q (mm/year)",
             subtitle = paste("Long-term Mean Annual:",
                              round(df_q_list$annual$Annual_Total, digits = 2),
                              "(mm/year)"))
      
    }, res = 100)
    
    output$daily_q <- renderDygraph({
      xts_obj <- xts(x = df_q_list$daily$Simulated,
                     order.by = df_q_list$daily$Date)
      dygraph(xts_obj,
              ylab = "Discharge (cms)") %>%
        dySeries("V1", label = "Discharge (cms)",
                color = "blue") %>%
        dyRangeSelector()
    })

  }, priority = 1)
  
  
  
  # Concentration plots --------------------------------------------------------
  output$which_year_conc <- renderUI({
    sliderInput("selected_year_conc", "Year:", 
                min = year(input$date_range[1]),
                max = year(input$date_range[2]),
                round = TRUE, step = 1, width = "100%",
                value = round(mean(year(input$date_range[1]),
                                   year(input$date_range[2]))))
  })
  
  observe({
    req(processed_data$df, input$selected_year_conc)
    df_conc_list <- df_calc_conc(
      processed_data$df
    )
    
    output$longterm_monthly_conc <- renderPlot({
      df_conc_list$longterm_monthly %>%
        mutate(Scenario = ifelse(Scenario == "Base", "Base Case",
                                 paste("Scenario", Scenario))) %>%
        ggplot(aes(x = Month, y = Monthly_Mean)) +
        geom_col(fill = "orange2", color = "black", linewidth = 0.4) +
        facet_grid(Constituent ~ Scenario, scales = "free_y") +
        labs(x = "", y = "Concentration (mg/L)")
    }, res = 100)
    
    output$monthly_conc <- renderPlot({
      df_conc_list$monthly %>%
        mutate(Scenario = ifelse(Scenario == "Base", "Base Case",
                                 paste("Scenario", Scenario))) %>%
        dplyr::filter(Year == input$selected_year_conc) %>%
        mutate(Month = ordered(Month, levels = month.abb)) %>%
        ggplot(aes(x = Month, y = Monthly_Mean)) +
        geom_col(fill = "orange2", color = "black", linewidth = 0.4) +
        facet_grid(Constituent ~ Scenario, scales = "free_y") +
        labs(x = "",
             y = "Concentration (mg/L)")
      
    }, res = 100)
    
    output$annual_conc <- renderPlot({
      df_conc_list$annual %>%
        mutate(Scenario = ifelse(Scenario == "Base", "Base Case",
                                 paste("Scenario", Scenario))) %>%
        ggplot(aes(x = Year, y = Annual_Mean)) +
        geom_point(color = "orange2", shape = 16, size = 1.5) +
        geom_line(color = "black", linewidth = 0.3) +
        facet_grid(Constituent ~ Scenario, scales = "free_y") +
        labs(x = "",
             y = "Concentration (mg/L)",
             subtitle = paste("Long-term Mean Annual:",
                              round(df_conc_list$annual$Annual_Mean, digits = 2),
                              "(mg/L)"))
      
    }, res = 100)
    
    output$daily_conc <- renderDygraph({
      df_iter <- df_conc_list$daily %>%
        dplyr::select(c(Date, Constituent, Scenario, Simulated)) %>%
      pivot_wider(names_from = c(Constituent, Scenario),
                  values_from = Simulated,
                  names_sep = "-")
      
      xts_obj <- xts(x = df_iter[, c(2:ncol(df_iter))],
                     order.by = df_iter$Date)
      dygraph(xts_obj,
              ylab = "Concentration (mg/L)") %>%
        dyRangeSelector()
    })
    
  }, priority = 2)
  
  
  # Load plots -----------------------------------------------------------------
  output$which_year_load <- renderUI({
    sliderInput("selected_year_load", "Year:", 
                min = year(input$date_range[1]),
                max = year(input$date_range[2]),
                round = TRUE, step = 1, width = "100%",
                value = round(mean(year(input$date_range[1]),
                                   year(input$date_range[2]))))
  })
  
  observe({
    req(processed_data$df, input$selected_year_load)
    df_load_list <- df_calc_load(
      processed_data$df
    )
    
    output$longterm_monthly_load <- renderPlot({
      df_load_list$longterm_monthly %>%
        mutate(Scenario = ifelse(Scenario == "Base", "Base Case",
                                 paste("Scenario", Scenario)),
               Monthly_Mean_Total = Monthly_Mean_Total / 1000) %>%
        ggplot(aes(x = Month, y = Monthly_Mean_Total)) +
        geom_col(fill = "snow4", color = "black", linewidth = 0.4) +
        facet_grid(Constituent ~ Scenario, scales = "free_y") +
        labs(x = "", y = "Load (tonnes/month)")
    }, res = 100)
    
    output$monthly_load <- renderPlot({
      df_load_list$monthly %>%
        mutate(Scenario = ifelse(Scenario == "Base", "Base Case",
                                 paste("Scenario", Scenario)),
               Monthly_Total = Monthly_Total / 1000) %>%
        dplyr::filter(Year == input$selected_year_load) %>%
        mutate(Month = ordered(Month, levels = month.abb)) %>%
        ggplot(aes(x = Month, y = Monthly_Total)) +
        geom_col(fill = "snow4", color = "black", linewidth = 0.4) +
        facet_grid(Constituent ~ Scenario, scales = "free_y") +
        labs(x = "",
             y = "Load (tonnes/month)")
      
    }, res = 100)
    
    output$annual_load <- renderPlot({
      df_load_list$annual %>%
        mutate(Scenario = ifelse(Scenario == "Base", "Base Case",
                                 paste("Scenario", Scenario)),
               Annual_Total = Annual_Total / 1000) %>%
        ggplot(aes(x = Year, y = Annual_Total)) +
        geom_point(color = "snow4", shape = 16, size = 1.5) +
        geom_line(color = "black", linewidth = 0.3) +
        facet_grid(Constituent ~ Scenario, scales = "free_y") +
        labs(x = "",
             y = "Load (tonnes/year)",
             subtitle = paste("Long-term Mean Annual:",
                              round(df_load_list$annual$Annual_Total, digits = 2),
                              "(tonnes/year)"))
      
    }, res = 100)
    
    output$daily_load <- renderDygraph({
      df_iter <- df_load_list$daily %>%
        dplyr::select(c(Date, Constituent, Scenario, Simulated)) %>%
        pivot_wider(names_from = c(Constituent, Scenario),
                    values_from = Simulated,
                    names_sep = "-")
      
      xts_obj <- xts(x = df_iter[, c(2:ncol(df_iter))],
                     order.by = df_iter$Date)
      dygraph(xts_obj,
              ylab = "Load (kg/day)") %>%
        dyRangeSelector()
    })
    
  }, priority = 2) 
}

shinyApp(ui, server)











