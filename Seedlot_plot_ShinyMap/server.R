library(tidyverse) # data wrangling
library(raster) # Map
library(sf) # Map
library(shiny) # shiny base package
library(shinycssloaders) # load symbol
library(leaflet) # Map
library(shinyBS) # layout
library(waiter) # loading sign reactively
library(shinyjs) # shiny java
library(DT) # Render datatable
library(shinyWidgets) # Confirmation messages 

setwd("~/Uni/Doctorate/Samples/Seedlot_plot_data/")

# Load in datasets used
LoadedinData <- read.csv("data/final_seedloty_plot.csv")
# Load current projection data
maxent_MQuin <- raster("data/maxent_MQuin.tiff")
maxent_MR <- raster("data/maxent_MR.tiff")
maxent_intersection <- raster("data/maxent_intersection.tiff")
maxent_studyarea <- st_read("data/maxent_studyarea.gpkg")

# Palettes 
# For seedlots based on severity - actively shifts based on observed data
MR_Sev_pal <- colorNumeric(
  palette = "YlOrRd",  # Yellow → Orange → Red
  domain = LoadedinData$Mean_seedling_score_rustassay,
  na.color = "gray50"
)

# For SDMs
pal_mr <- colorRampPalette(c("grey95","#C6DC96","#C6DC96","darkolivegreen","yellow"))
pal_mq <- colorRampPalette(c("grey95","#EAC8BF","#EAC8BF","orange","forestgreen"))
pal_int <- colorRampPalette(c("grey95","#C6DC96","#C6DC96","#45B055","#4870BD", "deeppink4"))


############################################################################################

function(input, output, session) {
  
  
  ######################################## Seed lot map plots (renders 'map')
  ########### Set initial render
  # Track the first visit to the tab 
  first_visit <- reactiveVal(TRUE) 
  
  # Render initial map for first visit
  observeEvent(input$tabs, {
    if (input$tabs == "Seed lot map" && first_visit()) { 
      
      data <- LoadedinData 
      popup_info <- paste0(
        "<b>Seedlot: </b>", data$Seedlot, "<br>",
        "<p>",
        "Seedlings Scored (Rust Assay): ", round(data$Seedling_number_rustassay, 2), "<br>",
        "Mean Score (Rust Assay): ", round(data$Mean_seedling_score_rustassay, 2), "<br>",
        "SD Score (Rust Assay): ", round(data$Sd_seedling_score_rustassay, 2), "<br>", 
        "</p>",
        "<p>",
        "Seedlings Scored (Genomic Prediction): ", round(data$Seedling_number_genompred, 2), "<br>",
        "Mean Score (Genomic Prediction): ", round(data$Mean_seedling_score_genompred, 2), "<br>",
        "SD Score (Genomic Prediction): ", round(data$Sd_seedling_score_genompred, 2), "<br>", 
        "</p>",
        "<p>",
        "Latitude collected: ", data$latitude, "<br>",
        "Longitude collected: ", data$longitude, 
        "</p>"
      )
      
      leafletProxy("map") %>%
        clearMarkers() %>%
        addCircleMarkers(
          lng = data$longitude,
          lat = data$latitude,
          layerId = data$Seedlot,
          popup = popup_info,
          stroke = FALSE,
          radius = 6,
          fillOpacity = 0.8,
          fillColor = MR_Sev_pal(data$Mean_seedling_score_rustassay),
          options = markerOptions(rust = data$Mean_seedling_score_rustassay),
          clusterOptions = markerClusterOptions(
            spiderfyDistanceMultiplier = 1.5,
            iconCreateFunction = JS(
              "function(cluster) {
              var markers = cluster.getAllChildMarkers();
              var sum = 0;
              var count = 0;
              for (var i = 0; i < markers.length; i++) {
                var rust = markers[i].options.rust;
                if (rust != null) { sum += rust; count += 1; }
              }
              var avg = count > 0 ? sum / count : 0;
              var color = '';
              if (avg < 10) { color = '#ffffb2'; }
              else if (avg < 30) { color = '#fecc5c'; }
              else if (avg < 50) { color = '#fd8d3c'; }
              else { color = '#e31a1c'; }
              return L.divIcon({
                html: '<div style=\"background-color:' + color + '\"><span>' + cluster.getChildCount() + '</span></div>',
                className: 'marker-cluster',
                iconSize: L.point(40, 40)
              });
            }"
            )
          )
        ) %>%
        
        addLegend(
          position = "bottomright",
          pal = MR_Sev_pal,
          values = data$Mean_seedling_score_rustassay,
          title = "Seedlot's mean rust assay score",
          opacity = 1,
          layerId = "marker_legend"
        ) 
      
      # Mark that the tab has been visited once
      first_visit(FALSE)
    }
  })
  
 
  ############## Reactive renders
  
  # Render initial map 
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng=151.20990, lat=-33.865143, zoom=8) %>% 
      addProviderTiles(providers$CartoDB.Positron) 
  })
  
  # Filter seedlots based on input filters
  filtered_data <- reactive({
    data <- LoadedinData
    
    # Apply rust filter only if checkbox is TRUE
    if (input$filter_rust) {
      data <- data %>% 
        filter(Mean_seedling_score_rustassay >= as.numeric(input$rustFilter[1]),
               Mean_seedling_score_rustassay <= as.numeric(input$rustFilter[2]))
    }
    
    if (input$filter_geno) {
      data <- data %>%
        filter(Mean_seedling_score_genompred >= as.numeric(input$genoFilter[1]),
               Mean_seedling_score_genompred <= as.numeric(input$genoFilter[2]))
    }
    
    if (input$filter_geno_pres) {
      data <- data %>%
        filter(!is.na(Mean_seedling_score_genompred))
    }
    
    if (input$filter_assay_pres) {
      data <- data %>%
        filter(!is.na(Mean_seedling_score_rustassay))
    }
    
    if (input$filter_seedling_numb) {
      data <- data %>%
        filter(Seedling_number_rustassay >= as.numeric(input$seedlNumbFilter[1]) & 
                 Seedling_number_rustassay <= as.numeric(input$seedlNumbFilter[2]) |
                 Seedling_number_genompred >= as.numeric(input$seedlNumbFilter[1]) & 
                 Seedling_number_genompred <= as.numeric(input$seedlNumbFilter[2]) )
    }
    
    data
  })
  
  observe({
    data <- filtered_data()
    
    # Create popup labels for seedlots
    popup_info <- paste0(
      "<b>Seedlot: </b>", data$Seedlot, "<br>",
      
      "<p>",
      "Seedlings Scored (Rust Assay): ", round(data$Seedling_number_rustassay, 2), "<br>",
      "Mean Score (Rust Assay): ", round(data$Mean_seedling_score_rustassay, 2), "<br>",
      "SD Score (Rust Assay): ", round(data$Sd_seedling_score_rustassay, 2), "<br>", 
      "</p>",
      
      "<p>",
      "Seedlings Scored (Genomic Prediction): ", round(data$Seedling_number_genompred, 2), "<br>",
      "Mean Score (Genomic Prediction): ", round(data$Mean_seedling_score_genompred, 2), "<br>",
      "SD Score (Genomic Prediction): ", round(data$Sd_seedling_score_genompred, 2), "<br>", 
      "</p>",
      
      "<p>",
      "Latitude collected: ", data$latitude, "<br>",
      "Longitude collected: ", data$longitude, 
      "</p>"
    )
    
    ### Loading in Map
    leafletProxy("map") %>%
      ## Clean map to update reactively to changes
      clearMarkers() %>%
      clearMarkerClusters() %>%
      removeControl(layerId = "marker_legend") %>% 
      
      ## Seedlots
      addCircleMarkers(
        lng = data$longitude,
        lat = data$latitude,
        popup = popup_info,
        stroke = FALSE,
        radius = 6,
        fillOpacity = 0.8,
        fillColor = MR_Sev_pal(data$Mean_seedling_score_rustassay),
        
        ## Colouring in seedlots based on severity
        options = markerOptions(
          rust = data$Mean_seedling_score_rustassay  # pass custom variable here
        ),
        
        ## JS for colouring based on rust in child clusters
        clusterOptions = markerClusterOptions( 
          spiderfyDistanceMultiplier=1.5,
          # Colour clusters
          iconCreateFunction = JS(" 
          function(cluster) {
          var markers = cluster.getAllChildMarkers();
          var sum = 0;
          var count = 0;
          for (var i = 0; i < markers.length; i++) {
            var rust = markers[i].options.rust;
            if (rust != null) {
              sum += rust;
              count += 1;
            }
          }
          var avg = count > 0 ? sum / count : 0;
          
          var color = '';
          if (avg < 10) { color = '#ffffb2'; }
          else if (avg < 30) { color = '#fecc5c'; }
          else if (avg < 50) { color = '#fd8d3c'; }
          else { color = '#e31a1c'; }

          return L.divIcon({
            html: '<div style=\"background-color:' + color + '\"><span>' + cluster.getChildCount() + '</span></div>',
            className: 'marker-cluster',
            iconSize: L.point(40, 40)
          });
        }
        "),
          colors = MR_Sev_pal
          
        )
      )
    
    
    # Add legend on map
    if (nrow(data) > 1 && !all(is.na(data$Mean_seedling_score_rustassay))) {
      leafletProxy("map") %>%
        addLegend(
          position = "bottomright",
          pal = MR_Sev_pal,
          values = data$Mean_seedling_score_rustassay,
          title = "Seedlot's mean rust assay score",
          opacity = 1,
          layerId = "marker_legend"
        ) 
    }
  })
    
    ## Loading in rasters for SDM
    observe({
      req(input$overlay_SDM == "show")
      # Loading symbol
     w <- Waiter$new(html = spin_2(), color = "#FFFFFFAA")
       w$show()
     on.exit(w$hide())
      
      # Loading rasters
      selected_raster <- switch(input$layer_curr,
                                "MQuin" = maxent_MQuin,
                                "MR" = maxent_MR,
                                "Intersection" = maxent_intersection)
      selected_palette <- switch(input$layer_curr,
                                 "MQuin" = pal_mq(20),
                                 "MR" = pal_mr(20),
                                 "Intersection" = pal_int(20))
      color_pal <- colorNumeric(palette = selected_palette, domain = values(selected_raster), na.color = "transparent")
      
      # Plotting rasters
      leafletProxy("map") %>%
        clearImages() %>%
        removeControl(layerId = "raster_legend") %>% 
        addRasterImage(selected_raster, colors = selected_palette, opacity = 0.5, group = input$layer_curr) %>% 
        addLegend(position = "topright", pal = color_pal, values = values(selected_raster), title = "SDM value", layerId = "raster_legend", group = input$layer_curr)
    })
    
    ## Hide raster if Hide
    observe({
      req(input$overlay_SDM == "hide")
      leafletProxy("map") %>%
        clearImages() %>%
        removeControl(layerId = "raster_legend")
    })
    
    
    ## Loading in the user input location
    observeEvent(input$go, {
      req(input$latitude); req(input$longitude)
      inp_latitude <- as.numeric(input$latitude)
      inp_longitude <- as.numeric(input$longitude)
      
      # Zoom map to location
      leafletProxy("map") %>% 
        flyToBounds(lng1 = inp_longitude-0.15, lng2 = inp_longitude+0.15, lat1 = inp_latitude-0.15, lat2 = inp_latitude+0.15, options = c(duration=0.3))
    })
    
    ###########
    ## Function: Select seedlots - Print clicked_marker in sidePanel with a button for "add to list". When pressed, add clicked_marker to selected_seedlots
    last_clicked_marker <- reactiveValues(id = NULL, lat = NULL, lng = NULL)
    
    observeEvent(input$map_marker_click, {
      clicked_marker <- input$map_marker_click
      last_clicked_marker$id  <- clicked_marker$id
    })
    
    # If not a marker pressed, hide button
    observeEvent(input$map_click, {
      last_clicked_marker$id  <- NULL
    })
    
    # Show button only if a marker is clicked
    output$click_info <- renderUI({
      req(last_clicked_marker$id)
      if (is.null(last_clicked_marker$id)) return(NULL)
      tagList(
        h4("Selected Marker"),
        actionButton("add_seedlot", "Record seed lot")
      )
    })
    
    selected_seedlots <- reactiveValues(seedlot_ID = c())
    
    observeEvent(input$add_seedlot, {
      req(last_clicked_marker$id)
      
      if (!last_clicked_marker$id %in% selected_seedlots$seedlot_ID) {
        selected_seedlots$seedlot_ID  <- c(selected_seedlots$seedlot_ID,  last_clicked_marker$id)
      }
    })
    
    # Filter data reactively then plot - hide if 
      # Hide table if nothing selected
    
    observe({
      if (length(selected_seedlots$seedlot_ID) > 0) {
        updateTextInput(session, "have_selection", value = "TRUE")
      } else {
        updateTextInput(session, "have_selection", value = "FALSE")
      }
    })
    
    LoadedinData_rounded <- LoadedinData %>%
      mutate_if(is.numeric, round, digits = 2)
    
    # Table for rust assay
    selected_seedlot_data <- reactive({
      req(selected_seedlots$seedlot_ID)
      selected_seedlot_data <- LoadedinData_rounded %>% 
        filter(Seedlot %in% selected_seedlots$seedlot_ID) %>% 
        select(Seedlot, latitude, longitude, Seedling_number_rustassay,	Mean_seedling_score_rustassay, Sd_seedling_score_rustassay) %>% 
        unique()
      colnames(selected_seedlot_data) <- c("Seed lot", "Latitude collected", "Longitude collected", "Number of seedlings scored", "Mean seedling score", "Seedling score SD")
      selected_seedlot_data
    })
    
    output$marker_table_1 <- renderDT({
      datatable(
        selected_seedlot_data(),
        selection = "single",   # single row click
        rownames = FALSE,
        options = list(
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE
        )
      )
    })
    
    # Remove seedlot when clicked - DF 1
    observeEvent(input$marker_table_1_rows_selected, {
      selected_row <- input$marker_table_1_rows_selected
      if (length(selected_row)) {
        seedlot_to_remove <- selected_seedlot_data()[selected_row, "Seed lot"]
        
        confirmSweetAlert(
          session = session,
          inputId = "confirm_remove",
          title = "Remove Seedlot?",
          text = paste("Removing recorded seed lot: ", seedlot_to_remove),
          type = "question",
          btn_labels = c("Cancel", "Remove"),
          closeOnClickOutside = TRUE,
          showCloseButton = TRUE,
          allowEscapeKey = TRUE,
          btn_colors = c("grey70", "#D9344D"),
          danger_mode = TRUE
        )
      }
    })
    
    # Table for GP
    selected_seedlot_data_2 <- reactive({
      req(selected_seedlots$seedlot_ID)
      selected_seedlot_data_2 <- LoadedinData_rounded %>% 
        filter(Seedlot %in% selected_seedlots$seedlot_ID) %>% 
        select(Seedlot, latitude, longitude,	MatLine_Genompred , Seedling_number_genompred, Mean_seedling_score_genompred, Sd_seedling_score_genompred) %>% 
        unique()
      colnames(selected_seedlot_data_2) <- c("Seed lot", "Latitude collected", "Longitude collected", "Maternal line score", "Number of seedlings scored", "Mean seedling score", "Seedling score SD")
      selected_seedlot_data_2
    })

    output$marker_table_2 <- renderDT({
      datatable(
        selected_seedlot_data_2(),
        selection = "single",   # single row click
        rownames = FALSE,
        options = list(
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE
        )
      )
    })
    
    # Remove seedlot when clicked - DF 2
    observeEvent(input$marker_table_2_rows_selected, {
      selected_row <- input$marker_table_2_rows_selected
      
      if (length(selected_row)) {
        seedlot_to_remove <- selected_seedlot_data_2()[selected_row, "Seed lot"]
      
        confirmSweetAlert(
          session = session,
          inputId = "confirm_remove",
          title = "Remove Seedlot?",
          text = paste("Removing recorded seed lot: ", seedlot_to_remove),
          type = "question",
          btn_labels = c("Cancel", "Remove"),
          closeOnClickOutside = TRUE,
          showCloseButton = TRUE,
          allowEscapeKey = TRUE,
          btn_colors = c("grey70", "#D9344D"),
          danger_mode = TRUE
        )
      }
    })
    
    observeEvent(input$confirm_remove, {
      if (isTRUE(input$confirm_remove)) {
        selected_row_1 <- input$marker_table_1_rows_selected
        seedlot_to_remove_1 <- selected_seedlot_data()[selected_row_1, "Seed lot"]
        
        selected_row_2 <- input$marker_table_2_rows_selected
        seedlot_to_remove_2 <- selected_seedlot_data()[selected_row_2, "Seed lot"]
        
        selected_seedlots$seedlot_ID <- setdiff(selected_seedlots$seedlot_ID, seedlot_to_remove_1)
        selected_seedlots$seedlot_ID <- setdiff(selected_seedlots$seedlot_ID, seedlot_to_remove_2)
      }
    })
}
