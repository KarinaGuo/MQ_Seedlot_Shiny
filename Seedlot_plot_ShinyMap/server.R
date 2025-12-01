library(tidyr) # data wrangling
library(raster) # Map
library(sf) # Map
library(shiny) # shiny base package
library(shinycssloaders) # load symbol
library(leaflet) # Map
library(shinyBS) # layout
library(waiter) # loading sign reactively
library(DT) # Render datatable
library(shinyjs) # Allowing java
library(shinyWidgets) # Extra additions for UI
library(lubridate) # Using year() for current year
library(htmlwidgets) # Extra additions for UI (html)
library(tidyverse) # Data wrangling
library(ggridges) # GGplot geom ridges
library(plotly) # Plot output

setwd("~/Uni/Doctorate/Ch Seedlot_plot_data/")

# Load in datasets used
## Files for report gen (keep at top)
source(file = "Seedlot_plot_ShinyMap/Seedlot_plot_shiny.R")

LoadedinData <- read.csv("data/final_seedloty_plot.csv") %>% unique()
LoadedinData <- LoadedinData[!duplicated(LoadedinData$Seedlot),]

# Load current projection data
maxent_MQuin <- raster("data/maxent_MQuin.tiff")
maxent_MR <- raster("data/maxent_MR.tiff")
maxent_intersection <- raster("data/maxent_intersection.tiff")
maxent_studyarea <- st_read("data/maxent_studyarea.gpkg")

MQ_observations <- read.csv("data/MQ_locs_clean.csv"); MQ_observations <- MQ_observations[,2:3]
MR_observations <- read.csv("data/MR_locs_clean.csv"); MR_observations <- MR_observations[,2:3]

# Load future projection data
years_clust <- c("2021-2040", "2041-2060", "2061-2080")
ssp_list <- c("126", "245", "370", "585")

for (species in c("MQuin", "MR")){
  for (cluster in years_clust){
    for (ssp in ssp_list){
      read_file <- raster(paste0("data/maxent_",species, cluster, "_", ssp, ".tiff"))
      assign(paste0("maxent_",species, cluster, "_", ssp), read_file)
    }
  }
}

for (cluster in years_clust){
  for (ssp in ssp_list){
    read_file <- raster(paste0("data/future_maxent_intersection", cluster, "_", ssp, ".tiff"))
    assign(paste0("maxent_intersection", cluster, "_", ssp), read_file)
  }
}

##
date=Sys.Date()

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



function(input, output, session) {
  #Seed lot map plots  ###########################################################################################  
  
  # Set initial render
  # Track the first visit to the tab 
  first_visit <- reactiveVal(TRUE) 
  
  ## Marker images
  # For circle markers
  svg_urls <- sprintf(
    "data:image/svg+xml,%s",
    URLencode(sprintf(
      "<svg xmlns='http://www.w3.org/2000/svg' width='16' height='16'>
       <circle cx='8' cy='8' r='6' fill='%s' opacity='0.8'/>
     </svg>", 
      MR_Sev_pal(LoadedinData$Mean_seedling_score_rustassay)
    ), reserved = TRUE)
  )
  
  # List of selected seedlots
  selected_seedlots <- reactiveValues(seedlot_ID = character(0))
  
  ##
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
        clearMarkerClusters() %>%
        removeControl(layerId = "marker_legend") %>%
        addPolygons(data = maxent_studyarea, color = "black", weight = 1, fill = FALSE, group = "Outline") %>% 
        
        addMarkers(
          lng = data$longitude,
          lat = data$latitude,
          popup = popup_info,
          layerId = data$Seedlot,   # for clusters
          
          ## Convert circle style to icons
          icon = icons(
            iconUrl = svg_urls,
            iconWidth = 16,
            iconHeight = 16
          ),
          
          options = markerOptions(rust = data$Mean_seedling_score_rustassay,
                                  layerId = data$Seedlot),
          
          clusterOptions = markerClusterOptions(
            spiderfyDistanceMultiplier = 1.5,
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
      ")
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
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = maxent_studyarea, color = "black", weight = 1, fill = FALSE, group = "Outline") %>% 
      onRender(JS("
      function(el, x) {
        var map = this;

        map.on('layeradd', function(e) {
          // When a cluster group is added, attach the clusterclick listener
          if (e.layer instanceof L.MarkerClusterGroup) {

            e.layer.on('clusterclick', function(ev) {
              var markers = ev.layer.getAllChildMarkers();
              var ids = markers.map(function(m) { return m.options.layerId; });

              Shiny.setInputValue('cluster_ids', ids, {priority: 'event'});
            });
          }
        });
      }
    ")) 
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
      removeMarker(layerId = data$Seedlot) %>%
      clearMarkerClusters() %>%
      removeControl(layerId = "marker_legend") %>%
      
      addMarkers(
        lng = data$longitude,
        lat = data$latitude,
        popup = popup_info,
        layerId = data$Seedlot,   # for clusters
          
        icon =  icons(iconUrl = svg_urls, iconWidth = 16, iconHeight = 16), ## Convert circle style to icons
        
        options = markerOptions(rust = data$Mean_seedling_score_rustassay,
                                layerId = data$Seedlot),
        
        clusterOptions = markerClusterOptions(
          spiderfyDistanceMultiplier = 1.5,
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
      ")
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
    
  ## Marker IDs
  Marker_RV <- reactiveValues(marker_ids = NULL)
  
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
      
      selected_observations <- switch(input$layer_curr,
                                      "MQuin" = MQ_observations,
                                      "MR" = MR_observations,
                                      "Intersection" = data.frame(rbind(MQ_observations, MR_observations))) 
      selected_observations$colour <- switch(input$layer_curr,
                                             "MQuin" = "#2e7031",
                                             "MR" = "#ff5024",
                                             "Intersection" = c(rep("#2e7031", nrow(MQ_observations)),
                                                                rep("#ff5024", nrow(MR_observations))))
      
      
      # Plotting rasters
      leafletProxy("map") %>%
        clearImages() %>%
        removeControl(layerId = "raster_legend") %>% 
        addRasterImage(selected_raster, colors = selected_palette, opacity = 0.5, group = input$layer_curr) %>% 
        addLegend(position = "topright", pal = color_pal, values = values(selected_raster), title = "SDM value", layerId = "raster_legend", group = input$layer_curr)
      
      if (input$MRMQ_Obs == "show_obs"){
        leafletProxy("map") %>%
          clearMarkers() %>% 
          addCircleMarkers(
            data = selected_observations,
            lng = ~decimalLongitude,
            lat = ~decimalLatitude,
            fillColor = ~colour,
            group = input$layer_curr,
            stroke = FALSE,
            radius = 2,
            fillOpacity = 0.5
          )
      }

    })
    
    ## Hide raster if Hide
    observe({
      req(input$overlay_SDM == "hide")
      
      leafletProxy("map") %>%
        clearImages() %>%
        clearMarkers() %>% 
        removeControl(layerId = "raster_legend")
    })
    
    observe({
      req(input$MRMQ_Obs == "hide_obs")
      
      leafletProxy("map") %>%
        clearMarkers()
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
    
    ## Clicked markers
    last_clicked_marker <- reactiveValues(id = NULL, lat = NULL, lng = NULL)
    last_clicked_cluster <- reactiveValues(id = NULL, lat = NULL, lng = NULL)
    
    ## Cluster click
    observeEvent(input$cluster_ids, {
      last_clicked_cluster$id  <- input$cluster_ids
    })
    
    observeEvent(input$map_marker_click, {
      last_clicked_marker$id  <- input$map_marker_click$id
      last_clicked_cluster$id  <- input$cluster_ids
    })
    

    
    # Show button only if a marker is clicked
    output$click_info <- renderUI({
      req(any(!is.null(last_clicked_marker$id), !is.null(last_clicked_cluster$id)))
      
      #if (is.null(last_clicked_marker$id) & is.null(last_clicked_cluster$id)) return(NULL)
      
      # Only paste whichever is not null
      if (!is.null(last_clicked_marker$id)) {
        seedlot_list_clicked <- last_clicked_marker$id
      } else if (!is.null(last_clicked_cluster$id)) {
        seedlot_list_clicked <- last_clicked_cluster$id
      } else {
        return(NULL)
      }
      
      tagList(
        h4("Selected Marker"),
        p(
          if (length(seedlot_list_clicked) > 20) {
            paste(paste(head(seedlot_list_clicked, 20), collapse = ", "), "... too many to list")
          } else {
            paste(seedlot_list_clicked, collapse = ", ")
          }),
        actionButton("add_seedlot", "Record seed lot")
      )
    })
    
    observeEvent(input$add_seedlot, {
      req(any(
        !is.null(last_clicked_marker$id),
        !is.null(last_clicked_cluster$id)
      )) 
      
      # append to list
      if (!is.null(last_clicked_marker$id)) {
        selected_seedlots$seedlot_ID <- unique(
          c(selected_seedlots$seedlot_ID, last_clicked_marker$id)
        )
        last_clicked_marker$id  <- NULL
        last_clicked_cluster$id  <- NULL
      } else if (!is.null(last_clicked_cluster$id)) {
        selected_seedlots$seedlot_ID <- unique(
          c(selected_seedlots$seedlot_ID, last_clicked_cluster$id)
        )
        last_clicked_marker$id  <- NULL
        last_clicked_cluster$id  <- NULL
      } else {
        return(NULL)
      }
    })
    
    ##
    # If not a marker pressed, hide button
    observeEvent(input$map_click, {
      last_clicked_marker$id  <- NULL
      last_clicked_cluster$id  <- NULL
    })
    
    
    # Filter data reactively then plot - hide if  if nothing selected
    observe({
      if (length(selected_seedlots$seedlot_ID) > 0) {
        updateTextInput(session, "have_selection", value = "TRUE")
      } else {
        updateTextInput(session, "have_selection", value = "FALSE")
      }
    })
    
    
    ######## Add larger circle for seedlots added to selection
    # observe({
    #   
    #   if (length(selected_seedlots$seedlot_ID) > 0) {
    #     data <- LoadedinData 
    #     selected_data <- data[data$Seedlot %in% selected_seedlots$seedlot_ID, ]
    #     
    #     leafletProxy("map") %>%
    #       addCircleMarkers(
    #         lng = selected_data$longitude,
    #         lat = selected_data$latitude,
    #         radius = 12,                        # larger than original
    #         color = "red",                       # border color
    #         weight = 2,
    #         fillColor = "transparent",           # or a semi-transparent fill
    #         fillOpacity = 0.3,
    #         layerId = paste0("selected_", selected_data$Seedlot), # unique IDs
    #         options = pathOptions(clickable = FALSE) # ensures they don't interfere with marker clicks
    #       )
    #   }
    # })
  
    
    #########
    LoadedinData_rounded <- LoadedinData %>%
      mutate_if(is.numeric, round, digits = 2)
    
    # Table for rust assay
    selected_seedlot_data <- reactive({
      req(selected_seedlots$seedlot_ID)
      selected_seedlot_data <- LoadedinData_rounded %>% 
        filter(Seedlot %in% selected_seedlots$seedlot_ID) %>% 
        select(Seedlot, latitude, longitude, Seedling_number_rustassay,	Mean_seedling_score_rustassay, Sd_seedling_score_rustassay) %>% 
        distinct(Seedlot, .keep_all = TRUE)
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
        distinct(Seedlot, .keep_all = TRUE) 
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
    
    observeEvent(input$report_gen_butt, {
      confirmSweetAlert(
        session = session,
        inputId = "confirm_report",
        title = "Produce report?",
        text = paste("Creating report at Selected_seedlots",date,".csv"),
        type = "question",
        btn_labels = c("Cancel", "Generate"),
        closeOnClickOutside = TRUE,
        showCloseButton = TRUE,
        allowEscapeKey = TRUE,
        btn_colors = c("grey70", "darkseagreen2"),
        danger_mode = TRUE
      )
    })
    
    observeEvent(input$confirm_report, {
      
      req(input$confirm_report)
      selected_seedlot_data_allrepo <- LoadedinData_rounded %>% 
        filter(Seedlot %in% selected_seedlots$seedlot_ID) %>% 
        select(Seedlot, latitude, longitude, Seedling_number_rustassay,	Mean_seedling_score_rustassay, Sd_seedling_score_rustassay, MatLine_Genompred , Seedling_number_genompred, Mean_seedling_score_genompred, Sd_seedling_score_genompred) %>% 
        distinct(Seedlot, .keep_all = TRUE)
      colnames(selected_seedlot_data_allrepo) <- c("Seed lot", "Latitude collected", "Longitude collected", "Number of seedlings scored (Assay)", "Mean seedling score (Assay)", "Seedling score SD (Assay)", "Maternal line score (GenomPred)", "Number of seedlings scored (GenomPred)", "Mean seedling score (GenomPred)", "Seedling score SD (GenomPred)")
      
      write.csv(selected_seedlot_data_allrepo, file = paste0("Selected_seedlots_",date,".csv"), row.names=FALSE)
      showNotification("Report generated.", type = "message")
    })
    
    # Site risk tab ######################################################################

    ## Add click and point map 
    
    output$map_siterisk <- renderLeaflet({
      leaflet() %>%
        setView(lng=151.20990, lat=-33.865143, zoom=8) %>% 
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(data = maxent_studyarea, color = "black", weight = 1, fill = FALSE, group = "Outline")
      })
    
    coords <- reactiveValues(lat = NULL, lng = NULL)
    
    observeEvent(input$map_siterisk_click, {
      
      # Register map click for location
      req(input$map_siterisk_click)
      coords$lat <- input$map_siterisk_click$lat
      coords$lng <- input$map_siterisk_click$lng
      
      updateTextInput(session, "latitude_risk",  value = round(coords$lat, 6))
      updateTextInput(session, "longitude_risk", value = round(coords$lng, 6))
      
      # Fetch the location name from Nominatim
      url <- paste0("https://nominatim.openstreetmap.org/reverse?format=json&lat=", input$map_siterisk_click$lat, "&lon=", input$map_siterisk_click$lng)
      response <- GET(url, user_agent("R"))  # Nominatim requires a user agent
      data <- fromJSON(content(response, "text", encoding = "UTF-8"))
      
      if (!is.null(data$display_name)) {
        location <- data$display_name
        location_shrt <-  sub(", New South Wales, .*?, Australia.*", "", location)
        location_shrt_2 <- sub("^[^,]*,\\s*", "", location_shrt)
        
        updateTextInput(session, "site_name",  value = location_shrt_2)
      }
      
      ## Update stored names for report
      output$site_name_rep <- renderText ({ location_shrt_2 })
      output$latitude_risk <- renderText ({ round(coords$lng, 6) })
      output$longitude_risk <- renderText ({ round(coords$lat, 6) })
        
    })
    
    ## Update coords with text input
    
    observeEvent(input$latitude_risk, {
      req(input$latitude_risk)
      val <- (as.numeric(input$latitude_risk))
      coords$lat <- val
    })
    
    observeEvent(input$longitude_risk, {
      req(input$longitude_risk)
      val <- (as.numeric(input$longitude_risk))
      coords$lng <- val
    })
    
    ## Update map plot from click OR text
    observe({
      req(coords$lat, coords$lng)
      
      leafletProxy("map_siterisk") %>%
        clearGroup("selection") %>%
        addCircleMarkers(
          lng = coords$lng,
          lat = coords$lat,
          radius = 3,
          color = "darkgreen",
          fillOpacity = 0.5,
          group = "selection"
        )
    })
    
    SDM_curr_overlap_score <- reactive({
      # Convert input to numeric
      lat <- as.numeric(coords$lat)
      lon <- as.numeric(coords$lng)
      
      # Check if inputs are valid numbers
      if (is.na(lat) || is.na(lon)) {
        return(NULL)  # or some default/error value
      }
      
      # Call the extract function
      extract(maxent_intersection, cbind(lon, lat))
    })
  
    output$SDM_curr_overlap_score <- renderText({
      req(coords$lat & coords$lng)
      value <- SDM_curr_overlap_score()
      if (is.null(value) | is.na(value)){
        "Outside of SDM map"
      } else {
        round(value, 3) 
      }
      })
  
    SDM_fut_overlap_score <- reactive({
      # Convert input to numeric
      lat <- as.numeric(coords$lat)
      lon <- as.numeric(coords$lng)
      
      # Check if inputs are valid numbers
      if (is.na(lat) || is.na(lon)) {
        return(NULL)  
      }
      
      # Call the extract function
      extract(`maxent_intersection2021-2040_126`, cbind(lon, lat))
    })
    
    output$SDM_fut_overlap_score <- renderText({
      req(coords$lat & coords$lng)
      value <- SDM_fut_overlap_score()
      if (is.null(value) | is.na(value)){
        "Outside of SDM map"
      } else {
        round(value, 3) 
      }
    })
    
    # Calculate site score and disease risk
    observeEvent(input$calculate_score, {
      
      score_Site_DisPres <- ifelse(input$Site_DisPres == "low_site_dispres", 1,
                                   ifelse(input$Site_DisPres == "high_site_dispres", 2, 0))
      score_Site_DisPres <- score_Site_DisPres/2
      
      score_Water_pres <- ifelse(input$Water_pres == "notpres_water", 1,
                                 ifelse(input$Water_pres == "pres_water", 2, 0))
      score_Water_pres <- score_Water_pres/2
      
      score_Edge_eff <- ifelse(input$Edge_eff == "notpres_edge", 1,
                               ifelse(input$Edge_eff == "pres_edge", 2, 0))
      score_Edge_eff <- score_Edge_eff/2
      
      score_Time_lastburn <- if (input$Time_lastburn != "No Burn") {(year(Sys.Date()) - as.numeric(input$Time_lastburn))} else {0}
      score_Time_lastburn <- 15-score_Time_lastburn/15
      
      score_Burn_severity <- ifelse(input$Burn_severity == "low_burn_severity", 1,
                                ifelse(input$Burn_severity == "mod_burn_severity", 2,
                                       ifelse(input$Burn_severity == "high_burn_severity", 3, 0)))
      score_Burn_severity <- score_Burn_severity/3
      
      score_Adult_genompredres <- ifelse(input$Adult_genompredres == "low_adult_resistance", 1,
                                    ifelse(input$Adult_genompredres == "mod_adult_resistance", 2,
                                           ifelse(input$Adult_genompredres == "high_adult_resistance", 3, 0)))
      score_Adult_genompredres <- score_Adult_genompredres/3
      
      score_Seedling_genompredres <- ifelse(input$Seedling_genompredres == "low_seedl_resistance", 1,
                                         ifelse(input$Seedling_genompredres == "mod_seedl_resistance", 2,
                                                ifelse(input$Seedling_genompredres == "high_seedl_resistance", 3, 0)))
      score_Seedling_genompredres <- score_Seedling_genompredres/3
      
      score_Geno_conf <- ifelse(input$Geno_conf == "low_geno_conf", 1,
                                ifelse(input$Geno_conf == "mod_geno_conf", 2,
                                       ifelse(input$Geno_conf == "high_geno_conf", 3, 0)))
      score_Geno_conf <- score_Geno_conf/3
      
      if (score_Geno_conf > 0){
        score_Adult_genompredres <- score_Adult_genompredres * score_Geno_conf
        score_Seedling_genompredres <- score_Seedling_genompredres * score_Geno_conf
      }
      
      ## If crossed out - revert score to 0
      is_disabled <- function(x) {ifelse(is.null(x), 0, x)}
      
      if (is_disabled(input$SDM_fut_disabled) == 1) score_SDM_fut <- 0
      if (is_disabled(input$SDM_curr_disabled) == 1) score_SDM_curr <- 0
      if (is_disabled(input$Site_DisPres_p_disabled) == 1) score_Site_DisPres <- 0
      if (is_disabled(input$Site_DisPres_p_disabled) == 1) score_Water_pres <- 0
      if (is_disabled(input$Edge_eff_p_disabled) == 1) score_Edge_eff <- 0
      if (is_disabled(input$Time_lastburn_p_disabled) == 1) score_Time_lastburn <- 0
      if (is_disabled(input$Burn_severity_p_disabled) == 1) score_Burn_severity <- 0
      if (is_disabled(input$Adult_genompredres_p_disabled) == 1) score_Adult_genompredres <- 0
      if (is_disabled(input$Seedling_genompredres_p_disabled) == 1) score_Seedling_genompredres <- 0
      if (is_disabled(input$Geno_conf_p_disabled) == 1) score_Geno_conf <- 0
      
      ## Calculate scores
      total_score = c(score_Site_DisPres, 
                        score_Water_pres, 
                        score_Edge_eff,
                        score_Time_lastburn,
                        score_Burn_severity,
                        score_Adult_genompredres,
                        score_Seedling_genompredres,
                        score_Geno_conf)
      
      score_counted <- as.numeric(sum(total_score)/sum(sum(total_score>0)))
      score_counted_cat <- ifelse (score_counted < 0.3, "Low", 
                                   ifelse(score_counted >= 0.3 & score_counted < 0.6, "Moderate",
                                          ifelse(score_counted >= 0.6, "High", "No score available")))
      
      text_color <- switch(score_counted_cat,
                           "High"     = "#8c0d0d",
                           "Moderate" = "#a37d22", 
                           "Low"      = "#2f6915",
                           "black") # Default color if "No score available"
      
      # Return the text wrapped in HTML with the color applied
      output$risk_score <- renderUI({
        span(score_counted_cat, style = paste0("color:", text_color, "; font-weight: bold; font-size: 1em;"))
      })
      
    })
    
#  Future current tab ######################################################################
    
    # Future current tab
    first_visit_future <- reactiveVal(TRUE) 
    
    output$map_2 <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(data = maxent_studyarea, color = "black", weight = 1, fill = FALSE, group = "Outline")
    })
    
    observeEvent(input$tabs, {
      if (input$tabs == "Future projections" && first_visit_future()) { 
        selected_raster_fut <- get("maxent_MQuin2021-2040_126")
        color_pal_fut <- colorNumeric(palette = pal_mq(20), domain = values(selected_raster_fut), na.color = "transparent")
        
        leafletProxy("map_2") %>%
          #addProviderTiles(providers$CartoDB.Positron) %>%
          addPolygons(data = maxent_studyarea, color = "black", weight = 1, fill = FALSE, group = "Outline") %>% 
          addRasterImage(selected_raster_fut, colors = pal_mq(20), opacity = 0.8, group = input$layer_fut) %>%
          addLegend(position = "topright", pal = color_pal_fut, 
                    values = values(selected_raster_fut), title = "SDM value", group = input$layer_fut)
      }
    })

    
    observe({req(input$layer_fut, input$year_clust, input$ssp)  # Ensure inputs are available
      
      species_fut <- gsub("_fut", "", input$layer_fut) 
      raster_name_fut <- paste0("maxent_", species_fut, input$year_clust, "_", input$ssp) 
      selected_raster_fut <- get(raster_name_fut) 
      
      raster_name_curr <- paste0("maxent_", species_fut) 
      selected_raster_curr <- get(raster_name_curr)
      
      selected_palette_fut <- switch(input$layer_fut,
                                     "MQuin_fut" = pal_mq(20),
                                     "MR_fut" = pal_mr(20),
                                     "intersection_fut" = pal_int(20))
      
      color_pal_fut <- colorNumeric(palette = selected_palette_fut, domain = values(selected_raster_fut), na.color = "transparent")
      
      
      if (!is.null(input$fut_show_both) && input$fut_show_both == "fut_hide_pres") {
        leafletProxy("map_2") %>%
          clearImages() %>%
          clearControls() %>%
          addRasterImage(selected_raster_fut, colors = selected_palette_fut, opacity = 0.8, group = input$layer_fut) %>%
          addLegend(position = "topright", pal = color_pal_fut, 
                    values = values(selected_raster_fut), title = "SDM value", group = input$layer_fut)
      }
      
      if (!is.null(input$fut_show_both) && input$fut_show_both == "fut_show_pres") {
        
        # Raster of difference (future - current)
        fut_pres_diff <- selected_raster_fut - selected_raster_curr
        
        # Color palette for difference (centered around 0)
        pal_int_fut <- colorRampPalette(c('#261323', 'deeppink4', "white", "darkolivegreen", "#1b2613"))
        color_pal_fut <- colorNumeric(palette = pal_int_fut(20), domain = c(-1.3,1.2), na.color = "transparent") # Range set with biggest diff observed, manually set for static color scale
        
        # Plot the raster of differences
        leafletProxy("map_2") %>%
          clearImages() %>%
          clearControls() %>%
          addRasterImage(fut_pres_diff, colors = color_pal_fut, opacity = 0.8, group = input$layer_fut) %>%
          addLegend(position = "topright", pal = color_pal_fut, 
                    values = c(-.71,1.2), title = "Difference in SDM value", group = input$layer_fut)
      }

    })
    
    #  Report generation tab ######################################################################
    observe({
      if (length(selected_seedlots$seedlot_ID) > 0) { 
        shinyjs::show("selection_panel")
        shinyjs::hide("no_selection")
      } else {
        shinyjs::hide("selection_panel")
        shinyjs::show("no_selection")
      }
    })
    
    observe({
      input$calculate_score
      
      #print(input$latitude_risk=="")
      
      if (is.null(input$latitude_risk) || is.na(input$latitude_risk) || input$latitude_risk=="") { 
        shinyjs::hide("site_info")
      } else {
        shinyjs::show("site_info")
      }
    })
    
    ##### Plot of seedlots characteristics
    selected_seedlot_rustassay_score_allrepo <- reactive({
      req(length(selected_seedlots$seedlot_ID) > 0)
      
      left_join(mat_line_rustassay_score, LoadedinData) %>% 
        dplyr::filter(Seedlot %in% selected_seedlots$seedlot_ID) %>% 
        dplyr::filter(!is.na(Seedling_score_rustassay))
    })
    
    mean_matLine_RustAssay <- reactive({
      df <- selected_seedlot_rustassay_score_allrepo()
      req(nrow(df) > 0)
      
      df %>% 
        dplyr::select(Seedlot, Mean_seedling_score_rustassay) %>% 
        dplyr::distinct()
    })
    
    selected_seedlot_genompred_score_allrepo <- reactive({
      req(length(selected_seedlots$seedlot_ID) > 0)
      
      left_join(mat_line_genompred_score, LoadedinData) %>% 
        dplyr::filter(Seedlot %in% selected_seedlots$seedlot_ID)%>% 
        dplyr::filter(!is.na(Seedling_score_genompred))
    })
    
    mean_matLine_GenomPred <- reactive({
      df <- selected_seedlot_genompred_score_allrepo()
      req(nrow(df) > 0)
      
      df %>% 
        dplyr::select(Seedlot, Mean_seedling_score_genompred) %>% 
        dplyr::distinct()
    })
    
    mean_matLine_MatLineGenomPred <- reactive({
      df <- selected_seedlot_genompred_score_allrepo()
      req(nrow(df) > 0)
      
      df %>% 
        dplyr::select(Seedlot, MatLine_Genompred) %>% 
        dplyr::distinct()
    })
      
  
    observe({
      req(length(selected_seedlots$seedlot_ID) > 0)
      
      output$seedlot_rustass_plot_geomridge <- renderPlot({
        ggplot() +
          geom_density_ridges2(data=selected_seedlot_rustassay_score_allrepo(), colour="grey40", alpha = 0.9, aes(x = Seedling_score_rustassay, y=Seedlot, fill=Seedling_number_rustassay, alpha = 0.7), scale =1.3) +
          #geom_hline(data = mean_matLine_RustAssay(), aes(yintercept = as.numeric(factor(Seedlot)), yend = as.numeric(factor(Seedlot)) + 0.8), colour = "darkred", linewidth = 0.5, linetype="dashed") +
          #geom_segment(data = mean_matLine_RustAssay(), aes(x = 0, xend = Mean_seedling_score_rustassay, y = as.numeric(factor(Seedlot)), yend = as.numeric(factor(Seedlot))), colour = "darkred", linewidth = 0.5) +
          geom_segment(data = mean_matLine_RustAssay(), aes(x = Mean_seedling_score_rustassay, xend = Mean_seedling_score_rustassay, y = as.numeric(factor(Seedlot)), yend = as.numeric(factor(Seedlot)) + 0.8), colour = "red", linewidth = 0.5, linetype="dashed") +
          labs(title = "Characterising rust resistance assay scores", 
               subtitle="Red dashed line represents mean seedling rust assay score of seed lot",
               y = "Seed lot ID",
               x = "Raw assay score",
               fill = "Number assayed") +
          theme_bw() +
          scale_fill_viridis_c()
        }, height = function() {
          30 * length(selected_seedlots$seedlot_ID)
        })
      
      output$seedlot_genompred_plot_geomridge <- renderPlot({
        ggplot() +
          geom_density_ridges2(data=selected_seedlot_genompred_score_allrepo(), colour="grey40", alpha = 0.9, aes(x = Seedling_score_genompred, y=Seedlot, fill=Seedling_number_genompred), scale =1.3) +
          #geom_hline(data = mean_matLine_GenomPred(), aes(yintercept = as.numeric(factor(Seedlot))), colour = "darkblue", linewidth = 0.5, linetype="dashed") +
          #geom_segment(data = mean_matLine_GenomPred(), aes(x = 0, xend = Mean_seedling_score_genompred, y = as.numeric(factor(Seedlot)), yend = as.numeric(factor(Seedlot)) + 0.5), colour = "darkblue", linewidth = 0.5) +
          geom_segment(data = mean_matLine_GenomPred(), aes(x = Mean_seedling_score_genompred, xend = Mean_seedling_score_genompred, y = as.numeric(factor(Seedlot)), yend = as.numeric(factor(Seedlot)) + 0.8), colour = "purple", linewidth = 0.5, linetype="dotted") +
          geom_segment(data = mean_matLine_MatLineGenomPred(), aes(x = MatLine_Genompred, xend = MatLine_Genompred, y = as.numeric(factor(Seedlot)), yend = as.numeric(factor(Seedlot)) + 0.8), colour = "blue", linewidth = 0.5, linetype="dashed") +
          labs(title = "Characterising rust resistance assay scores", 
               subtitle="Blue dashed line represents mean seedling genomic prediction score of seed lot\nPurple dotted line represents the maternal line's genomic prediction score",
               y = "Seed lot ID",
               x = "Genomic prediction score",
               fill = "Number assayed") +
          theme_bw() +
          scale_fill_viridis_c()
        }, height = function() {
          30 * length(selected_seedlots$seedlot_ID)
        })
    })
    
    #### Table of seedlots
    
    output$marker_table_1_repo <- renderDT({
      datatable(
        selected_seedlot_data(),
        selection = "single",   # single row click
        rownames = FALSE,
        options = list(
          paging = TRUE,
          searching = TRUE,
          ordering = TRUE
        )
      )
    })
    
    output$marker_table_2_repo <- renderDT({
      datatable(
        selected_seedlot_data_2(),
        selection = "single",   # single row click
        rownames = FALSE,
        options = list(
          paging = TRUE,
          searching = TRUE,
          ordering = TRUE
        )
      )
    })
    
    ##### Plot of seedlots on map
     observe({ # Delay load until map made
      req(input$tabs == "Summary results" & length(selected_seedlots$seedlot_ID) > 0)
      
      output$map_rep <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addCircleMarkers(data=LoadedinData, lng = ~longitude, lat = ~latitude, fillColor="black", radius = 1.5, opacity=0.3, stroke=FALSE) %>% 
          addPolygons(data = maxent_studyarea, color = "black", weight = 1, fill = FALSE, group = "Outline") 
      })
      
      data_repo <- selected_seedlot_data()
      colnames (data_repo) <- c("Seedlot", "Lat", "Long", "N", "Mean_score", "Sd_score")
      data_repo_df <- data.frame(data_repo)
      
      map_rep_pal <- colorNumeric("YlOrRd", domain = data_repo_df$Mean_score)
       
      if (!is.null(input$latitude_risk) && !is.na(input$longitude_risk)) { ## If user sets site 
       lat <- as.numeric(input$latitude_risk)
       lon <- as.numeric(input$longitude_risk)
        
        leafletProxy("map_rep") %>% 
          addCircleMarkers(data = data_repo_df, lng = ~Long, lat = ~Lat, fillColor = map_rep_pal(data_repo_df$Mean_score), color= "black", weight=1, radius = 3, popup = ~Seedlot, stroke=TRUE) %>%
          addLegend("bottomright", pal = map_rep_pal, values = data_repo_df$Mean_score, title = "Rust assay score") %>% 
          addMarkers(lng = lon, lat = lat)
      } else {
        leafletProxy("map_rep") %>% 
          addCircleMarkers(data = data_repo_df, lng = ~Long, lat = ~Lat, fillColor = map_rep_pal(data_repo_df$Mean_score), color= "black", weight=1, radius = 3, popup = ~Seedlot, stroke=TRUE) %>%
          addLegend("bottomright", pal = map_rep_pal, values = data_repo_df$Mean_score, title = "Rust assay score")
      }
      
    })
     
     
}
