library(leaflet)
library(dplyr)
library(shiny)
library(shinyBS)
library(shinycssloaders)
LoadedinData <- read.csv("~/Uni/Doctorate/Samples/Seedlot_plot_data/final_seedloty_plot.csv")

fluidPage(
  titlePanel("Melaleuca quinquenervia assayed seedlots"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("latitude", "Latitude: ", value = "-33.8688"),
      textInput("longitude", "Longitude: ", value = "151.2093"),
      actionButton("go", "Go to location"),
      
      # Rust filter
      checkboxInput("filter_rust", "Filter by rust assay score", FALSE),
      conditionalPanel(
        condition = "input.filter_rust == true",
        sliderInput("rustFilter",
                    "Mean seedling score (rust assay):",
                    min = 0,
                    max = 100,
                    value = c(0, 100),
                    step = 10)
      ),
      
      # Genomic filter
      checkboxInput("filter_geno", "Filter by genomic prediction score", FALSE),
      conditionalPanel(
        condition = "input.filter_geno == true",
        sliderInput("genoFilter",
                    "Mean seedling score (genomic prediction):",
                    min = min(LoadedinData$Mean_seedling_score_genompred, na.rm = TRUE),
                    max = 3, 
                    value = c(0,3),
                    step = 0.1)
      ),
      
      # Collapsible panel for additional filters
      bsCollapse(
        id = "filtersCollapse",
        open = NULL,  # start collapsed
        bsCollapsePanel(
          title = "Additional Filters",
          checkboxInput("filter_assay_pres", "Remove seedlots with no resistance assay score", FALSE),
          checkboxInput("filter_geno_pres", "Remove seedlots with no genomic prediction score", FALSE),
          checkboxInput("filter_seedling_numb", "Filter by number of seedlings scored", FALSE),
          conditionalPanel(
            condition = "input.filter_seedling_numb == true",
            sliderInput("seedlNumbFilter",
                        "Number of seedlings scored in either method:",
                        min = 1,
                        max = 100, 
                        value = c(1,100),
                        step = 5)
          )
        )
      )
    ), 
    
    mainPanel(
      withSpinner(
        leafletOutput("map", width = "100%", height = "90vh")
      )
    )
  )
)
 