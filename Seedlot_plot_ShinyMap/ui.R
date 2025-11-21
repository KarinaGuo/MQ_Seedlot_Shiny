library(tidyr) # data wrangling
library(raster) # Map
library(shiny) # shiny base package
library(shinycssloaders) # load symbol
library(leaflet) # Map
library(shinyBS) # layout
library(waiter) # loading sign reactively

setwd("~/Uni/Doctorate/Samples/Seedlot_plot_data/")
LoadedinData <- read.csv("data/final_seedloty_plot.csv")

fluidPage(
  useWaiter(), # loading symbol
  titlePanel("Melaleuca quinquenervia assayed seedlots"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("latitude", "Latitude: ", value = "-33.8688"),
      textInput("longitude", "Longitude: ", value = "151.2093"),
      actionButton("go", "Go to location"),
      
      hr(),
      h4("Seedlot filters", style = "text-align:center;"),
      
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
        condition = "input.filter_geno",
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
            condition = "input.filter_seedling_numb",
            sliderInput("seedlNumbFilter",
                        "Number of seedlings scored in either method:",
                        min = 1,
                        max = 100, 
                        value = c(1,100),
                        step = 5)
          )
        )
      ),
    
    ## SDMs - current and future projection
    hr(),
    h4("Species distribution model (SDM)", style = "text-align:center;"),
    
    radioButtons("overlay_SDM", "Toggle SDMs",
                 choices = c("Show" = "show",
                             "Hide" = "hide"),
                 selected = "hide"
    ),
    
    
    conditionalPanel(condition = "input.overlay_SDM == 'show'",
                     radioButtons(
        "layer_curr", strong("Choose Layer:"),
        choices = c("Melaleuca quinquenervia" = "MQuin",
                    "Myrtle rust" = "MR",
                    "Intersection" = "Intersection"),
        selected = "MQuin"
      )
    )
    ),
    
    
    ## Main map
    mainPanel(
      withSpinner(color="grey60",
                  leafletOutput("map", width = "100%", height = "90vh")
      )
    )
  )
)
 