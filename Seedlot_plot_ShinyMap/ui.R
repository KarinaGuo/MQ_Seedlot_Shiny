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
  useWaiter(),  # Loading spinner
  titlePanel("Assayed Seedlots"),
  tabsetPanel(id = "tabs",
              
              ## Information page
           tabPanel("Information on plots",
                    fluidPage(
                      titlePanel("A short How To:"),
                      
                      sidebarPanel(p("Insert nav, keywords, definitions")),
                      
                      mainPanel(
                        h3("Introduction", style = "text-align:center;"),
                        br(),hr(),br(),
                        
                        p("Introduction on MR, use, seed lot, resistance"),
                        p("Choose area of study, select seed lots"),
                        br(),hr(),br(),
                        
                        h3("Seed lot information", style = "text-align:center;"),
                        p("Information on what the different levels of resistance means and measurement"),
                        p("Drop down toggle on more information"),
                        br(),hr(),br(),
                        
                        h3("Considerations when selecting seedlots", style = "text-align:center;"),
                        p("Amount of seedlings, genetic diversity, seed lot reserves, age of seed lot, Resistance needed"),
                        p("Drop down toggle on more information of GD, include FsT plot")
                        # p("p creates a paragraph of text."),
                        # p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", style = "font-family: 'times'; font-si16pt"),
                        # strong("strong() makes bold text."),
                        # em("em() creates italicized (i.e, emphasized) text."),
                        # br(),
                        # code("code displays your text similar to computer code"),
                        # div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
                        # br(),
                        # p("span does the same thing as div, but it works with",
                        #   span("groups of words", style = "color:blue"),
                        #   "that appear inside a paragraph.")
                      )
                    )
           ),
## Main Tab
tabPanel("Seed lot map",

fluidPage(
  useWaiter(), # loading symbol
  titlePanel("Melaleuca quinquenervia assayed seed lots"),
  
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
)
  )
)