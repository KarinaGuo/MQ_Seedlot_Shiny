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
library(httr)
library(jsonlite)

setwd("~/Uni/Doctorate/Ch Seedlot_plot_data/")
LoadedinData <- read.csv("data/final_seedloty_plot.csv")

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

# Tab: Seedlot plot #######################################################################

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
      ),
       radioButtons("MRMQ_Obs", "Show present distribution?",
                    choices = c("Show" = "show_obs",
                                "Hide" = "hide_obs"),
                    selected = "hide_obs"),
    ),
    
    hr(),
    uiOutput("click_info")
    ),
    
    ## Main map
    mainPanel(
      withSpinner(color="grey60",
                  leafletOutput("map", width = "100%", height = "70vh"),
      ),
   
      ## Add generate report button
      hidden(textInput("have_selection", "sel", value = "FALSE")),
      conditionalPanel(condition = "input.have_selection == 'TRUE'",
                       br(),
                       actionButton("report_gen_butt","Write selected seedlot report")
                       ),
      
      ## Add table of selected seedlots
      useShinyjs(),
      #hidden(textInput("have_selection", "sel", value = "FALSE")),
      conditionalPanel(condition = "input.have_selection == 'TRUE'",
                       h3("Selected seed lots"),
                       p("Click on seed lots to remove from selection"),
                       br(),
                       h4("Rust assay score of seed lots"),
                       DTOutput("marker_table_1"),
                       hr(),
                       h4("Genomic prediction score of seed lots"),
                       DTOutput("marker_table_2")
                       )

    )
  )
)
),

# Tab: Site risk ###############################################

tabPanel("Site risk",
         tags$head(
           tags$style(HTML("
    .disabled-item {
      color: #999999;
      text-decoration: line-through;
      opacity: 0.6;
    }
    .clickable {
      cursor: pointer;
    }
  ")),
           
           tags$script(HTML("
      $(document).on('click', '.clickable', function() {
        let id = $(this).attr('id');
        $(this).toggleClass('disabled-item');
        
        let isDisabled = $(this).hasClass('disabled-item') ? 1 : 0;
        Shiny.setInputValue(id + '_disabled', isDisabled, {priority: 'event'});
      });
  "))
         ),
         
         fluidRow(
           hr(),
           column(6,
                  div(style = "display:flex; flex-direction: column; align-items: center;",
                      textInput("site_name", "Site name (for report):", value = ""),
                      textInput("latitude_risk", "Site Latitude:", value = ""),
                      textInput("longitude_risk", "Site Longitude:", value = "")
                  )
           ),
           
           column (6,
                   p("Click to select or type in location decimal coordinates"),
                   leafletOutput("map_siterisk", height = "30vh")
                   ),
           hr(),
           
           column(9,
             p(em("Hover over titles for more information", style = "text-align:center;")),
             p(em("Press on any score to remove from calculation", style = "text-align:center;"))
             ),
             
           column(6,
                  h3("Site scoring"),
                  
                  p(id="SDM_curr_p", class="clickable", strong("Current SDM Overlap Score: "), textOutput("SDM_curr_overlap_score_UI", inline = TRUE)),
                  p(id="SDM_fut_p", class="clickable", strong("Future SDM Overlap Score (2021-2040 SSP126): "), textOutput("SDM_fut_overlap_score_UI", inline = TRUE)),
                  
                  div(
                    span(id="Site_DisPres_p", class="clickable", strong("Site Disease presence: ")),
                    title = "Have the site been observed to have high diease presence?",
                    radioButtons("Site_DisPres", NULL, 
                                 choices = c("Low" = "low_site_dispres",
                                             "High" = "high_site_dispres",
                                             "Unknown" = "unknown_site_dispres"),
                                 selected = "unknown_site_dispres")
                  ),
                  
                  div(
                    span(id="Water_pres_p", class="clickable", strong("Water body nearby? ")), 
                    title = "Is there a source of water nearby? \n It increases relative humidity and thus renders greater MR risk",
                    radioButtons("Water_pres", NULL,
                                 choices = c("Yes" = "pres_water",
                                             "No" = "notpres_water",
                                             "Unknown" = "unknown_water"),
                                 selected = "unknown_water")
                  ),
                  
                  div(
                    span(id="Edge_eff_p", class="clickable", strong("Edge effect or/and foot traffic present? ")), 
                    title = "Is the population fragmented and sees high people traffic? \n Fragmentation or open forest may increase spore flux",
                    radioButtons("Edge_eff", NULL,
                                 choices = c("Yes" = "pres_edge",
                                             "No" = "notpres_edge",
                                             "Unknown" = "unknown_edge"),
                                 selected = "unknown_edge")
                  ),
                  
                  div(
                    span(id="Time_lastburn_p", class="clickable", strong("Time since last burnt (>2010): ")),
                    title = "If the site was burnt recently, the population exhibits higher abiotic stress",
                    sliderTextInput("Time_lastburn", NULL,
                                    choices = c("No Burn", seq(from = 2010, to = year(Sys.Date()))),
                                    grid = TRUE,
                                    selected="No Burn",
                                    dragRange=FALSE)
                  ),
                  
                  div(
                    span(id="Burn_severity_p", class="clickable", strong("Severity of burn: ")),
                    title = "How severe was the most recent burn?",
                    radioButtons("Burn_severity", NULL, 
                                 choices = c("Low" = "low_burn_severity",
                                             "Moderate" = "mod_burn_severity",
                                             "High" = "high_burn_severity",
                                             "Unknown" = "unknown_burn_severity"),
                                 selected = "unknown_burn_severity")
                  )
           ),
           
           column(6,
                  h3("Disease scoring"),
                  h4("Were any individuals genotyped (using DArTag)"),
                  
                  div(
                    span(id="Adult_genompredres_p", class="clickable", strong("Adult genomic prediction of resistance: ")),
                    title = "If genotyping has been done across adults are there resistance alleles in the population (% resistant in lower third)",
                    radioButtons("Adult_genompredres", NULL,
                                 choices = c("Low" = "low_adult_resistance",
                                             "Moderate" = "mod_adult_resistance",
                                             "High" = "high_adult_resistance",
                                             "Unknown" = "unknown_adult_resistance"),
                                 selected = "unknown_adult_resistance")
                  ),
                  
                  div(
                    span(id="Seedling_genompredres_p", class="clickable", strong("Seedling genomic prediction of resistance: ")),
                    title = "If genotyping has been done across seedlings are there resistance alleles in the population (% resistant in lower third)",
                    radioButtons("Seedling_genompredres", NULL,
                                 choices = c("Low" = "low_seedl_resistance",
                                             "Moderate" = "mod_seedl_resistance",
                                             "High" = "high_seedl_resistance",
                                             "Unknown" = "unknown_seedl_resistance"),
                                 selected = "unknown_seedl_resistance")
                  ),
                  
                  conditionalPanel(
                    condition = "any(!input.Seedling_genompredres == 'unknown_seedl_resistance', !input.Adult_genompredres == 'unknown_adult_resistance')",
                    div(
                      span(id="Geno_conf_p", class="clickable", strong("Genotyping confidence: ")),
                      title = "How confident does the genotyping reflect the site? \n How many individuals were genotyped?",
                      radioButtons("Geno_conf", NULL,
                                   choices = c("Low" = "low_geno_conf",
                                               "Moderate" = "mod_geno_conf",
                                               "High" = "high_geno_conf",
                                               "Unknown" = "unknown_seedl_resistance"),
                                   selected = "unknown_seedl_resistance")
                    )
                  )),
           column(12,
                  hr(),
                  
                  div(style = "display:flex; flex-direction: column; align-items: center;",
                      p(span("Risk score", uiOutput("risk_score", container = span), style = "font-weight: bold; font-size: 1em;")),
                      br(),
                      actionButton("calculate_score", "Generate"),
                      
                  ),
                  hr(),
           )
         )
),


# Tab: Future Projections ###############################################
tabPanel("Future projections",
         sidebarLayout(
           sidebarPanel(
             radioButtons("layer_fut", "Choose Layer to Display:",
                          choices = c("Melaleuca quinquenervia" = "MQuin_fut",
                                      "Myrtle rust" = "MR_fut",
                                      "Intersection" = "intersection_fut"),
                          selected = "MQuin_fut"),
             
             br(),  # Add space between inputs
             
             tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), # Removing the minor ticks in the slider input
             sliderTextInput("year_clust", "Average climate year cluster:",
                             choices = years_clust,
                             selected = "2021-2040",
                             grid = TRUE),
             
             br(),  # Add space between inputs
             
             tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), # Removing the minor ticks in the slider input
             sliderTextInput("ssp", "Climate scenario (SSP):",
                             choices = ssp_list,
                             selected = "126",
                             grid = TRUE),
             
             br(),
             radioButtons("fut_show_both", "Overlay present distribution difference?",
                          choices = c("Show" = "fut_show_pres",
                                      "Hide" = "fut_hide_pres"),
                          selected = "fut_hide_pres")
           ),
           
           mainPanel(
             withSpinner(color="grey60",
               leafletOutput("map_2", height = "90vh"))
           )
         )
),

# Tab: Generate report ###############################################

tabPanel("Summary results",
        titlePanel("Generated Report:"),
        
        useShinyjs(), 
         sidebarPanel(p("Insert nav")),
         mainPanel(
           #leafletOutput("map_rep"),
           
           
           h3("No seed lots selected from map", id = "no_selection"),
           div(
             id = "selection_panel",
                              
             h3("Selected Seed Lots", style = "text-align:center;"),
             br(),
             
             h4("Table of Selected Seed lot"),
             h5("Rust assay score of seed lots"),
             DTOutput("marker_table_1_repo"),
             hr(),
             h5("Genomic prediction score of seed lots"),
             DTOutput("marker_table_2_repo"),
             hr(),br(),
             
             h4("Plot of seedlots characteristics"),
             h5("Rust assay score of seed lots"),
             plotOutput("seedlot_rustass_plot_geomridge", height = "100%"),
             hr(),
             h5("Genomic prediction score of seed lots"),
             plotOutput("seedlot_genompred_plot_geomridge", height = "100%"),
             hr(),br(),
             
             h4("Plot of seedlots selected and marker of site location"),
             leafletOutput("map_rep", height = "60vh"),
             
             div(
               id = "site_info", style = "display:none;",
               
               br(),hr(),br(),
               
               h3("Site Information", style = "text-align:center;"),
               
               p(strong("Site selected: "), textOutput("site_name_rep",  inline = TRUE)),
               p(strong("At location: "), textOutput("latitude_risk", inline = TRUE), textOutput("longitude_risk",  inline = TRUE)),
               br(),hr(),br(),
               
               h4("Survey scores and site risk recorded"),
               p(em("Hover over titles for more information", style = "text-align:center;")),
               p(em("Press on any score to remove from calculation", style = "text-align:center;")),
                
               h3("Site scoring"),
               
               span(strong("Current SDM Overlap Score: "),  textOutput("SDM_fut_rep", inline = TRUE), "\n"),
               span(strong("Future SDM Overlap Score (2021-2040 SSP126): "),  textOutput("score_SDM_fut", inline = TRUE)),
               div(span(title = "Has the site been observed to have high disease presence?", strong("Site Disease presence: "), textOutput("score_Site_DisPres_rep", inline = TRUE))),
               div(title = "Is there a source of water nearby? \n It increases relative humidity and thus renders greater MR risk", span(strong("Water body nearby? ")),textOutput("score_Water_pres_rep", inline = TRUE)),
               div(title = "Is the population fragmented and sees high people traffic? \n Fragmentation or open forest may increase spore flux", span(strong("Edge effect or/and foot traffic present? ")),textOutput("score_Edge_eff_rep", inline = TRUE)),
               div(title = "If the site was burnt recently, the population exhibits higher abiotic stress", span(strong("Time since last burnt (>2010): ")), textOutput("Time_lastburn_p_rep", inline = TRUE)),
               div(title = "How severe was the most recent burn?", span(strong("Severity of burn: ")), textOutput("score_Burn_severity", inline = TRUE)),
               
               h3("Disease scoring"),
               h4("Were any individuals genotyped (using DArTag)"),
               
               div(title = "If genotyping has been done across adults are there resistance alleles in the population (% resistant in lower third)", span(strong("Adult genomic prediction of resistance: ")),textOutput("Adult_genompredres_p_rep", inline = TRUE)),
               div(title = "If genotyping has been done across adults are there resistance alleles in the population (% resistant in lower third)", span(strong("Seedling genomic prediction of resistance: ")),textOutput("Seedling_genompredres_p_rep", inline = TRUE)),
               conditionalPanel(
                 condition = "any(!input.Seedling_genompredres == 'unknown_seedl_resistance', !input.Adult_genompredres == 'unknown_adult_resistance')",
                   div(title = "How confident does the genotyping reflect the site? \n How many individuals were genotyped?", strong("Genotyping confidence: ", textOutput("Geno_conf_p_rep", inline = TRUE))),
                   ),
               
               h3("Final score"),
               #span("Site risk score: ", textOutput ("risk_score", inline = TRUE))
             ),
    
               
               
               p("Inputted survey res"),
               
               br(),hr(),br(),
               # 
               # h3("Future prediction", style = "text-align:center;"),
               # p("Plot of intersection of future climate grid of all ssps and years"),
               # 
               # br(),hr(),br(),
               # 
               # h3("Simulations of restoration", style = "text-align:center;"),
               # p("Plot of intersection of future climate")
               
               
               # p("Information on what the different levels of resistance means and measurement"),
               # p("Drop down toggle on more information"),
               # br(),hr(),br(),
               
           )
           
           
           )

         )
  
  )
)