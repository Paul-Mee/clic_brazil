
# Required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(gghighlight)) install.packages("gghighlight", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(mapview)) install.packages("mapview", repos = "http://cran.us.r-project.org")
if(!require(shiny.i18n)) install.packages("shiny.i18n", repos = "http://cran.us.r-project.org")
if(!require(markdown)) install.packages("markdown", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(gsubfn)) install.packages("gsubfn", repos = "http://cran.us.r-project.org")
if(!require(rgdal)) install.packages("rgdal", repos = "http://cran.us.r-project.org")
if(!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")
if(!require(rdrop2)) install.packages("rdrop2", repos = "http://cran.us.r-project.org")
if(!require(plotROC)) install.packages("plotROC", repos = "http://cran.us.r-project.org")
if(!require(EpiEstim)) install.packages("EpiEstim", repos = "http://cran.us.r-project.org")
if(!require(caTools)) install.packages("caTools", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(zoo)) install.packages("zoo", repos = "http://cran.us.r-project.org")
if(!require(openxlsx)) install.packages("openxlsx", repos = "http://cran.us.r-project.org")
if(!require(shinyBS)) install.packages("shinyBS", repos = "http://cran.us.r-project.org")
if(!require(thematic)) install.packages("thematic", repos = "http://cran.us.r-project.org")
if(!require(bslib)) install.packages("bslib", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# Load global variables
app_title <- "COVID-19 Local Information Comparison (CLIC Brazil)"

options(shiny.sanitize.errors = TRUE)

start_time <- Sys.time()
load(file = "./input_data/app_files.RDS")
end_time <- Sys.time()
new_time <- end_time - start_time
new_time


basemap <-  leaflet(data = spatial) %>%
  htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomright' }).addTo(this)}") %>% 
  addProviderTiles(providers$CartoDB.Voyager) %>%
  addCircleMarkers(col = ~pal(cum_cases), 
                   opacity = 0.9,
                   radius = 5, 
                   weight=1,
                   popup= ~paste0("<b>", Area, "</b>", 
                                  "<br/>",                   
                                  popup, ": ",
                                  round(cum_cases, 1),
                                  "<br/>",
                                  "Date: ",
                                  DateUntil)) %>%
  # addTiles() %>%
  addLegend("bottomright",
            pal = pal,
            values = ~log1p(cum_cases),
            labFormat = labelFormat(
              transform=function(x) round(expm1(x),1)),
            opacity = 0.7, 
            title = "Cases")

cumulative_plot <- function(Brazil_cases_time, plot_date) {
  plot_df = dplyr::filter(Brazil_cases_time, date_end <=plot_date)
  g1 = ggplot(plot_df, aes(x = date_end, y = cum_cases)) +
    geom_line(size=1.2) +
    theme_minimal() +
    scale_x_date(date_labels = "%b %d",
                 limits = c(min(Brazil_cases_time$date_end),
                            DateUntil)) +
    xlab("Date") + ylab("Cumulative cases")
  g1
}

municipal_plot <- function(Brazil_cases_areas, plot_date) {
  plot_df = dplyr::filter(Brazil_cases_areas, date_end <=plot_date)
  g1 = ggplot(plot_df, aes(x = date_end, y = Area)) +
    geom_line(size=1.2) +
    theme_minimal() +
    scale_x_date(date_labels = "%b %d",
                 limits = c(min(Brazil_cases_time$date_end),
                            DateUntil)) +
    xlab("Date") + ylab("Municipalities with > 50 cases")
  g1
}

# areas <- as.character(Brazil_cases_sp$Area[Brazil_cases_sp$cum_cases > 100])
# data_available <- data.table(areas=unique(sort(areas[!is.na(areas)])))

thematic::thematic_shiny()

ui <- navbarPage(
  
  # head(includeCSS("styles.css")),
  
  theme=bs_theme(version=4,
                 bootswatch = "minty",
                 base_font = font_google("Roboto")) %>%
    bs_add_rules(
      sprintf('@import "%s"', 
              "styles.css")
    ) ,
  title = div(
    a(img(src="cmmid_logo.png",
          height="50px"),
      href="https://cmmid.lshtm.ac.uk/"),
    span(app_title, style="line-height:50px")),
  
  tabPanel(title="Home",
           sidebarLayout(
             sidebarPanel(
               width=3,
               
               pickerInput(inputId = "outcome_select",
                           label="Select Variable",
                           choices = c('Case incidence',
                                       'Cases',
                                       "Death incidence",
                                       "Deaths"),
                           selected = 'Cases',
                           multiple = FALSE),
               sliderInput(inputId= "plot_date",
                           label = "Select date",
                           min = as.Date(min_date,"%Y-%m-%d"),
                           max = as.Date(max_date,"%Y-%m-%d"),
                           value = as.Date(max_date),
                           step=days(1),
                           timeFormat = "%d %b", 
                           animate=TRUE),
               box(width = NULL,
                   status = "primary",
                   solidHeader = FALSE,
                   collapsible = FALSE,
                   infoBoxOutput("total_cases", width = 12)),
               br(),
               box(width = NULL,
                   status = "primary",
                   solidHeader = FALSE,
                   collapsible = FALSE,
                   infoBoxOutput("total_munic", width = 12)),
               br(),
               plotOutput("cumulative_plot", 
                          height="200px", 
                          width="100%"),
               br(),br()
               
               
             ),
             
             mainPanel( width=9,
                        tabPanel("",
                                 leafletOutput("map",
                                               width="95%",
                                               height=450)
                                 
                        )
                        
             )
           )
  ),
  tabPanel(title="Local area comparison",
           
           sidebarLayout(
             sidebarPanel(
               width=4,
               
               pickerInput("region_select",
                           label="Select State",
                           choices = sort(unique(
                             as.character(Brazil_cases_sp$Name))),
                           selected = "Sao Paulo",
                           multiple = FALSE),
               
               pickerInput("area_select", 
                           label="Select Municipality",   
                           choices = dplyr::filter(data_available,
                                                   stringr::str_detect(
                                                     areas, '_SP')),
                           selected = NULL,
                           multiple = FALSE), 
               
               
               pickerInput("outcome_select2",
                           "Select Measure",
                           choices = c("Cumulative case incidence",
                                       "Cumulative death incidence"),
                           selected = c("Cumulative case incidence"),
                           multiple = FALSE),
               br(),
             ),
             
             mainPanel(
               tabPanel("",
                        plotOutput("p1", height="400px"),
                        htmlOutput("selected_text"),
                        br(), br(),
                        br(), br(),
                        plotOutput("p2",
                                   height="400px"),
                        br(),
                        htmlOutput("intervention_text"),
                        br(),br(),br()
               )
             )
           )
  ),
  tabPanel(title="Trends",
           
           sidebarLayout(
             sidebarPanel(
               width=3,
               
               pickerInput("region_select2",
                           label="Select National or a State",
                           choices = c("National",
                                       sort(
                                         unique(
                                           as.character(
                                             Brazil_cases_sp$Name)))),
                           selected = "National",
                           multiple = FALSE),
               
             ),
             
             mainPanel(
               tabPanel("",
                        plotOutput("p3", height="900px"),
                        br(), br(), br()
                        
               )
             )
           )
  ),
  
  tabPanel(title="Rt plot",
           
           sidebarLayout(
             sidebarPanel(
               width=4,
               
               pickerInput("region_select_rt1",
                           label="Select a City-State",
                           choices = city_states,
                           selected = "Manaus_AM",
                           multiple = FALSE),
               pickerInput("region_select_rt2",
                           label="Select a City-State",
                           choices = city_states,
                           selected = "Rio de Janeiro_RJ",
                           multiple = FALSE),
               pickerInput("region_select_rt3",
                           label="Select a City-State",
                           choices = city_states,
                           selected = "São Caetano do Sul_SP",
                           multiple = FALSE),
               pickerInput("region_select_rt4",
                           label="Select a City-State",
                           choices = city_states,
                           selected = "São Paulo_SP",
                           multiple = FALSE),
               
               br(),
               
               
             ),
             
             mainPanel(
               tabPanel("",
                        plotOutput("p_rt",
                                   height="800px"),
                        tags$br(), tags$br(),
                        tags$h3("Rt plot"),
                        tags$p("This plot shows changes in the average
                                     reproduction number (Rt) within the chosen
                                     municipalities over time. The reproduction
                                     number estimates the average number of
                                     secondary cases generated by one infectious
                                     person and values below 1.0 indicate that
                                     the number of new infections is reducing.
                                     Estimates were made using the APE renewal
                                     approach from Parag et al. 2019 . We assume
                                     a mean serial interval (time between
                                     successive transmission events) of 2.97
                                     days (standard deviation 3.29) based on
                                     recent data from Brazil Prete et al. 2019.
                                     Reproduction number estimates are updated
                                     daily but only produced up to 14 days ago
                                     due to delays in diagnosing and reporting
                                     more recent cases that would lead to
                                     underestimates of Rt if more recent data
                                     were used. We assume testing, case
                                     confirmation and reporting efforts are
                                     constant over time. Increases or decreases
                                     will increase of decrease Rt respectively.
                                     We also assume that all reported cases are
                                     locally acquired and not imported. In order
                                     to correct for under-reporting of cases on
                                     particular days of the week (particularly
                                     on Sundays and Mondays) we applied a
                                     smoothing function to the incident case
                                     numbers which re-distributed the daily case
                                     counts in order to get roughly equal numbers
                                     of cases reported for each day of the week.
                                     The following limitations in the method
                                     should be noted:"),
                        tags$li("We only generate Rt estimates for places
                              that have reported data for at least 30 days and
                              which have a total of at least 100 cases reported."),
                        tags$li("We did not account for uncertainty in
                              the serial interval. In addition, our approach
                              does not account for negative support in the
                              serial interval. This means our estimates may be
                              overly precise and/or biased."),
                        tags$li("We used a mean shift to map reported cases
                              back to the date of infection. This approach is
                              known to produce Rt estimates that are biased
                              downwards when Rt is greater than 1 and biased
                              upwards when Rt is less than 1. We also did not
                              account for uncertainties in the delay from onset
                              to report or from infection to onset our estimates."),
                        tags$li("The APE renewal approach from Parag et al.
                              optimises the window across the whole time series
                              considered. This means that the smoothness of
                              historic estimates may change as new data becomes
                              available and in areas with prolonged stable
                              periods real time estimates will be biased towards
                              smoother changes over time."),
                        tags$li("To account for right truncation of
                              reported cases we only produce estimates up to 10
                              days ago. However this data may still be incomplete
                              and therefore our latest estimates may be biased
                              downward."),
                        
                        br())
             )
           )
  ),
  
  tabPanel(title="Model forecast",
           
           fluidRow(
             column(7,
                    mapviewOutput("map2",
                                  width="100%",
                                  height=550)),
             column(4, plotOutput("aucplot",
                                  width="100%",
                                  height=550))
           ),
           tags$br(), tags$br(),
           tags$h3("Predicted probabilities map"),
           tags$p("This section predicts the probability that
                   each municipality will experience a new record
                   incidence in the future, i.e. that it has not yet
                   passed its peak incidence. More specifically, the
                   map shows the estimated probability that a new
                   record incidence will occur in the next 30 days.
                   Darker colours in the map correspond to a higher
                   probability of setting a new record. In other words,
                   a high probability means that at least one day out
                   of the next 30 is likely to have an incidence higher
                   than any observed so far. Likewise, a small probability
                   means that the daily incidence over the next 30 days is
                   likely to stay less than previous highs."),
           tags$p("Model details. The probabilities are estimated by Cox
                   regression in the R software (the “coxph” function in
                   the “survival” package). The event being modelled is
                   the setting of a new record daily standardized incidence.
                   In general, each municipality has set more than one record
                   incidence since the start of its outbreak. The analysis
                   time is the number of days since the threshold number of
                   cases was met and the municipality started to be tracked.
                   The covariates, selected by likelihood ratio test, are:"),
           tags$li("difference between the latest daily incidence and the
                    record incidence,"),
           tags$li("as before, but based on the incidence 6 days ago,"),
           tags$li("as before, but based on the incidence 7 days ago,"),
           tags$li("state of Brazil (categorical variable)"),
           tags$li("socioeconomic development index (SDI)."),
           tags$p("Clustering within municipality is included as a
                   frailty term."),
           tags$p("To estimate the probability of a new record being set
                   over the next 30 days, the",
                  tags$a(href = 'https://stat.ethz.ch/R-manual/R-patched/library/survival/html/predict.coxph.html',
                         'predict'), "method for the",
                  tags$em("coxph"), "function is used. First, the expected
                   number of times that a record will be set over the next 30
                   days is estimated. Then, the estimated probability of
                   any new record being set in that time is calculated
                   as 1-exp(-1 x expected number of new records). This
                   takes into account the baseline hazard."),
           
           tags$h3("ROC curve"),
           tags$p("To assess the predictive performance of the model,
                   the data were divided into training and test datasets.
                   The training dataset was obtained by omitting the last
                   30 days of data from each municipality. Then, the above
                   Cox regression model was applied to the training dataset,
                   to predict the 30 days that had been omitted. For each
                   municipality, the prediction was whether or not a new
                   record high daily incidence was set. These predictions
                   were compared with the actual occurrence of new record
                   highs. These results are displayed in the form of a ROC
                   (receiver operating characteristic) curve."),
           tags$br(), tags$br()
  ),
  
  tabPanel(title="Download data",
           tags$h4("Download local area data"),
           numericInput("maxrows",
                        "Number of rows to show", 5),
           downloadButton("downloadCsv",
                          "Download data as CSV"),
           tags$br(), tags$br(),
           verbatimTextOutput("rawtable"),
           tags$br(),tags$br(),
           tags$h4("Download Rt data"),
           numericInput("maxrows2",
                        "Number of rows to show", 5),
           downloadButton("downloadCsv2",
                          "Download Rt data as CSV"),
           tags$br(), tags$br(),
           verbatimTextOutput("rawtable_rt"),
           tags$br(),tags$br(),
           tags$h4("Download covariates data"),
           numericInput("maxrows3",
                        "Number of rows to show", 5),
           downloadButton("downloadCsv3",
                          "Download covariates as CSV"),
           tags$br(), tags$br(),
           verbatimTextOutput("rawtable_covars"),
           tags$br(),tags$br()
  ),
  
  tabPanel(title="About this site",
           tags$div(
             tags$h4("Data Sources"),
             tags$h5("(1) COVID-19 Cases"),
             uiOutput("text1"),
             tags$br(),
             tags$h5("(2) Age distribution of COVID-19 cases"),
             uiOutput("text2"),
             tags$br(),
             tags$h5("(3) Age distribution data"),
             uiOutput("text3"),
             tags$br(),
             tags$h5("(4) COVID-19 Intervention data"),
             uiOutput("text4"),
             tags$br(),
             
             tags$h4("Methods"),
             tags$h5(uiOutput("age_label3")),
             uiOutput("text5"),
             tags$br(),
             
             tags$h5("Covariates"),
             uiOutput("text6"),
             
             tags$br(),
             
             tags$h4("Authors"),
             uiOutput("text15"),
             
             tags$h4("Funding"),
             uiOutput("text14"),
             
             
             tags$br(), tags$br(),
             tags$h4("Disclaimer"),
             uiOutput("text7"),
             
             tags$br(),
             tags$h4("Contact"),
             tags$a(href="oliver.brady@lshtm.ac.uk",
                    "Oliver.Brady@lshtm.ac.uk"),
             "London School of Hygiene and Tropical Medicine",
             "Keppel Street",
             "London",
             "WC1E 7HT",
             tags$br(), tags$br(), tags$br()
           )
  ),
  tabPanel(title="How to use",
           tags$div(
             tags$h3("COVID-19 Local Information Comparison"),
             tags$h4("(CLIC Brazil)"),
             tags$br(),
             tags$h4("Introduction"),
             uiOutput("text8"),
             
             tags$br(),
             tags$h4("National Overview tab"),
             uiOutput("text9"),
             
             tags$br(),
             tags$h4("Local area comparison tab"),
             uiOutput("text10"),
             
             tags$br(),
             tags$h5("Cumulative case plot"),
             imageOutput(outputId="saopaulo",
                         inline=TRUE),
             
             tags$br(),tags$br(),
             tags$h5("Timing of interventions plot"),
             uiOutput("text11"),
             tags$br(),
             imageOutput("boxplot",
                         inline=TRUE),
             
             tags$br(),tags$br(),
             tags$h4("Trends tab"),
             uiOutput("text12"),
             tags$br(),
             imageOutput("trends",
                         inline=TRUE),
             tags$br(),tags$br(),
             
             tags$h4("Data download tab"),
             uiOutput("text13"),
             
             tags$br(), tags$br(), tags$br()
           )
  )
  
)

server <- function(input, output, session) {
  
  # Tab 1 - HOME
  spatial_reactive_db <- reactive({
    
    db <- Brazil_cases_sp %>%
      dplyr::filter(date_end <= input$plot_date)
    
    if (input$outcome_select=="Cases") { 
      
      # trim to just latest number of cumulative cases / incidence
      db <- aggregate(cum_cases ~ Area + X + Y, 
                      data = db, 
                      FUN = max)
      # Rename outcome
      names(db)[4] <- "outcome"
      
      # Create size for circle markers
      db$size <- log1p(db$outcome*3)
      
      # make sf object
      db <- st_as_sf(db, coords = c("X", "Y"))
      
      # set crs
      st_crs(db)   <- 4326
      
      # Convert to spatial points data frame
      spatial      <- as(db, "Spatial")
      return(spatial)
    }
    if (input$outcome_select=="Case incidence") { 
      db            <- aggregate(standardised_cases ~ Area + X + Y, 
                                 data = db,
                                 FUN = max)
      names(db)[4]  <- "outcome"
      db$size       <- sqrt(db$outcome*4)
      db            <- st_as_sf(db, coords = c("X", "Y"))
      st_crs(db)    <- 4326
      spatial       <- as(db, "Spatial")
      return(spatial)
    }
    if (input$outcome_select=="Death incidence") { 
      db            <- aggregate(standardised_deaths ~ Area + X + Y, 
                                 data = db,
                                 FUN = max)
      names(db)[4]  <- "outcome"
      db$size       <- sqrt(db$outcome*4)
      db            <- st_as_sf(db, coords = c("X", "Y"))
      st_crs(db)    <- 4326
      spatial       <- as(db, "Spatial")
      return(spatial)
    }
    if (input$outcome_select=="Deaths") { 
      db            <- aggregate(cum_deaths ~ Area + X + Y, 
                                 data = db,
                                 FUN = max)
      names(db)[4]  <- "outcome"
      db$size       <- sqrt(db$outcome*4)
      db            <- st_as_sf(db, coords = c("X", "Y"))
      st_crs(db)    <- 4326
      spatial       <- as(db, "Spatial")
      return(spatial)
    }
  })
  
  output$map <- renderLeaflet({
    basemap
  })
  
  toListen <- reactive({
    list(input$outcome_select, input$plot_date)
  })
  
  observeEvent(toListen(), {
    
    popup  <- input$outcome_select
    
    my_bks <- unique(quantile(spatial_reactive_db()$outcome, 
                              probs=seq(0,1, by=0.2)))
    
    pal <- colorBin("YlOrBr", domain=spatial_reactive_db()$outcome, 
                    bins=my_bks)
    
    leafletProxy("map") %>% 
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      
      addCircleMarkers(data = spatial_reactive_db(), 
                       weight = 1, 
                       radius = 5, 
                       fillOpacity = 0.1, 
                       color = ~pal(outcome),
                       popup= ~paste0("<b>", Area, "</b>", 
                                      "<br/>",
                                      popup, ": ",
                                      round(outcome, 1),
                                      "<br/>",
                                      "Date: ",
                                      input$plot_date)) %>%
      addLegend("bottomright",
                pal = pal,
                values = spatial_reactive_db()$outcome,
                # labFormat = labelFormat(
                # transform=function(x) round(expm1(x),1)),
                opacity = 0.7,
                title = input$outcome_select)
  })
  
  output$total_cases <- renderInfoBox({
    infoBox(
      title = paste("Cases up to", 
                    as.character(format(as.Date(DateUntil), "%d %b %Y")),
                    ":", sep=" "), 
      fill = FALSE,
      value =  format(as.numeric(total), big.mark=","),
      icon = icon("user-friends"),
      color = "black")
  })
  
  output$total_munic <- renderInfoBox({
    infoBox(
      title = paste("Municipalities with more than 50 cases", 
                    ":", sep=" "), 
      fill = FALSE,
      value =  format(as.numeric(length(Brazil_cases_cum_cases$Area)), 
                      big.mark=","),
      icon = icon("map-marked-alt"),
      color = "black")
  })
  
  formatted_date <- reactive({
    as.Date(input$plot_date)
  })
  
  output$cumulative_plot <- renderPlot({
    p1 <- cumulative_plot(Brazil_cases_time, formatted_date())
    p2 <- municipal_plot(Brazil_cases_areas, formatted_date())
    
    ggarrange(p1, p2, nrow=1, ncol=2)
  })
  
  # Tab 2 - LOCAL AREA COMPARISON
  
  observeEvent(input$region_select, {
    areas2 <- as.character(
      Brazil_cases_sp$Area[as.character(Brazil_cases_sp$Name) ==
                             input$region_select & 
                             Brazil_cases_sp$cum_cases > 100])
    
    data_available2 <- unique(sort(areas2[!is.na(areas2)]))
    
    updatePickerInput(session = session, 
                      inputId = "area_select", 
                      choices = data_available2, 
                      selected = NULL)
    
    
  }, ignoreInit = TRUE)
  
  area_db1 <- reactive({
    
    # compariosn matrix
    comp_mat <- cbind(x_dat[x_dat$Area == input$area_select,
                            "standardised_cases"] >= timeSUM[, 1],
                      x_dat[x_dat$Area == input$area_select,
                            "standardised_cases"] >= timeSUM[, 2],
                      x_dat[x_dat$Area == input$area_select,
                            "standardised_cases"] >= timeSUM[, 3])
    comp_mat_sum <- as.logical(apply(comp_mat, 2, median))
    
    if(all(comp_mat_sum)){OB_sum = "above average"}
    if(sum(comp_mat_sum) < 3){OB_sum = "average"}
    if(sum(comp_mat_sum) < 2){OB_sum = "below average"}
    
    if(input$outcome_select2=="Cumulative death incidence") {
      x_dat %<>%
        dplyr::mutate(Region=str_sub(Area, start= -2),
                      outcome=standardised_cases,
                      outcome=outcome  * 1000)
    }
    if(input$outcome_select2=="Cumulative case incidence"){
      x_dat %<>%
        dplyr::mutate(Region=str_sub(Area, start= -2),
                      outcome=standardised_cases)
    }
    
    of1 <- dplyr::filter(x_dat, Area != input$area_select) %>%
      select(Days_since_start, outcome, Area) %>%
      group_by(Days_since_start) %>%
      summarise(ymax=quantile(outcome, probs=0.975),
                ymin=quantile(outcome, probs=0.025),
                outcome=quantile(outcome, probs=0.5),
                Area="Other States",
                Region="Other States")
    of2 <- dplyr::filter(x_dat, Area == input$area_select) %>%
      select(Days_since_start, outcome, Area) %>%
      mutate(Region=Area,
             ymax=NA,
             ymin=NA)
    of3 <- dplyr::filter(x_dat, Name == input$region_select) %>%
      select(Days_since_start, outcome, Area) %>%
      group_by(Days_since_start) %>%
      summarise(ymax=quantile(outcome, probs=0.975),
                ymin=quantile(outcome, probs=0.025),
                outcome=quantile(outcome, probs=0.5),
                Area=input$region_select,
                Region=input$region_select)
    
    list(of1=of1, 
         of2=of2, 
         of3=of3,
         of4=OB_sum)
    
  })
  
  output$selected_text <- renderText({
    
    paste("After accounting for different population sizes",
            "and age structures of COVID-19 affected municipalities,",
            "the outbreak in",
            substr(input$area_select, 1, nchar(input$area_select)-3),
            "is currently tracking", "<b>", area_db1()$of4, "</b>",
            "compared to other areas in Brazil")
    
  })
  
  
  
  toListen2 <- reactive({
    list(input$region_select, input$area_select)
  })
  
  observeEvent(toListen2(), {
    
    output$p1 <- renderPlot({
      
      if(input$outcome_select2=="Cumulative death incidence") {
        my_label <- "Standardised death incidence per 10,000 \n residents (log scale)"
      }
      if(input$outcome_select2=="Cumulative case incidence"){
        my_label <- "Standardised case incidence per 10,000 \n residents (log scale)"
      }
      
      colorData <- c("red3", "grey60", "orange")
      
      ggplot(area_db1()$of1, aes(fill=Area)) +
        geom_ribbon(data=area_db1()$of1, 
                    aes(x=Days_since_start, 
                        ymin=ymin,
                        ymax=ymax),
                    alpha=0.40) +
        geom_ribbon(data=area_db1()$of3, 
                    aes(x=Days_since_start, 
                        ymin=ymin,
                        ymax=ymax,
                        fill=Area),
                    alpha=0.40) +
        geom_line(data=area_db1()$of2, 
                  aes(x=Days_since_start, 
                      y=outcome), 
                  colour="red3",
                  size=1) +
        theme_minimal() +
        scale_y_continuous(trans='log10') +
        scale_fill_manual(values = colorData, name = "") +
        xlab("Days since start of the outbreak (incidence above 1 case per 10,000 residents)") +
        labs(y=my_label) +
        ggtitle(input$outcome_select2) +
        theme(plot.title = element_text(hjust = 0.5)) + 
        theme(legend.position="bottom") +
        theme(legend.text=element_text(size=16),
              legend.title=element_text(size=14)) +
        theme(axis.text.x = element_text(size=9),
              axis.text.y = element_text(size=9),
              axis.title.x = element_text(size=12),
              axis.title.y = element_text(size=12)) +
        theme(strip.text.x = element_text(size=9)) +
        theme(plot.title = element_text(size=16)) 
      
    })
    
  })
  
  plotdb <- reactive({
    
    # precompute a variable that states whether interventions in 
    # the local area were later or earlier than the mean
    E_L <- Int_long[Int_long$Area ==
                      input$area_select, "Intervention_type"] >= 
      aggregate(Intervention_type ~ time, 
                Int_long,FUN = mean)$Intervention_type
    Int_long$PlotCol = "black"
    Int_long$PlotCol[Int_long$Area == input$area_select] = "red"
    
    # polygons for before and after outbreak began
    poly <- data.frame(y = c(min(Int_long$Intervention_type),
                             min(Int_long$Intervention_type),
                             rep(0, 4),
                             max(Int_long$Intervention_type),
                             max(Int_long$Intervention_type)),
                       x = c(0, 5, 5, 0, 0, 5, 5, 0),
                       Fcol = c(rep("blue", 4), rep("red", 4)))
    
    dbx <- list(of1=Int_long,
                of2=poly,
                of3=E_L,
                of4=z_dat)
  })
  
    output$p2 <- renderPlot({
      
      y_lab <- "Days since start of the outbreak (incidence above 1 case per 10,000 residents)"
      
      p2 <- ggplot(plotdb()$of1, aes(x=time, y=Intervention_type)) +
        geom_boxplot(aes(x=time, y=Intervention_type,
                         alpha = 0.75), 
                     outlier.shape = NA, show.legend = FALSE)+
        geom_polygon(data = plotdb()$of2, 
                     aes(x = x, y =y, fill = Fcol, 
                         alpha = 0.5), 
                     show.legend = TRUE) +
        scale_fill_manual("", 
                          values = c("#00BFC4", "#F8766D"),
                          labels=c("Before outbreak",
                                   "Outbreak")) +
        geom_jitter(size = 2, alpha = 0.1) +
        geom_boxplot(aes(x=time, y=Intervention_type,
                         alpha = 0.75),
                     outlier.shape = NA, show.legend = FALSE)+
        geom_point(aes(x=time, y=Intervention_type), 
                   data = plotdb()$of1[plotdb()$of1$Area == input$area_select, ],
                   color = rgb(1,0,0,0.5), size= 3) +
        scale_alpha(guide = 'none') +
        xlab("") +
        ylab(y_lab) +
        coord_flip() +
        ggtitle("Timing of interventions in different municipalities") +
        theme_classic() +
        theme(axis.text.x = element_text(size=11),
              axis.text.y = element_text(size=11),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(legend.text = element_text(size = 11)) +
        theme(plot.title = element_text(size=18))
      
      p2
    
  })
  
    output$intervention_text <- renderText({
      
        paste("On average, interventions were put in place ",
              "<b>", c("earlier", "later")[median(plotdb()$of3) + 1], "</b>",
              " in the outbreak in",
              substr(input$area_select, 1, nchar(input$area_select)-3),
              "compared to other municipalities in Brazil")
    })
  
    
    # Tab 3 - TRENDS
    
    output$p3 <- renderPlot({
      
      if(input$region_select2 != "National"){
        myRegion <- unique(Brazil_cases_sp$Region[Brazil_cases_sp$Name == 
                                                    input$region_select2])
      } else {
        myRegion <- "National"
      }
      
        g3 <- Trends_plot_list[[myRegion]][["Density"]] +
          theme_minimal() +
          theme(legend.text=element_text(size=12),
                legend.title=element_text(size=12)) +
          theme(axis.text.x = element_text(size=12),
                axis.text.y = element_text(size=12),
                axis.title.x = element_text(size=12),
                axis.title.y = element_text(size=12)) +
          theme(strip.text.x = element_text(size = 14))
        
        g4 <- Trends_plot_list[[myRegion]][["SDI"]] +
          theme_minimal() +
          theme(legend.text=element_text(size=12),
                legend.title=element_text(size=12)) +
          theme(axis.text.x = element_text(size=12),
                axis.text.y = element_text(size=12),
                axis.title.x = element_text(size=12),
                axis.title.y = element_text(size=12)) +
          theme(strip.text.x = element_text(size = 14))
        
        g5 <- Trends_plot_list[[myRegion]][["Sewerage"]] +
          theme_minimal() +
          theme(legend.text=element_text(size=12),
                legend.title=element_text(size=12)) +
          theme(axis.text.x = element_text(size=12),
                axis.text.y = element_text(size=12),
                axis.title.x = element_text(size=12),
                axis.title.y = element_text(size=12)) +
          theme(strip.text.x = element_text(size = 14))
        
        g6 <- Trends_plot_list[[myRegion]][["Travel_time"]] +
          theme_minimal() +
          theme(legend.text=element_text(size=12),
                legend.title=element_text(size=12)) +
          theme(axis.text.x = element_text(size=12),
                axis.text.y = element_text(size=12),
                axis.title.x = element_text(size=12),
                axis.title.y = element_text(size=12)) +
          theme(strip.text.x = element_text(size = 14))
      
      
      ggarrange(g3, g4, g5, g6,
                labels=NULL,
                ncol=2, nrow=2, label.x=-0.03,
                font.label=list(size=18, face="bold"),
                common.legend=FALSE, legend="right")
      
    })
    
    # Tab 4 - RT PLOTS
    data_rt <- reactive({
      
      plot_data <- all_plot_data[ which(((all_plot_data$city_state==input$region_select_rt1) | 
                                           (all_plot_data$city_state==input$region_select_rt2) | 
                                           (all_plot_data$city_state==input$region_select_rt3) | 
                                           (all_plot_data$city_state==input$region_select_rt4)) & 
                                          (all_plot_data$Date>=format(as.Date(cut_off_date), 
                                                                      "%Y-%m-%d"))) ,]
      plot_title     <-  "Estimated Rt with 95%CI"
      plot_sub_title <- "Mean Serial Interval = 6.5 days - Truncation = 4 days - Mean delay = 10 days "
      
      ## Add ranges to data
      ## Set plot ranges
      ## leave this in case we decide to include ranges for complete
      ## and incomplete data 
      xlim2 <- plot_data$Date[nrow(plot_data) - 10]
      
      ### Plot range of estimates for Rt
      plot_data$Municipality <- plot_data$city_state
      
      ### Selecting y range 
      tmp_dat <- plot_data %>% 
        group_by(city_state) %>% 
        arrange(desc(Rt_Smooth)) %>% 
        slice(1) %>% 
        ungroup()
      ymax_lim <- min(tmp_dat$Rt_Smooth) + 0.1
      
      tmp_dat <- plot_data %>% 
        group_by(city_state) %>% 
        arrange(Rt_Smooth) %>% 
        slice(1) %>% 
        ungroup()
      ymin_lim <- min(tmp_dat$Rt_Smooth) - 0.1
      
      ymax_lim = 1.4
      ymin_lim = 0.8
      xmax_lim = max(plot_data$Date) 
      xmin_lim = xmax_lim - 150
      
      db_out <- list(of1=plot_data,
                     of2=plot_title,
                     of3=plot_sub_title,
                     of4=xlim2,
                     of5=ymax_lim,
                     of6=ymin_lim,
                     of7=xmax_lim,
                     of8=xmin_lim)
      
    }) 
    
    # Rt tab
    output$p_rt <- renderPlot({
      
      g1 <-  ggplot(data=data_rt()$of1, 
                    aes(x = Date, 
                        y = Rt_Smooth, 
                        color=Municipality, 
                        group=Municipality))+ 
        geom_line() +
        geom_ribbon(aes(ymin = Rt_Smooth_LCI, 
                        ymax = Rt_Smooth_UCI,
                        fill = Municipality),
                    linetype="blank",alpha=0.1) + 
        xlab("Date") +
        ylab("Rt") +
        ggtitle(label    = data_rt()$of2, 
                subtitle = data_rt()$of3) +
        geom_hline(yintercept=1.0, linetype="solid") +
        scale_x_date(date_breaks = "3 week", date_labels="%d-%b") +
        coord_cartesian(ylim = c(data_rt()$of6, data_rt()$of5),
                        xlim = c(data_rt()$of8, data_rt()$of7)) +
        theme_minimal() +
        theme(legend.text=element_text(size=12),
              legend.title=element_text(size=12)) +
        theme(axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              axis.title.x = element_text(size=12),
              axis.title.y = element_text(size=12)) +
        theme(strip.text.x = element_text(size = 14))
      g1
    })
    
    # Tab 5 - MODEL FORECAST
    
    output$map2 <- renderLeaflet({
      
      my_bks <- seq(0,1, by=0.2)
      
      pal <- colorNumeric("YlOrBr", NULL)
      
      leaflet(data = peakSF,
              options = leafletOptions(zoomControl = FALSE)) %>%
        htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomright' }).addTo(this)}") %>% 
        addProviderTiles(providers$CartoDB.Voyager,
                         options = providerTileOptions(opacity = 0.35)) %>%
        addProviderTiles(providers$CartoDB.Voyager) %>%
        addCircleMarkers(col = ~pal(PredictProb), 
                         opacity = 0.9,
                         radius = 5, 
                         weight=1,
                         popup= ~paste0("<b>", Area, "</b>", 
                                        "<br/>", " Predicted probability: ",
                                        round(PredictProb, 1),
                                        "<br/>")) %>%
        # addTiles() %>%
        addLegend("bottomright",
                  pal = pal,
                  values = ~log1p(PredictProb),
                  labFormat = labelFormat(
                    transform=function(x) round(expm1(x),1)),
                  opacity = 0.7, 
                  title = "Predicted probabilities")
      
    })
    
    auc_data <- reactive({
      AUCplot +
        theme_minimal() +
        theme(legend.text=element_text(size=12),
              legend.title=element_text(size=12)) +
        theme(axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14)) +
        theme(strip.text.x = element_text(size = 14)) +
        xlab("False positive fraction") +
        ylab("True positive fraction") +
        annotate("text", x=0.65, y=0.275,
                 label=paste0("The area under this ROC curve is ",
                              round(AUCDF[1, "AUC"], 3)))
      
    })
    
    output$aucplot <- renderPlot({
      auc_data()
    })
    
    # Download Data tab
    out_dat <- reactive({
      out_data <- re.route.origin(BigStandard$standardised_incidence)
      
      # and add intervention timign data
      out_data <- district.start.date.find(out_data, BigStandard$Intervention)
      
      names(out_data) <- tolower(names(out_data))
      out_data
    })
    
    output$rawtable <- renderPrint({
      orig <- options(width = 1000)
      print(head(out_dat(), n=input$maxrows), row.names = FALSE)
      options(orig)
    })
    
    output$rawtable_rt <- renderPrint({
      orig <- options(width = 1000)
      print(head(data.table(all_plot_data), 
                 n=input$maxrows2), row.names = FALSE)
      options(orig)
    })
    
    output$rawtable_covars <- renderPrint({
      orig <- options(width = 1000)
      print(head(data.table(BigStandard$standardised_incidence), 
                 n=input$maxrows3), row.names = FALSE)
      options(orig)
    })
    
    output$text1 <- renderUI({

               HTML("The data on COVID-19 cases aggregated by municipality 
        is obtained from the Brasil.IO COVID-19 project repository which is, 
        updated on a daily basis. Municipality is the second administrative, 
        level in Brazil (below state). There are 5,570 municipalities in, 
        Brazil with an average of 37,728 inhabitants per municipality., 
        This dataset contains confirmed COVID-19 cases and deaths obtained,
        from the bulletins of the State Health Secretariats across the country., 
        Data is obtained from this source and our application is updated daily, 
        at 09:00 GMT. For more information see:
        <a href = 'https://brasil.io/dataset/covid19/caso_full/',>
             brasil.io </a></u>")
    })
    
    output$text2 <- renderUI({
               HTML("This was derived from data on notified COVID-19 cases
                 reported throughout Brazil between 2nd Feb and 8th June,
                 2020 collected by the Brazilian Ministry of Health and is
                 used with their permission.</u>")
    })
    
    output$text3 <- renderUI({
               HTML("Data on the distribution of the population by age 
                        for each municipality was obtained from the Instituto
                        Brasileiro de Geografia e Estatisitica (IBGE) national 
                        demographic census for 2010. The data was downloaded
                        from <a href = 'https://sidra.ibge.gov.br/tabela/3107',>
                        IBGE </a></u>")
    })
    
    output$text4 <- renderUI({
               HTML("Data on the types of interventions implemented, and 
                   the dates of their introduction were extracted from data 
                   collated by the 
                   <a href='https://www.cepal.org/en/topics/covid-19',>
                   Cepal Observatory </a> with edits and updates on timing of
                   interventions at the state level by Dr Andreza A de Souza 
                   Santos, director of the Brazilian Studies
                   <a href='https://www.lac.ox.ac.uk/brazilian-studies-programme#/',>
                   Programme </a>. This dataset is summarised in a recent 
                   pre-print 
                   <a href='https://www.medrxiv.org/content/10.1101/2020.04.25.20077396v1',>
                   manuscript </a> </u>")
    })
    
    output$text5 <- renderUI({
               HTML("The aim of our standardisation process is to make
                   data comparable between areas that have different 
                   population sizes and age profiles. We first assign the 
                   number of cases reported in each area (1) a hypothetical
                   age distribution based on the Brazilian national age 
                   distribution of COVID-19 cases (2). We then use local 
                   area age-stratified population data (3) to calculate 
                   age-specific incidence (standardised incidence) which 
                   is a measure that is comparable between areas. A measure
                   of standardised incidence of COVID-19 cases per 10,000 
                   inhabitants is then calculated based on the national 
                   age profile of Brazil for visualisation in this 
                   application. Days since the start of the outbreak are
                   calculated from whenever each area passes a cumulative 
                   incidence threshold equivalent to 1 case per 10,000 
                   inhabitants. </u>")
    })
    
    output$text6 <- renderUI({
               HTML("The Socio Demographic Index (SDI) is a composite 
                        relative measure of development ranging from 0 
                        (lowest) to 1 (highest). It is calculated using 
                        the average rankings of income per capita, fertility
                        rate and mean years of education among all
                        municipalities as measured by the 2010 IBGE census. 
                        Variables for population density, access to piped 
                        water and access to sewage system or septic tank were 
                        also obtained from the 2010 IBGE census. Travel time
                        to biggest city in the state was calculated using 
                        <a href='https://www.worldpop.org',>  WorldPop </a> 
                        population data and the 
                        <a href='https://developers.google.com/earth-engine/datasets/catalog/Oxford_MAP_friction_surface_2015_v1_0',> 
                        Malaria Atlas Project </a>
                        travel time friction surface using the 
                        <a href='https://malariaatlas.org/application-project/malariaatlas_package/',>
                        malariaAtlas </a> R 
                        package accumulated cost route finding algorithm. 
                        Travel time represents the land-based travel time 
                        between the most densely populated area in the 
                        municipality that is experiencing the COVID-19 
                        outbreak and the most densely populated area in the 
                        corresponding state. We may expect higher COVID-19 
                        incidence rates in highly accessible areas due to 
                        frequent re-introduction of the virus, or conversely 
                        we may expect higher incidence rates in remote areas
                        due to the increased challenge of adhering to movement
                        restrictions.  </u>")
    })
    
    output$text7 <- renderUI({
               HTML("The aim of this site is to complement the resources 
                        provided by the Brazilian ministry of health. This 
                        app has been developed for research purposes only 
                        and is not suitable to use as medical advice or to 
                        assess your personal level of risk. Data are provided 
                        without any warranty of any kind, either express or 
                        implied. The entire risk arising out of the use or 
                        performance of the app and associated data remains 
                        with you. In no event shall the London School of 
                        Hygiene and Tropical Medicine (LSHTM), the authors, 
                        or anyone else involved in the creation, production, 
                        or delivery of the app be liable for any damages 
                        whatsoever including, without limitation, damages for 
                        loss of business profits, business interruption, loss
                        of business information, or other pecuniary loss 
                        arising out of the use of or inability to use this 
                        app even if LSHTM has been advised of the possibility
                        of such damages. </u>")
    })
    
    output$text8 <- renderUI({
      HTML("The COVID-19 Local Information Comparison (CLIC 
                        Brazil) web application allows local public health
                        decision makers and researchers to compare the current
                        COVID-19 epidemic in different local areas 
                        (municipalities) across Brazil. It aims to:
                        <li> Identify hotspots of COVID-19 disease.</li>
                        <li>Compare the outbreak trajectories between
                        municipalities to understand where the epidemic is 
                        growing fastest.</li>
                        <li>Assess the socioeconomic drivers of COVID-19 risk.</li> </u>")
    })
    
    output$text9 <- renderUI({
               HTML("This tab shows a map of Brazil in which each 
                        municipality with a COVID-19 outbreak is indicated by 
                        a circle. By selecting the options under 'Select 
                        Variable', the map can be configured to display either
                        the total number of COVID-19 cases or the incidence per 
                        10,000 people in the population. Cirlces with a dartker 
                        shading indicate higher values. By sliding the 'date'
                        button you can get a snapshot of the epidemic on 
                        previous days and explore how the epidemic has evolved.
                        The plots and text box provide summary information on 
                        the growth of the national epidemic. </u>")
    })
    
    output$text10 <- renderUI({
      HTML("The main graph on this tab shows the growth of the 
                   epidemic over time in each muncipality. Case numbers have
                   been 'age-standardised' to allow comparisons between 
                   municipalities with different population sizes and age 
                   structures. The timing of the start of the outbreak in each
                   muncipality has also been standardised to the first day 
                   on which a case incidence of greater than 1 case per 10,000 
                   residents was reported. - Using the top pull-down list on 
                   the left you select the state in which you municipality of 
                   interest is located the trajectories for all municpalities 
                   in the state will be highlighted in orange on the plot. 
                   - The central pull-down list allows you to select the 
                   municipality of interest, these are labelled with the 
                   Municipality name and the State name. - The lower pull-down
                   allows you to select the measure of interest from the
                   following options; Cumulative case incidence, Cumulative
                   death incidence, hospital bed occupancy and ITU bed 
                   occupancy (The details of the methods used to calculate 
                   these parameters are given in the 'About this site' tab) 
                   </br> </br> For example, if you compare the trajectory for 
                    cumulative case incidence for São Paulo municipality with 
                    the trajectories for all municipalities in São Paolo 
                    State and (orange) and all municipalities in Brazil, you 
                    would see this output: </u>")
    })
    
    output$text11 <- renderUI({
               HTML("This graph shows the time different interventions were 
                        announced relative to when the epidemic began in each 
                        municipality. The time origin (day 0) for each 
                        municipality is the first day on which a case incidence
                        of greater than 1 case per 10,000 residents was reported. 
                        Each black dot represents one municipality with the red 
                        dots representing the municipality selected in the 
                        dropdown menu. Dots in the blue area represent 
                        municipalities where the intervention was announced 
                        before the local Covid-19 epidemic began while dots in 
                        the red area show municipalities where interventions 
                        were only announced after the outbreak had begun. The 
                        box plot summarises the median, interquartile range and 
                        range of timings for each municipality. The further 
                        right the red dot is on this plot the later in the 
                        epidemic interventions were announced compared to other 
                        areas. </u>")
    })
    
    output$text12 <- renderUI({
               HTML("This tab allows the user to compare the characteristics
                   of municipalities with worse or better COVID-19 epidemics. 
                   Using the pulldown tab you can select to view either national
                   data or the data for one particular state. 
                   </br> </br>
                   The barcharts show differences in age-standardised 
                   incidence at different points in the epidemic for subsets of
                   municipalities grouped according to particular characteristics. 
                   The vertical line shows the range of the data for each subset
                   and the rectangle shows the value for the median and 
                   interquartile range. The sets of lines show the data in 10
                   day intervals (from 10 to 70 days) from the day at which an 
                   incidence of 1 case per 10,000 people was reported in a 
                   municipality. </br>
                   </br> For example, the upper left plot (shown below) shows 
                        the association between area population density and 
                        incidence. The larger the gap between the bars the 
                        larger the difference in the COVID-19 epidemic between
                        high and low population density areas. More detailed 
                        analysis can be conducted by downloading the data from 
                        the data download tab.  </u>")
    })
    
    output$text13 <- renderUI({
      HTML("If you wish to carry out your own analyses on the
                   standardised data you can download the full dataset in comma
                   separated variable (csv) format from here. A description of 
                   the variables included is shown below. 
                   </br><b> area - </b> Municipality name and State (2 letter 
                   code). (NB UTF-8 encoding should be used to correctly format
                   the place names) 
                   </br><b> date_end - </b> Date of data update (there is one
                   row per municipality per day) 
                   </br><b> cum_cases - </b> Cumulative case count 
                   </br><b> cum_deaths - </b> Cumulative death count 
                   </br><b> bed_occ_2_5,bed_occ_50 & bed_occ_97_5 - </b> 2.5%, 
                   50% & 97.5% credible intervals for the predicted number of 
                   hospital beds occupied by patients with COVID-19 based on 
                   the cumulative data 
                   </br><b> itu_bed_occ_2_5,itu_bed_occ_50 & itu_bed_occ_97_5 - </b>
                   2.5%, 50% & 97.5% credible intervals for the predicted number
                   of Intensive care unit beds occupied by patients with COVID-19 
                   based on the cumulative data. 
                   </br><b> standardised_cases - </b> Age standardised cumulative
                   case incidence 
                   </br><b> statdardised_deaths - </b> Age standardised 
                   cumulative death incidence 
                   </br><b> stan_bed_occ_2_5,stan_bed_occ_50 & 
                   stan_bed_occ_97_5 - </b>
                   2.5% 50% & 97.5% credible intervals for the predicted number 
                   of hospital beds occupied by patients with COVID-19 based on
                   the age standardised data. 
                   </br><b> stan_itu_bed_occ_2_5,stan_itu_bed_occ_50 & 
                   stan_itu_bed_occ_97_5 - </b> 2.5% 50% & 97.5% credible 
                   intervals for the predicted number of Intensive care unit 
                   beds occupied by patients with COVID-19 based on the age 
                   standardised data. 
                   </br><b> region - </b> State (2 letter code) 
                   </br><b> popden - </b> Population Density (individuals per 
                   km<sup>2</sup>) 
                   </br><b> sdi - </b> Social demogrpahic index for the 
                   mucipality (See Covariates in the 'About this site' - tab)
                   </br><b> piped_water - </b> Proportion of population in the 
                   municipality with access to a piped water supply
                   </br><b> sewage_or_septic - </b> Proportion of population in 
                   the municipality with access to sewage system or a septic 
                   tank 
                   </br><b> travel_time - </b> The land-based travel time between 
                   the most densely populated area in the municipality and the
                   most densely populated area in the corresponding State
                   </br><b> x - </b> The longitude of the municiplaity in decimal 
                   degrees 
                   </br><b> y - </b> The latitude of the municiplaity in decimal
                   degrees
                   </br><b> days_since_start - </b> The number of days since the
                   age standardised incidene was greater than 1 case per 10,000 
                   residents
                    </br></br>
                    The following variables are indicators which are coded 1 if 
                    the intervention described had been initiated by this date; 
                    </br><b> awareness_campaigns_start - </b> COVID-19 awareness 
                    campaigns 
                    </br><b> border_closure_start - </b> International border 
                    closures 
                    </br><b> domestic_travel_restrictions_start - </b> Domestic 
                    travel restrictions 
                    </br><b> economic_measures_start - </b> Economic interventions
                    </br><b> border_health_screening_start - </b> Health screening 
                    at international borders
                    </br><b> international_flight_suspension_start - </b> 
                    International flights suspended 
                    </br><b> isolation_quarrantine_start - </b>  Isolation and 
                    quarantine for those infected with COVID-19
                    </br><b> limit_on_public_gatherings_start -  </b> A limit 
                    was placed on public gatherings 
                    </br><b> schools_closure_start - </b>  School closures 
                    initiated 
                    </br><b> workplace_closure_start - </b>  Workplace closures 
                    initiated </u>")
    })
    
    output$text14 <- renderUI({
               HTML("This app was developed as part of the CADDE 
                        <a href = 'https://www.caddecentre.org',> project </a>
                        This project was supported through a São Paulo Research 
                        Foundation (FAPESP) and Medical Research Council CADDE 
                        partnership award (MR/S0195/1 and FAPESP 18/14389-0). </u>")
    })
    
    
  output$text15 <- renderUI({
    HTML("<div class='py-5 team4'>
  <div class='container'>
    <div class='row justify-content-center mb-4'>
      <div class='col-md-7 text-center'>
      </div>
    </div>
    <div class='row'>
      <!-- column  -->
      <div class='col-lg-3 mb-4'>
        <!-- Row -->
        <div class='row'>
          <div class='col-md-12'>
            <img src='oliver.jpg' alt='wrapkit' class='img-fluid rounded-circle' />
          </div>
          <div class='col-md-12 text-center'>
            <div class='pt-2'>
              <h5 class='mt-4 font-weight-medium mb-0'>Dr Oliver Brady</h5>
              <h6 class='subtitle mb-3'>Associate Professor</h6>
              <p>London School of Hygiene & Tropical Medicine</p>
              <p><a class='button'' href='mailto:oliver.brady@lshtm.ac.uk'>Contact</a></p>
              <!-- Add icon library -->
              <ul class='list-inline'>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-facebook'></i></a></li>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-twitter'></i></a></li>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-instagram'></i></a></li>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-behance'></i></a></li>
              </ul>
            </div>
          </div>
        </div><!-- Row -->
        <div class='row'>
          <div class='col-md-12'>
            <img src='andre1.png' alt='wrapkit' class='img-fluid rounded-circle' />
          </div>
          <div class='col-md-12 text-center'>
            <div class='pt-2'>
              <h5 class='mt-4 font-weight-medium mb-0'>Dr Andre Luis Acosta</h5>
              <h6 class='subtitle mb-3'>Postdoctoral Researcher</h6>
              <p>Public Health School, University of São Paulo.</p>
              <p><a class='button'' href='mailto:andreluisacosta@usp.br'>Contact</a></p>
              <ul class='list-inline'>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-facebook'></i></a></li>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-twitter'></i></a></li>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-instagram'></i></a></li>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-behance'></i></a></li>
              </ul>
            </div>
          </div>
        </div>
        <!-- Row -->
      </div>
      <!-- column  -->
      <!-- column  -->
      <div class='col-lg-3 mb-4'>
        <!-- Row -->
        <div class='row'>
          <div class='col-md-12'>
            <img src='paul.jpg' alt='wrapkit' class='img-fluid rounded-circle' />
          </div>
          <div class='col-md-12 text-center'>
            <div class='pt-2'>
              <h5 class='mt-4 font-weight-medium mb-0'>Dr Paul Mee</h5>
              <h6 class='subtitle mb-3'>Assistant Professor</h6>
              <p>London School of Hygiene & Tropical Medicine</p>
              <p><a class='button'' href='mailto:paul.mee@lshtm.ac.uk'>Contact</a></p>
              <ul class='list-inline'>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-facebook'></i></a></li>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-twitter'></i></a></li>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-instagram'></i></a></li>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-behance'></i></a></li>
              </ul>
            </div>
          </div>
        </div>
        <!-- Row -->
        <div class='row'>
          <div class='col-md-12'>
            <img src='neal.jpg' alt='wrapkit' class='img-fluid rounded-circle' />
          </div>
          <div class='col-md-12 text-center'>
            <div class='pt-2'>
              <h5 class='mt-4 font-weight-medium mb-0'>Prof Neal Alexander</h5>
              <h6 class='subtitle mb-3'>Professor of Medical Statistics and Epidemiology</h6>
              <p>London School of Hygiene & Tropical Medicine</p>
              <p><a class='button'' href='mailto:neal.alexander@lshtm.ac.uk'>Contact</a></p>
              <ul class='list-inline'>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-facebook'></i></a></li>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-twitter'></i></a></li>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-instagram'></i></a></li>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-behance'></i></a></li>
              </ul>
            </div>
          </div>
        </div>
        <!-- Row -->
      </div>
      <!-- column  -->
      <!-- column  -->
      <div class='col-lg-3 mb-4'>
        <!-- Row -->
        <div class='row'>
          <div class='col-md-12'>
            <img src='felipe.jpg' alt='wrapkit' class='img-fluid rounded-circle' />
          </div>
          <div class='col-md-12 text-center'>
            <div class='pt-2'>
              <h5 class='mt-4 font-weight-medium mb-0'>Dr Felipe J Colón-González</h5>
              <h6 class='subtitle mb-3'>Assistant Professor</h6>
              <p>London School of Hygiene & Tropical Medicine</p>
              <p><a class='button'' href='mailto:felipe.colon@lshtm.ac.uk'>Contact</a></p>
              <ul class='list-inline'>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-facebook'></i></a></li>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-twitter'></i></a></li>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-instagram'></i></a></li>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-behance'></i></a></li>
              </ul>
            </div>
          </div>
        </div>
        <!-- Row -->
        <div class='row'>
          <div class='col-md-12'>
            <img src='cmmid_sq.jpg' alt='wrapkit' class='img-fluid rounded-circle' />
          </div>
          <div class='col-md-12 text-center'>
            <div class='pt-2'>
              <h5 class='mt-4 font-weight-medium mb-0'>CMMID nCov working group</h5>
              <p>London School of Hygiene & Tropical Medicine</p>
              <p><a class='button'' href='https://www.lshtm.ac.uk/research/centres/centre-mathematical-modelling-infectious-diseases'>Contact</a></p>
              <ul class='list-inline'>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-facebook'></i></a></li>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-twitter'></i></a></li>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-instagram'></i></a></li>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-behance'></i></a></li>
              </ul>
            </div>
          </div>
        </div>
        <!-- Row -->
      </div>
      <!-- column  -->
      <!-- column  -->
      <div class='col-lg-3 mb-4'>
        <!-- Row -->
        <div class='row'>
          <div class='col-md-12'>
            <img src='andreza.jpg' alt='wrapkit' class='img-fluid rounded-circle' />
          </div>
          <div class='col-md-12 text-center'>
            <div class='pt-2'>
              <h5 class='mt-4 font-weight-medium mb-0'>Dr Andreza De Souza Santos</h5>
              <h6 class='subtitle mb-3'>Director of the Brazilian Studies Programme</h6>
              <p>Latin American Centre, Oxford School of Global and Area Studies.</p>
              <p><a class='button'' href='mailto:andreza.desouzasantos@lac.ox.ac.uk'>Contact</a></p>
              <ul class='list-inline'>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-facebook'></i></a></li>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-twitter'></i></a></li>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-instagram'></i></a></li>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-behance'></i></a></li>
              </ul>
            </div>
          </div>
        </div>
        <!-- Row -->
        <div class='row'>
          <div class='col-md-12'>
            <img src='cadde.jpg' alt='wrapkit' class='img-fluid rounded-circle' />
          </div>
          <div class='col-md-12 text-center'>
            <div class='pt-2'>
              <h5 class='mt-4 font-weight-medium mb-0'>CADDE working group</h5>
              <p>Brazil-UK Centre for (Arbo)virus Discovery, Diagnosis, Genomics and Epidemiology</p>
              <p><a class='button'' href='https://https://www.caddecentre.org'>Contact</a></p>
              <ul class='list-inline'>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-facebook'></i></a></li>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-twitter'></i></a></li>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-instagram'></i></a></li>
                <li class='list-inline-item'><a href='#' class='text-decoration-none d-block px-1'><i class='icon-social-behance'></i></a></li>
              </ul>
            </div>
          </div>
        </div>
        <!-- Row -->
      </div>
    </div>
  </div>
</div>")
  })
 
  output$saopaulo <- renderImage({
    
    return(list(
      src = "input_data/saopaulo.png",
      contentType = "image/png",
      alt = "Image",
      width = "50%", height = "250"
    ))  
  }, deleteFile = FALSE)
  
  output$boxplot <- renderImage({
    
    return(list(
      src = "input_data/boxplot.jpg",
      contentType = "image/jpg",
      alt = "Image",
      width = "50%", height = "250"
    ))  
  }, deleteFile = FALSE)
  
  output$trends <- renderImage({
    
    return(list(
      src = "input_data/trends.jpg",
      contentType = "image/jpg",
      alt = "Image",
      width = "50%", height = "250"
    ))  
  }, deleteFile = FALSE)
  
   
}

shinyApp(ui, server)

