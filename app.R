library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(sf)
library(tigris) # geojoin
library(htmlwidgets)
library(shinythemes)
library(shinycssloaders)
library(shinyjs)
library(reactable)
library(rmapshaper)
library(stringr)

# ====================================================
## Import data
incarceration_trends <-
  read_csv("incarceration_trends_wrangled.csv")
incarceration_trends_table <-
  read_csv("map_data/all_years_incarceration_trends_map.csv")
rates_by_race <- read.csv("rates_by_race_wrangled.csv")
county_shapefile <- read_rds("county_shapefile_wrangled.rds")
filtered <- incarceration_trends

# ==========================================================

## Widgets
# For interactive maps widget:
year_choices <- unique(incarceration_trends$year)
demographic_choices <-
  c(
    "total_jail_prison_pop_rate",
    "female_jail_prison_pop_rate",
    "male_jail_prison_pop_rate",
    "aapi_jail_prison_pop_rate",
    "black_jail_prison_pop_rate",
    "latinx_jail_prison_pop_rate",
    "native_jail_prison_pop_rate",
    "white_jail_prison_pop_rate"
  )
demographic_choices_names <-
  c(
    "Total Jail & Prison Pop Rate",
    "Female Jail & Prison Pop Rate",
    "Male Jail & Prison Pop Rate",
    "AAPI Jail & Prison Pop Rate",
    "Black Jail & Prison Pop Rate",
    "Latinx Jail & Prison Pop Rate",
    "Native Jail & Prison Pop Rate",
    "White Jail & Prison Pop Rate"
  )
names(demographic_choices) <- demographic_choices_names

# Bar Graph
state_options <- unique(rates_by_race$State)

## UI
ui <- navbarPage(
  theme = shinytheme("sandstone"),
  title = "Incarceration Rates in America",
  # Tab 1: Interactive Map
  tabPanel(
    title = "Interactive Map",
    titlePanel("Jail and Prison Combined Incarceration Rate 1970 - 2018"),
    sidebarPanel(
      sliderInput(
        inputId = "yearInput",
        label = "Year:",
        value = 2010,
        min = 1970,
        max = 2018,
        sep = ''
      ),
      selectInput(
        inputId = "demographicInput",
        label = "Demographic variable:",
        choices = demographic_choices,
        selected = "2018",
        multiple = FALSE
      ),
      plotOutput("secondaryMapPlot")
    ),
    mainPanel(leafletOutput(
      "mymap", width = "700px", height = "500px"
    ))
  ),
  # Tab 2: Bar Graph
  tabPanel(title = "Bar Graph",
           sidebarLayout(
             sidebarPanel(
               selectInput(
                 inputId = "histvar",
                 label = "Choose a state:",
                 choices = state_options,
                 selected = "Wisconsin"
               ),
               
             ),
             mainPanel(plotOutput(outputId = "hist", height = 700))
           )),
  # Tab 3: Table
  tabPanel(
    title = "Table",
    titlePanel(
      "Data Table for Interactive Map: Jail & Prison Combined Incarceration Rate 1970 - 2018"
    ),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "state_filter",
          "Select a state:",
          unique(filtered$state),
          multiple = TRUE
        ),
        selectInput(
          "county_filter",
          "Select a county:",
          unique(filtered$county),
          multiple = TRUE
        ),
        selectInput(
          "year_filter",
          "Select a year:",
          unique(filtered$year),
          multiple = TRUE
        ),
      ),
      mainPanel(reactableOutput("table"))
    )
  )
)

## Server
server <- function(input, output) {

  # ====================================================
  # Tab 1: Interactive Map
  
  # Create reactive variable responsive to year option
  year_prison_jail <- reactive({
    d <- read_csv(paste0(
        "map_data/",
        input$yearInput,
        "_incarceration_trends_map.csv"
      ))
    
    d <- left_join(county_shapefile, d, by = "fips")
    
    return(d)
  })
  
  # ====================================================
  # Primary map rendering code
    
  output$mymap <- renderLeaflet({
    # create customized color bins
    bins <- c(0, 100, 250, 500, 1000, 2500, 5000, 10000, 30000)
    # choose a palette suitable for this map
    pal <-
      colorBin(palette = "OrRd",
               bins = bins,
               na.color = "#D3D3D3")
    
    d <- year_prison_jail()
    # create the map using Leaflet
    d %>%
      leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(-95.7129, 37.0902, zoom = 4) %>%
      # add counties
      addPolygons(
      # set default zoom-in so it focuses
        fillColor = ~ pal(d[[paste0(input$demographicInput)]]),
        layerId = paste0(d$yfips,"_",d$county," County, ",d$state),
        stroke = FALSE,
        smoothFactor = .5,
        opacity = 1,
        fillOpacity = 0.7,
        # add hovering effects
        highlightOptions = highlightOptions(
          weight = 5,
          fillOpacity = 1,
          color = "black",
          bringToFront = TRUE
        ),
        # add popup and fill in information that we want to display
        popup = ~ paste0(
          "County Name: ",
          county %>% str_to_title(),
          "<br>",
          "State: ",
          state %>% str_to_title(),
          "<br>",
          "Total Population: ",
          total_pop,
          "<br>",
          "Jail Population Rate: ",
          total_jail_pop_rate,
          "<br>",
          "Prison Population Rate: ",
          total_prison_pop_rate,
          "<br>",
          "Jail and Prison Combined Population Rate: ",
          total_jail_prison_pop_rate %>% round(2)
        )
      ) %>%
      # add a legend that shows the meaning of the color
      addLegend(
        "bottomright",
        pal = pal,
        values = ~ total_jail_prison_pop_rate,
        title = gsub("_", " ", paste0(input$demographicInput)),
        opacity = 0.7
      )
  })
  
  # ====================================================
  # Secondary plot for Map Tab
  
  # Initialize our reactive variable with default value of Hampshire County, MA.
  rv <- reactiveValues()
  rv$fips <- "25015" #default value of Hampshire County for secondary plot
  rv$countyState <- "Hampshire County, MA"
  rv$secondaryMapPlotData <- incarceration_trends_table %>% filter(fips == "25015")
  
  observeEvent(input$mymap_shape_click, {
    
    event <- input$mymap_shape_click
    
    rv$fips <- str_sub(event$id,5,9)
    rv$countyState <- str_sub(event$id,11,-1)
    
    rv$secondaryMapPlotData <- incarceration_trends_table %>% filter(fips == rv$fips)
  })
  
  # This is the actual render code for the secondary plot
  output$secondaryMapPlot <- renderPlot({
    ggplot(rv$secondaryMapPlotData, aes(x = year, y = rv$secondaryMapPlotData[[paste0(input$demographicInput)]])) +
      ggtitle(paste0("Time series for: ",rv$countyState), subtitle = paste0("Plotting selected variable: ",input$demographicInput)) +
      geom_smooth(se = FALSE) +
      xlab("Year") +
      ylab("Incaceration Rate for Selected Demographic")
  })
  
  # ====================================================
  # Tab 2: Histogram
  
  data_for_hist <- reactive({
    data <- rates_by_race %>% filter(State == input$histvar)
  })
  
  
  output$hist <- renderPlot({
    ggplot(data = data_for_hist(), aes(x = race, y = rate)) +
      geom_bar(stat = "identity",
               width = 0.5,
               fill = "#2c7fb8") +
      labs(
        x = "Ethnicity",
        y = "Rate of Incarceration (Prisoners per 100,000 residents)",
        title = "Rates of Incarceration By Ethnicity",
        subtitle = "Source: U.S. Bureau of Justice Statistics data for 2019."
      ) +
      theme(
        plot.title = element_text(
          family = "serif",
          face = "bold",
          color = 1,
          size = 23
        ),
        plot.caption = element_text(size = 10),
        axis.text.x = element_text(family = "serif",
                                   size = 12),
        axis.text.y = element_text(family = "serif",
                                   size = 12),
        axis.title.x = element_text(family = "serif",
                                    size = 16),
        axis.title.y = element_text(family = "serif",
                                    size = 16)
      )
    
  })
  
  # ====================================================
  # Tab 3: Table
  output$table <- renderReactable({
    reactable(
      incarceration_trends_table[,],
      filterable = TRUE,
      searchable = TRUE,
      fullWidth = TRUE,
      bordered = TRUE,
      striped = TRUE,
      selection = "multiple",
      details = colDef(
        name = "JSON for Row",
        details = JS(
          "function(rowInfo) {
          return 'Details for row: ' + rowInfo.index +
          '<pre>' + JSON.stringify(rowInfo.row._original, null, 2) + '</pre>'
        }"
        ),
        html = TRUE,
        width = 60
      )
    )
  })
  
  observe({
    # Filter data
    filtered <- if (length(input$state_filter) > 0) {
      incarceration_trends[incarceration_trends$state %in% input$state_filter, ]
    } else {
      incarceration_trends
    }
    
    current_selection <- input$county_filter
    
    if (length(input$state_filter) > 0) {
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "county_filter",
        choices = unique(filtered$county),
        selected = current_selection
      )
    }
    
    filtered <- if (length(input$county_filter) > 0) {
      filtered[filtered$county %in% input$county_filter,]
    } else {
      filtered
    }
    
    filtered <- if (length(input$year_filter) > 0) {
      filtered[filtered$year %in% input$year_filter,]
    } else {
      filtered
    }
    
    input$state_filter
    
    updateReactable("table", data = filtered)
  })
  
}

##Shiny App
shinyApp(ui = ui, server = server, options = list(height = 800, width = 900))