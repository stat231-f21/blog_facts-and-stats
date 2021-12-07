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

# ====================================================
## Import data
incarceration_trends <- read_csv("incarceration_trends_wrangled_7.csv")

rates_by_race<- read.csv("rates_by_race_wrangled.csv")
filtered <- incarceration_trends

# read in shapefile
county_shapefile <- read_sf("cb_2018_us_county_500k.shp") %>%
  janitor::clean_names() 

# take out statefp and countyfp and rename geoid column into fips
county_shapefile <- county_shapefile %>%
  select(-statefp, -countyfp) %>%
  mutate(fips = as.numeric(geoid))



# ==========================================================

## Widgets
# For interactive maps widget:
year_choices <- unique(incarceration_trends$year)
demographic_choices <- c("total_jail_prison_pop_rate","female_prison_pop_rate","male_jail_prison_pop_rate", "aapi_jail_prison_pop_rate","black_jail_prison_pop_rate", "latinx_jail_prison_pop_rate", "native_jail_prison_pop_rate", "white_jail_prison_pop_rate")

# Bar Graph
state_options <- unique(rates_by_race$ID)

## UI
ui <- navbarPage(
  theme = shinytheme("sandstone"),
  title = "Incarceration Rates in America",
  # Tab 1: Bar Graph
  tabPanel(
    title = "Bar Graph",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "histvar",
          label = "Choose a state:",
          choices = state_options,
          selected = "Wisconsin"),
        
      ),
      mainPanel(plotOutput(outputId = "hist", height= 700))
    )
  ),
  # Tab 2: Interactive Map
  tabPanel(
    title = "Interactive Map",
    titlePanel("Prison + Jail Incarceration Rate 1970 - 2018"),
    sidebarPanel(sliderInput(inputId = "yearInput",
                             label = "Year:",
                             value = 2010,
                             min = 1970,
                             max = 2018,
                             sep = ''),
                 selectInput(inputId = "demographicInput",
                             label = "Demographics:",
                             choices = demographic_choices,
                             selected = "2018",
                             multiple = FALSE),),
    mainPanel(leafletOutput("mymap", width = "900px", height = "600px"))
  ),
  # Tab 3: Table
  tabPanel(
    title = "Table",
    titlePanel("Data Table for Interactive Map: Prison + Jail Incarceration Rate 1970 - 2018"),
    sidebarLayout(
      sidebarPanel(
        selectInput("state_filter", "Select a state:", unique(filtered$state), multiple = TRUE),
        selectInput("county_filter", "Select a county:", unique(filtered$county), multiple = TRUE),
        selectInput("year_filter", "Select a year:", unique(filtered$year), multiple = TRUE),
      ),
      mainPanel(reactableOutput("table"))
    )
  )
)

## Server
server <- function(input, output) {
  data_for_hist <- reactive({
    data <- rates_by_race %>% filter(ID == input$histvar)
  })

  # Tab 1: Histogram
  output$hist <- renderPlot({
    ggplot(data = data_for_hist(), aes(x = race, y = rate)) +
      geom_bar(stat ="identity", width = 0.5, fill = "#2c7fb8") +
      labs(x = "Ethnicity",
           y = "Rate of Incarceration (Prisoners per 100,000 residents)",
           title = "Rates of Incarceration By Ethnicity",
           subtitle = "Source: U.S. Bureau of Justice Statistics data for 2019.") +
      theme(
        plot.title = element_text(family = "serif",             
                                  face = "bold",              
                                  color = 1,             
                                  size = 23),
        plot.caption = element_text(size = 10),
        axis.text.x = element_text(family = "serif",             
                                   size = 12),
        axis.text.y = element_text(family = "serif",             
                                   size = 12),
        axis.title.x = element_text(family = "serif",             
                                    size = 16),
        axis.title.y = element_text(family = "serif",             
                                    size = 16))
    
  })
  
  # Tab 2: Interactive Map
  # create reactive variable responsive to year option
  year_prison <- reactive({
    # filter the data set to keep only the entries in the year selected
    filtered <- incarceration_trends %>%
      filter(year == input$yearInput) 
    # join the filtered dataset with county shapefile
    m <- left_join(county_shapefile, filtered, by = "fips")
    return(m)
  })
  
  output$mymap <- renderLeaflet({
    # create customized color bins 
    bins <- c(0, 50, 100, 250, 500, 1000, 2500, 5000, 10000)
    # choose a palette suitable for this map
    pal <- colorBin(palette = "OrRd", bins = bins, na.color = "#D3D3D3")
    
    data = year_prison()
    # create the map using Leaflet  
    year_prison() %>% 
      leaflet() %>% 
      addProviderTiles(provider = "CartoDB.Positron") %>%
      # set default zoom-in so it focuses on mainland America
      setView(-95.7129, 37.0902, zoom = 4) %>%
      # add counties
      addPolygons(fillColor = ~ pal(data[[paste0(input$demographicInput)]]),
                  stroke = FALSE,
                  smoothFactor = .5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  # add hovering effects
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "black",
                                                      bringToFront = TRUE),
                  # add popup and fill in information that we want to display
                  popup = ~ paste0("County Name: ", county %>% str_to_title(), "<br>",
                                   "State: ", state %>% str_to_title(), "<br>",
                                   "Total Population: ", total_pop,"<br>",
                                   "Total Jail Population Rate: ", total_jail_pop_rate, "<br>",
                                   "Total Prison Population Rate: ", total_prison_pop_rate, "<br>",
                                   "Total Jail-Prison Population rate: ",
                                   total_jail_prison_pop_rate %>% round(2))) %>%
      # add a legend that shows the meaning of the color
      addLegend("bottomright",
                pal = pal,
                values = ~ total_jail_prison_pop_rate,
                title = paste0(input$demographicInput),
                opacity = 0.7)
    }
  )
  
  # Tab 3: Table
  output$table <- renderReactable({
    
    reactable(
      incarceration_trends[,],
      filterable = TRUE,
      searchable = TRUE,
      fullWidth = TRUE,
      bordered = TRUE,
      striped = TRUE,
      selection = "multiple",
      details = colDef(
        name = "JSON for Row",
        details = JS("function(rowInfo) {
          return 'Details for row: ' + rowInfo.index +
          '<pre>' + JSON.stringify(rowInfo.row._original, null, 2) + '</pre>'
        }"),
        html = TRUE,
        width = 60
      )
    )
  }
  )
  
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
      filtered[filtered$county %in% input$county_filter, ]
    } else {
      filtered
    }
    
    filtered <- if (length(input$year_filter) > 0) {
      filtered[filtered$year %in% input$year_filter, ]
    } else {
      filtered
    }
    
    input$state_filter
    
    updateReactable("table", data = filtered)
  })
  
}

##Shiny App
shinyApp(ui = ui, server = server)
