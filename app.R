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

## Import data
incarceration_trends
county_shapefile

## Widgets
# For interactive maps widget:
year_choices <- unique(incarceration_trends$year)

# Bar Graph

state_options <- unique(rates_by_race$ID)

## UI
ui <- navbarPage(
  title = "Scores",
  # Tab 1: Histogram
  tabPanel(
    title = "Histogram",
    tabPanel(
      title = "Histogram",
      
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "histvar",
            label = "Choose a year:",
            choices = state_options,
            selected = "wisconsin"),
          
        ),
        mainPanel(plotOutput(outputId = "hist", height= 700))
      )
    )
  ),
  
#MAP
ui <- fluidPage(
  titlePanel("Jail Incarceration Rate 1970 - 2018"),
  selectInput(inputId = "yearInput",
              label = "Year:",
              choices = year_choices,
              selected = "2018",
              multiple = FALSE),
  #sliderInput(inputId = "yearInput", 
                #label = "Year", 
                #min = 1970, 
                #max = 2018, 
                #value = 1970, 
                #step = 1,
                #animate =
                  #animationOptions(interval = 300, loop = TRUE)),
    mainPanel(leafletOutput("mymap")
    )
  )
)

## Server
server <- function(input, output) {
data_for_hist <- reactive({
  data <- rates_by_race %>% filter(ID==input$histvar)
})

output$hist <- renderPlot({
  
  ggplot(data= data_for_hist(), aes(x=race, y=rate)) +
    geom_bar(stat="identity", width=0.5, fill = "#2c7fb8") +
    labs(x = "Ethnicity",
         y = "Rate of Incarceration (Prisoners per 100,000 residents)",
         title = "Rates of Incarceration By Ethnicity") +
    
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

  year_jail <- reactive({
    filtered <- incarceration_trends %>%
      filter(year == input$yearInput) 
    
      m <- left_join(county_shapefile, filtered, by = "fips")
      return(m)
      #geo_join(county_shapefile, filtered, 'fips', 'fips', how = "left")
  })
  
  output$mymap <- renderLeaflet({
    bins <- c(0, 100, 250, 500, 1000, 2500, 5000, 10000, 30000)
    pal <- colorBin(palette = "OrRd", bins = bins, na.color = "#D3D3D3")
    
    year_jail() %>% 
      leaflet() %>% 
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(-95.7129, 37.0902, zoom = 3) %>%
      addPolygons(fillColor = ~ pal(year_jail()$total_jail_pop_rate),
                  stroke = FALSE,
                  smoothFactor = .5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "black",
                                                      bringToFront = TRUE),
                  popup = ~ paste0("County Name: ", county_name %>% str_to_title(), "<br>",
                                   "State: ", state %>% str_to_title(), "<br>",
                                   "Total Jail Population: ", total_jail_pop, "<br>",
                                   "Total Jail-Population rate: ",
                                   total_jail_pop_rate %>% round(2))) %>%
      addLegend("bottomright",
                pal = pal,
                values = ~ total_jail_pop_rate,
                title = "Total Jail Population Rate (per 100,000",
                opacity = 0.7)
  })
}

##Shiny App
shinyApp(ui = ui, server = server)
