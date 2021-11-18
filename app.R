library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(sf)
library(tigris) # geojoin
library(htmlwidgets)
library(shinythemes)

## Import data
incarceration_trends
county_shapefile

## Widgets
# For interactive maps widget

## UI
ui <- fluidPage(
  titlePanel("Jail Incarceration Rate 1970 - 2018"),
      sliderInput(inputId = "yearInput", 
                label = "Year", 
                min = 1970, 
                max = 2018, 
                value = 1970, 
                step = 1,
                animate =
                  animationOptions(interval = 300, loop = TRUE)),
    mainPanel(leafletOutput("mymap")
    )
  )

## Server
server <- function(input, output) {
  year_jail <- reactive({
    filtered <- county_shapefile %>% 
      left_join(incarceration_trends[incarceration_trends$year == input$yearInput])
      #incarceration_trends %>%
      #filter(year == input$yearInput) %>%
      #geo_join(county_shapefile, filtered, 'fips', 'fips', how = "left")
  })
  
  output$mymap <- renderLeaflet({
    bins <- c(0, 100, 250, 500, 1000, 2500, 5000, 10000, 30000)
    pal <- colorBin(palette = "OrRd", bins = bins, na.color = "#D3D3D3")
    
    year_jail() <- leaflet() %>% 
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
