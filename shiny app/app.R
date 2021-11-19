library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(sf)
library(tigris) # geojoin
library(htmlwidgets)
library(shinythemes)
library(viridis)

## Import Data
incarceration_trends
county_shapefile
rates_by_race

## Widgets
# For interactive maps widget:
year_choices <- unique(incarceration_trends$year)
state_options <- unique(rates_by_race$ID)

## UI
ui <- navbarPage(
  theme = shinytheme("simplex"),
  title = "Incarceration Rates in America",
  # Tab 1: Histogram
    tabPanel(
      title = "Racial Comparison by States",
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
    ),
  # Tab 2: Map
  tabPanel(
    title = "Interactive Maps",
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "yearInput",
                    label = "Year:",
                    choices = year_choices,
                    selected = "2018",
                    multiple = FALSE)),
      mainPanel(leafletOutput("mymap", height= 600))
    )
  ),
  
  # Map over-time
  tabPanel(
    title = "Incarceration Trends overtime (1970-2018)",
    sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = "yearInput", 
        label = "Year", 
        min = 1970, 
        max = 2018, 
        value = 1970, 
        step = 1,
        animate =
        animationOptions(interval = 500, loop = TRUE))),
      mainPanel(plotOutput(outputId = "overtime"))
  )
)
)

## Server
server <- function(input, output) {
  # Histogram
  data_for_hist <- reactive({
    data <- rates_by_race %>% filter(ID==input$histvar)
  })
  
  output$hist <- renderPlot({
    
    ggplot(data= data_for_hist(), aes(x=race, y=rate)) +
      geom_bar(stat="identity", width=0.5, fill = "#2c7fb8") +
      labs(x = "Ethnicity",
           y = "Rate of Incarceration (Prisoners per 100,000 residents",
           title = "Rates of Incarceration By Ethnicity)") +
      
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
  
  # Maps
  year_jail <- reactive({
    filtered <- incarceration_trends %>%
      filter(year == input$yearInput) 
    
    m <- left_join(county_shapefile, filtered, by = "fips")
    return(m)
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
                popup = ~ paste0("County Name: ", name %>% str_to_title(), "<br>",
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
  
  # Map Overtime
  data_for_overtime <- reactive({
    filtered1 <- incarceration_trends %>%
      filter(year == 2018) 
    
    m1 <- left_join(county_shapefile, filtered1, by = "fips")
    return(m1)
  })
  
  output$overtime <- renderPlot({
    ggplot(data_for_overtime(), aes(fill = total_jail_pop_rate)) +
      geom_sf() +
      scale_fill_viridis(option = "magma", direction = -1) +
      theme_void() +
      labs(title="Jail Incarceration Rate by County", fill = "per \n100,000 residents")
  })
}

##Shiny App
shinyApp(ui = ui, server = server)
