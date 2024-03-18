

library(shiny)
library(tidyverse)
library(readr)
library(leaflet)
library(sf)


# UI definition
ui <- fluidPage(
  titlePanel("California Hantavirus Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      verbatimTextOutput("sidebar_text")
    ),
    
    mainPanel(
      fluidRow(
        column(
          width = 12,  # Adjust the width as needed
          tabsetPanel(
            tabPanel("Vector Seropositivity Map",
                     leafletOutput("vector_prevalence_map")), 
            tabPanel("Human Cases Map",
                     leafletOutput("hantavirus_cases_map")),            
            tabPanel("Human Cases by County",
                     plotOutput("human_cases_plot")),
            tabPanel("Vector Seropositivity by County",
                     plotOutput("hantavirus_carriage_plot")),
            tabPanel("Linear Model",
                     plotOutput("lm_plot"))
          )
        ),
        column(
          width = 11, 
          
        )
      )
    )
  )
)


# Server logic
server <- function(input, output, session) {
  
  
  output$sidebar_text <- renderText({
"Hantaviruses are vector-borne pathogens
that cause severe pulmonary disease 
in humans. In the US, these viruses are
carried by North American Deer Mice and 
shed in their droppings, urine and saliva.
Humans become infected by inhaling 
aerosolized viral material that is 
generated when the air around mouse
droppings and nesting material is
disturbed in confined, poorly-ventilated 
spaces.

This dashboard is meant to visualize 
the relationship between Sin Nombre 
Hantavirus carriage in North American 
Deer Mice and human infections in the 
state of California.
    
Although human cases are rare, a linear
model can be fit to the data in 
counties with human infections.
The model shows a positive correlation
between viral seroprevalence in 
mice and infections in humans that 
interact with themin the environment."
  })
  
  data <- reactive({
    # Read and preprocess data
    Hantavirus_update <- read_csv('Deer Mouse Prevalence.csv')
    counties <- read_csv('Countypopulations.csv')
    
    # Merge data
    Hantavirus_updated <- merge(Hantavirus_update, counties, by = "County")
    
    # Data manipulation
    Hantavirus_updated <- Hantavirus_updated %>%
      mutate(pPrev = Prevalence,
             Human_pPrev = Human_Cases / Human_Population,
             prevalence_ratio = Human_pPrev / (Prevalence/100))
    
    Hantavirus_updated
  })
  
  # Render vector prevalence map
  output$vector_prevalence_map <- renderLeaflet({
    req(data())
    
    # Load California county boundaries
    county_boundaries <- st_read("california_counties.dbf") 
    
    # Merge vector prevalence with county boundaries
    county_map_data <- merge(county_boundaries, data(), by.x = "NAME", by.y = "NAME", all.x = TRUE)
    # Filter out missing values from the "Prevalence" column
    county_map_data <- county_map_data[!is.na(county_map_data$Prevalence), ]
    
    # Create a color palette for the legend
    pal <- colorNumeric(palette = "YlOrRd", domain = county_map_data$Prevalence, na.color = "white")
    
    # Create the Leaflet map
    leaflet(county_map_data) %>%
      # Remove the basemap
      setView(lng = -119.4179, lat = 36.7783, zoom = 5.3) %>% # Adjust the view as needed
      addTiles() %>%
      # Add polygons with custom fillColor
      addPolygons(fillColor = ~pal(Prevalence),
                  fillOpacity = 1,
                  weight = 1,
                  color = "black",
                  popup = ~paste(County, "<br>", "Prevalence: ", Prevalence)) %>%
      # Add legend
      addLegend(position = "bottomright", # Position the legend
                pal = pal, # Specify the color palette
                values = ~Prevalence, # Values to label in the legend
                title = "% Seropositive Mice", # Title of the legend
                opacity = 1) # Adjust legend opacity if needed
  })
  
  # Render Hantavirus cases map
  output$hantavirus_cases_map <- renderLeaflet({
    req(data())
    
    # Load California county boundaries
    county_boundaries <- st_read("california_counties.dbf") 
    
    # Merge case counts with county boundaries
    county_map_data <- merge(county_boundaries, data(), by.x = "NAME", by.y = "NAME", all.x = TRUE)
    # Filter out missing values from the "Human_Cases" column
    county_map_data <- county_map_data[!is.na(county_map_data$Human_Cases), ]
    
    # Create a color palette for the legend
    pal <- colorNumeric(palette = "YlOrRd", domain = county_map_data$Human_Cases, na.color = "white")
    
    # Create the Leaflet map
    leaflet(county_map_data) %>%
      # Remove the basemap
      setView(lng = -119.4179, lat = 36.7783, zoom = 5.3) %>% # Adjust the view as needed
      addTiles() %>%
      # Add polygons with custom fillColor
      addPolygons(fillColor = ~pal(Human_Cases),
                  fillOpacity = 1,
                  weight = 1,
                  color = "black",
                  popup = ~paste(County, "<br>", "Cases: ", Human_Cases)) %>%
      # Add legend
      addLegend(position = "bottomright", # Position the legend
                pal = pal, # Specify the color palette
                values = ~Human_Cases, # Values to label in the legend
                title = "Human Cases", # Title of the legend
                opacity = 1) # Adjust legend opacity if needed
  })
  
  # Render human cases plot
  output$human_cases_plot <- renderPlot({
    req(data())
    data() %>%
      filter(Human_Cases > 0) %>%
      group_by(County) %>%
      ggplot(aes(x = reorder(County, Human_Cases), y = Human_Cases)) +
      geom_bar(stat = "identity", fill = "black") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
      theme_test()+
      coord_flip() +
      scale_y_continuous(limits = c(0, 25)) +
      labs(title = "Human Cases of Hantavirus by County (1980 - 2020)",
           x = "County",
           y = "Case Count")
  })
  
  # Render hantavirus carriage plot
  output$hantavirus_carriage_plot <- renderPlot({
    req(data())
    data() %>%
      filter(pPrev > 0) %>%
      group_by(County) %>%
      ggplot(aes(x = reorder(County, pPrev), y = pPrev)) +
      geom_bar(stat = "identity", fill = "black") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
      theme_test()+
      coord_flip() +
      scale_y_continuous(limits = c(0, 40)) +
      labs(title = "Hantavirus Carriage among North American Deer Mice, 2013 - 2022",
           x = "County",
           y = "% Prevalence")
  })
  
  # Render linear model plot
  output$lm_plot <- renderPlot({
    req(data())
    Updated_lm <- data() %>%
      filter(Human_Cases >= 2)
    
    updated_model <- lm(Human_pPrev ~ pPrev, data = Updated_lm)
    ggplot(Updated_lm, aes(x = pPrev, y = Human_pPrev)) +
      geom_point() +
      geom_abline(intercept = -8.824761e-05, slope = 3.191788e-05) +
      theme_test()+
      labs(title = "Human Prevalence vs Vector Prevalence (R^2 = .54)",
           x = "Hantavirus Prevalence in North American Deer Mice (%)",
           y = "Percent Prevalence in Humans (%)")
  })

}

# Run
shinyApp(ui, server)
