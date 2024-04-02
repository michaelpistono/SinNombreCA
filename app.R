
library(shiny)
library(tidyverse)
library(readr)
library(leaflet)
library(sf)
library(patchwork)
library(reshape2)
library(car)
library(magrittr)




# Define UI for application
ui <- fluidPage(
  titlePanel("California Hantavirus Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      verbatimTextOutput("sidebar_text")
    ),
    
    mainPanel(
      fluidRow(
        column(
          width = 12,  
          tabsetPanel(
            tabPanel("Vector Prevalence",
                     fluidRow(
                       column(width = 12, leafletOutput("vector_prevalence_map")),
                       column(width = 12, plotOutput("hantavirus_carriage_plot"))
                     )),
            tabPanel("Human Cases",
                     fluidRow(
                       column(width = 12, leafletOutput("hantavirus_case_map")),
                       column(width = 12, plotOutput("human_cases_plot"))
                     )),
            tabPanel("Human Prevalence",
                     fluidRow(
                       column(width = 12, leafletOutput("human_prevalence_map")),
                       column(width = 12, plotOutput("human_prevalence_plot"))
                     ))
          )
        )
      )
    )
  )
)


# Server logic
server <- function(input, output, session) {
  
  
  output$sidebar_text <- renderText({
    "Hantaviruses are vector-borne pathogens
that cause severe respiratory disease 
in humans. Sin Nombre hantavirus--the
most common in the United States--is
primarily carried by North American 
deer mice and shed in their feces, 
urine and saliva. Humans are infected 
by inhaling aerosolized viral particles 
that are generated when the air around 
mouse droppings or nesting material is 
disturbed, especially in confined, 
poorly-ventilated spaces. The virus is
not known to pass person-to-person.
    
Although infection is rare in humans,
the results are often fatal: 
hantavirus pulmonary syndrome,
the most common form of disease,
has a 38% case fatality risk and most
patients require intensive care and
intubation. There is no vaccine or 
specific cure for hantaviruses.

Deer mice can be found in most places
throughout the California, but they're
less common in urban environments. They 
prefer rural or sparcely-inhabited
areas and many of their interactions
with humans occur when the mice enter 
homes looking for food or nesting material.
Infestations are also common in sheds, 
abandoned vehicles and seasonal cabins 
that lay dormant for long periods."
    
  })
  
  data <- reactive({
    
    Hantavirus_update <- read_csv('Deer Mouse Prevalence.csv')
    
    Hantavirus_updated <- Hantavirus_update %>%
      mutate(human_prevalence = human_cases / human_population,
             vector_prevalence = avg_vector_prevalence/100,
             prevalence_ratio = human_prevalence/avg_vector_prevalence,
             pop_density = (1/pop_density))
  })
  
  
  output$vector_prevalence_map <- renderLeaflet({
    req(data())
    
    
    county_boundaries <- st_read("california_counties.shp") 
    
 
    county_map_data <- merge(county_boundaries, data(), by.x = "GEOIDFQ", by.y = "AFFGEOID", all.x = TRUE)
    
    county_map_data <- county_map_data[!is.na(county_map_data$avg_vector_prevalence), ]
    
    
    pal <- colorNumeric(palette = "YlOrRd", domain = county_map_data$avg_vector_prevalence, na.color = "white")
    
  
    leaflet(county_map_data) %>%
     
      setView(lng = -119.4179, lat = 36.7783, zoom = 5.3) %>% # Adjust the view as needed
      addTiles() %>%
      
      addPolygons(fillColor = ~pal(avg_vector_prevalence),
                  fillOpacity = 1,
                  weight = 1,
                  color = "black",
                  popup = ~paste(County, "<br>", "Prevalence: ", avg_vector_prevalence)) %>%
      
      addLegend(position = "bottomright", # Position the legend
                pal = pal, # Specify the color palette
                values = ~avg_vector_prevalence, # Values to label in the legend
                title = "% Seropositive Mice", # Title of the legend
                opacity = 1) # Adjust legend opacity if needed
    
    
  })
  
  # Render Hantavirus cases map
  output$hantavirus_case_map <- renderLeaflet({
    req(data())
    
    # Load California county boundaries
    county_boundaries <- st_read("california_counties.shp") 
    
    # Merge case counts with county boundaries
    county_map_data <- merge(county_boundaries, data(), by.x = "GEOIDFQ", by.y = "AFFGEOID", all.x = FALSE)
   
    county_map_data <- county_map_data[!is.na(county_map_data$human_cases), ]
    
    # Create a color palette for the legend
    pal <- colorNumeric(palette = "YlOrRd", domain = county_map_data$human_cases, na.color = "white")
    
    # Create the Leaflet map
    leaflet(county_map_data) %>%
      # Remove the basemap
      setView(lng = -119.4179, lat = 36.7783, zoom = 5.3) %>% # Adjust the view as needed
      addTiles() %>%
      # Add polygons with custom fillColor
      addPolygons(fillColor = ~pal(human_cases),
                  fillOpacity = 1,
                  weight = 1,
                  color = "black",
                  popup = ~paste(County, "<br>", "Prevalence: ", human_cases)) %>%
      # Add legend
      addLegend(position = "bottomright", # Position the legend
                pal = pal, # Specify the color palette
                values = ~human_cases, # Values to label in the legend
                title = "Human Cases", # Title of the legend
                opacity = 1) # Adjust legend opacity if needed
  })
  

  output$human_prevalence_map <- renderLeaflet({
    req(data())
    
    # Load California county boundaries
    county_boundaries <- st_read("california_counties.shp") 
    
    # Merge case counts with county boundaries
    county_map_data <- merge(county_boundaries, data(), by.x = "GEOIDFQ", by.y = "AFFGEOID", all.x = FALSE)
    
    county_map_data <- county_map_data[!is.na(county_map_data$human_cases), ]
    county_map_data <- county_map_data%>%
      mutate(human_prevalence = (human_cases / human_population)*10000,
             vector_prevalence = avg_vector_prevalence/100,
             prevalence_ratio = human_prevalence/avg_vector_prevalence,
             pop_density = (1/pop_density))
    
    # Create a color palette for the legend
    pal <- colorNumeric(palette = "YlOrRd", domain = county_map_data$human_prevalence, na.color = "white")
    
    # Create the Leaflet map
    leaflet(county_map_data) %>%
      # Remove the basemap
      setView(lng = -119.4179, lat = 36.7783, zoom = 5.3) %>% # Adjust the view as needed
      addTiles() %>%
      # Add polygons with custom fillColor
      addPolygons(fillColor = ~pal(human_prevalence),
                  fillOpacity = 1,
                  weight = 1,
                  color = "black",
                  popup = ~paste(County, "<br>", "Prevalence: ", human_prevalence)) %>%
      # Add legend
      addLegend(position = "bottomright", # Position the legend
                pal = pal, # Specify the color palette
                values = ~human_prevalence, # Values to label in the legend
                title = "Cases/10,000 People", # Title of the legend
                opacity = 1) # Adjust legend opacity if needed
  })
  
  
  
  output$hantavirus_carriage_plot <- renderPlot({
    req(data())
    data() %>%
      filter(avg_vector_prevalence > 0) %>%
      group_by(County) %>%
      ggplot(aes(x = reorder(County, avg_vector_prevalence), y = avg_vector_prevalence)) +
      geom_bar(stat = "identity", fillColor = ~pal(avg_vector_prevalence)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
      theme_test()+
      coord_flip() +
      scale_y_continuous(limits = c(0, 40)) +
      labs(title = "Average Hantavirus Prevalence Among Deer Mice, 2000 - 2022",
           x = "County",
           y = "Prevalence (%)")
  })
  
  
  
  
  # Render human cases plot
  output$human_cases_plot <- renderPlot({
    req(data())
    data() %>%
      filter(human_cases > 0) %>%
      group_by(County) %>%
      ggplot(aes(x = reorder(County, human_cases), y = human_cases)) +
      geom_bar(stat = "identity", fillColor = ~pal(human_cases)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
      theme_test()+
      coord_flip() +
      scale_y_continuous(limits = c(0, 25)) +
      labs(title = "Human Cases of Hantavirus by County (1980 - 2020)",
           x = "County",
           y = "Case Count")
  })
  

  
  # Render hantavirus carriage plot
  output$human_prevalence_plot <- renderPlot({
    req(data())
    data() %>%
      filter(human_prevalence > 0) %>%
      group_by(County) %>%
      ggplot(aes(x = reorder(County, human_prevalence), y = human_prevalence)) +
      geom_bar(stat = "identity", fillColor = ~pal(human_prevalence)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
      theme_test()+
      coord_flip() +
      scale_y_continuous(limits = c(0, .0020)) +
      labs(title = "Hantavirus Prevalence in Humans, 1980 - 2022",
           x = "County",
           y = "Prevalence (%)")
  })
  
}

# Run the application
shinyApp(ui, server)
