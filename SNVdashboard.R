library(tidyverse)
library(leaflet)
library(sf)
library(reshape2)
library(car)
library(magrittr)
library(httr)
library(jsonlite)
library(shiny)


ca_access_token <- "893a0451-03da-4072-b87e-eeaa5f99ac6e"
access_token <- "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"
esri_token <- "AAPKebecfb908b6b4967966629c5ea06dae1z6U3gCGH5p762e6dURo8TnctKjdjqUs5kRcUIypc5O0xXQ-KDs3w9WiQs32PrauQ"

fetch_data_as_dataframe <- function(url) {
  
  response <- httr::GET(url, httr::add_headers(Authorization = paste("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", access_token)))
  dataframe <- read.csv(text = httr::content(response, as = "text", encoding = "UTF-8"))
  return(dataframe)
}

#fetch_json_data <- function(url) {
# response <- httr::GET(url, httr::add_headers(Authorization = paste("AAPKebecfb908b6b4967966629c5ea06dae1z6U3gCGH5p762e6dURo8TnctKjdjqUs5kRcUIypc5O0xXQ-KDs3w9WiQs32PrauQ", esri_token)))
#json_data <- httr::content(response, as = "text", encoding = "UTF-8")
#json_parsed <- jsonlite::fromJSON(json_data)
#return(json_parsed)
#}


fetch_shapefile_as_sf <- function(url, ca_access_token = "893a0451-03da-4072-b87e-eeaa5f99ac6e") {
  # Make GET request
  response <- httr::GET(url, httr::add_headers(Authorization = ca_access_token))

  # Create a temporary file
  temp_zip <- tempfile(fileext = ".zip")
  
  # Write response to the temporary file
  writeBin(httr::content(response, as = "raw"), temp_zip)
  
  # Extract the contents of the ZIP file
  unzip_dir <- tempdir()
  unzip(temp_zip, exdir = unzip_dir)
  
  # Print out the contents of the extracted directory
  extracted_files <- list.files(unzip_dir, full.names = TRUE, recursive = TRUE)
  print(extracted_files)
  
  # Check each extracted file for a shapefile
  shp_files <- extracted_files[grep("\\.shp$|\\.SHP$", extracted_files)]
  
  # Read the first shapefile found into an sf object
  sf_data <- st_read(shp_files[1])
  return(sf_data)
}


# Define UI
ui <- fluidPage(
  titlePanel("California Sin Nombre Hantavirus Dashboard"),
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
            tabPanel("Human Prevalence",
                     fluidRow(
                       column(width = 12, leafletOutput("human_prevalence_map")),
                       column(width = 12, plotOutput("human_prevalence_plot"))
                     )),
            tabPanel("Human Cases",
                     fluidRow(
                       column(width = 12, leafletOutput("human_cases_map")),
                       column(width = 12, plotOutput("human_cases_plot"))
                     ))
          )
        )
      )
    )
  )
)

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
  

  #res2 = fetch_json_data("https://caltrans-gis.dot.ca.gov/arcgis/rest/services/CHboundary/County_Boundaries/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
  #res2df = as.data.frame(res2$features$properties)
  #res2df.final = res2df[1:58, ]
  
  
  df <- fetch_data_as_dataframe("https://raw.githubusercontent.com/michaelpistono/SinNombreCA/main/Deer%20Mouse%20Prevalence.csv")
  df = df%>%mutate(human_prevalence = (human_cases / human_population)*10000)
  
  
  res2 = fetch_shapefile_as_sf("https://data.ca.gov/dataset/e212e397-1277-4df3-8c22-40721b095f33/resource/b0007416-a325-4777-9295-368ea6b710e6/download/ca_county_boundaries.zip")
  county_map_data <- merge(res2, df, by.x = "GEOIDFQ", by.y = "AFFGEOID", all.x = TRUE)
  county_map_sf <- st_as_sf(county_map_data, wkt = "geometry")
  county_map_sf <- st_set_crs(county_map_sf, 4326)
  county_map_sf <- st_transform(county_map_sf, crs = 4269)
  
  
  output$human_cases_map <- renderLeaflet({

    pal <- colorNumeric(palette = "YlOrRd", domain = county_map_sf$human_cases, na.color = "white")
    
    leaflet(county_map_sf) %>%
      
      setView(lng = -119.4179, lat = 36.7783, zoom = 5.3) %>% 
      addTiles() %>%
      
      addPolygons(fillColor = ~pal(human_cases),
                  fillOpacity = 1,
                  weight = 1,
                  color = "black",
                  popup = ~paste(County, "<br>", "Human Cases: ", human_cases)) %>%
      
      addLegend(position = "bottomright", # Position the legend
                pal = pal, # Specify the color palette
                values = ~human_cases, # Values to label in the legend
                title = "Human Cases", # Title of the legend
                opacity = 1) # Adjust legend opacity if needed
  })
  
  output$vector_prevalence_map <- renderLeaflet({
    
    pal <- colorNumeric(palette = "YlOrRd", domain = county_map_sf$avg_vector_prevalence, na.color = "white")
    
    leaflet(county_map_sf) %>%
      
      setView(lng = -119.4179, lat = 36.7783, zoom = 5.3) %>% 
      addTiles() %>%
      
      addPolygons(fillColor = ~pal(avg_vector_prevalence),
                  fillOpacity = 1,
                  weight = 1,
                  color = "black",
                  popup = ~paste(County, "<br>", "Vector Seropositivity: ", avg_vector_prevalence)) %>%
      
      addLegend(position = "bottomright", 
                pal = pal, 
                values = ~avg_vector_prevalence, 
                title = "Vector Seropositivity (%)",
                opacity = 1) 
  })
  
  
  output$human_prevalence_map <- renderLeaflet({
    
    pal <- colorNumeric(palette = "YlOrRd", domain = county_map_sf$human_prevalence, na.color = "white")
    
    leaflet(county_map_sf) %>%
      
      
      setView(lng = -119.4179, lat = 36.7783, zoom = 5.3) %>% 
      addTiles() %>%
      
      addPolygons(fillColor = ~pal(human_prevalence),
                  fillOpacity = 1,
                  weight = 1,
                  color = "black",
                  popup = ~paste(County, "<br>", "Prevalence in Humans: ", human_prevalence)) %>%
      
      addLegend(position = "bottomright", 
                pal = pal, 
                values = ~human_prevalence, 
                title = "Cases per 10,000",
                opacity = 1) 
  })
  
  
  output$hantavirus_carriage_plot <- renderPlot({
    
    county_map_sf%>%
      filter(avg_vector_prevalence > 0) %>%
      group_by(County) %>%
      ggplot(aes(x = reorder(County, avg_vector_prevalence), y = avg_vector_prevalence)) +
      geom_bar(stat = "identity", fill = "black") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
      theme_test()+
      coord_flip() +
      scale_y_continuous(limits = c(0, 40)) +
      labs(title = "Average Hantavirus Prevalence Among Deer Mice, 2000 - 2022",
           x = "County",
           y = "Prevalence (%)")
  })
  
  output$human_cases_plot <- renderPlot({
    
    county_map_sf%>%
      filter(human_cases > 0) %>%
      group_by(County) %>%
      ggplot(aes(x = reorder(County, human_cases), y = human_cases)) +
      geom_bar(stat = "identity", fill = "black") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
      theme_test()+
      coord_flip() +
      scale_y_continuous(limits = c(0, 40)) +
      labs(title = "Human Hantavirus Cases, 1980 - 2022",
           x = "County",
           y = "Cases")
  })
  
  output$human_prevalence_plot <- renderPlot({
    
    county_map_sf%>%
      filter(human_prevalence > 0) %>%
      group_by(County) %>%
      ggplot(aes(x = reorder(County, human_prevalence), y = human_prevalence)) +
      geom_bar(stat = "identity", fill = "black") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
      theme_test()+
      coord_flip() +
      scale_y_continuous(limits = c(0, 20)) +
      labs(title = "Hantavirus Cases Per 10,000 People",
           x = "County",
           y = "Cases per 10,000 People")
  })
  
}  
shinyApp(ui = ui, server = server)