#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Global Environment
library(shiny)
library(googlesheets)
library(leaflet)
library(RDSTK)
library(leaflet.extras)



# Define UI for application that draws a histogram
ui <- fluidPage(
  #title
  titlePanel("PRCC Asset Map"),
  
  sidebarLayout(
    
    #filters here
    sidebarPanel(
      
    ),
    mainPanel(
      
      #map
      leafletOutput('prcc_map', height = 1000)
      #, tableOutput("results")
      
    )
    
  )
 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #Reading Data
  spreadSheet <- gs_title("Copy of PRCC Asset Data Template")
  mapData <- as.data.frame(gs_read(ss=spreadSheet, ws = "Master Sheet"))
  
  #Preparing Data
  #Geocode Addresses
  #Create full Address for geocoding
  mapData$fullAddress <- paste(mapData$Address, mapData$City, mapData$State, sep = ", ")
  for(i in 1:nrow(mapData))
  {
    # Print("Working...")
    result <- street2coordinates(mapData$fullAddress[i])
    mapData$lon[i] <- as.numeric(result["longitude"])
    mapData$lat[i] <- as.numeric(result["latitude"])
    mapData$geoAddress[i] <- as.character(result[3])
  }
  
  #Map
  output$prcc_map <- renderLeaflet({
    #Map
    prccMap <- mapData %>%
      leaflet() %>%
      addTiles()%>%
      setView(lat = 41.8781, lng = -87.6298, zoom = 12) %>%
      addMarkers(lat = ~lat, lng = ~lon, popup = ~paste("<strong><a href='", Website, "' target='_blank'>", Name, "</a></strong><br>", Address, "<br>", City, ", ", State, Zip), clusterOptions = markerClusterOptions())%>%
      addResetMapButton() %>%
      addSearchOSM() 
    prccMap
    })

 
}

# Run the application 
shinyApp(ui = ui, server = server)

