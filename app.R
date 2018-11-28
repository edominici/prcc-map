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
library(zipcode)
data(zipcode)



# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    # Include our custom CSS
    #includeCSS("style.css")
  ),
  
  #title
  titlePanel("PRCC Asset Map"),
  
  sidebarLayout(

    #filters here
    sidebarPanel(
      selectInput('zip_list', 'Select Your Community','60201')
    ),

    
    mainPanel(
      #map
      leafletOutput('prcc_map', height = 1000)
      #, tableOutput("results")

    )
    
  )
 
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  #Reading Data
  spreadSheet <- gs_title("Copy of PRCC Asset Data Template")
  mapData <- as.data.frame(gs_read(ss=spreadSheet, ws = "Master Sheet"))
  
  #Preparing Data
  #Geocode Addresses
  #Create full Address for geocoding
  mapData$fullAddress <- paste(mapData$Address, mapData$City, mapData$State, mapData$Zip,sep = ", ")
  for(i in 1:nrow(mapData))
  {
    # Print("Working...")
    result <- street2coordinates(mapData$fullAddress[i])
    mapData$lon[i] <- as.numeric(result["longitude"])
    mapData$lat[i] <- as.numeric(result["latitude"])
    mapData$geoAddress[i] <- as.character(result[3])
  }
  
  # get zipcodes from google sheet
  get_zipcodes = reactive({
    zip_data = mapData$Zip
  })
  
  observe({
    updateSelectInput(session, "zip_list",choices = get_zipcodes()
  )})

  
 
  #Map
  output$prcc_map <- renderLeaflet({
    
    # get longitude and latitude of selected zipcode
    print(as.numeric(input$zip_list))
    input_Zip = subset(zipcode, zip==as.numeric(input$zip_list),c("longitude", "latitude"))
   
    #Map
    prccMap <- mapData %>%
      
      leaflet() %>%
      addTiles()%>%
      setView(lat = as.numeric(input_Zip["latitude"]), lng =as.numeric(input_Zip["longitude"]), zoom = 15) %>%

      addMarkers(lat = ~lat, lng = ~lon, popup = ~paste("<strong><a href='", Website, "' target='_blank'>", Name, "</a></strong><br>", Address, "<br>", City, ", ", State, Zip), clusterOptions = markerClusterOptions())%>%
      #addResetMapButton() %>%
      addSearchOSM(options = searchOptions(position = "topright"))
    prccMap
  })
 
}

# Run the application 
shinyApp(ui = ui, server = server)

