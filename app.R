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
library(ggmap)
library(zipcode)
library(tidyr)
data(zipcode)

spreadSheet <- gs_title("DominiciE_PRCC_Chicago2017")
mapData <- as.data.frame(gs_read(ss=spreadSheet, ws = "Locations"))
sepData2 <- separate_rows(mapData, "Payment Types", sep = ",")
sepData1 <- separate_rows(sepData2, "Services Offered", sep = ",")
sepData <- separate_rows(sepData1, "Populations Served", sep = ",")
head(sepData)

service_choices <- unique(as.character(sepData$"Services Offered"))
spanish_choices <- unique(as.character(sepData$"Spanish"))
payment_choices <- unique(as.character(sepData$"Payment Types"))
zip_choices <- c(60007, 60018, 60068, 60106, 60131, 60176, 60601, 60602, 60603, 60604, 60605, 60606, 60607, 60608, 60609, 60610, 60611, 60612, 60613, 60614, 60615, 60616, 60617, 60618, 60619, 60620, 60621, 60622, 60623, 60624, 60625, 60626, 60628, 60629, 60630, 60631, 60632, 60633, 60634, 60636, 60637, 60638, 60639, 60640, 60641, 60642, 60643, 60644, 60645, 60646, 60647, 60649, 60651, 60652, 60653, 60654, 60655, 60656, 60657, 60659, 60660, 60661, 60706, 60707, 60714, 60804, 60827)




# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    # Include our custom CSS
    includeCSS("style.css")
  ),
  
  #title
  titlePanel(
    tags$div(id = "pagetitle",
             tags$h1("PRCC",tags$span(id = "subtitle","Asset Map"))
    )
    
  ),
  tags$hr(),
  sidebarLayout(
    #filters here
    sidebarPanel(id = 'sidebar',
      tags$ol(
                 tags$li(id='zipsearch',
                   tags$div(
                            selectInput(
                              'zipInput', 'SELECT YOUR ZIPCODE', choices = c("Select zip code", zip_choices), selected = "Select zip code")
                   )
                 ),
                 tags$li(
                   selectInput(
                     'servicesInput', 'Services:', choices = c(service_choices[!is.na(service_choices)]),
                     selectize = TRUE, multiple = TRUE
                   )
                 ),

                 tags$li(
                   selectInput(
                     'languagesInput', 'Language:', choices = c(service_choices[!is.na(spanish_choices)]), 
                     selectize = TRUE, multiple = TRUE
                   )
                 ),  
                 
                 tags$li(
                   selectInput(
                     'paymentInput', 'Payment Options:', choices = c(service_choices[!is.na(payment_choices)]), 
                     selectize = TRUE, multiple = TRUE
                   )
                 )
      )
                 
    ),
    
    
    
    mainPanel(
      #map
      leafletOutput('prccMap', height = 1000)
      #, tableOutput("results")
      
    )
    
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  
  # get zipcodes from google sheet
  
  
  
  
  #Map
  output$prccMap <- renderLeaflet({
    
    # get longitude and latitude of selected zipcode
    
    
    #Map
    prccMap <- mapData %>%
      
      leaflet() %>%
      addTiles()%>%
      # setView(lat = 41.8781, lng = -87.6298, zoom = 12) %>%
      addMarkers(lat = ~Lat, lng = ~Lng, popup = ~paste("<strong><a href='", Website, "' target='_blank'>", Name, "</a></strong><br>", Street, "<br>", City, ", ", State, Zip), clusterOptions = markerClusterOptions())
      #addResetMapButton() %>%
      #addSearchOSM(options = searchOptions(position = "topright"))
    prccMap
  })
  
  
  
  observeEvent(input$zipInput, {
    if(input$zipInput == "Select zip code"){
      proxy <-leafletProxy("prccMap", data = mapData)
      proxy %>%
        setView(lat = 41.8781, lng = -87.6298, zoom = 13)
      
    }else{
      print(as.numeric(input$zipInput))
      input_Zip = subset(zipcode, zip==as.numeric(input$zipInput),c("longitude", "latitude"))
      print(input_Zip)
      proxy <-leafletProxy("prccMap", data = mapData)
      proxy %>%
        setView(lat = as.numeric(input_Zip["latitude"]), lng =as.numeric(input_Zip["longitude"]), zoom = 15)
      
    }
    
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
