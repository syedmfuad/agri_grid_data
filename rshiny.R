library(shiny)
library(readr)
library(raster)

# Define the UI
ui <- fluidPage(
  titlePanel("Crop and Country Selection"),
  sidebarLayout(
    sidebarPanel(
      selectInput("crop", "Select Crop:", choices = c("WHEA", "RICE")),
      selectInput("country", "Select Country:", choices = c("NGA", "CMR")),
      selectInput("agricultural_output", "Select Output Type:", choices = c("PRODUCTIVITY", "YIELD")),
      br(),
      downloadButton("download_csv", "Download CSV")
    ),
    mainPanel(
      textOutput("selected_options")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  
  # Display selected options
  output$selected_options <- renderText({
    paste("Selected Crop:", input$crop, "\n",
          "Selected Country:", input$country, "\n",
          "Selected Output Type:", input$agricultural_output)
  })
  
  
  # Create a reactive data frame
  data <- reactive({
    
    bd <- raster::getData('GADM', country=input$country, level=1)
    ad <- length(bd)
    
    thetahat=matrix(NA,ad,1)
    names=matrix(NA,ad,2)
    
    #x <- paste0("C:/Users/syedm/Desktop/soil/spam2010v2r0_global_prod.geotiff/spam2010V2r0_global_P_", input$crop, "_A.tif")
    
    
    
    x <- ifelse(input$agricultural_output == "PRODUCTIVITY", paste0("C:/Users/syedm/Desktop/soil/spam2010v2r0_global_prod.geotiff/spam2010V2r0_global_P_", input$crop, "_A.tif"),
                ifelse(input$agricultural_output == "YIELD", paste0("C:/Users/syedm/Desktop/soil/spam2010v2r0_global_yield.geotiff/spam2010V2r0_global_Y_", input$crop, "_A.tif"), ""))
    
    
    
    raster1 <- raster(here::here(x)) # load file
    # apply function
    
    bd_raster <- crop(raster1, extent(bd))
    #plot(bd_raster)
    bd_raster2 <- mask(bd_raster, bd)
    #plot(bd_raster2)
    #plot(bd, add=TRUE)
    
    d <- extract(x=bd_raster2, y=bd, fun=mean, na.rm=TRUE, sp=T)
    thetahat[,1]=d@data[,11] #was 13 before
    names[,1]=d@data[,2]
    names[,2]=d@data[,4]
    mat <- cbind(names, thetahat)
    
    data <- as.data.frame(mat)
    
  })
  
  
  
  # Download the data as a CSV file
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("crop_country_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(data(), file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
