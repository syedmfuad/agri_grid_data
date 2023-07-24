library(shiny)
library(dplyr)
library(raster)

# Define the UI
ui <- fluidPage(
  titlePanel("Crop and Country Selection"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("countries", "Add Countries:", choices = c("NGA", "CMR", "TZA", "COD", "AGO", "COG"), multiple = TRUE),
      conditionalPanel(
        condition = "input.countries.length > 0",
        selectizeInput("crop", "Select Crop:", choices = c("WHEA", "RICE", "MAIZ", "BARL", "PMIL", "SMIL"), multiple = TRUE),
        selectizeInput("output_type", "Select Output Type:", choices = c("PRODUCTIVITY", "YIELD", "HARV_AREA"), multiple = TRUE)
      ),
      br(),
      downloadButton("download_csv", "Download CSV")
    ),
    mainPanel(
      dataTableOutput("result_table")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Function to extract data for a single country and crop
  extractData <- function(country, crop, output_type) {
    bd <- raster::getData('GADM', country = country, level = 1)
    ad <- length(bd)
    
    thetahat <- vector("numeric", ad)
    names1 <- vector("character", ad)
    names2 <- vector("character", ad)
    
    x <- ifelse(output_type == "PRODUCTIVITY", 
                paste0("C:/Users/syedm/Desktop/soil/spam2010v2r0_global_prod.geotiff/spam2010V2r0_global_P_", crop, "_A.tif"),
                ifelse(output_type == "YIELD", 
                       paste0("C:/Users/syedm/Desktop/soil/spam2010v2r0_global_yield.geotiff/spam2010V2r0_global_Y_", crop, "_A.tif"), 
                ifelse(output_type == "HARV_AREA", 
                       paste0("C:/Users/syedm/Desktop/soil/spam2010v2r0_global_harv_area.geotiff/spam2010V2r0_global_H_", crop, "_A.tif"), ""))) 
    
    raster1 <- raster(here::here(x))
    bd_raster <- crop(raster1, extent(bd))
    bd_raster2 <- mask(bd_raster, bd)
    
    d <- extract(x = bd_raster2, y = bd, fun = mean, na.rm = TRUE, sp = TRUE)
    thetahat <- d@data[, 11] # was 13 before
    names1 <- d@data[, 2]
    names2 <- d@data[, 4]
    
    data <- data.frame(Country = rep(country, ad),
                       Crop = rep(crop, ad),
                       stringsAsFactors = FALSE)
    
    data$District <- names2
    data[[output_type]] <- thetahat
    
    data
  }
  
  # Create a reactive data frame
  result <- reactive({
    countries <- input$countries
    crops <- input$crop
    output_types <- input$output_type
    
    data <- data.frame(Country = character(),
                       Crop = character(),
                       stringsAsFactors = FALSE)
    
    for (country in countries) {
      for (crop in crops) {
        row <- data.frame(Country = country,
                          Crop = crop,
                          stringsAsFactors = FALSE)
        
        for (output_type in output_types) {
          extracted_data <- extractData(country, crop, output_type)
          col_name <- output_type
          
          row <- do.call("rbind", replicate(n=nrow(extracted_data), row, simplify = FALSE)) #added this myself
          
          row <- row[1:nrow(extracted_data),]
          
          row$District <- extracted_data$District
          row[[col_name]] <- extracted_data[[col_name]]
          
          row %>% relocate(District, .after=Country)
          
        }
        
        data <- rbind(data, row)
      }
    }
    
    data
  })
  
  # Render the result table
  output$result_table <- renderDataTable({
    result()
  })
  
  # Download the data as a CSV file
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("crop_country_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(result(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
