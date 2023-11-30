

library(shiny)
library(dplyr)
library(raster)
library(googledrive)

# Define the UI
ui <- fluidPage(
  titlePanel("Spatial Production Crop, Country, and Output Selection"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("countries", "Add Countries:", choices = c("AGO", "BEN", "BWA", "BFA", "BDI", "CPV", "CMR", "CAF", "TCD", 
                                                                "COM", "COG", "COD", "CIV", "DJI", "GNQ", "ERI", "SWZ", "ETH", 
                                                                "GAB", "GMB", "GHA", "GIN", "GNB", "KEN", "LSO", "LBR", "MDG", 
                                                                "MWI", "MLI", "MRT", "MUS", "MOZ", "NAM", "NER", "NGA", "RWA", 
                                                                "STP", "SEN", "SYC", "SLE", "SOM", "TZA", "ZAF"), multiple = TRUE),
      conditionalPanel(
        condition = "input.countries.length > 0",
        selectizeInput("crop", "Select Crop:", choices = c("ACOF", "BANA", "BARL", "BEAN", "CASS", "CHIC", "CNUT", "COCO", 
                                                           "COTT", "COWP", "GROU", "LENT", "MAIZ", "OCER", "OFIB", "OILP", 
                                                           "OOIL", "OPUL", "ORTS", "PIGE", "PLNT", "PMIL", "POTA", "RAPE", 
                                                           "RCOF", "REST", "RICE", "SESA", "SMIL", "SORG", "SOYB", "SUGB", 
                                                           "SUGC", "SUNF", "SWPO", "TEAS", "TEMF", "TOBA", "TROF", "VEGE", 
                                                           "WHEA", "YAMS"), multiple = TRUE),
        selectizeInput("output_type", "Select Output Type:", choices = c("PRODUCTION", "YIELD"), multiple = TRUE)
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
  
  
  
  read_raster_from_drive <- function(folder_url, crop_chosen, output) {
    
    #drive_auth(cache = ".secrets")
    drive_auth(cache = ".secrets", email = TRUE, use_oob = TRUE)
    
    folder <- drive_get(as_id(folder_url))
    files <- drive_ls(folder$id)
    
    file_name <- ifelse(output == "PRODUCTION", 
                        paste0("spam2017V2r1_SSA_P_", crop_chosen, "_A.tif"),
                        ifelse(output == "YIELD", 
                               paste0("spam2017V2r1_SSA_Y_", crop_chosen, "_A.tif"), ""))
    
    
    #file_name <- paste0("spam2017V2r1_SSA_P_", crop_chosen, "_A.tif")
    
    file <- files[which(files$name == file_name),]
    
    if (length(file) == 0) {
      print("File not found.")
    } else {
      
      temp_file <- tempfile(fileext = ".tif")
      x <- drive_download(as_id(file$id), path = temp_file)
      
      raster1 <- raster(here::here(x$local_path))
      
      return(raster1)
      
      file.remove(temp_file)
      
    }
  }
  
  
  
  
  # Function to extract data for a single country and crop
  extractData <- function(country, crop, output_type) {
    bd <- raster::getData('GADM', country = country, level = 1)
    ad <- length(bd)
    
    thetahat <- vector("numeric", ad)
    names1 <- vector("character", ad)
    names2 <- vector("character", ad)
    
    folder_url <- ifelse(output_type == "PRODUCTION", "https://drive.google.com/drive/folders/1FqeBF-y4iMMHIxCEybXAlwqPJ7KSJkd5",
                         ifelse(output_type == "YIELD", "https://drive.google.com/drive/folders/11imEWGPvGsesshkPuwj3s7_lWHUiPleY", ""))
    
    raster1 <- read_raster_from_drive(folder_url=folder_url, crop_chosen=crop, output=output_type)
    
    #raster1 <- raster(here::here(x))
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
