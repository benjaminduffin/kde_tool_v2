#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Libraries ---------------------------------------------------------------
library(shiny)
library(spatialEco)
library(leaflet)
library(sf)



# File upload size limits -------------------------------------------------
# sets max size to 30MB
options(shiny.maxRequestSize=30*1024^2)


# Define the UI -----------------------------------------------------------

ui <- fluidPage(
  
  # Application title
  titlePanel("HMS EFH Kernel Density Estimation and Percent Volume Contour Tool"),
  
  # Sidebar with a file upload for .xlsx or csv files? 
  sidebarLayout(
    
    # begin sidebar
    sidebarPanel(
      
      # ADD A PICTURE OF A FISH
      img(src = "bft_pic.PNG"),
      
      tags$hr(), # horizontal line
      
      ## add title for each 'section'
      h3("File Upload"),
      # select input file type 
      radioButtons("fileType", "Choose file type", 
                   choices = list(".csv/text" = 1, ".xlsx" = 2), 
                   selected = 1, 
                   inline = TRUE
      ),
      
      tags$hr(),
      # upload the file 
      fileInput("file1", "Choose .csv or .xlsx file", 
                multiple = F, 
                accept = c(".csv", ".xlsx", "text/csv", "text/comma-separated-values,text/plain")
      ), 
      
      
      tags$hr(), # horizontal line
      
      ## Columns selection section 
      h3("Select Spatial Data"),
      
      # long
      selectInput('x_col', "Select X data (longitude)", 
                  choices = NULL), 
      # lat 
      selectInput("y_col", "Select y data (latitude)", 
                  choices = NULL), 
      
      # action button to map the xy data
      actionButton("plot_xy", "Plot Points"), 
      
      
      #---------------------------- # this needs to change (use preselected params) ------------
      ## Provide model parameters 
      tags$hr(), 
      h3("Model Parameters"),
      
      # bandwidth
      # href(adehabitatHR) = ((var(x)+var(y)) * 0.5) * n^(-1/6); 
      # MASS::bandwidht.nrd; 'rule of thumb'
      radioButtons("bandwidth", "Select bandwidth (h)", 
                   choices = list("href (default)"=1, "LSCV"=2), 
                   selected = 1, 
                   inline = T),
      
      # kernel selection ---------------- REMOVE
      radioButtons("kernel", "Select kernel (EPA not compatible with LSCV)", 
                   choices = list("Bivariate Norm" = 1, "epa" = 2), 
                   selected = 1, 
                   inline = T), 
      
      # grid for kernelUD ------------------------------------------- CHECK GRID CALC
      numericInput("grid_size", "Enter grid size", 
                   value = "1000", min = 100),
      
      # percent volume contour for polygon
      selectInput("pvc", "Percent Volume Contour", 
                  choices = c(50, 75, 90, 95), 
                  selected = 0.95), 
      
      # add action button to run the model 
      actionButton("run_model", "Execute KDE"), 
      
      ## add button to download the pvc as a shapefile 
      tags$hr(),
      
      h3("Download File"),
      # enter species 
      textInput("species", "Enter Species"), 
      
      # save button - downloadhandler
      downloadButton("download_data", "Download") # downloadhandler
      # save button - shinyFiles:: NOT RUN
      #actionButton("download_file", "Download File")
      
    ), 
    
    ## output from uploaded data and selected variables 
    mainPanel(
      br(),
      
      h3("Glimpse of the data you uploaded:"), 
      textOutput("data_dims"),
      
      # head of data 
      tableOutput("summary_table"), 
      tags$hr(), 
      
      # add breaks 
      br(), 
      
      # plot of xy data from selected columns
      h3("This provides a look at the spatial distribution of the data you've uploaded to the tool. If there are any anomolies (e.g., points that 
            fall on land), you may need to correct the data prior to using the tool."),
      
      leafletOutput("eda_map"), 
      
      tags$hr(), 
      
      br(), 
      
      # plot the output homerange 
      h3("Output from KDE with specified percent volume contour:"),
      leafletOutput("homerange_map")
      
    
    )
    
  )
)



# Define the server logic -------------------------------------------------

server <- function(input, output, session) {
  
  ## create bounding box 
  
  
  ## data uploaded by user 
  data_upload <- reactive({ # data_upload() is the main set of data the user uploads 
    #load the data 
    inFile <- input$file1
    
    req(inFile)
    
    if(input$fileType == "1") {
      f <-  read.csv(inFile$datapath, #-------------------------- encoding? 
                     header = T, 
                     stringsAsFactors = F)
    } else {
      f <-  read_xlsx(inFile$datapath, # ------------------------ check on sheet selection?
                      col_types = "text",
                      sheet = 1)
    }
    
    
    f <- f[rowSums(is.na(f)) != ncol(f), ]
    
    # get variable names for selection 
    vars <- names(f)
    
    # update long
    updateSelectInput(session, "x_col", "Select x data (longitude)", choices = vars)
    # update lat
    updateSelectInput(session, "y_col", "Select y data (latitude)", choices = vars)
    
    f
    
  })
  
  # output dimensions of uploaded data
  output$data_dims <- renderText({
    dat <- data_upload()
    
    paste0("Rows: ", dim(dat)[1], " Columns: ", dim(dat)[2])
    
  })
  
  # output table to examine data 
  output$summary_table <- function(){
    
    rbind(head(data_upload()), tail(data_upload())) %>%
      kable() %>%
      kable_styling(full_width = F, font_size = 15) %>% 
      scroll_box(height = "425px")
    
  }
  # subset for just the xy data 
  data_subset <- eventReactive()
  
  # data_subset <- eventReactive(input$plot_xy, {
  #   
  #   xy_vars <- c(input$x_col, input$y_col)
  #   subdat <- subset(data_upload(), select = xy_vars)
  #   subdat <- sapply(subdat, as.numeric)
  #   subdat
  #   
  # })
  
  
  
  # output exploratory map of point data 
  output$eda_map <- renderLeaflet({
    
    leaflet() %>% 
      addTiles() %>%
      setView(lng = -64, lat = 31, zoom = 3)
    
  })
  
  
  # updates the map based on input 
  observeEvent(data_subset(), { # observe the data subset
    
    leafletProxy("eda_map") %>% 
      
      clearShapes() %>%
      clearMarkers() %>%
      
      setView(lng = mean(data_subset()[,1]), 
              lat = mean(data_subset()[,2]), 
              zoom = 3) %>%
      
      # add circles for the points of data 
      addCircleMarkers(lng = data_subset()[,1], lat = data_subset()[,2], 
                       radius = 1)
    
    
    
  })
  
  
  # create the KDE surface from the subset of data (which is only the point data)
  kde_data <- eventReactive(input$run_model, {
    
    # input selection for bandwidth ---------------------------------- CHANGE
    bandwidth <- ifelse(input$bandwidth == 1, "href", "LSCV")
    
    # input selection for kernel ------------------------------------- CHANGE
    kern_sel <- ifelse(input$kernel == 1 | bandwidth == "LSCV", "bivnorm", "epa")
    
    # input for grid
    grid_size <- input$grid_size
    
    # input parameter for volume
    p <- input$pvc
    
    # add progress bar 
    withProgress(message = "Hamster spinning wheel", value = 0.1, { 
      # make spatial point data
      sp_dat <- SpatialPoints(cbind(data_subset()[,1], data_subset()[,2]))
      
      incProgress(detail = "Estimating kernel density", amount = 0.25) 
      
      kud <- kernelUD(sp_dat, 
                      h = bandwidth,
                      kern = kern_sel,
                      grid = grid_size) 
      
      incProgress(detail = "Calculating contours", amount = 0.75)
      
      hr <- getverticeshr(kud, percent = p)
      
      incProgress(detail = "Complete", amount = 1)
      
      
      
    })
    
    hr
    
    
  })
  
  
  # build the basemap of the polygon PVC 
  output$homerange_map <- renderLeaflet({
    
    leaflet() %>% 
      addTiles() %>%
      setView(lng = -64, lat = 31, zoom = 3)
    
  })
  
  # update map with pvc polygon 
  observeEvent(kde_data(), {
    
    leafletProxy("homerange_map") %>%
      
      clearShapes() %>% 
      clearMarkers() %>%
      
      setView(lng = mean(data_subset()[,1]), 
              lat = mean(data_subset()[,2]), 
              zoom = 2) %>%
      
      addCircleMarkers(lng = data_subset()[,1], lat = data_subset()[,2], 
                       radius = 0.25, col = 'red') %>%
      
      addPolygons(data = kde_data()) 
    
    
  })
  
  
  # save the file to a drive - METHOD OF DOWNLOADBUTTON 
  output$download_data <- downloadHandler(
    
    # 
    filename = function() { # dynamic name for downloaded file 
      paste0("PVC_", input$pvc, "_", input$species, "_", Sys.Date(), ".zip")
    },
    
    #filename = "pvc_export.tar",
    #filename = "pvc_export.zip",
    
    content = function(file) {
      
      # if file exists, remove
      if (length(Sys.glob("PVC*"))>0){
        file.remove(Sys.glob("PVC*"))
        
      }
      
      # write the shapefiles to root directory 
      writeOGR(kde_data(), 
               dsn = paste0("PVC_", input$pvc, "_", input$species, '.shp'), # dsn needs to match Sys.glob regex below
               layer = "pvc",                                       # and in the file.remove()
               driver = "ESRI Shapefile"
      )
      
      # zip files 
      zip("PVC_export.zip", file = Sys.glob(paste0("PVC_", input$pvc, "_", input$species, ".*"))) # here
      
      # copy files to function
      file.copy("PVC_export.zip", file)
      
      # removes files
      if (length(Sys.glob(paste0("PVC_", input$pvc, "_", input$species, ".*")))>0){ # and here
        file.remove(Sys.glob(paste0("PVC_", input$pvc, "_", input$species, ".*"))) # and here
      }
      
    }, 
    contentType = "application/zip"
    
  )
  
}  

# Run the application 
shinyApp(ui = ui, server = server)
