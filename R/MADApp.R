library(shiny) 
library(shinyjs) 
library(shinythemes)
library(EBImage)
library(fs)
library(DT)
library(tidyr)
library(plotly)

# UI ---------------------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("yeti"),
  shinyjs::useShinyjs(),
  titlePanel("Microarray Data Analysis App"),
  tags$style(type='text/css', "#stop { float:right; }"),
  actionButton("stop", "EXIT"),
  tabsetPanel(id = "tabs",

## Image Editor (1th Tab) ------------------------------------------------------
              tabPanel("Image Editor", value = "tab1",
                       sidebarLayout(
                         sidebarPanel(
                           dateInput("testdate", label = "Date of test:", value = NULL),
                           radioButtons("upload",
                                        label = ("Load Image or Sample"),
                                        choices = list("Load Image" = 1,
                                                       "Sample" = 2),
                                        selected = 1),
                           conditionalPanel(
                             condition = "input.upload == 1",
                             fileInput(inputId = 'file1',
                                       label = 'Load Image',
                                       placeholder = 'JPEG, PNG, and TIFF are supported',
                                       accept = c(
                                         "image/jpeg",
                                         "image/x-png",
                                         "image/tiff",
                                         ".jpg",
                                         ".png",
                                         ".tiff")
                             )
                           ),

                           uiOutput("rotatePanel"),

                           sliderInput("rows", "Number of rows:", min = 1, max = 30,  value = 3),
                           sliderInput("cols", "Number of columns:", min = 1, max = 30,  value = 3),

                           radioButtons("selectMode",
                                        label = "Selection mode",
                                        choices = list("Rectangular" = 1,
                                                       "Parallelogram" = 2),
                                        selected = 1)
                         ), # END OF SIDEBAR PANEL

                         mainPanel(
                           h3('Cropping and Segmentation', align = "center"),
                           plotOutput("plot1",
                                      click = "plot_click",
                                      dblclick = "plot_dblclick",
                                      hover = "plot_hover",
                                      brush = "plot_brush"),
                           br(),
                           h6("Click and drag to select a region of interest. Double click on the selected region to zoom.", align = "center"),
                           br(),
                           column(6,
                                  actionButton("reset", label = "Reset"),
                                  tags$style(type='text/css', "#reset { display: block; width:50%; margin-left: auto; margin-right:auto;}"),
                           ),
                           column(6, shinyjs::disabled(
                             actionButton("applyGrid", label = "Apply Grid")),
                             tags$style(type='text/css', "#applyGrid { display: block; width:50%; margin-left: auto; margin-right:auto;}"),
                           )

                           ) # END OF MAIN PANEL
                       ) # END OF SIDEBAR LAYOUT
              ), # END OF TAB PANEL

## Grid Conf (2nd Tab) -----------------------------------------------------
              tabPanel("Grid Configuration", value = "tab2",
                       sidebarLayout(
                         sidebarPanel(
                             # fileInput(inputId = "gridConfFile",
                             #           label = "Load Grid Configuration File"
                             # ),
                             fileInput(inputId = "gridLabels",
                                       label = "Load Grid Label File"),
                             textOutput("gridLabelsMsg"),
                             # downloadButton("downloadGrid", "Save Grid Conf"), # It can be adapted to save label file as well.
                             hr(),
                             textInput("analyteName", label="Analyte name", value=""),
                             actionButton("addAnalyte", "Add Analyte"),
                             selectInput("analyteSelected",
                                          label = ("Select Analyte"),
                                          choices = c("Please add analyte"),
                                          selected = 1),
                             hr(),
                             actionButton("segmentation", label = "Apply Segmentation")
                         ), # END OF SIDEBAR PANEL
                         mainPanel(
                           h3('Grid Configuration', align = "center"),
                           plotOutput("plot2",
                                      click = "plot2_click",
                                      dblclick = "plot2_dblclick",
                                      hover = "plot2_hover",
                                      brush = brushOpts(id="plot2_brush",
                                                        delay=50000)
                                      )
                         )# END OF MAIN PANEL
                       ) # END OF SIDEBAR LAYOUT
              ), # END OF TAB PANEL

## Background Cor (3rd Tab) ------------------------------------------------
              tabPanel("Background Correction", value = "tab3",
                       sidebarLayout(
                         sidebarPanel(
                           radioButtons("colorImage",
                                        label = ("Color image?"),
                                        choices = list("No" = 1,
                                                       "Yes" = 2),
                                        selected = 1),

                           conditionalPanel(
                             condition = "input.colorImage == 2",
                             radioButtons("channel",
                                          label = ("Conversion mode"),
                                          choices = list("luminance",
                                                         "gray",
                                                         "red",
                                                         "green",
                                                         "blue"),
                                          selected = "luminance")
                           ),

                           # radioButtons("invert",
                           #              label = ("Spots are darker than background?"),
                           #              choices = list("No" = FALSE,
                           #                             "Yes" = TRUE),
                           #              selected = FALSE),

                           radioButtons("thresh",
                                        label = ("Threshold Method"),
                                        choices = list("Otsu" = 1,
                                                       "Quantile" = 2
                                        ), 
                                        selected = 1),

                           conditionalPanel(
                             condition = "input.thresh == 2",
                             numericInput(inputId = "quantile1",
                                          label = "Probability [%]:",
                                          value = 99,
                                          min = 0,
                                          max = 100,
                                          step = 0.1,
                                          width = NULL)
                           ),

                           conditionalPanel(
                             condition = "input.thresh == 1",
                             radioButtons("otsuMode",
                                          label = "Choose otsu mode",
                                          choices = list("global" = 1,
                                                         "local" = 2),
                                          selected = 1)
                           ),
                           hr(),
                           actionButton("threshold", label = "Apply Threshold"),
                           hr(),
                           actionButton("data", label = "Add To Data"),
                           hr(),
                           fileInput(inputId = 'multiImages',
                                     label = 'Analyse multiple images with same settings',
                                     multiple = TRUE,
                                     placeholder = 'JPEG, PNG, and TIFF are supported',
                                     accept = c(
                                       "image/jpeg",
                                       "image/x-png",
                                       "image/tiff",
                                       ".jpg",
                                       ".png",
                                       ".tiff")),
                           actionButton("multiImagesView", "Check Images"),
                           actionButton("multiImagesAnalyse", "Analyze"),
                           hr(),
                           actionButton("showIntensData", label = "Show Intensity Data")
                         ), # END OF SIDEBAR PANEL
                         mainPanel(
                           HTML(
                             paste(
                               h3('Background Correction', align = "center"),
                               verbatimTextOutput("thresh"),br(),
                               h4('Signal Intensity Above Background and Signal after Background Correction', align = "center"),
                               splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot3"), plotOutput("plot4")),
                               verbatimTextOutput("meanIntens"),
                               verbatimTextOutput("medianIntens"),
                               '<br/>','<br/>'
                             )
                           ),
                           width = 8
                         ) # END OF MAIN PANEL
                       ) # END OF SIDEBAR LAYOUT
              ), # END OF TAB PANEL
              ## Start of Tab Data

## Intensity Data (4th Tab) ------------------------------------------------
              tabPanel("Intensity Data", value = "tab4",
                       sidebarLayout(
                         sidebarPanel(
                           fileInput(inputId = "intensDataLabels",
                                     label = "Load Label File"),
                           hr(),
                           downloadButton("downloadData", "Download Data"),
                           tags$style(type='text/css', "#downloadData { display: block; width:50%;}"),
                           br(),
                           actionButton("deleteData", label = "Delete Data"),
                           tags$style(type='text/css', "#deleteData { display: block; width:50%;}"),

                         ),
                         mainPanel(
                           DTOutput("intens")
                         )# END OF MAIN PANEL
                       )# END OF SIDEBAR LAYOUT
              ), # END OF TAB PANEL
              ## Start of Tab Data

## Timeseries (5th Tab) ----------------------------------------------------
              tabPanel("Timeseries", value = "tab5",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("date", label="Choose Date:", choices = ""),
                           selectInput("param", label="Choose Parameter:", 
                                       choices=list("mean",
                                                     "median",
                                                    "threshold"="thresh")),
                           actionButton("plotme", "Plot timeseries"),
                           tags$style(type='text/css', "#plotme { display: block; width:50%;}"),
                           br(),br(),
                         ),
                         mainPanel(
                           DTOutput("checkerboard")
                         )# END OF MAIN PANEL
                       )# END OF SIDEBAR LAYOUT
              ),# END OF TAB PANEL

## Timeseries Plot (6th Tab) -----------------------------------------------
              tabPanel("Timeseries plot", value = "plotly",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("plotAnalyte", label="Choose Analyte:", choices = "")
                         ),
                         mainPanel(
                           h3("Plot Output"),
                           plotly::plotlyOutput("plotly")
                         )# END OF MAIN PANEL
                       )# END OF SIDEBAR LAYOUT
              )# END OF TAB PANEL
  )# END OF TAB SET PANEL
) # END OF UI


# Helper functions --------------------------------------------------------

# Calculate linear function by given points
getLinFunc <- function(x1, y1, x2, y2) {
  m <- (y2-y1) / (x2-x1)
  b <- y1 - m * x1

  linFunc <- function(x) {
    y <- NULL
    if (length(x) != 1) {
      for (i in x) {
        y <- c(y, m*i + b)
      }
    } else {
      y <- m * x + b
    }
    y
  }
}

# Fix the image size. It is required sometimes due to rounding errors when binding
# subimages into an whole image.
fix_subimages <- function(img_grid) {
  min_h <- 100000
  min_w <- 100000

  for (row in img_grid) {
    for (i in row) {
      if (min_h > dim(i)[1]) min_h <- dim(i)[1]
      if (min_w > dim(i)[2]) min_w <- dim(i)[2]
    }
  }

  for (row in 1:length(img_grid)) {
    for (i in 1:length(img_grid[[row]])) {
      if (length(dim(img_grid[[row]][[i]])) == 3) {
        img_grid[[row]][[i]] <- img_grid[[row]][[i]][1:min_h,1:min_w,]
      } else {
        img_grid[[row]][[i]] <- img_grid[[row]][[i]][1:min_h,1:min_w]
      }
    }
  }
  img_grid
}

# SERVER -----------------------------------------------------------------------
server <- function(input, output, session) {

  # Initialization ----------------------------------------------------------
  options(shiny.maxRequestSize=50*1024^2) #file can be up to 50 mb; default is 5 mb
  shinyImageFile <- reactiveValues(img_origin = NULL, Threshold = NULL, 
                                   zoom = NULL, fh=FALSE, fv=FALSE, rot=0, fineRot=0)
  roi <- reactiveValues(image = NULL, cell_w = NULL, cell_h = NULL, cols = NULL, rows = NULL,
                        xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL, grid = NULL)
  analytes <- NULL

  updateSelectInput(session, "analyteSelected", choices=names(analytes))

  intensData <- reactiveValues(dates=NULL, dataFrame=NULL)

  multiImages <- reactiveValues(idx = NULL)

  LETTERS <- readRDS("letters.rds")

  # Load panel ------------------------------------------------------------
  observe({
    #default: upload image
    if(input$upload == 1){
      output$plot1 <- renderPlot({
        if(is.null(input$file1)) {
          output$rotatePanel <- renderUI({})
        }
        validate(need(!is.null(input$file1), "Must load a valid jpg, png, or tiff"))
      })
    }
    if(input$upload == 2){
      isolate({
        # using sample image
        img <- readImage("sample_array.png")
        shinyImageFile$img_origin <- img
        shinyImageFile$filename <- "SAMPLE"
        output$plot1 <- renderPlot({ EBImage::display(img, method = "raster") })
        drawRotatePanel()
      })
    }
  }) # end of observe

  #if they enter a new file, their file will become the new imageFile
  observeEvent(input$file1, {
    isolate({
      shinyImageFile$filename <- input$file1$name
      img <- readImage(input$file1$datapath)
      shinyImageFile$img_origin <- img
      output$plot1 <- renderPlot({EBImage::display(img, method = "raster")})
      drawRotatePanel()
    })
  })


  # Rotation panel ----------------------------------------------------------
  drawRotatePanel <- function() {
    output$rotatePanel <- renderUI({
      tagList(
        sliderInput("rotate", "Rotate image",
                    min=-45, max=45, value=0),
        actionButton("rotateCCW", "-90"),
        actionButton("rotateCW", "+90"),
        actionButton("fliphor", "FH"),
        actionButton("flipver", "FV"),
      )
    })
  }

  observeEvent(input$rotate, {
    isolate({
      shinyImageFile$fineRot <- input$rotate
    })
  })

  observeEvent(input$rotateCCW, {
    isolate({
        shinyImageFile$rot <- shinyImageFile$rot - 90
    })
  })

  observeEvent(input$rotateCW, {
    isolate({
        shinyImageFile$rot <- shinyImageFile$rot + 90
    })
  })

  observeEvent(input$fliphor, {
    isolate({
        if (shinyImageFile$fh == TRUE) {
          shinyImageFile$fh <- FALSE
        } else {
          shinyImageFile$fh <- TRUE
        }
    })
  })

  observeEvent(input$flipver, {
    isolate({
        if (shinyImageFile$fv == TRUE) {
          shinyImageFile$fv <- FALSE
        } else {
          shinyImageFile$fv <- TRUE
        }
    })
  })

  render_plot <- function() {
    transformed_image <- transform_image(shinyImageFile$img_origin)
    plotAndGrid_mainPlot(transformed_image)
  }
  
  transform_image <- function(image) {
    transformed_image <- image
    if (!is.null(shinyImageFile$zoom)) {
      transformed_image <- croppedImage(image,
                                        shinyImageFile$zoom$xmin,
                                        shinyImageFile$zoom$ymin,
                                        shinyImageFile$zoom$xmax,
                                        shinyImageFile$zoom$ymax)
    }
    if (shinyImageFile$fh) {
      transformed_image <- EBImage::flip(transformed_image)
    }
    if (shinyImageFile$fv) {
      transformed_image <- EBImage::flop(transformed_image)
    }
    if (!is.null(transformed_image)) {
      transformed_image <- EBImage::rotate(transformed_image, (shinyImageFile$rot + shinyImageFile$fineRot), 
                                         output.dim = dim(transformed_image)[1:2], bg.col="white")
    }
  }

  observeEvent({shinyImageFile$fh
                shinyImageFile$fv
                shinyImageFile$rot
                shinyImageFile$fineRot}, {
      render_plot()
  })
  
  # Main plot - cropping -------------------------------------------------------

  # Cropping function
  croppedImage <- function(image, xmin, ymin, xmax, ymax){
    if(length(dim(image)) == 2)
      image <- image[xmin:xmax, ymin:ymax, drop = FALSE]
    else if(length(dim(image)) == 3)
      image <- image[xmin:xmax, ymin:ymax, ,drop = FALSE]
    image
  }

  # Zooming
  observeEvent(input$plot_dblclick,{
    isolate({
      if (is.null(shinyImageFile$zoom)) {
        p <- input$plot_brush
        validate(need(p$xmax <= dim(shinyImageFile$img_origin)[1], 
                      "Highlighted portion is out of bounds on the x-axis"))
        validate(need(p$ymax <= dim(shinyImageFile$img_origin)[2], 
                      "Highlighted portion is out of bounds on the y-axis"))
        validate(need(p$xmin >= 0,
                      "Highlighted portion is out of bounds on the x-axis"))
        validate(need(p$ymin >= 0,
                      "Highlighted portion is out of bounds on the y-axis"))
        shinyImageFile$zoom$xmin <- p$xmin
        shinyImageFile$zoom$ymin <- p$ymin
        shinyImageFile$zoom$xmax <- p$xmax
        shinyImageFile$zoom$ymax <- p$ymax
        render_plot()

        session$resetBrush("plot_brush")
        shinyjs::enable("reset")
        shinyjs::disable("applyGrid")
      }
    })
  })
  
  # Main plot - griding ---------------------------------------------------
  observeEvent(input$plot_brush,{
    render_plot()
  })

  plotAndGrid_mainPlot <- function(transformed_image) {
    output$plot1 <- renderPlot({
      if (!is.null(transformed_image)) EBImage::display(transformed_image, method = "raster")
      p <- input$plot_brush
      if (!is.null(p)) {
        if (input$selectMode == 2) { # Parallelogram mode
          colcuts <- seq(p$xmin, p$xmax, length.out = input$cols + 1)
          rowcuts <- seq(p$ymin, p$ymax, length.out = input$rows + 2)

          linFunc1 <- getLinFunc(p$xmin, rowcuts[2], p$xmax, rowcuts[1])
          linFunc2 <- getLinFunc(p$xmin, rowcuts[length(rowcuts)], p$xmax, rowcuts[length(rowcuts)-1])

          for (x in colcuts) {
            ymin <- linFunc1(x)
            ymax <- linFunc2(x)
            lines(x = rep(x, 2), y = c(ymin, ymax), col="red")
          }

          for (i in 1:length(rowcuts)) {
            lines(x = c(p$xmin, p$xmax), y = c(rowcuts[i+1], rowcuts[i]), col="red")
          }
        } else {
          colcuts <- seq(p$xmin, p$xmax, length.out = input$cols + 1)
          rowcuts <- seq(p$ymin, p$ymax, length.out = input$rows + 1)

          for (x in colcuts) {
            lines(x = rep(x, 2), y = c(p$ymin, p$ymax), col="red")
          }
          for (y in rowcuts) {
            lines(x = c(p$xmin, p$xmax), y = rep(y, 2), col="red")
          }
        }
        shinyjs::enable("reset")
        shinyjs::enable("applyGrid")
      }
    })
  }



  # Main plot - buttons --------------------------------------------------------

  observeEvent(input$reset,{
    isolate({
      shinyImageFile$shiny_img_final <- shinyImageFile$img_origin
      shinyImageFile$rot <- 0
      shinyImageFile$fh <- FALSE
      shinyImageFile$fv <- FALSE
      shinyImageFile$zoom <- NULL
      render_plot()
      session$resetBrush("plot_brush")
      shinyjs::disable("applyGrid")
    })
  })
  
  observeEvent(input$applyGrid, {
    isolate({
      # When apply grid button is pressed all required information about the grid
      # is calculated and saved.
      p <- input$plot_brush
      roi$image <- transform_image(shinyImageFile$img_origin)
      roi$cols <- input$cols
      roi$rows <- input$rows
      roi$cell_w <- (p$xmax - p$xmin) / input$cols
      roi$cell_h <- (p$ymax - p$ymin) / input$rows
      roi$xmin <- p$xmin
      roi$xmax <- p$xmax
      roi$ymin <- p$ymin
      roi$ymax <- p$ymax
      roi$mode <- input$selectMode
      roi$grid <- matrix(0, nrow=input$rows, ncol=input$cols)

      updateTabsetPanel(session, "tabs", selected = "tab2")
    })
  })
  
# GRID CONFIGURATION TAB
  
# Grid configuration -----------------------------------------------------------
  # observeEvent(input$gridConfFile, {
  #   load(input$gridConfFile$datapath)
  #   roi <<- roi
  #   analytes <<- analytes
  # })

  observeEvent(input$gridLabels, {
    labels <- read.csv(input$gridLabels$datapath, header=FALSE)
    if (nrow(labels) != nrow(roi$grid) * ncol(roi$grid)) {
      showNotification("Labels can't be fitted in the grid...", type="error")
    } else {
      new_matrix <- matrix(0, nrow=nrow(roi$grid), ncol=ncol(roi$grid))
      names <- unique(labels[,2])
      for (row in 1:nrow(labels)){
        cell <- list()
        splitted_str <- strsplit(labels[row,1],split="(?<=[a-zA-Z])\\s*(?=[0-9])",perl=TRUE)
        cell[1] <- splitted_str[[1]][1]
        cell[2] <- splitted_str[[1]][2]
        cell <- c(match(cell[1], LETTERS), as.numeric(cell[2]))
        idx <- match(labels[row,2],names)

        new_matrix[cell[2],cell[1]] <- idx
      }
      analytes <<- names
      roi$grid <- data.frame(new_matrix)
      updateSelectInput(session, "analyteSelected", choices=analytes, selected=analytes[length(analytes)])
      updateSelectInput(session, "plotAnalyte", choices=analytes, selected=analytes[length(analytes)])
    }
  })

  observeEvent(input$addAnalyte, {
    if (input$analyteName == "") return(0)
    if (is.na(match(input$analyteName, analytes))) {
      analytes <<- c(analytes, input$analyteName)
    } else {
      showNotification("Analyte already exsist.", type="error")
    }
    updateSelectInput(session, "analyteSelected", choices=analytes, selected=analytes[length(analytes)])
  })

  # Name and color -------------------------------------------------------------
  # observeEvent(input$color, {
  #   if (input$gridFill != "0" && input$gridFill != "-1") {
  #     analytes$color[[as.integer(input$gridFill)]] <- input$color
  #   }
  # })

  # observeEvent(input$analyteName, {
  #   if (input$gridFill != "0" && input$gridFill != "-1") {
  #     analytes$name[[as.integer(input$gridFill)]] <- input$analyteName
  #   }
  # })
  
  # observeEvent(input$gridFill, {
  #   if (input$gridFill != "0" && input$gridFill != "-1") {
  #     updateTextInput(session, "analyteName", value=analytes$name[as.integer(input$gridFill)])
  #     updateColourInput(session, "color", value=analytes$color[as.integer(input$gridFill)])
  #   } else {
  #     updateTextInput(session, "analyteName", value="")
  #     updateColourInput(session, "color", value="red")
  #   }
  #   session$resetBrush("plot2_brush")
  # })
  

# Interactive grid functions ----------------------------------------------
  observeEvent(input$plot2_click, {
    isolate({
      p <- input$plot2_click

      if (!(p$x > roi$xmax) & !(p$x < roi$xmin) & !(p$y > roi$ymax) & !(p$y < roi$ymin)) {

        if (roi$mode == 2) { # Parallelogram
          colcuts <- seq(roi$xmin, roi$xmax, length.out = input$cols + 1)
          rowcuts <- seq(roi$ymin, roi$ymax, length.out = input$rows + 2)

          ymin <- getLinFunc(colcuts[1], rowcuts[2], colcuts[length(colcuts)], rowcuts[1])(p$x)
          cell_h <- rowcuts[2] - rowcuts[1]

          cell_x <- ceiling((p$x - roi$xmin) / roi$cell_w)
          cell_y <- ceiling((p$y - ymin) / cell_h)
          if (cell_y > roi$rows) cell_y <- roi$rows
        } else { # Rectangle
          cell_x <- ceiling((p$x - roi$xmin) / roi$cell_w)
          cell_y <- ceiling((p$y - roi$ymin) / roi$cell_h)
        }
        if (!is.na(match(input$analyteSelected, analytes))) {
          roi$grid[cell_y, cell_x] <- match(input$analyteSelected, analytes)
        }
      }
    })
  })
  
  observeEvent(input$plot2_brush, {
    isolate({
      p <- input$plot2_brush

      if (p$xmax > roi$xmax) p$xmax <- roi$xmax - 1
      if (p$xmin < roi$xmin) p$xmin <- roi$xmin + 1
      if (p$ymax > roi$ymax) p$ymax <- roi$ymax - 1
      if (p$ymin < roi$ymin) p$ymin <- roi$ymin + 1
      
      
      if (!(p$xmax < roi$xmin) & !(p$xmin > roi$xmax) & !(p$ymin > roi$ymax) & !(p$ymax < roi$ymin)) {
        
        if (roi$mode == 2) {
          colcuts <- seq(roi$xmin, roi$xmax, length.out = input$cols + 1)
          rowcuts <- seq(roi$ymin, roi$ymax, length.out = input$rows + 2)
          
          ymin1 <- getLinFunc(colcuts[1], rowcuts[2], colcuts[length(colcuts)], rowcuts[1])(p$xmin)
          ymin2 <- getLinFunc(colcuts[1], rowcuts[2], colcuts[length(colcuts)], rowcuts[1])(p$xmax)
          cell_h <- rowcuts[2] - rowcuts[1]
          
          cell_x1 <- ceiling((p$xmin - roi$xmin) / roi$cell_w)
          cell_y1 <- ceiling((p$ymin - ymin1) / cell_h)
          cell_x2 <- ceiling((p$xmax - roi$xmin) / roi$cell_w)
          cell_y2 <- ceiling((p$ymax - ymin2) / cell_h)
          
          if (cell_y1 > input$rows) cell_y1 <- input$rows
          if (cell_y2 > input$rows) cell_y2 <- input$rows
          
        } else {
          cell_x1 <- ceiling((p$xmin - roi$xmin) / roi$cell_w)
          cell_y1 <- ceiling((p$ymin - roi$ymin) / roi$cell_h)
          cell_x2 <- ceiling((p$xmax - roi$xmin) / roi$cell_w)
          cell_y2 <- ceiling((p$ymax - roi$ymin) / roi$cell_h)
        }
        
        for(cell_y in cell_y1:cell_y2) {
          for(cell_x in cell_x1:cell_x2) {
            if (!is.na(match(input$analyteSelected, analytes))) {
              roi$grid[cell_y, cell_x] <- match(input$analyteSelected, analytes)
            }
          }
        }
      }
      session$resetBrush("plot2_brush")
    })
  })

  # Grid rendering
  plotAndGrid_gridConfig <- function(plot_name, image) {
    output[[plot_name]] <- renderPlot({
      EBImage::display(image, method = "raster", margin=c(0,15,0,0))
      
      if (!is.null(roi$grid)) {
        if (roi$mode == 2) {  # Parallelogram
          colcuts <- seq(roi$xmin, roi$xmax, length.out = input$cols + 1)
          rowcuts <- seq(roi$ymin, roi$ymax, length.out = input$rows + 2)
          
          linFunc1 <- getLinFunc(roi$xmin, rowcuts[2], roi$xmax, rowcuts[1])
          linFunc2 <- getLinFunc(roi$xmin, rowcuts[length(rowcuts)], roi$xmax, rowcuts[length(rowcuts)-1])
          
          for (x in colcuts) {
            ymin <- linFunc1(x)
            ymax <- linFunc2(x)
            lines(x = rep(x, 2), y = c(ymin, ymax), col="red")
          }
          
          for (i in 1:length(rowcuts)) {
            lines(x = c(roi$xmin, roi$xmax), y = c(rowcuts[i+1], rowcuts[i]), col="red")
          }
          
          for(cell_y in 1:nrow(roi$grid)) {
            for(cell_x in 1:ncol(roi$grid)) {
              if (roi$grid[cell_y, cell_x] == -1) {
                y_min <- getLinFunc(colcuts[1],rowcuts[cell_y+1],
                                    colcuts[length(colcuts)], rowcuts[cell_y])(colcuts[cell_x])
                y_max <- getLinFunc(colcuts[1],rowcuts[cell_y+1],
                                    colcuts[length(colcuts)], rowcuts[cell_y])(colcuts[cell_x+1])
                cell_h <- rowcuts[2] - rowcuts[1]
                lines(x=c(colcuts[cell_x], colcuts[cell_x] + roi$cell_w),
                      y=c(y_min, y_max+cell_h), col="red")
                lines(x=c(colcuts[cell_x], colcuts[cell_x] + roi$cell_w),
                      y=c(y_min+cell_h, y_max), col="red")
              } else if (roi$grid[cell_y, cell_x] != 0) {
                y_pos <- getLinFunc(colcuts[1],rowcuts[cell_y+1],
                                    colcuts[length(colcuts)], rowcuts[cell_y])(colcuts[cell_x])
                
                # Annotation
                anno <- roi$grid[cell_y, cell_x]
                anno_adj <- c(-0.5, 1.5)

                if (analytes[anno] == input$analyteSelected) color <- "blue" else color <- "red"
                
                text(x=colcuts[cell_x], y=y_pos, adj=anno_adj,
                     label=anno, col=color)
              }
            }
          }
          # Draw coordinates
          for(cell_x in 1:ncol(roi$grid)) {
            y_pos <- getLinFunc(colcuts[1],rowcuts[2],
                                colcuts[length(colcuts)], rowcuts[1])(colcuts[cell_x])
            text(x=colcuts[cell_x], y=y_pos, adj=c(-0.5,-1), label=LETTERS[cell_x], col="red")
          }
          for(cell_y in 1:nrow(roi$grid)) {
            text(x=colcuts[1], y=rowcuts[cell_y+1], adj=c(1.5,1.5), label=cell_y, col="red")
          }
        } else { # Rectangular
          colcuts <- seq(roi$xmin, roi$xmax, length.out = input$cols + 1)
          rowcuts <- seq(roi$ymin, roi$ymax, length.out = input$rows + 1)
          
          for (x in colcuts) {
            lines(x = rep(x, 2), y = c(roi$ymin, roi$ymax), col="red")
          }
          for (y in rowcuts) {
            lines(x = c(roi$xmin, roi$xmax), y = rep(y, 2), col="red")
          }
          
          for(cell_y in 1:nrow(roi$grid)) {
            for(cell_x in 1:ncol(roi$grid)) {
              if(roi$grid[cell_y, cell_x] == -1) {
                lines(x=c(colcuts[cell_x], colcuts[cell_x] + roi$cell_w),
                      y=c(rowcuts[cell_y], rowcuts[cell_y] + roi$cell_h), col="red")
                lines(x=c(colcuts[cell_x], colcuts[cell_x] + roi$cell_w),
                      y=c(rowcuts[cell_y] + roi$cell_h, rowcuts[cell_y]), col="red")
              } else if (roi$grid[cell_y, cell_x] != 0) {
                # Annotation
                anno <- roi$grid[cell_y, cell_x]
                anno_adj <- c(-0.5, 1.)
                
                if (analytes[anno] == input$analyteSelected) color <- "blue" else color <- "red"

                text(x=colcuts[cell_x], y=rowcuts[cell_y], adj=anno_adj,
                     label="•", col=color, cex=1.5)
              }
            }
          }
          
          # Draw coordinates
          for (cell_x in 1:ncol(roi$grid)) {
            text(x=colcuts[cell_x], y=rowcuts[1], adj=c(-0.5,-1), label=LETTERS[cell_x], col="red")
          }
          for (cell_y in 1:nrow(roi$grid)) {
            text(x=colcuts[1], y=rowcuts[cell_y], adj=c(1.5, 1.5), label=cell_y, col="red")
          }
        }
      }
    })
  }

  observeEvent({roi$image 
                roi$grid}, {
    plotAndGrid_gridConfig("plot2", roi$image)
  })

  # Segmentation ---------------------------------------------------------------
  observeEvent(input$segmentation,{
    if (!is.null(roi$grid)) {
      image <- transform_image(shinyImageFile$img_origin)
      segmentation.list <- segmentation(image)
      shinyImageFile$segmentation_list <- segmentation.list
      updateTabsetPanel(session, "tabs", selected = "tab3")
    } else {
      showNotification("Error: Grid not applied", type="error")
    }
  })

  segmentation <- function(final_img) {
    isolate({
      # Check if the region of interest is out of the bounds
      if (roi$xmax <= dim(final_img)[1] & roi$ymax <= dim(final_img)[2] &
          roi$xmin >= 0 & roi$ymin >= 0) {
        
        if (roi$mode == 2) {
        
          colcuts <- seq(roi$xmin, roi$xmax, length.out = input$cols + 1)
          rowcuts <- seq(roi$ymin, roi$ymax, length.out = input$rows + 2)
          
          segmentation.list <- vector("list", length = input$rows)  
          count <- 0
          for(y in 1:nrow(roi$grid)){
            tmp.row <- vector("list", length = input$cols)
            img <- final_img
            
            # Cutting the row
            if (length(dim(img)) == 2)
              img <- img[roi$xmin:roi$xmax, rowcuts[y]:rowcuts[y+2]]
            if (length(dim(img)) == 3)
              img <- img[roi$xmin:roi$xmax, rowcuts[y]:rowcuts[y+2],]
        
            # Computing the linear function for top and bottom border of the row.
            LinFunc1 <- getLinFunc(1, dim(img)[2]/2, dim(img)[1], 1)
            LinFunc2 <- getLinFunc(1, dim(img)[2], dim(img)[1], dim(img)[2]/2)
            
            # Removing out of bound pixels
            x_px <- 1:dim(img)[1]
            y_px <- LinFunc1(x_px)
            
            for (i in x_px) {
              if (length(dim(img)) == 2) {
                img[i, 1:y_px[i]] <- 1
                img[i, dim(img)[2]:(dim(img)[2]-rev(y_px)[i])] <- 1
              } else if (length(dim(img)) == 3) {
                img[i, 1:y_px[i],] <- 1
                img[i, dim(img)[2]:(dim(img)[2]-rev(y_px)[i]),] <- 1
              }
            }
            
            # Computing the cutpoints in the row
            row_cutpoints <- seq(1, dim(img)[1], length.out=roi$cols + 1)
            w <- row_cutpoints[2] - row_cutpoints[1]
            h <- dim(img)[2] - LinFunc1(row_cutpoints[2])
            if (length(dim(img)) == 3) {
              # Extracting spots from the row
              for(i in 1:(length(row_cutpoints)-1)){
                sub_img <- img[row_cutpoints[i]:(row_cutpoints[i]+w),,]
                # Reducing the NAs by cutting top and bottom empty parts
                y_cut <- LinFunc1(row_cutpoints[i+1])
                sub_img <- sub_img[,y_cut:(y_cut+h),]
                tmp.row[[i]] <- sub_img
              }
            } else if (length(dim(img)) == 2) {
              # Extracting spots from the row
              for(i in 1:(length(row_cutpoints)-1)){
                sub_img <- img[row_cutpoints[i]:(row_cutpoints[i]+w),]
                # Reducing the NAs by cutting top and bottom empty parts
                y_cut <- LinFunc1(row_cutpoints[i+1])
                sub_img <- sub_img[,y_cut:(y_cut+h)]
                tmp.row[[i]] <- sub_img
              }
            }
            # Add the row to segmentation list
            segmentation.list[[y]] <- tmp.row
          }
          segmentation.list
        } else if (roi$mode == 1) {
          colcuts <- seq(roi$xmin, roi$xmax, length.out = input$cols + 1)
          rowcuts <- seq(roi$ymin, roi$ymax, length.out = input$rows + 1)
          
          segmentation.list <- vector("list", length = nrow(roi$grid))
          count <- 0
          for (y in 1:nrow(roi$grid)) {
            tmp.row <- vector("list", length = ncol(roi$grid))
            for (x in 1:ncol(roi$grid)) {
              img <- final_img
              if(length(dim(img)) == 2) {
                img <- img[colcuts[x]:colcuts[x+1], rowcuts[y]:rowcuts[y+1]]
              }
              if (length(dim(img)) == 3) {
                img <- img[colcuts[x]:colcuts[x+1], rowcuts[y]:rowcuts[y+1],]
              }
              tmp.row[[x]] <- img
            }
            segmentation.list[[y]] <- tmp.row
          }
          segmentation.list
        }
      } else {
        showNotification("Error: The grid is out of bounds", duration=5, type="error")
      }
    })
  }
  
  # Threshold ------------------------------------------------------------------
  observeEvent(input$threshold,{
    if (!is.null(shinyImageFile$segmentation_list)) {
      shinyImageFile$threshData <- threshold(shinyImageFile$segmentation_list, TRUE)
    } else {
      showNotification("Error: Segmentation data not available.", duration=5, type="error")
    }
  })
  
  threshold <- function(segmentation_list, display_plots=FALSE) {
    threshData <- NULL
    seg.list <- segmentation_list
    seg.list <- fix_subimages(seg.list)
    op <- par(no.readonly = TRUE)
  
    # Otsu
    if(input$thresh == 1){
      Background.Threshold <- matrix(ncol=roi$cols,nrow=roi$rows)
      img_final <- transform_image(shinyImageFile$img_origin)
      if (colorMode(img_final) > 0) {
        img_final <- 1 - channel(img_final, input$channel) 
      }
      thr <- otsu(img_final)
      concat_img1 <- NULL
      for(y in 1:roi$rows){
        row_segment <- NULL
        for(x in 1:roi$cols){
          img <- seg.list[[y]][[x]]
          if (roi$grid[y,x] != -1) {
            # if(input$invert) {
            #   img <- 1 - img
            # }
            if(colorMode(img) > 0){
              img <- 1-channel(img, input$channel)
            }
            if (input$otsuMode == 2) {
              thr <- otsu(img)
            }
            Background.Threshold[y,x] <- thr
            signal <- imageData(img) > Background.Threshold[y,x]
          } else {
            Background.Threshold[y,x] <- NA
            signal <- matrix(0, nrow=nrow(imageData(img)), ncol=ncol(imageData(img)))
          }
          row_segment <- EBImage::abind(row_segment, signal, along=1)
        }
        concat_img1 <- EBImage::abind(concat_img1, row_segment, along=2)
      }
      if (display_plots) output$plot3 <- renderPlot({display(concat_img1, method="raster")})
      threshData$Threshold <- Background.Threshold
      threshData$Mean_Intensities <- matrix(NA, ncol=input$cols, nrow=input$rows)
      threshData$Median_Intensities <- matrix(NA, ncol=input$cols, nrow=input$rows)
      threshData$Valid_Pixels <- matrix(NA, ncol=input$cols, nrow=input$rows)
      threshData$False_Positives <- matrix(NA, ncol=input$cols, nrow=input$rows)

      concat_img2 <- NULL
      for(y in 1:roi$rows){
        row_segment <- NULL
        for(x in 1:roi$cols){
          img <- seg.list[[y]][[x]]
          if (roi$grid[y,x] != -1) {
            if(colorMode(img) > 0){
              img <- 1-channel(img, input$channel)
            }
            # if(input$invert) {
            #   img <- 1 - img
            # }
            if (input$otsuMode == 2) {
              thr <- otsu(img)
            }
            signal <- imageData(img) > thr
            imageData(img) <- (imageData(img) - thr)*signal
            valid_px <- sum(signal) / (dim(signal)[1] * dim(signal)[2]) > 0.2
            threshData$Mean_Intensities[y,x] <- mean(imageData(img)[valid_px], na.rm = TRUE)
            threshData$Median_Intensities[y,x] <- median(imageData(img)[valid_px], na.rm = TRUE)
            threshData$Valid_Pixels[y,x] <- sum(signal) / (dim(signal)[1] * dim(signal)[2])
          } else {
            threshData$Mean_Intensities[y,x] <- NA
            threshData$Median_Intensities[y,x] <- NA
            threshData$Valid_Pixels[y,x] <- NA
            threshData$False_Positives[y,x] <- NA
            imageData(img) <- matrix(0, nrow=nrow(imageData(img)), ncol=ncol(imageData(img)))        
          }
          row_segment <- EBImage::abind(row_segment, imageData(img), along=1)
        }
        concat_img2 <- EBImage::abind(concat_img2, row_segment, along=2)
      }
      if (display_plots) output$plot4 <- renderPlot({display(concat_img2, method="raster")})
    }
    
    # Quantile
    if(input$thresh == 2){
      Background <- vector(mode= "list", length=roi$cols*roi$rows)
      count <- 0
      for(y in 1:roi$rows){
        for(x in 1:roi$cols){
          count <- count + 1
          img <- seg.list[[y]][[x]]
          if(colorMode(img) > 0){
            img <- 1-channel(img, input$channel)
          }
          Background[[count]] <- as.numeric(imageData(img))
        }
      }
      Background.Threshold <- quantile(unlist(Background),
                                       probs = input$quantile1/100)
      
      concat_img1 <- NULL
      for(y in 1:roi$rows){
        row_segment <- NULL
        for(x in 1:roi$cols){
          img <- seg.list[[y]][[x]]
          if (roi$grid[y,x] != -1) {
            if(colorMode(img) > 0){
              img <- 1-channel(img, input$channel)
            }
            # if(input$invert) {
            #   img <- 1 - img
            # }
            signal <- imageData(img) > Background.Threshold
          } else {
            signal <- matrix(0, nrow=nrow(imageData(img)), ncol=ncol(imageData(img)))
          }
          row_segment <- EBImage::abind(row_segment,signal, along=1)
        }
        concat_img1 <- EBImage::abind(concat_img1, row_segment, along=2)
      }
      if (display_plots) output$plot3 <- renderPlot({display(concat_img1, method="raster")})
      threshData$Threshold <- Background.Threshold
      threshData$Mean_Intensities <- matrix(NA, ncol=input$cols, nrow=input$rows)
      threshData$Median_Intensities <- matrix(NA, ncol=input$cols, nrow=input$rows)
      
      concat_img2 <- NULL
      for(y in 1:roi$rows){
        row_segment <- NULL
        for(x in 1:roi$cols){
          img <- seg.list[[y]][[x]]
          if (roi$grid[y,x] != -1) {
            if(colorMode(img) > 0){
              img <- 1-channel(img, input$channel)
            }
            # if(input$invert) {
            #   img <- 1 - img
            # }
            signal <- imageData(img) > Background.Threshold
            imageData(img) <- (imageData(img) - Background.Threshold)*signal
            threshData$Mean_Intensities[y,x] <- mean(imageData(img)[signal])
            threshData$Median_Intensities[y,x] <- median(imageData(img)[signal])
          } else {
            threshData$Mean_Intensities[y,x] <- NA
            threshData$Median_Intensities[y,x] <- NA
            imageData(img) <- matrix(0, nrow=nrow(imageData(img)), ncol=ncol(imageData(img)))
          }
          row_segment <- EBImage::abind(row_segment,imageData(img), along=1)
        }
        concat_img2 <- EBImage::abind(concat_img2, row_segment, along=2)
      }
      if (display_plots) output$plot4 <- renderPlot({display(concat_img2, method="raster")})
    }
    threshData
  }

# Input data -------------------------------------------------------------------

  inputData <- function(imageName, threshData) {
    intens_data$dates[[paste0(input$testdate)]][["mean"]] <- threshData$Mean_Intensities
    intens_data$dates[[paste0(input$testdate)]][["median"]] <- threshData$Median_Intensities
    intens_data$dates[[paste0(input$testdate)]][["thresh"]] <- threshData$Threshold
    intens_data$dates[[paste0(input$testdate)]][["valid"]] <- threshData$Valid_Pixels
    intens_data$dates[[paste0(input$testdate)]][["falseP"]] <- ifelse(threshData$Valid_Pixels <= 0.2 & threshData$Valid_Pixels > 0, 1, 0)
    intens_data$dates[[paste0(input$testdate)]][["grid"]] <- roi$grid
    
    if(input$thresh == 1){
      BG.method <- matrix(c("Otsu", NA), nrow = 1,
                          ncol = 2, byrow = TRUE)
      colnames(BG.method) <- c("Background", "Probability")
    }
    if(input$thresh == 2){
      BG.method <- matrix(c("quantile", input$quantile1),
                          nrow = 1, ncol = 2, byrow = TRUE)
      colnames(BG.method) <- c("Background", "Probability")
    }
    
    seg.list <- shiny_image_file$segmentation_list
    img <- seg.list[[1]][[1]]
    if(colorMode(img) > 0){
      MODE <- input$channel
      DF <- data.frame("Date" = input$testdate,
                       "File" = imageName,
                       "Mode" = MODE,
                       BG.method,
                       check.names = FALSE)
    }else{
      DF <- data.frame("Date" = input$testdate,
                       "File" = imageName,
                       "Mode" = NA,
                       BG.method,
                       check.names = FALSE)
    }
    
    # Assign names of analytes
    Ana <- sapply(unlist(roi$grid), function(x) {if (x!="-1" & x!="0") analytes[as.numeric(x)] else NA})
    
    Mean <- as.vector(threshData$Mean_Intensities)
    Median <- as.vector(threshData$Median_Intensities)
    Thresh <- as.vector(threshData$Threshold)
    
    # Generating of position list
    Pos <- NULL
    for (col in 1:ncol(threshData$Mean_Intensities)) {
      Pos <- c(Pos, paste0(LETTERS[col], 1:nrow(threshData$Mean_Intensities)))
    }
    
    DF <- cbind(DF, Pos, Ana, Mean, Median, Thresh)
    intens_data$dataFrame <- rbind(intens_data$dataFrame, DF)
  }
  
  observeEvent(input$intens_dataLabels, {
    labels <- read.csv(input$intens_dataLabels$datapath, header=FALSE)
    
    for (row in 1:nrow(labels)) {
      pos <- labels[row,1]
      
      rows <- which(intens_data$dataFrame$Pos == pos)
      for (i in rows) {
        intens_data$dataFrame[i,]$Ana <- labels[row,2]
      }
    }
  })
  
# Multi Analysis ----------------------------------------------------------
  observeEvent(input$multiImagesAnalyse, {
    if (!is.null(shiny_image_file$threshData)) {
      if (!is.null(input$multiImages)) {
        withProgress(message="Analyzing...", value = 0, {
          for (i in 1:length(input$multiImages$datapath)) {
            img <- readImage(input$multiImages$datapath[i])
            # Image cropping
            if (!is.null(shiny_image_file$zoom)) {
              img <- cropped_image(img,
                                  shiny_image_file$zoom$xmin,
                                  shiny_image_file$zoom$ymin,
                                  shiny_image_file$zoom$xmax,
                                  shiny_image_file$zoom$ymax)
            }
            # Image transformations
            if (shiny_image_file$fh) {
              img <- EBImage::flip(img)
            }
            if (shiny_image_file$fv) {
              img <- EBImage::flop(img)
            }
            img <- EBImage::rotate(img, shiny_image_file$rot + shiny_image_file$fineRot, bg.col="white")
            
            seg.list <- segmentation(img)
            t <- threshold(seg.list)
            inputData(input$multiImages$name[i], t)

            incProgress(1/length(input$multiImages$datapath),
                        detail = paste0("Image ", i, "/", length(input$multiImages$datapath)))
          }
        })
      }
      updateTabsetPanel(session, "tabs", selected = "tab4")
    } else {
      showNotification("Please perform a full analysis for an image to proceed.", type="warning")
    }
  })
  

# Multi Images Viewer -----------------------------------------------------
  observeEvent(input$multiImagesView, {
    if (!is.null(input$multiImages)) {
      showModal(modalDialog(
        title = "Image Viewer",
        plotOutput("multiImage"),
        footer = tagList(
          textOutput("imgCount", inline=TRUE),
          actionButton("b", "<"),
          actionButton("n", ">")
        ),
        easyClose = TRUE
      ))
    }
  })
  
  observeEvent(input$multiImages, {
    multiImages$idx <- 1
  })
  
  observeEvent(input$b, {
    if (!is.null(input$multiImages)) {
      multiImages$idx <- multiImages$idx - 1
      if (multiImages$idx == 0) {
        multiImages$idx <- length(input$multiImages$datapath)
      }
    }
  })
  
  observeEvent(input$n, {
    if (!is.null(input$multiImages)) {
      multiImages$idx <- multiImages$idx + 1
      if (multiImages$idx > length(input$multiImages$datapath)) {
        multiImages$idx <- 1
      }
    }
  })
  
  observeEvent(multiImages$idx, {
    if (multiImages$idx != 0) {
      multImg <- readImage(input$multiImages$datapath[multiImages$idx])
      multImg <- transform_image(multImg) # TODO fix transformation for multimple image analysis
    
      output$imgCount <- renderText(paste0("Image ", multiImages$idx, "/", length(input$multiImages$datapath)))
      plotAndGrid_gridConfig("multiImage", multImg)
    }
  })
  
  observeEvent(input$data, {
    if (!is.null(shiny_image_file$threshData)) {
      inputData(shiny_image_file$filename, shiny_image_file$threshData)
      showNotification("Intensity data added", type="message")
    } else {
      showNotification("Error: Threshold data not available", duration=5, type="error")
    }
  })
  
  observeEvent(intens_data$dataFrame, {
    output$intens <- renderDT({
      datatable(intens_data$dataFrame, rownames=FALSE)
    })
  })
  
  observeEvent(input$showintens_data,{
      updateTabsetPanel(session, "tabs", selected = "tab4")
  })
  
  observeEvent(input$intensFile,{
    isolate({
      DF <- read.csv(input$intensFile$datapath, header = TRUE, 
                     check.names = FALSE)
      intens_data$dataFrame <- DF
    })
  })
  
  observeEvent(input$deleteData,{
      intens_data$dataFrame <- NULL
  })
  
  ##HELPFUL TEXTS##
  output$thresh <- renderText({
    if(!is.null(shiny_image_file$threshData$Threshold))
      paste0("Threshold(s): ", paste0(signif(shiny_image_file$threshData$Threshold, 4), collapse = ", "))
  })
  output$meanIntens <- renderText({
    if(!is.null(shiny_image_file$threshData$Mean_Intensities))
      paste0("Mean intensities: ", paste0(signif(shiny_image_file$threshData$Mean_Intensities), collapse = ", "))
  })
  
  output$medianIntens <- renderText({
    if(!is.null(shiny_image_file$threshData$Median_Intensities))
      paste0("Median intensities: ", paste0(signif(shiny_image_file$threshData$Median_Intensities, 4), collapse = ", "))
  })
  
  ##DOWNLOAD##
  
  #allows user to download data
  output$downloadData <- downloadHandler(
    filename = "IntensityData.csv",
    content = function(file) {
      write.csv(intens_data$dataFrame, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$stop,{
      stopApp()
  })
  # Timeseries module ----------------------------------------------------------
  observeEvent(intens_data$dates, {
    updateSelectInput(session, "date", choices=sort(names(intens_data$dates)))
  })
  
  # Update checkerboard on change
  observe({
    if (!is.null(intens_data$dates)) {
      output$checkerboard <- renderDT({
        table1 <- intens_data$dates[[input$date]][[input$param]]
        colnames(table1) <- LETTERS[1:ncol(table1)]
        rownames(table1) <- 1:nrow(table1)
        table1 <- signif(table1, 3)
        datatable(data=table1, options=list(pageLength=dim(table1)[1]),
                  selection = list(target = 'cell'))
      })
    }
  })
    
  generatePlotLines <- function(table_list, coor, param) {
    p_lines <- list()
    for(n in sort(names(table_list))){
      for(i in 1:nrow(coor)) {
        row <- coor[i,][1]
        col <- coor[i,][2]
        p_lines[[paste0(LETTERS[col], row)]] <- c(p_lines[[paste0(LETTERS[col], row)]],table_list[[n]][[param]][row,col])
      }
    }
    p_lines
  }
  
  observeEvent(input$plotAnalyte, {
    isolate({
      # updateTabsetPanel(session, "tabs", selected = "plotly")
      
      output$plotly <- plotly::renderPlotly({
        analyte_idx <- match(input$plotAnalyte, analytes)
        coor <- which(roi$grid == analyte_idx, arr.ind = TRUE)
        # coor <- input$checkerboard_cells_selected
        validate(need(!is.na(coor[1]), "Please select a cell on previous tab."))
        validate(need(!any(coor==0), "ERROR: Invalid cell selected."))  
        p_lines <- generatePlotLines(intens_data$dates, coor, input$param) 
        data <- list()
        
        for(n in names(p_lines)) {
          data[[paste0(n)]] <- p_lines[[n]]
        }
        x <- factor(names(intens_data$dates))
        data <- data.frame(data)
        data <- cbind(x,data)
        data <- data[order(data$x),] # Sorts the date entries.

        
        fig <- plot_ly(type="scatter")
        
        for(i in names(p_lines)) {
          # fig <- fig %>% plotly::add_trace(data=data, x = ~x, y = as.formula(paste0("~`", i, "`")), name = i, mode="scatter")
          fig <- fig %>% plotly::add_trace(data=data, x = ~x, y = data[[i]], name = i, mode="scatter")
        }
        
        a <- list(
          title = "Timeline",
          showticklabels = TRUE,
          tickangle = 45,
          exponentformat = "E",
          tickformat="%d/%m/%Y"
        )
        
        y <- list(
          title = "Intensities"
        )
        
        fig <- fig %>% layout(xaxis = a, yaxis = y)
        fig
      })
    })
  })
    
} #end of server

shinyApp(ui, server)
