# Helper functions --------------------------------------------------------

# Calculate linear function by given points
getLinFunc <- function(x1, y1, x2, y2) {
  m <- (y2 - y1) / (x2 - x1)
  b <- y1 - m * x1

  linFunc <- function(x) {
    y <- NULL
    if (length(x) != 1) {
      for (i in x) {
        y <- c(y, m * i + b)
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

  for (row in seq_along(img_grid)) {
    for (i in seq_along(img_grid[[row]])) {
      if (length(dim(img_grid[[row]][[i]])) == 3) {
        img_grid[[row]][[i]] <- img_grid[[row]][[i]][1:min_h, 1:min_w, ]
      } else {
        img_grid[[row]][[i]] <- img_grid[[row]][[i]][1:min_h, 1:min_w]
      }
    }
  }
  img_grid
}

gen_mask <- function(img_matrix) {
  euclidean_dis <- function(p1, p2) {
    sqrt((p1[1]-p2[1])^2 + (p1[2]-p2[2])^2)
  }
  tmp_mat <- matrix(0, nrow=nrow(img_matrix), ncol=ncol(img_matrix))
  c <- nrow(tmp_mat)/2
  for (i in 1:nrow(tmp_mat)) {
    for (j in 1:ncol(tmp_mat)) {
      tmp_mat[j,i] <- euclidean_dis(c(j,i), c(c, c))
    }
  }
  tmp_mat
}

# SERVER -----------------------------------------------------------------------
server <- function(input, output, session) { # nolint

  # Initialization ----------------------------------------------------------
  options(shiny.maxRequestSize = 50 * 1024^2) #file can be up to 50 mb; default is 5 mb
  shiny_image_file <- reactiveValues(img_origin = NULL, img_final = NULL, Threshold = NULL, 
                                     threshData = NULL, analytes = NULL, grid = NULL)
  roi <- reactiveValues(image = NULL, cell_w = NULL, cell_h = NULL, cols = NULL, rows = NULL,
                        xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL, grid = NULL)

  fine_rot <- reactiveVal(0)

  intens_data <- reactiveValues(dates = NULL, dataFrame = NULL)

  multiImages <- reactiveValues(idx = NULL)

  LETTERS <- readRDS("letters.rds")

  # Load panel ------------------------------------------------------------
  observe({
    #default: upload image
    if (input$upload == 1){
      if (is.null(input$file1)) {
        output$rotatePanel <- renderUI({})
        shiny_image_file$img_origin <- NULL
        shiny_image_file$img_final <- NULL
        shiny_image_file$filename <- NULL
      } else {
        shiny_image_file$filename <- input$file1$name
        img <- readImage(input$file1$datapath)
        shiny_image_file$img_origin <- img
        shiny_image_file$img_final <- img
        drawRotatePanel()
      }
      output$plot1 <- renderPlot({
        validate(need(!is.null(input$file1), "Must load a valid jpg, png, or tiff"))
      })
    }
    if (input$upload == 2){
      isolate({
        # using sample image
        img <- readImage("sample_array.png")
        shiny_image_file$img_origin <- img
        shiny_image_file$img_final <- img
        shiny_image_file$filename <- "SAMPLE"
        drawRotatePanel()
      })
    }
  }) # end of observe

  # Rotation panel ----------------------------------------------------------
  drawRotatePanel <- function() {
    output$rotatePanel <- renderUI({
      tagList(
        sliderInput("rotate", "Rotate image",
                    min = -30, max = 30, value = 0, step = 0.1),
        actionButton("rotateCCW", "-90"),
        actionButton("rotateCW", "+90"),
        actionButton("fliphor", "FH"),
        actionButton("flipver", "FV"),
      )
    })
  }

  observeEvent(input$rotate, {
    isolate({
      fine_rot(input$rotate)
    })
  })

  observeEvent(input$rotateCCW, {
    isolate({
        shiny_image_file$img_final <- EBImage::rotate(shiny_image_file$img_final, 
                                                      -90,
                                                      output.dim = dim(shiny_image_file$img_final)[1:2], 
                                                      bg.col = "white")
    })
  })

  observeEvent(input$rotateCW, {
    isolate({
        shiny_image_file$img_final <- EBImage::rotate(shiny_image_file$img_final, 
                                                      90,
                                                      output.dim = dim(shiny_image_file$img_final)[1:2],
                                                      bg.col = "white")
    })
  })

  observeEvent(input$fliphor, {
    isolate({
        shiny_image_file$img_final <- flip(shiny_image_file$img_final)
    })
  })

  observeEvent(input$flipver, {
    isolate({
        shiny_image_file$img_final <- flop(shiny_image_file$img_final)
    })
  })

  observeEvent({shiny_image_file$img_final
                fine_rot()}, {
      if (!is.null(shiny_image_file$img_final)) {
        img <- EBImage::rotate(shiny_image_file$img_final,
                              fine_rot(),
                              output.dim = dim(shiny_image_file$img_final)[1:2],
                              bg.col = "white")
        plotAndGrid_mainPlot(img)
      }
  })

  # Main plot - cropping -------------------------------------------------------

  # Cropping function
  cropped_image <- function(image, xmin, ymin, xmax, ymax) {
    if (length(dim(image)) == 2)
      image <- image[xmin:xmax, ymin:ymax, drop = FALSE]
    else if (length(dim(image)) == 3)
      image <- image[xmin:xmax, ymin:ymax, , drop = FALSE]
    image
  }

  # Zooming
  observeEvent(input$plot_dblclick, {
    isolate({
        p <- input$plot_brush
        validate(need(p$xmax <= dim(shiny_image_file$img_origin)[1], 
                      "Highlighted portion is out of bounds on the x-axis"))
        validate(need(p$ymax <= dim(shiny_image_file$img_origin)[2], 
                      "Highlighted portion is out of bounds on the y-axis"))
        validate(need(p$xmin >= 0,
                      "Highlighted portion is out of bounds on the x-axis"))
        validate(need(p$ymin >= 0,
                      "Highlighted portion is out of bounds on the y-axis"))
        img <- EBImage::rotate(shiny_image_file$img_final,
                              fine_rot(),
                              output.dim = dim(shiny_image_file$img_final)[1:2],
                              bg.col = "white")
        shiny_image_file$img_final <- cropped_image(img, p$xmin, p$ymin, p$xmax, p$ymax)

        fine_rot(0)
        updateSliderInput(session, "rotate", value=0)
        session$resetBrush("plot_brush")
        shinyjs::enable("reset")
        shinyjs::disable("applyGrid")
    })
  })

  # Main plot - griding ---------------------------------------------------

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

          for (i in seq_along(rowcuts)) {
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

  observeEvent(input$reset, {
    isolate({
      shiny_image_file$img_final <- shiny_image_file$img_origin
      fine_rot(0)
      updateSliderInput(session, "rotate", value=0)
      session$resetBrush("plot_brush")
      shinyjs::disable("applyGrid")
    })
  })
  
  observeEvent(input$applyGrid, {
    isolate({
      # When apply grid button is pressed all required information about the grid
      # is calculated and saved.
      p <- input$plot_brush
      
      if (p$xmin < 0 | p$xmax > dim(shiny_image_file$img_final)[1] | p$ymin < 0 | p$ymax > dim(shiny_image_file$img_final)[2]) {
        showNotification("Grid out of boundaries.", type="error")
      } else {
        roi$image <- EBImage::rotate(shiny_image_file$img_final, 
                                     fine_rot(),
                                     output.dim = dim(shiny_image_file$img_final)[1:2],
                                     bg.col = "white")
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
      }
    })
  })
  
# GRID CONFIGURATION TAB
  
# Grid configuration -----------------------------------------------------------
  observeEvent(input$gridLabels, {
    isolate({
      labels <- read.csv(input$gridLabels$datapath, header=FALSE)
      new_matrix <- matrix(0, nrow=nrow(roi$grid), ncol=ncol(roi$grid))
      remove_rows <- NULL
      for (row in 1:nrow(labels)){
        cell <- list()
        splitted_str <- strsplit(labels[row,1],split="(?<=[a-zA-Z])\\s*(?=[0-9])",perl=TRUE)
        cell[1] <- splitted_str[[1]][1]
        cell[2] <- splitted_str[[1]][2]
        cell <- c(match(cell[1], LETTERS), as.numeric(cell[2]))
        
        if (cell[2] > nrow(roi$grid) | cell[1] > ncol(roi$grid)) {
          remove_rows <- c(remove_rows, row)
        }
      }
      if (!is.null(remove_rows)) {
        labels <- labels[-remove_rows,]
      }
      analyte_names <- unique(labels[,2])
      for (row in 1:nrow(labels)){
        cell <- list()
        splitted_str <- strsplit(labels[row,1],split="(?<=[a-zA-Z])\\s*(?=[0-9])",perl=TRUE)
        cell[1] <- splitted_str[[1]][1]
        cell[2] <- splitted_str[[1]][2]
        cell <- c(match(cell[1], LETTERS), as.numeric(cell[2]))

        new_matrix[cell[2],cell[1]] <- match(labels[row,2],analyte_names)
      }
      roi$grid <- data.frame(new_matrix)
      shiny_image_file$analytes <- analyte_names
      updateSelectInput(session, "analyteSelected", choices=shiny_image_file$analytes, 
                        selected=shiny_image_file$analytes[length(shiny_image_file$analytes)])
      updateSelectInput(session, "plotAnalyte", choices=shiny_image_file$analytes, 
                        selected=shiny_image_file$analytes[length(shiny_image_file$analytes)])
      updateSelectInput(session, "showAnalyte", choices=shiny_image_file$analytes, 
                        selected=shiny_image_file$analytes[1])
    })
  })

  observeEvent(input$addAnalyte, {
    if (input$analyteName == "") return(0)
    if (is.na(match(input$analyteName, shiny_image_file$analytes))) {
      shiny_image_file$analytes <- c(shiny_image_file$analytes, input$analyteName)
    } else {
      showNotification("Analyte already exist.", type="error")
    }
    updateSelectInput(session, "analyteSelected", choices=shiny_image_file$analytes, 
                      selected=shiny_image_file$analytes[length(shiny_image_file$analytes)])
  })

# Interactive grid functions ----------------------------------------------
  inputClick <- function(plot_input, false_positive=FALSE) {
    p <- plot_input
    
    if (!(p$x > roi$xmax-roi$xmin) & !(p$x < 0) & !(p$y > roi$ymax-roi$ymin) & !(p$y < 0)) {
      
      if (roi$mode == 2) { # Parallelogram
        colcuts <- seq(0, roi$xmax-roi$xmin, length.out = input$cols + 1)
        rowcuts <- seq(0, roi$ymax-roi$ymin, length.out = input$rows + 2)
        
        ymin <- getLinFunc(colcuts[1], rowcuts[2], colcuts[length(colcuts)], rowcuts[1])(p$x)
        cell_h <- rowcuts[2] - rowcuts[1]
        
        cell_x <- ceiling(p$x / roi$cell_w)
        cell_y <- ceiling((p$y - ymin) / cell_h)
        if (cell_y > roi$rows) cell_y <- roi$rows
      } else { # Rectangle
        cell_x <- ceiling(p$x / roi$cell_w)
        cell_y <- ceiling(p$y / roi$cell_h)
      }
      if (false_positive) {
        if (shiny_image_file$threshData$False_Positives[cell_y, cell_x] == 1) {
          shiny_image_file$threshData$False_Positives[cell_y, cell_x] <- 0
        } else {
          shiny_image_file$threshData$False_Positives[cell_y, cell_x] <- 1
        }
      } else if (!is.na(match(input$analyteSelected, shiny_image_file$analytes))) {
        roi$grid[cell_y, cell_x] <- match(input$analyteSelected, shiny_image_file$analytes)
      }
    }
  }
  
  inputSelect <- function(plot_input) {
    p <- plot_input
    
    if (p$xmax > roi$xmax - roi$xmin) p$xmax <- roi$xmax - roi$xmin
    if (p$xmin < 0) p$xmin <- 0
    if (p$ymax > roi$ymax - roi$ymin) p$ymax <- roi$ymax - roi$ymin
    if (p$ymin < 0) p$ymin <- 0
    
    
    if (!(p$xmax < 0) & !(p$xmin > roi$xmax - roi$xmin) & !(p$ymin > roi$ymax - roi$ymin) & !(p$ymax < 0)) {
      if (roi$mode == 2) {
        colcuts <- seq(0, roi$xmax-roi$xmin, length.out = input$cols + 1)
        rowcuts <- seq(0, roi$ymax-roi$ymin, length.out = input$rows + 2)
        
        ymin1 <- getLinFunc(colcuts[1], rowcuts[2], colcuts[length(colcuts)], rowcuts[1])(p$xmin)
        ymin2 <- getLinFunc(colcuts[1], rowcuts[2], colcuts[length(colcuts)], rowcuts[1])(p$xmax)
        cell_h <- rowcuts[2] - rowcuts[1]
        
        cell_x1 <- ceiling(p$xmin / roi$cell_w)
        cell_y1 <- ceiling((p$ymin - ymin1) / cell_h)
        cell_x2 <- ceiling(p$xmax / roi$cell_w)
        cell_y2 <- ceiling((p$ymax - ymin2) / cell_h)
        
        if (cell_y1 > input$rows) cell_y1 <- input$rows
        if (cell_y2 > input$rows) cell_y2 <- input$rows
        
      } else {
        cell_x1 <- ceiling(p$xmin / roi$cell_w)
        cell_y1 <- ceiling(p$ymin / roi$cell_h)
        cell_x2 <- ceiling(p$xmax / roi$cell_w)
        cell_y2 <- ceiling(p$ymax / roi$cell_h)
      }
      
      for(cell_y in cell_y1:cell_y2) {
        for(cell_x in cell_x1:cell_x2) {
          if (!is.na(match(input$analyteSelected, shiny_image_file$analytes))) {
            roi$grid[cell_y, cell_x] <- match(input$analyteSelected, shiny_image_file$analytes)
          }
        }
      }
    }
    session$resetBrush("plot2_brush")
  }
  
  
  observeEvent(input$plot2_click, {
    isolate({
      inputClick(input$plot2_click)
    })
  })

  observeEvent(input$plot2_brush, {
    isolate({
      inputSelect(input$plot2_brush)
    })
  })
  
  observeEvent(input$plot5_click, {
    isolate({
      inputClick(input$plot5_click, false_positive=TRUE)
    })
  })

  # Grid rendering
  plotAndGrid_gridConfig <- function(plot_id, image, mode="gridConfig") {
    output[[plot_id]] <- renderPlot({
      image <- cropped_image(image, roi$xmin, roi$ymin, roi$xmax, roi$ymax)
      EBImage::display(image, method = "raster", margin=c(0,30,0,0))

      if (!is.null(roi$grid)) {
        if (input$selectMode == 2) {  # Parallelogram
          colcuts <- seq(0, roi$xmax-roi$xmin, length.out = input$cols + 1)
          rowcuts <- seq(0, roi$ymax-roi$ymin, length.out = input$rows + 2)

          linFunc1 <- getLinFunc(0, rowcuts[2], roi$xmax-roi$xmin, rowcuts[1])
          linFunc2 <- getLinFunc(0, rowcuts[length(rowcuts)], roi$xmax-roi$xmin, rowcuts[length(rowcuts)-1])

          for (x in colcuts) {
            ymin <- linFunc1(x)
            ymax <- linFunc2(x)
            lines(x = rep(x, 2), y = c(ymin, ymax), col="red")
          }

          for (i in 1:length(rowcuts)) {
            lines(x = c(0, roi$xmax-roi$xmin), y = c(rowcuts[i+1], rowcuts[i]), col="red")
          }

          for (cell_y in 1:nrow(roi$grid)) {
            for (cell_x in 1:ncol(roi$grid)) {
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
              } else if (roi$grid[cell_y, cell_x] != 0 | mode == "false_positives") {
                y_pos <- getLinFunc(colcuts[1],rowcuts[cell_y+1],
                                    colcuts[length(colcuts)], rowcuts[cell_y])(colcuts[cell_x])
                
                # Annotation
                anno <- roi$grid[cell_y, cell_x]
                anno_adj <- c(-0.1, 1)
                
                if (mode == "false_positives") {
                  if (shiny_image_file$threshData$False_Positives[cell_y, cell_x] == 1) {
                    text(x=colcuts[cell_x], y=y_pos, adj=anno_adj,
                         label="•", col="purple", cex=1.7)
                  }
                } else {
                  if (shiny_image_file$analytes[anno] == input$analyteSelected) color <- "green" else color <- "red"
                  text(x=colcuts[cell_x], y=y_pos, adj=anno_adj,
                       label="•", col=color, cex=1.7)
                }
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
          colcuts <- seq(0, roi$xmax-roi$xmin, length.out = input$cols + 1)
          rowcuts <- seq(0, roi$ymax-roi$ymin, length.out = input$rows + 1)
          
          for (x in colcuts) {
            lines(x = rep(x, 2), y = c(0, roi$ymax-roi$ymin), col="red")
          }
          for (y in rowcuts) {
            lines(x = c(0, roi$xmax-roi$xmin), y = rep(y, 2), col="red")
          }
          
          for(cell_y in 1:nrow(roi$grid)) {
            for(cell_x in 1:ncol(roi$grid)) {
              if(roi$grid[cell_y, cell_x] == -1) {
                lines(x=c(colcuts[cell_x], colcuts[cell_x] + roi$cell_w),
                      y=c(rowcuts[cell_y], rowcuts[cell_y] + roi$cell_h), col="red")
                lines(x=c(colcuts[cell_x], colcuts[cell_x] + roi$cell_w),
                      y=c(rowcuts[cell_y] + roi$cell_h, rowcuts[cell_y]), col="red")
              } else if (roi$grid[cell_y, cell_x] != 0 | mode == "false_positives") {
                # Annotation
                anno <- roi$grid[cell_y, cell_x]
                anno_adj <- c(-0.1, 1)
                
                if (mode == "false_positives") {
                  if (shiny_image_file$threshData$False_Positives[cell_y, cell_x] == 1) {
                    text(x=colcuts[cell_x], y=rowcuts[cell_y], adj=anno_adj,
                      label="•", col="purple", cex=1.7)
                  }
                } else {
                  if (shiny_image_file$analytes[anno] == input$analyteSelected) color <- "green" else color <- "red"
                  text(x=colcuts[cell_x], y=rowcuts[cell_y], adj=anno_adj,
                       label="•", col=color, cex=1.7)
                }
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
      image <- shiny_image_file$img_final
      segmentation.list <- segmentation(image)
      shiny_image_file$segmentation_list <- segmentation.list
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
    if (!is.null(shiny_image_file$segmentation_list)) {
      shiny_image_file$threshData <- threshold(shiny_image_file$segmentation_list, TRUE)
      plotAndGrid_gridConfig("plot5", shiny_image_file$img_final, "false_positives")
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
      img_final <- shiny_image_file$img_final
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
            threshData$Mean_Intensities[y,x] <- mean(imageData(img)[signal], na.rm = TRUE)
            threshData$Median_Intensities[y,x] <- median(imageData(img)[signal], na.rm = TRUE)
            threshData$Valid_Pixels[y,x] <- sum(signal) / (dim(signal)[1] * dim(signal)[2])
            threshData$False_Positives[y,x] <- threshData$Valid_Pixels[y,x] < input$falseP_thresh & threshData$Valid_Pixels[y,x] > 0
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
            signal <- imageData(img) > Background.Threshold
            imageData(img) <- (imageData(img) - Background.Threshold)*signal
            threshData$Mean_Intensities[y,x] <- mean(imageData(img)[signal])
            threshData$Median_Intensities[y,x] <- median(imageData(img)[signal])
            threshData$Valid_Pixels[y,x] <- sum(signal) / (dim(signal)[1] * dim(signal)[2])
            threshData$False_Positives[y,x] <- threshData$Valid_Pixels[y,x] < input$falseP_thresh & threshData$Valid_Pixels[y,x] > 0
          } else {
            threshData$Mean_Intensities[y,x] <- NA
            threshData$Median_Intensities[y,x] <- NA
            threshData$Valid_Pixels[y,x] <- NA
            threshData$False_Positives[y,x] <- NA
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
  
  observeEvent(input$falseP_thresh, {
    if (!is.null(shiny_image_file$threshData$Valid_Pixels)) {
      shiny_image_file$threshData$False_Positives <- shiny_image_file$threshData$Valid_Pixels < input$falseP_thresh & shiny_image_file$threshData$Valid_Pixels > 0
    }
  })

# Input data -------------------------------------------------------------------

  inputData <- function(imageName, threshData, exclude_false) {
    if (exclude_false) {
      intens_data$dates[[paste0(input$testdate)]][["mean"]] <- ifelse(threshData$False_Positives, NA, threshData$Mean_Intensities)
      intens_data$dates[[paste0(input$testdate)]][["median"]] <- ifelse(threshData$False_Positives, NA, threshData$Median_Intensities)
    } else {
      intens_data$dates[[paste0(input$testdate)]][["mean"]] <- threshData$Mean_Intensities
      intens_data$dates[[paste0(input$testdate)]][["median"]] <- threshData$Median_Intensities
    }
    
    intens_data$dates[[paste0(input$testdate)]][["thresh"]] <- threshData$Threshold    
    intens_data$dates[[paste0(input$testdate)]][["valid"]] <- threshData$Valid_Pixels
    intens_data$dates[[paste0(input$testdate)]][["falseP"]] <- threshData$False_Positives
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
    Ana <- sapply(unlist(roi$grid), function(x) {if (x!="-1" & x!="0") shiny_image_file$analytes[as.numeric(x)] else NA})
    
    if (exclude_false) {
      Mean <- as.vector(ifelse(threshData$False_Positives, NA, threshData$Mean_Intensities))
      Median <- as.vector(ifelse(threshData$False_Positives, NA, threshData$Median_Intensities))
    } else {
      Mean <- as.vector(threshData$Mean_Intensities)
      Median <- as.vector(threshData$Median_Intensities)
    }
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
  
  
  observeEvent(input$go2postprocess, {
    updateTabsetPanel(session, "tabs", selected = "tab4")
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
      inputData(shiny_image_file$filename, shiny_image_file$threshData, exclude_false = input$removeFalseP)
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
  
  observeEvent(input$showIntensData,{
      updateTabsetPanel(session, "tabs", selected = "tab5")
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
                  selection = list(target = 'cell', selectable=FALSE))
        })
    }
  })
  
  observe({
    AnalyteCells <- which(roi$grid == match(input$showAnalyte, shiny_image_file$analytes), arr.ind=TRUE)
    selectCells(dataTableProxy("checkerboard"), selected=AnalyteCells, ignore.selectable = TRUE)
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
        analyte_idx <- match(input$plotAnalyte, shiny_image_file$analytes)
        coor <- which(roi$grid == analyte_idx, arr.ind = TRUE)
        # coor <- input$checkerboard_cells_selected
        validate(need(!is.na(coor[1]), "Please select an analyte."))
        validate(need(!any(coor==0), "ERROR: Invalid analyte selected."))  
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
