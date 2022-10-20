library(shiny) 
library(shinyjs) 
library(EBImage)
library(fs)
library(DT)
library(plotly)

# UI ---------------------------------------------------------------------------
ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Microarray Data Analysis App"),
  tags$style(type="text/css", "#stop { float:right; }"),
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
                                      brush = "plot_brush",
                                      height="500px"),
                           h6("Click and drag to select a region of interest. Double click on the selected region to zoom.", align = "center"), # nolint
                           br(),
                           column(6,
                                  actionButton("reset", label = "Reset"),
                                  tags$style(type="text/css", "#reset { display: block; width:50%; margin-left: auto; margin-right:auto;}"), # nolint
                           ),
                           column(6, shinyjs::disabled(
                             actionButton("applyGrid", label = "Apply Grid")),
                             tags$style(type="text/css", "#applyGrid { display: block; width:50%; margin-left: auto; margin-right:auto;}"), # nolint
                           )
                          ) # END OF MAIN PANEL
                       ) # END OF SIDEBAR LAYOUT
              ), # END OF TAB PANEL

## Grid Conf (2nd Tab) -----------------------------------------------------
              tabPanel("Grid Configuration", value = "tab2",
                       sidebarLayout(
                         sidebarPanel(
                             fileInput(inputId = "gridLabels",
                                       label = "Load Grid Label File"),
                             hr(),
                             textInput("analyteName", label = "Analyte name", value = ""),
                             actionButton("addAnalyte", "Add Analyte"),
                             selectInput("analyteSelected",
                                          label = ("Select Analyte"),
                                          choices = c("Please add analyte"),
                                          selected = 1),
                             hr(),
                             actionButton("segmentation", label = "Apply Segmentation")
                         ), # END OF SIDEBAR PANEL
                         mainPanel(
                           h3("Grid Configuration", align = "center"),
                           plotOutput("plot2",
                                      click = "plot2_click",
                                      dblclick = "plot2_dblclick",
                                      hover = "plot2_hover",
                                      brush = brushOpts(id="plot2_brush",
                                                        delay=50000),
                                      height="500px"
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
                           actionButton("go2postprocess", "Proceed to post-processing")
                           # fileInput(inputId = "multiImages",
                           #           label = "Analyse multiple images with same settings",
                           #           multiple = TRUE,
                           #           placeholder = "JPEG, PNG, and TIFF are supported",
                           #           accept = c(
                           #             "image/jpeg",
                           #             "image/x-png",
                           #             "image/tiff",
                           #             ".jpg",
                           #             ".png",
                           #             ".tiff")),
                           # actionButton("multiImagesView", "Check Images"),
                           # actionButton("multiImagesAnalyse", "Analyze"),
                           # hr(),
                         ), # END OF SIDEBAR PANEL
                         mainPanel(
                            h3("Background Correction", align = "center"),
                            verbatimTextOutput("thresh"),
                            br(),
                            h4("Signal Intensity Above Background and Signal after Background Correction", align = "center"), # nolint
                            splitLayout(plotOutput("plot3"), plotOutput("plot4")),
                            verbatimTextOutput("meanIntens"),
                            verbatimTextOutput("medianIntens"),
                            br(),
                            br()
                         ) # END OF MAIN PANEL
                       ) # END OF SIDEBAR LAYOUT
              ), # END OF TAB PANEL
              ## Start of Tab Data

## Post-processing tab (4th Tab) ---------------------------------------------------
              tabPanel("Post-processing", value = "tab4",
                sidebarLayout(
                  sidebarPanel(
                    numericInput("falseP_thresh", "False positives threshold", 0.3, min=0, max=1, step=0.01),
                    hr(),
                    checkboxInput("removeFalseP", "Exclude false positives"),
                    actionButton("data", label = "Add To Data"),
                    hr(),
                    actionButton("showIntensData", label = "Show Intensity Data")
                  ),
                  mainPanel(
                    h3("False positives overview", align = "center"),
                      plotOutput("plot5",
                                click = "plot5_click",
                                dblclick = "plot5_dblclick",
                                hover = "plot5_hover",
                                brush = brushOpts(id="plot5_brush",
                                                  delay=50000),
                                height = "500px"
                                ),
                    h6("Click on the grid to add/remove false positive mark.", align = "center"), # nolint
                  )
                )
              ),

## Intensity Data (5th Tab) ------------------------------------------------
              tabPanel("Intensity Data", value = "tab5",
                       sidebarLayout(
                         sidebarPanel(
                           fileInput(inputId = "intensDataLabels",
                                     label = "Load Label File"),
                           hr(),
                           downloadButton("downloadData", "Download Data"),
                           tags$style(type = "text/css", "#downloadData { display: block; width:50%;}"),
                           br(),
                           actionButton("deleteData", label = "Delete Data"),
                           tags$style(type = "text/css", "#deleteData { display: block; width:50%;}"),

                         ),
                         mainPanel(
                           DTOutput("intens")
                         )# END OF MAIN PANEL
                       )# END OF SIDEBAR LAYOUT
              ), # END OF TAB PANEL
              ## Start of Tab Data

## Timeseries (5th Tab) ----------------------------------------------------
              tabPanel("Checkerboard Overview", value = "tab6",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("date", label = "Choose Date:", choices = ""),
                           selectInput("param", label = "Choose Parameter:",
                                       choices = list("mean",
                                                      "median",
                                                      "threshold" = "thresh",
                                                      "valid",
                                                      "false positives" = "falseP")),
                           selectInput("showAnalyte", label = "Choose Analyte:", choices = ""),
                           tags$style(type = "text/css", "#plotme { display: block; width:50%;}"),
                           br(),
                           br(),
                         ),
                         mainPanel(
                           DTOutput("checkerboard")
                         )# END OF MAIN PANEL
                       )# END OF SIDEBAR LAYOUT
              ), # END OF TAB PANEL

## Timeseries Plot (6th Tab) -----------------------------------------------
              tabPanel("Timeseries plot", value = "plotly",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("plotAnalyte", label = "Choose Analyte:", choices = "")
                         ),
                         mainPanel(
                           h3("Plot Output"),
                           plotly::plotlyOutput("plotly")
                         )# END OF MAIN PANEL
                       )# END OF SIDEBAR LAYOUT
              )# END OF TAB PANEL
  )# END OF TAB SET PANEL
) # END OF UI
