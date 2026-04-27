#' @import data.table
#' @import ggplot2
#' @import bs4Dash
#' @import RColorBrewer
#' @importFrom scales label_number label_percent hue_pal percent
#' @importFrom stringr str_replace_all
#' @importFrom plotly plot_ly add_trace add_segments add_markers add_bars layout ggplotly renderPlotly plotlyOutput
#' @importFrom stats weighted.mean lm predict density sd
#' @importFrom htmltools div tags HTML tagList
#' @importFrom shiny shinyApp runApp reactive debounce eventReactive renderPlot renderUI uiOutput observe req validate need fluidRow column selectInput sliderInput actionButton br icon plotOutput updateSelectInput conditionalPanel
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyjs useShinyjs disable enable
#' @importFrom geobr read_state
#' @importFrom arrow read_parquet write_parquet
#' @importFrom fs path dir_create dir_exists dir_delete file_exists
#' @importFrom tools R_user_dir
NULL