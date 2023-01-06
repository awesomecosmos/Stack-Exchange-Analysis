library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(here)
library(shiny)

source("src/utils.R")

files_and_names <- dataset_reader(path = here("data/"))
all_files <- files_and_names[[1]]
all_dataset_names <- files_and_names[[2]]
dataset_date <- "2022-12-07" # this is the date the dataset was downloaded

################################################################################
# UI
ui <- fluidPage(
  titlePanel("StackExchange Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset","Select a StackExchange dataset:",
                  all_dataset_names, all_dataset_names[2]),
      sliderInput("n","Select number of data points to display:",
                  0,50,10),
      dateRangeInput("dates", "Enter date range of StackExchange posts:",
                     start = "2021-01-01", end = dataset_date,
                     max = dataset_date, format = "yyyy-mm-dd")
    ),
    mainPanel(
      plotOutput("Plot_top_tag_per_year"),
      plotOutput("Plot_top_n_tags_by_count"),
      plotOutput("Plot_top_n_tags_by_year")
    )
  )
)

################################################################################
# Server
server <- function(input, output) {
  rval_dataset <- reactive({input$dataset})
  rval_n <- reactive({input$n})
  rval_creation_date_start <- reactive({input$dates[[1]]})
  rval_creation_date_end <- reactive({input$dates[[2]]})
  
  index <- reactive({match(rval_dataset(),all_dataset_names)})
  current_file <- reactive({str_replace(all_files[index()],".csv",".rds")})
  dataset_name <- reactive({all_dataset_names[index()]})
  
  df <- reactive({
    readRDS(paste0("data/rds/",current_file())) %>% 
      filter(CreationDate >= rval_creation_date_start() & CreationDate <= rval_creation_date_end())
    })
  
  # bar plot of top tag of each year
  top_tag_per_year <- reactive({
    df() %>%
      group_by(year,IndivTags) %>%
      summarize(num_tags=n()) %>%
      arrange(year,desc(num_tags)) %>%
      slice(1)
  })
  
  output$Plot_top_tag_per_year <- renderPlot({
    plot_top_tag_per_year(top_tag_per_year(), top_tag_per_year()$year,
                          top_tag_per_year()$num_tags, top_tag_per_year()$IndivTags,
                          dataset_name(), rval_n())
  })
  
  # frequency bar chart of top N tags
  tags_by_count <- reactive({
    df() %>%
      group_by(IndivTags) %>%
      summarize(num_tags=n()) %>%
      arrange(desc(num_tags))
  })
  
  top_n_tags_by_count <- reactive({
    tags_by_count()[1:rval_n(),]
  })
  
  output$Plot_top_n_tags_by_count <- renderPlot({
    plot_top_n_tags_by_count(top_n_tags_by_count(), top_n_tags_by_count()$IndivTags,
                             top_n_tags_by_count()$num_tags, dataset_name(), rval_n())
  })
  
  # line plot of top n tags over time
  tags_by_year <- reactive({
    df() %>%
      group_by(year,IndivTags) %>%
      summarize(num_tags=n()) %>%
      arrange(year,desc(num_tags))
  })
  
  top_n_tags_by_year <- reactive({
    tags_by_year() %>%
      filter(IndivTags %in% top_n_tags_by_count()$IndivTags)
  })
  
  output$Plot_top_n_tags_by_year <- renderPlot({
    plot_top_n_tags_by_year(top_n_tags_by_year(),top_n_tags_by_year()$year, top_n_tags_by_year()$num_tags,
                            top_n_tags_by_year()$IndivTags, top_n_tags_by_year()$IndivTags,
                            dataset_name(), rval_n())
  })
}

################################################################################
# Creating app
shinyApp(ui, server)