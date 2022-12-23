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

################################################################################
# UI
ui <- fluidPage(
  titlePanel("StackExchange Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset","Select a StackExchange dataset:",
                  all_dataset_names, all_dataset_names[2]),
      sliderInput("n","Select number of data points to display:",
                  0,50,10)
    ),
    mainPanel(
      # tableOutput("table"),
      plotOutput("Plot_top_n_tags_by_count"),
      plotOutput("Plot_top_tag_per_year"),
      plotOutput("Plot_top_n_tags_by_year")
    )
  )
)

################################################################################
# Server
server <- function(input, output) {
  rval_dataset <- reactive({input$dataset})
  rval_n <- reactive({input$n})
  
  index <- reactive({match(rval_dataset(),all_dataset_names)})

  current_file <- reactive({all_files[index()]})
  dataset_name <- reactive({all_dataset_names[index()]})

  raw <- reactive({read_csv(here("data", current_file()))})
  df <- reactive({clean_df(raw())})

  # frequency bar chart of top N tags
  tags_by_count <- reactive({
  df() %>%
    group_by(IndivTags) %>%
    summarize(num_tags=n()) %>%
    arrange(desc(num_tags))
  })
  
  output$table <- renderDataTable({all_files[1]})
  
  top_n_tags_by_count <- reactive({
    tags_by_count()[1:rval_n(),]
  })

  plot1 <- reactive({
    plot_top_n_tags_by_count(top_n_tags_by_count(), top_n_tags_by_count()$IndivTags,
                             top_n_tags_by_count()$num_tags, dataset_name(), rval_n())
  })
  output$Plot_top_n_tags_by_count <- renderPlot({plot1()})
  
  # bar plot of top tag of each year
  top_tag_per_year <- reactive({
    df() %>%
      group_by(year,IndivTags) %>%
      summarize(num_tags=n()) %>%
      arrange(year,desc(num_tags)) %>%
      slice(1)
  })

  plot2 <- reactive({
    plot_top_tag_per_year(top_tag_per_year(), top_tag_per_year()$year,
                          top_tag_per_year()$num_tags, top_tag_per_year()$IndivTags,
                          dataset_name(), rval_n())
  })

  output$Plot_top_tag_per_year <- renderPlot({plot2()})

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

  plot3 <- reactive({
    plot_top_n_tags_by_year(top_n_tags_by_year(),top_n_tags_by_year()$year, top_n_tags_by_year()$num_tags,
                            top_n_tags_by_year()$IndivTags, top_n_tags_by_year()$IndivTags,
                            dataset_name(), rval_n())
  })
  output$Plot_top_n_tags_by_year <- renderPlot({plot3()})
}

################################################################################
# Creating app
shinyApp(ui, server)