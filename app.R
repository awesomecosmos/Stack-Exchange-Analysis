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
      dateRangeInput("dates", "Enter date range of StackExchange posts (for 'Time Series Analysis' tab):",
                     start = "2021-01-01", end = dataset_date,
                     max = dataset_date, format = "yyyy-mm-dd"),
      downloadButton("download_plot_questions_timeseries","download me!")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Overall Analysis",
          plotOutput("Plot_top_tag_per_year"),
          plotOutput("Plot_top_n_tags_by_year"),
          plotOutput("Plot_top_n_tags_by_count")
        ),
        tabPanel("Time Series Analysis",
          plotOutput("plot_questions_timeseries"),
          plotOutput("plot_avg_duration_between_dates")
        )
      )
    )
  )
)

################################################################################
# Server
server <- function(input, output) {
  
  # defining re-usable reactive values
  rval_dataset <- reactive({input$dataset})
  rval_n <- reactive({input$n})
  rval_creation_date_start <- reactive({input$dates[[1]]})
  rval_creation_date_end <- reactive({input$dates[[2]]})
  
  index <- reactive({match(rval_dataset(),all_dataset_names)})
  current_file <- reactive({str_replace(all_files[index()],".csv",".rds")})
  dataset_name <- reactive({all_dataset_names[index()]})
  
  df <- reactive({
    readRDS(paste0("data/rds/",current_file()))
  })
  
  df_date_filtered <- reactive({
    df() %>% 
      filter(CreationDate >= rval_creation_date_start() & CreationDate <= rval_creation_date_end())
    })
  
  date_diff_for_display <- reactive({
    paste0(" During ",rval_creation_date_start()," - ", rval_creation_date_end())
  })
  
  # bar plot of top tag of each year
  top_tag_per_year <- reactive({
    df() %>%
      group_by(year,IndivTags) %>%
      summarize(num_tags=n()) %>%
      arrange(year,desc(num_tags)) %>%
      slice(1)
  })
  
  plot1 <- reactive({
    plot_top_tag_per_year(top_tag_per_year(), top_tag_per_year()$year,
                          top_tag_per_year()$num_tags, top_tag_per_year()$IndivTags,
                          dataset_name(), rval_n())
  })
  output$Plot_top_tag_per_year <- renderPlot({
    plot1()
  })
  
  # output$download_Plot_top_tag_per_year <- downloadHandler(
  #   filename = function() {'top_tag_per_yr.png'},
  #   content = function(file) {
  #     ggsave(file, plot = plot1(), device = "png")
  #   }
  # )
  # output$download_Plot_top_tag_per_year <- downloadHandler(
  #   filename = "top_tag_per_yr.png" ,
  #   content = function(file) {
  #     ggsave(plot1(), filename = file)
  #   })
  # output$download_Plot_top_tag_per_year <- downloadHandler(
  #   filename = function(file) {
  #     "top_tag_per_yr.png"
  #     #ifelse(is.null(input$DataFile), return(), str_c(input$Title, ".png"))
  #   },
  #   content = function(file) {
  #     ggsave(file, plot = plot1(), device = "png")#, width = 290, height = 265, units = "mm", device = "png")
  #   }
  # )
  
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
  
  # how many total questions were answered in the time range Time1 - Time2?
  # what percentage of these questions were marked as closed?
  questions_timeseries <- reactive({
    tmp <- df_date_filtered() %>% 
      select(CreationDate,ClosedDate,Body) %>% 
      distinct() %>% 
      group_by(CreationDate) %>% 
      mutate(
        n_questions_per_day = n(),
        question_closed_or_not = case_when(
          !is.na(ClosedDate) ~ 1,
          TRUE ~ 0
        )) %>% 
      select(CreationDate,n_questions_per_day,question_closed_or_not) %>% 
      distinct() %>% 
      group_by(CreationDate) %>% 
      slice(1)
  })
  
  n_total_questions <- reactive({sum(questions_timeseries()$n_questions_per_day)})
  n_closed_questions <- reactive({sum(questions_timeseries()$question_closed_or_not)})
  percent_closed <- reactive({round((n_closed_questions() / n_total_questions()) * 100, 2)})
  
  ggplot_questions_timeseries <- reactive({
    ggplot(questions_timeseries(), aes(x = CreationDate, y = n_questions_per_day)) + 
      geom_line() +
      geom_smooth(method="lm", se=FALSE) +
      labs(title = paste0("Number of Questions Asked on ",dataset_name(),
                          date_diff_for_display()), 
           x = "date", y = "number of questions",
           subtitle = paste0(n_closed_questions()," / ",n_total_questions()," questions closed (",percent_closed(),"%)"))
  })
  output$plot_questions_timeseries <- renderPlot({
    ggplot_questions_timeseries()
  })
  output$download_plot_questions_timeseries <- downloadHandler(
    filename = function() {'top_tag_per_yr.png'},
    content = function(file) {
      ggsave(file, plot = ggplot_questions_timeseries(), 
             width = 700, height = 300, units = "mm", device = "png")
    }
  )
  # output$download_Plot_top_tag_per_year <- downloadHandler(
  #   filename = "top_tag_per_yr.png" ,
  #   content = function(file) {
  #     ggsave(plot1(), filename = file)
  #   })
  
  # what is the average duration between CreationDate and [LastEditDate | LastActivityDate | ClosedDate]?
  avg_duration_between_dates <- reactive({
    tmp <- df_date_filtered() %>% 
      select(CreationDate,ClosedDate,LastEditDate,LastActivityDate,ClosedDate) %>% 
      distinct() %>% 
      mutate(
        diff_LastEditDate = case_when(
          !is.na(LastEditDate) ~ as.numeric(difftime(LastEditDate, CreationDate, units = "days")),
          is.na(LastEditDate) ~ as.numeric(difftime(dataset_date, CreationDate, units = "days"))
        ),
        diff_LastActivityDate = case_when(
          !is.na(LastActivityDate) ~ as.numeric(difftime(LastActivityDate, CreationDate, units = "days")),
          is.na(LastActivityDate) ~ as.numeric(difftime(dataset_date, CreationDate, units = "days"))
        ),
        diff_ClosedDate = case_when(
          !is.na(ClosedDate) ~ as.numeric(difftime(ClosedDate, CreationDate, units = "days")),
          is.na(ClosedDate) ~ as.numeric(difftime(dataset_date, CreationDate, units = "days"))
        )) %>% 
      group_by(CreationDate) %>% 
      mutate(
        avg_diff_LastEditDate = mean(diff_LastEditDate),
        avg_diff_LastActivityDate = mean(diff_LastActivityDate),
        avg_diff_ClosedDate = mean(diff_ClosedDate)
      ) %>% 
      select(CreationDate,avg_diff_LastEditDate,avg_diff_LastActivityDate,avg_diff_ClosedDate) %>% 
      distinct()
  })
  
  mean_avg_diff_LastEditDate <- reactive({round(mean(avg_duration_between_dates()$avg_diff_LastEditDate),0)})
  mean_avg_diff_LastActivityDate <- reactive({round(mean(avg_duration_between_dates()$avg_diff_LastActivityDate),0)})
  mean_avg_diff_ClosedDate <- reactive({round(mean(avg_duration_between_dates()$avg_diff_ClosedDate),0)})
  
  output$plot_avg_duration_between_dates <- renderPlot({
    plot <- ggplot(avg_duration_between_dates(), aes(x = CreationDate)) + 
      geom_line(aes(y = avg_diff_LastEditDate, colour="CreationDate - LastEditDate")) +
      geom_line(aes(y = avg_diff_LastActivityDate, colour="CreationDate - LastActivityDate")) +
      geom_line(aes(y = avg_diff_ClosedDate, colour="CreationDate - ClosedDate")) +
      scale_color_manual(name = "Avg. Difference Between:",
                         values = c("CreationDate - LastEditDate" = "dodgerblue2",
                                    "CreationDate - LastActivityDate" = "seagreen3",
                                    "CreationDate - ClosedDate" = "indianred2")) +
      labs(title = paste0("Average Difference Between Question Dates on ",
                          dataset_name(),
                          date_diff_for_display()),
           x = "date", y = "average number of days",
           caption = paste0(
             "Avg. difference in days between a question's creation and last activity: ",
             mean_avg_diff_LastActivityDate(), " days, ",
             "Avg. difference in days between a question's creatison and last edit: ",
             mean_avg_diff_LastEditDate(), " days, ",
             "Avg. difference in days between a question's creation and close: ",
             mean_avg_diff_ClosedDate(), "days."
             )
      )
    print(plot)
  })
  
}

################################################################################
# Creating app
shinyApp(ui, server)