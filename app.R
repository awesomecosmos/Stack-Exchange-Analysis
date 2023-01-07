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
      h4("Welcome to the StackExchange Dataset Explorer!"),
      p("You can select a StackExchange network and explore data related to 
        questions and answers on the site, their associated tags, and more!
        You can also filter the data according to a time range, and also download
        these plots for your own analysis!"),
      selectInput("dataset","Select a StackExchange dataset:",
                  all_dataset_names, all_dataset_names[2]),
      sliderInput("n","Select number of data points to display:",
                  0,50,10),
      dateRangeInput("dates", "Enter date range of StackExchange posts (for 'Time Series Analysis' tab):",
                     start = "2021-01-01", end = dataset_date,
                     max = dataset_date, format = "yyyy-mm-dd"),
      # p("You can find the source code for this app here:"),
      # a("https://github.com/awesomecosmos/Stack-Exchange-Analysis")
      
      p("You can find the source code for this app here:",
        a("Github", 
          href = "https://github.com/awesomecosmos/Stack-Exchange-Analysis"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Overall Analysis",
          plotOutput("Plot_top_tag_per_year"),
          downloadButton("download_Plot_top_tag_per_year","Download this plot"),
          plotOutput("Plot_top_n_tags_by_year"),
          downloadButton("download_plot_top_n_tags_by_year","Download this plot"),
          plotOutput("Plot_top_n_tags_by_count"),
          downloadButton("download_plot_top_n_tags_by_count","Download this plot")
        ),
        tabPanel("Time Series Analysis",
          plotOutput("plot_questions_timeseries"),
          downloadButton("download_plot_questions_timeseries","Download this plot"),
          plotOutput("plot_avg_duration_between_dates"),
          downloadButton("download_plot_avg_duration_between_dates","Download this plot")
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
  
  ggplot_top_tag_per_year <- reactive({
    ggplot(top_tag_per_year(), aes(x = year, y = num_tags, fill = IndivTags)) + 
      geom_col() +
      labs(title = paste0("Top Yearly Tag on ",dataset_name()), 
           x = "year", y = "frequency", fill = "Tag")
  })
  output$Plot_top_tag_per_year <- renderPlot({
    ggplot_top_tag_per_year()
  })
  output$download_Plot_top_tag_per_year <- downloadHandler(
    filename = function() {paste0(dataset_name(),'-top_tag_per_year.png')},
    content = function(file) {
      ggsave(file, plot = ggplot_top_tag_per_year(), 
             width = 700, height = 300, units = "mm",
             device = "png")
    }
  )
  
  
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
  
  ggplot_top_n_tags_by_count <- reactive({
    ggplot(top_n_tags_by_count(), aes(x = reorder(IndivTags, -num_tags), y = num_tags)) + 
      geom_col(fill = "blueviolet") + 
      labs(title = paste0("Top ",rval_n()," Tags on ",dataset_name()), 
           x = "tag", y = "frequency") +
      theme(axis.text.x = element_text(angle = 45,  hjust=1))
  })
  output$Plot_top_n_tags_by_count <- renderPlot({
    ggplot_top_n_tags_by_count()
  })
  output$download_plot_top_n_tags_by_count <- downloadHandler(
    filename = function() {paste0(dataset_name(),'-top_n_tags_by_count.png')},
    content = function(file) {
      ggsave(file, plot = ggplot_top_n_tags_by_count(), 
             width = 700, height = 300, units = "mm",
             device = "png")
    }
  )
  
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
  
  ggplot_top_n_tags_by_year <- reactive({
    ggplot(top_n_tags_by_year(), aes(x = year, y = num_tags, 
                                     group = IndivTags, color = IndivTags)) + 
      geom_line() +
      labs(title = paste0("Top ",rval_n()," Tags on ",dataset_name()," Over Time"), 
           x = "year", y = "frequency",
           color = "Tag")
  })
  output$Plot_top_n_tags_by_year <- renderPlot({
    ggplot_top_n_tags_by_year()
  })
  output$download_plot_top_n_tags_by_year <- downloadHandler(
    filename = function() {paste0(dataset_name(),'-top_n_tags_by_year.png')},
    content = function(file) {
      ggsave(file, plot = ggplot_top_n_tags_by_year(), 
             width = 700, height = 300, units = "mm",
             device = "png")
    }
  )
  
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
           subtitle = paste0(n_closed_questions()," / ",n_total_questions(),
                             " questions closed (",percent_closed(),"%)"))
  })
  output$plot_questions_timeseries <- renderPlot({
    ggplot_questions_timeseries()
  })
  output$download_plot_questions_timeseries <- downloadHandler(
    filename = function() {paste0(dataset_name(),'-questions_timeseries.png')},
    content = function(file) {
      ggsave(file, plot = ggplot_questions_timeseries(), 
             width = 700, height = 300, units = "mm",
             device = "png")
    }
  )
  
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
  
  ggplot_avg_duration_between_dates <- reactive({
    ggplot(avg_duration_between_dates(), aes(x = CreationDate)) + 
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
  })
  
  output$plot_avg_duration_between_dates <- renderPlot({
    ggplot_avg_duration_between_dates()
  })
  output$download_plot_avg_duration_between_dates <- downloadHandler(
    filename = function() {paste0(dataset_name(),'-avg_duration_between_dates.png')},
    content = function(file) {
      ggsave(file, plot = ggplot_avg_duration_between_dates(), 
             width = 700, height = 300, units = "mm",
             device = "png")
    }
  )
  
}

################################################################################
# Creating app
shinyApp(ui, server)