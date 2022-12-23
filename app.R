library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(here)
library(shiny)

# Define UI for miles per gallon app ----
ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Miles Per Gallon"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(),
  
  # Main panel for displaying outputs ----
  mainPanel()
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
}

shinyApp(ui, server)