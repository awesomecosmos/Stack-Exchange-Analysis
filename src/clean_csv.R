# This is a script to clean the raw CSVs and save them as RdS objects
# By reading RdS objects into the app, this helps in faster loading times

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(here)

source("src/utils.R")

files_and_names <- dataset_reader(path = here("data/"))
all_files <- files_and_names[[1]]
all_dataset_names <- files_and_names[[2]]
saveRDS(all_dataset_names, "data/all_dataset_names.rds")

for (index in 1:length(files_and_names[[1]])){
  filename <- str_replace(files_and_names[[1]][index],".csv","")
  raw <- read_csv(here("data", files_and_names[[1]][index])) 
  df <- clean_df(raw) %>% 
    mutate(dataset = files_and_names[[2]][index])
  saveRDS(df, paste0("data/rds/",filename,".rds"))
}
# 
# academia <- readRDS("data/academia.rds")
# astronomy <- readRDS("data/astronomy.rds")
# computer_science <- readRDS("data/computer_science.rds")
# cross_validated <- readRDS("data/cross_validated.rds")
# data_science <- readRDS("data/data_science.rds")
# french <- readRDS("data/french.rds")
# math_overflow <- readRDS("data/math_overflow.rds")
# mathematics <- readRDS("data/mathematics.rds")
# philosophy <- readRDS("data/philosophy.rds")
# physics <- readRDS("data/physics.rds")
# science_fiction_fantasy <- readRDS("data/science_fiction_fantasy.rds")
# space_exploration <- readRDS("data/space_exploration.rds")
# tex <- readRDS("data/tex.rds")
# worldbuilding <- readRDS("data/worldbuilding.rds")
# 
# mega_df <- rbind(academia,astronomy,computer_science,cross_validated,data_science,
#                  french,math_overflow,mathematics,philosophy,physics,
#                  science_fiction_fantasy,space_exploration,tex,worldbuilding)
# 
# saveRDS(mega_df, paste0("data/all_data.rds"))
# 




# 
# dataset_date <- "2022-12-07" # this is the date the dataset was downloaded
# 
# dateRangeInput("dates", "Enter date range of StackExchange posts:",
#                start = "2021-01-01", end = dataset_date,
#                max = dataset_date, format = "yyyy-mm-dd")
# 
# 
# 
# 
# 
# 
# 
# 
# library(readr)
# library(dplyr)
# library(tidyr)
# library(stringr)
# library(ggplot2)
# library(here)
# 
# # function to read all csv files and return files and filenames
# dataset_reader <- function(path){
#   all_files <- list.files(path=path, pattern=".csv", all.files=TRUE,
#                           full.names=FALSE)
#   all_dataset_names <- str_replace(all_files,".csv","")
#   all_dataset_names <- str_replace(all_dataset_names,"_"," ")
#   all_dataset_names <- str_replace(all_dataset_names,"_"," ")
#   all_dataset_names <- paste0(str_to_title(all_dataset_names)," StackExchange")
#   return(list(all_files,all_dataset_names))
# }
# 
# # function to clean raw dataframe
# clean_df <- function(raw_df){
#   df <- raw %>% 
#     select(CreationDate,LastEditDate,LastActivityDate,ClosedDate,
#            OwnerUserId,Body,Score,ViewCount,CommentCount,FavoriteCount,
#            AnswerCount,CommentCount,Tags) %>%
#     distinct() %>% 
#     mutate(
#       CreationDate = as.Date(as.POSIXct(CreationDate)),
#       LastEditDate = as.Date(as.POSIXct(LastEditDate)),
#       LastActivityDate = as.Date(as.POSIXct(LastActivityDate)),
#       ClosedDate = as.Date(as.POSIXct(ClosedDate)),
#       year = substring(CreationDate,1,4)
#     ) %>% 
#     filter(!is.na(Tags)) %>% 
#     mutate(IndivTags = str_split(Tags,">")) %>% 
#     unnest(IndivTags) %>% 
#     filter(IndivTags != "") %>% 
#     mutate(IndivTags = str_replace(IndivTags,"<","")) %>% 
#     distinct()
#   
#   return(df)
# }
# 
# # function to plot bar plot of top N tags
# plot_top_n_tags_by_count <- function(df,df.x,df.y,dataset_name,n){
#   plot <- ggplot(df, aes(x = reorder(df.x, -df.y), y = df.y)) + 
#     geom_col(fill = "blueviolet") + 
#     labs(title = paste0("Top ",n," Tags for ",dataset_name), x = "tag", y = "frequency") +
#     theme(axis.text.x = element_text(angle = 45,  hjust=1))
#   print(plot)
# }
# 
# # function to plot bar plot of top tag per year
# plot_top_tag_per_year <- function(df,df.x,df.y,df.fill,dataset_name,n){
#   plot <- ggplot(df, aes(x = df.x, y = df.y, fill = df.fill)) + 
#     geom_col() +
#     labs(title = paste0("Top Yearly Tag for ",dataset_name), x = "tag", y = "frequency",
#          fill = "Tag")
#   print(plot)
# }
# 
# # function to plot line chart of top n tags over time
# plot_top_n_tags_by_year <- function(df,df.x,df.y,df.group,df.color,dataset_name,n){
#   plot <- ggplot(df, aes(x = df.x, y = df.y, group = df.group, color = df.color)) + 
#     geom_line() +
#     labs(title = paste0("Top ",n," Tags for ",dataset_name," Over Time"), x = "tag", y = "frequency",
#          color = "Tag")
#   print(plot)
# }
