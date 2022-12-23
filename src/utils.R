library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(here)

# function to read all csv files and return files and filenames
dataset_reader <- function(path){
  all_files <- list.files(path=path, pattern=".csv", all.files=TRUE,
                          full.names=FALSE)
  all_dataset_names <- str_replace(all_files,".csv","")
  all_dataset_names <- str_replace(all_dataset_names,"_"," ")
  all_dataset_names <- str_replace(all_dataset_names,"_"," ")
  all_dataset_names <- paste0(str_to_title(all_dataset_names)," StackExchange")
  return(list(all_files,all_dataset_names))
}

# function to clean raw dataframe
clean_df <- function(raw_df){
  df <- raw_df %>% 
    select(c(CreationDate,Score,AnswerCount,CommentCount,Tags)) %>% 
    distinct() %>% 
    filter(!is.na(Tags)) %>% 
    mutate(year = substring(CreationDate,1,4)) %>% 
    mutate(IndivTags = str_split(Tags,">")) %>% 
    unnest(IndivTags) %>% 
    filter(IndivTags != "") %>% 
    mutate(IndivTags = str_replace(IndivTags,"<","")) %>% 
    distinct()
  return(df)
}

# function to plot bar plot of top N tags
plot_top_n_tags_by_count <- function(df,df.x,df.y,dataset_name,n){
  plot <- ggplot(df, aes(x = reorder(df.x, -df.y), y = df.y)) + 
    geom_col(fill = "blueviolet") + 
    labs(title = paste0("Top ",n," Tags for ",dataset_name), x = "tag", y = "frequency") +
    theme(axis.text.x = element_text(angle = 45,  hjust=1))
  print(plot)
}

# function to plot bar plot of top tag per year
plot_top_tag_per_year <- function(df,df.x,df.y,df.fill,dataset_name,n){
  plot <- ggplot(df, aes(x = df.x, y = df.y, fill = df.fill)) + 
    geom_col() +
    labs(title = paste0("Top Yearly Tag for ",dataset_name), x = "tag", y = "frequency",
         fill = "Tag")
  print(plot)
}

# function to plot line chart of top n tags over time
plot_top_n_tags_by_year <- function(df,df.x,df.y,df.group,df.color,dataset_name,n){
  plot <- ggplot(df, aes(x = df.x, y = df.y, group = df.group, color = df.color)) + 
    geom_line() +
    labs(title = paste0("Top ",n," Tags for ",dataset_name," Over Time"), x = "tag", y = "frequency",
         color = "Tag")
  print(plot)
}