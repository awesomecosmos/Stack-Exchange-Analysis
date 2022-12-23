library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(here)

source("src/utils.R")

files_and_names <- dataset_reader(path = here("data/"))
all_files <- files_and_names[[1]]
all_dataset_names <- files_and_names[[2]]

index <- 11
n <- 10

current_file <- all_files[index]
dataset_name <- all_dataset_names[index]

raw <- read_csv(here("data", current_file)) 

df <- clean_df(raw)

# frequency bar chart of top N tags
tags_by_count <- df %>% 
  group_by(IndivTags) %>% 
  summarize(num_tags=n()) %>% 
  arrange(desc(num_tags)) 

top_n_tags_by_count <- tags_by_count[1:n,]

plot_top_n_tags_by_count(top_n_tags_by_count, top_n_tags_by_count$IndivTags, 
                         top_n_tags_by_count$num_tags, dataset_name,n)

# bar plot of top tag of each year
top_tag_per_year <- df %>% 
  group_by(year,IndivTags) %>% 
  summarize(num_tags=n()) %>% 
  arrange(year,desc(num_tags)) %>% 
  slice(1)

plot_top_tag_per_year(top_tag_per_year, top_tag_per_year$year,
                      top_tag_per_year$num_tags, top_tag_per_year$IndivTags,
                      dataset_name,n)

# line plot of top n tags over time
tags_by_year <- df %>% 
  group_by(year,IndivTags) %>% 
  summarize(num_tags=n()) %>% 
  arrange(year,desc(num_tags))

top_n_tags_by_year <- tags_by_year %>% 
  filter(IndivTags %in% top_n_tags_by_count$IndivTags)

plot_top_n_tags_by_year(top_n_tags_by_year,top_n_tags_by_year$year, top_n_tags_by_year$num_tags,
                        top_n_tags_by_year$IndivTags, top_n_tags_by_year$top_n_tags_by_year,
                        dataset_name,n)
  


