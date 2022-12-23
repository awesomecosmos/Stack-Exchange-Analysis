library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(here)

source("src/utils.R")

all_files <- list.files(path=here("data/"), pattern=".csv", all.files=TRUE,
           full.names=FALSE)

current_file <- all_files[10]

dataset_name <- str_replace(current_file,".csv","")
dataset_name <- str_replace(dataset_name,"_"," ")
dataset_name <- paste0(str_to_title(dataset_name)," StackExchange")

raw <- read_csv(here("data", current_file)) 

df <- clean_df(raw)

n <- 10

# frequency bar chart of top N tags
tags_by_count <- df %>% 
  group_by(IndivTags) %>% 
  summarize(num_tags=n()) %>% 
  arrange(desc(num_tags)) 

top_n_tags_by_count <- tags_by_count[1:n,]

ggplot(top_n_tags_by_count, aes(x = reorder(IndivTags, -num_tags), y = num_tags)) + 
  geom_col(fill = "blueviolet") + 
  labs(title = paste0("Top ",n," Tags for ",dataset_name), x = "tag", y = "frequency") +
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

# bar plot of top tag of each year
top_tag_per_year <- df %>% 
  group_by(year,IndivTags) %>% 
  summarize(num_tags=n()) %>% 
  arrange(year,desc(num_tags)) %>% 
  slice(1)

ggplot(top_tag_per_year, aes(x = year, y = num_tags, fill = IndivTags)) + 
  geom_col() +
  labs(title = paste0("Top Yearly Tag for ",dataset_name), x = "tag", y = "frequency",
       fill = "Tag")

# line plot of top n tags over time
tags_by_year <- df %>% 
  group_by(year,IndivTags) %>% 
  summarize(num_tags=n()) %>% 
  arrange(year,desc(num_tags))

top_n_tags_by_year <- tags_by_year %>% 
  filter(IndivTags %in% top_n_tags_by_count$IndivTags)

ggplot(top_n_tags_by_year, aes(x = year, y = num_tags, group = IndivTags, color = IndivTags)) + 
  geom_line() +
  labs(title = paste0("Top ",n," Tags for ",dataset_name," Over Time"), x = "tag", y = "frequency",
       color = "Tag")
  


