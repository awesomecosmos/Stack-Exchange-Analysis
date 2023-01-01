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

index <- 2
n <- 10

current_file <- all_files[index]
dataset_name <- all_dataset_names[index]

raw <- read_csv(here("data", current_file)) 

# df <- clean_df(raw)
df <- raw %>% 
  select(c(CreationDate,LastEditDate,LastActivityDate,ClosedDate,
           OwnerUserId,Body,Score,ViewCount,CommentCount,FavoriteCount,
           AnswerCount,CommentCount,Tags)) %>%
  distinct() %>% 
  mutate(
    CreationDate = as.Date(as.POSIXct(CreationDate)),
    LastEditDate = as.Date(as.POSIXct(LastEditDate)),
    LastActivityDate = as.Date(as.POSIXct(LastActivityDate)),
    ClosedDate = as.Date(as.POSIXct(ClosedDate))
  ) %>% 
  filter(!is.na(Tags)) %>% 
  mutate(year = substring(CreationDate,1,4)) %>% 
  mutate(IndivTags = str_split(Tags,">")) %>% 
  unnest(IndivTags) %>% 
  filter(IndivTags != "") %>% 
  mutate(IndivTags = str_replace(IndivTags,"<","")) %>% 
  distinct()

creation_date_start <- "2021-01-01"
creation_date_end <- "2021-07-31"

CreationDate_range_df <- df %>% 
  filter(CreationDate >= creation_date_start & CreationDate <= creation_date_end)

# questions to answer:
# how many total questions were answered in the time range Time1 - Time2?
tmp <- CreationDate_range_df %>% 
  select(CreationDate,ClosedDate,Body) %>% 
  distinct()

n_total_questions <- length(unique(tmp$Body))

# what percentage of these questions were marked as closed?
tmp <- tmp %>% 
  filter(!is.na(ClosedDate)) %>% 
  mutate(n_closed_questions = n()) %>% 
  slice(1)

percent_closed <- (tmp$n_closed_questions / n_total_questions) * 100

# what is the average duration between CreationDate and [LastEditDate | LastActivityDate | ClosedDate]?
#as.numeric(difftime(creation_date_end,creation_date_start, units = "days"))
tmp <- CreationDate_range_df %>% 
  select(CreationDate,ClosedDate,LastEditDate,LastActivityDate,ClosedDate) %>% 
  distinct() %>% 
  mutate(
    diff_LastEditDate = as.numeric(difftime(LastEditDate, CreationDate, units = "days")),
    diff_LastActivityDate = as.numeric(difftime(LastActivityDate, CreationDate, units = "days")),
    diff_ClosedDate = as.numeric(difftime(ClosedDate, CreationDate, units = "days"))
  ) %>% 
  select(diff_LastEditDate,diff_LastActivityDate,diff_ClosedDate) %>% 
  distinct()

avg_diff_LastEditDate <- tmp %>% 
  filter(!is.na(diff_LastEditDate)) %>% 
  select(diff_LastEditDate) %>% 
  summarise(mean(diff_LastEditDate))
avg_diff_LastEditDate <- avg_diff_LastEditDate[[1]]

avg_diff_LastActivityDate <- tmp %>% 
  filter(!is.na(diff_LastActivityDate)) %>% 
  select(diff_LastActivityDate) %>% 
  summarise(mean(diff_LastActivityDate))
avg_diff_LastActivityDate <- avg_diff_LastActivityDate[[1]]

avg_diff_ClosedDate <- tmp %>% 
  filter(!is.na(diff_ClosedDate)) %>% 
  select(diff_ClosedDate) %>% 
  summarise(mean(diff_ClosedDate))
avg_diff_ClosedDate <- avg_diff_ClosedDate[[1]]

# what is the average score of questions for a chosen tag?
# what is the average score of questions in a time range?
tmp <- CreationDate_range_df %>% 
  select(IndivTags,Score) %>% 
  distinct() %>% 
  group_by(IndivTags) %>% 
  mutate(TotalScore = sum(Score)) %>% 
  mutate(AvgScore = mean(Score)) %>% 
  select(-Score) %>% distinct()
  
# how does score compare to ViewCount, CommentCount and FavoriteCount?
# how many distinct users (OwnerId) have asked questions for a certain tag?
# rank users (OwnerId) based on score, ViewCount, CommentCount and FavoriteCount




















# # frequency bar chart of top N tags
# tags_by_count <- df %>% 
#   group_by(IndivTags) %>% 
#   summarize(num_tags=n()) %>% 
#   arrange(desc(num_tags)) 
# 
# top_n_tags_by_count <- tags_by_count[1:n,]
# 
# plot_top_n_tags_by_count(top_n_tags_by_count, top_n_tags_by_count$IndivTags, 
#                          top_n_tags_by_count$num_tags, dataset_name,n)
# 
# # bar plot of top tag of each year
# top_tag_per_year <- df %>% 
#   group_by(year,IndivTags) %>% 
#   summarize(num_tags=n()) %>% 
#   arrange(year,desc(num_tags)) %>% 
#   slice(1)
# 
# plot_top_tag_per_year(top_tag_per_year, top_tag_per_year$year,
#                       top_tag_per_year$num_tags, top_tag_per_year$IndivTags,
#                       dataset_name,n)
# 
# # line plot of top n tags over time
# tags_by_year <- df %>% 
#   group_by(year,IndivTags) %>% 
#   summarize(num_tags=n()) %>% 
#   arrange(year,desc(num_tags))
# 
# top_n_tags_by_year <- tags_by_year %>% 
#   filter(IndivTags %in% top_n_tags_by_count$IndivTags)
# 
# plot_top_n_tags_by_year(top_n_tags_by_year,top_n_tags_by_year$year, top_n_tags_by_year$num_tags,
#                         top_n_tags_by_year$IndivTags, top_n_tags_by_year$IndivTags,
#                         dataset_name,n)
  


