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
# what percentage of these questions were marked as closed?
tmp <- CreationDate_range_df %>% 
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

n_total_questions <- sum(tmp$n_questions_per_day)
n_closed_questions <- sum(tmp$question_closed_or_not)
percent_closed <- round((n_closed_questions / n_total_questions) * 100, 2)

plot <- ggplot(tmp, aes(x = CreationDate, y = n_questions_per_day)) + 
  geom_line() +
  labs(title = paste0("Number of Questions Asked on ",dataset_name," Between ",creation_date_start," - ", creation_date_end), 
       x = "date", y = "number of questions",
       subtitle = paste0(n_closed_questions," / ",n_total_questions," questions closed (",percent_closed,"%)"))
print(plot)

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
tmp <- CreationDate_range_df %>% 
  select(CreationDate,Score,ViewCount,CommentCount,IndivTags) %>% 
  distinct() %>% 
  group_by(CreationDate,IndivTags) %>% 
  mutate(
    ratio_ViewCount = round(Score / ViewCount, 3),
    ratio_CommentCount = case_when(
      CommentCount == 0 ~ 0,
      TRUE ~ round(Score / CommentCount, 3)
    )
  )

# how many distinct users (OwnerUserId) have asked questions for a certain tag?
# rank users (OwnerUserId) based on score, ViewCount, CommentCount and FavoriteCount
chosen_tag <- "observational-astronomy"

tmp <- CreationDate_range_df %>% 
  select(CreationDate,OwnerUserId,Score,ViewCount,CommentCount,IndivTags) %>% 
  distinct() %>% 
  filter(IndivTags == chosen_tag) %>% 
  group_by(OwnerUserId) %>% 
  mutate(
    n_questions = n(),
    total_score = sum(Score),
    total_views = sum(ViewCount),
    total_comments = sum(CommentCount)
  ) %>% 
  select(OwnerUserId,n_questions,total_score,total_views,total_comments) %>% 
  distinct()



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
  


