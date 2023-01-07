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

df <- clean_df(raw)

creation_date_start <- "2021-01-01"
creation_date_end <- "2021-07-31"
dataset_date <- "2022-12-07" # this is the date the dataset was downloaded
chosen_tag <- "observational-astronomy"

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
  geom_smooth(method="lm", se=FALSE) +
  labs(title = paste0("Number of Questions Asked on ",dataset_name," Between ",creation_date_start," - ", creation_date_end), 
       x = "date", y = "number of questions",
       subtitle = paste0(n_closed_questions," / ",n_total_questions," questions closed (",percent_closed,"%)"))
print(plot)

# what is the average duration between CreationDate and [LastEditDate | LastActivityDate | ClosedDate]?
tmp <- CreationDate_range_df %>% 
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




plot <- ggplot(tmp, aes(x = CreationDate)) + 
  geom_line(aes(y = avg_diff_LastEditDate, colour="CreationDate - LastEditDate")) +
  geom_line(aes(y = avg_diff_LastActivityDate, colour="CreationDate - LastActivityDate")) +
  geom_line(aes(y = avg_diff_ClosedDate, colour="CreationDate - ClosedDate")) +
  scale_color_manual(name = "Avg. Difference Between:",
                     values = c("CreationDate - LastEditDate" = "dodgerblue2",
                                "CreationDate - LastActivityDate" = "seagreen3",
                                "CreationDate - ClosedDate" = "indianred2")) +
  labs(title = paste0("Average Difference Between Creation Date and other dates on ",
                      dataset_name," Between ",creation_date_start," - ", creation_date_end),
       x = "date", y = "average number of days")
print(plot)

# what is the average score of questions for a chosen tag?
# what is the average score of questions in a time range?
tmp <- CreationDate_range_df %>% 
  select(IndivTags,Score) %>% 
  distinct() %>% 
  group_by(IndivTags) %>% 
  mutate(
    TotalScore = sum(Score),
    AvgScore = mean(Score)
    ) %>% 
  ungroup() %>% 
  select(-Score) %>% distinct() %>% 
  arrange(desc(TotalScore)) 

tmp <- tmp[1:n,]

plot <- ggplot(tmp, aes(x = IndivTags)) + 
  geom_line(aes(y = AvgScore, group = 1), color="red") +
  geom_line(aes(y = TotalScore, group = 1), color="green") +
  labs(title = paste0("Total Score and Average Score for Questions by Tags on ",dataset_name," Between ",creation_date_start," - ", creation_date_end),
       x = "date", y = "average number of days")
print(plot)
  
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
  ) %>% 
  ungroup()

tmp1 <- tmp %>% 
  select(CreationDate,ratio_ViewCount,ratio_CommentCount) %>% 
  distinct()

plot <- ggplot(tmp1, aes(x = CreationDate)) + 
  # geom_line(aes(y = ratio_ViewCount, group = 1), color="red") +
  geom_line(aes(y = ratio_CommentCount, group = 1), color="green") +
  labs(title = paste0("Total Score and Average Score for Questions by Tags on ",dataset_name," Between ",creation_date_start," - ", creation_date_end),
       x = "date", y = "average number of days")
print(plot)

tmp2 <- tmp %>% 
  select(IndivTags,ratio_ViewCount,ratio_CommentCount) %>% 
  distinct()

tmp2 <- tmp2[1:n,]

plot <- ggplot(tmp2, aes(x = IndivTags)) + 
  geom_line(aes(y = ratio_ViewCount, group = 1), color="red") +
  geom_point(aes(y = ratio_CommentCount, group = 1), color="green") +
  labs(title = paste0("Tags vs Ratio of Score to ViewCount and CommentCount for ",dataset_name," Between ",creation_date_start," - ", creation_date_end),
       x = "tag", y = "ratio")
print(plot)

# how many distinct users (OwnerUserId) have asked questions for a certain tag?
# rank users (OwnerUserId) based on score, ViewCount, CommentCount and FavoriteCount
tmp <- CreationDate_range_df %>% 
  select(CreationDate,OwnerUserId,Score,ViewCount,CommentCount,IndivTags) %>% 
  distinct() %>% 
  filter(IndivTags == chosen_tag) %>% 
  group_by(OwnerUserId) %>% 
  mutate(
    OwnerUserId = toString(OwnerUserId),
    n_questions = n(),
    total_score = sum(Score),
    total_views = sum(ViewCount),
    total_comments = sum(CommentCount)
  ) %>% 
  ungroup() %>% 
  select(OwnerUserId,n_questions,total_score,total_comments) %>% 
  distinct()

tmp3 <- pivot_longer(tmp, cols=c('n_questions', 'total_score', 'total_comments'), 
             names_to='variable', 
             values_to="value")[16:28,]

ggplot(tmp3, aes(x=OwnerUserId, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  # facet_wrap(~ variable)
  labs(title = paste0("Tags vs Ratio of Score to ViewCount and CommentCount for ",dataset_name," Between ",creation_date_start," - ", creation_date_end),
       x = "OwnerUserId", y = "number")

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
  


