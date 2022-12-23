library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

raw <- read_csv("data/astronomy.csv")
head(raw)

df <- raw %>% 
  select(c(CreationDate,Score,AnswerCount,CommentCount,Tags)) %>% 
  distinct() %>% 
  filter(!is.na(Tags)) %>% 
  mutate(year = substring(CreationDate,1,4)) %>% 
  mutate(IndivTags = str_split(Tags,">")) %>% 
  unnest(IndivTags) %>% 
  filter(IndivTags != "") %>% 
  mutate(IndivTags = str_replace(IndivTags,"<","")) %>% 
  distinct()

# creating analytical dataframes
top_tag_per_year <- df %>% 
  group_by(year,IndivTags) %>% 
  summarize(num_tags=n()) %>% 
  arrange(year,desc(num_tags)) %>% 
  slice(1)

tags_by_year <- df %>% 
  group_by(year,IndivTags) %>% 
  summarize(num_tags=n()) %>% 
  arrange(year,desc(num_tags))

tags_by_count <- df %>% 
  group_by(IndivTags) %>% 
  summarize(num_tags=n()) %>% 
  arrange(desc(num_tags)) 

n <- 10
top_n_tags_by_count <- tags_by_count[1:n,]

# frequency bar chart of top N tags
ggplot(top_n_tags_by_count, aes(x = reorder(IndivTags, -num_tags), y = num_tags)) + 
  geom_col() + 
  labs(title = paste0("Top ",n," Tags"), x = "tag", y = "frequency") +
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

# time line chart of year vs tags
ggplot(top_tag_per_year, aes(x = year, y = num_tags, color = IndivTags)) + 
  geom_point()


