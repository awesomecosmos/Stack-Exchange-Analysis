library(readr)
library(dplyr)
library(tidyr)

df <- read_csv("data/astronomy.csv")
head(df)

df2 <- df %>% 
  select(c(CreationDate,Score,AnswerCount,CommentCount,Tags)) %>% 
  distinct() %>% 
  filter(!is.na(Tags))
