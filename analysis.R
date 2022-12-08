library(readr)
library(dplyr)
library(tidyr)

df <- read_csv("data/astronomy.csv")
head(df)

df2 <- df %>% 
  select(c(CreationDate,Score,AnswerCount,CommentCount,Tags)) %>% 
  distinct() %>% 
  filter(!is.na(Tags))

# split Date col into dates
# separate Tags and make into Long df

# frequency bar chart of top N tags
# time line chart of year vs tags