library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

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