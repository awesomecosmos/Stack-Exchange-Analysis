# This is a script to clean the raw CSVs and save them as RdS objects
# By reading RdS objects into the app, this helps in faster loading times

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(here)

source("src/utils.R")

files_and_names <- dataset_reader(path = here("data/"))
all_files <- files_and_names[[1]]
all_dataset_names <- files_and_names[[2]]
saveRDS(all_dataset_names, "data/all_dataset_names.rds")

for (index in 1:length(files_and_names[[1]])){
  filename <- str_replace(files_and_names[[1]][index],".csv","")
  raw <- read_csv(here("data", files_and_names[[1]][index])) 
  df <- clean_df(raw) %>% 
    mutate(dataset = files_and_names[[2]][index])
  saveRDS(df, paste0("data/rds/",filename,".rds"))
}