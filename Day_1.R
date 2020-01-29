# Day_1
# Laminaria collected along the Cape Peninsula
# Various data manipulations, analyses and graphs
# Logan Londt 
# 28/01/2020


# Loading packages
library(tidyverse)

laminaria <- read_csv("data/laminaria.csv")


## viewing data 

head(laminaria) #viewing the first 6 rows of dataset
tail(laminaria) #viewing the last 6 rows of dataset 
glimpse(laminaria) #overview of dataset 
view(laminaria) #opening dataset 
names(laminaria) #viewing the column names 

#4.3.3 in textbook
#tidyverse

lam_sub <- laminaria %>% # Tell R which dataframe we are using and making a file for the two variables 
  select(site, total_length) # Select only specific columns

lam_slice <- laminaria %>% 
  select(site, total_length) %>% # Select specific columns first
  slice(56:78)

lam_kom <- laminaria %>%
  filter(site == "Kommetjie")

laminaria %>% # Tell R which dataset to use
  filter(site == "Kommetjie") %>% # Filter out only records from Kommetjie
  nrow() # Count the number of remaining rows














