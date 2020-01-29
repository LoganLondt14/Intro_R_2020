# Day 1 
# Exercise: Practice functions covered in class 
# 28 Jan 2020
#Logan Londt 



#Loading packages 
library(tidyverse)

library(readr)
laminaria <- read_delim("data/laminaria.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)

#So R was only reading my file as 1 variable instead of 12 .
#I attempted to change a few things using the excel file but nothing budged . 
#I imported the file using the right click option and i used the code preview in the bottome right corner .
#The read delim function its used to read files that are comma separated
#I think this function means it used the ; to separate the values and , for decimals

#selecting the site and total length column
lam_sub <- laminaria %>% # Tell R which dataframe we are using and making a file for the two variables 
  select(site, total_length) # specific columns

#Adding another variable, half_length 
 total_length_half <- lam_sub %>%
   mutate(total_length_half = total_length/2) %>% #Halving the total length
   na.omit(total_length_half) #removing any NA
 
#Slicing columns that are lower than 100
#Attempt 1
 total_length_half <- lam_sub %>%
    select(site, total_length_half) %>% # Select specific columns first
    slice(total_length_half < 100)
 
#Attempt 2
 Below_100 <- total_length_half %>%
    select(site, total_length_half) %>% # Select specific columns first
    slice(44,47,52,57,58,60,62,63,64,65,69,70,97,98,103,104,105,106,107,108,109,119)
#Got it right but it took FOREVER 
 
#Attempt 3 
 Below_100_length <- total_length_half[(total_length_half [,3]<100),]
 
 

#Attempt 1 average length (Kommetjie)
 Average_length <- lam_kom %>%
    select(site, total_length) %>%
    summarise(Average_length = mean(total_length, na.rm = TRUE))
 
#Attempt 1 min value (Kommetjie)
 Minimum_Kom <- lam_kom %>%
    select(site, total_length) %>%
    summarise(Minimum_Kom = min(total_length, na.rm = TRUE))
#Attempt 1 max value kommertjie
 Maximum_Kom <- lam_kom %>%
    select(site, total_length) %>%
    summarise(Maximum_Kom = max(total_length, na.rm = TRUE))
 
 #I am all out of time but im assumming if it works for 1 site it works for all
 
 
 
 
 
 
 

 

 
 
 
 
 