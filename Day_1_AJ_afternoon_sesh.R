# Day 1 
# Purpose: To demonstrate some principles of data analysis
# Date 28 January 2020
# Logan Londt 


#Load packages 
library(tidyverse)
library(lubridate)

#read in the data 

temps <- read_csv("data/SACTN_day_1.csv") 
  # look at the data 
head(temps)
tail(temps, 12)
glimpse(temps)


# summarize the data 
mean_temps <- temps %>%
  mutate (mon = month(date)) %>%
  group_by(site, mon) %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE), 
            sd_temp = sd(temp, na.rm = TRUE))

# creating a graph 
ggplot(data = mean_temps, aes(x = mon, y = mean_temp)) +
  geom_point(aes(col = site))
    
        
  