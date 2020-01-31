# Day 4 
# tidy data
# Date: 31 January 2020
# Logan Londt 


#loading packages 
library(tidyverse)
library(lubridate)

#Loading the data
load("data/SACTN_mangled.RData") #R data 

#creating a plot looking at SACTN1-------------

ggplot(data = SACTN1, aes(x = date, y = temp)) + #looking at data sactn1 and explaining which variable is on the x and y axis 
  geom_line(aes(colour = site, group = paste0(site, src))) +
  labs(x = "", y = "Temperature (°C)", colour = "Site") + #labels for the x and y axis and differentiates between sites 
  theme_bw() #Theme used to display graph

#creating tidy data looking at SACTN2-------------

SACTN2_tidy <- SACTN2 %>%
  gather(DEA, KZNSB, SAWS, key = "src", value = "temp") # this helps to make data tidy and combines 3 columns into 1 column

#creating tidy data looking at SACTN2-------------

SACTN3_tidy1 <- SACTN3 %>% 
  spread(key = var, value = val)

#separating variables 

SACTN4a_tidy <- SACTN4a %>% 
  separate(col = index, into = c("site", "src"), sep = "/ ")

#separating the year column into its components

SACTN_tidy2 <- SACTN4a %>% 
  separate(col = index, into = c("site", "src"), sep = "/ ") %>% 
  mutate(day = lubridate::day(date),
         month = lubridate::month(date),
         year = lubridate::year(date))

#Doing the opposite, putting the month year and day together 

SACTN4b_tidy <- SACTN4b %>% 
  unite(year, month, day, col = "date", sep = "-") 



