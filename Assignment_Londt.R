###Exercise 2---------------------------
###LOGAN LONDT 

#Loading packages 
library(tidyverse)
library(dplyr)
library(RColorBrewer)
#QUESTION 1----------------------------
#Loading the data
load("data/SACTNmonthly_v4.0.RData")

#viewing the data 
head(SACTNmonthly_v4.0)
glimpse(SACTNmonthly_v4.0)
tail(SACTNmonthly_v4.0)
colnames(SACTNmonthly_v4.0)


#Filtering just for KZN 
monthly_2 <- SACTNmonthly_v4.0 %>% 
  filter(src == "KZNSB")

#Calculating the yearly mean 
monthly_3 <- monthly_2 %>% 
  separate(col = date, into = c("year", "month", "day"), sep = "-") %>% 
  group_by(site, year) %>% 
  mutate(mean_temp = temp) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE))
monthly_3


#plotting monthly 2
na.omit(monthly_3)

#changing to numeric 
monthly_3$year <-  as.numeric(monthly_3$year)

#plotting 

plot1 <- ggplot(monthly_3, aes(x = year, y = mean_temp)) + 
  #geom_point() + 
  geom_line(aes(y = monthly_3$mean_temp) , colour = "blue") +
  scale_x_continuous(breaks = seq(1980, 2000, 20)) +
  labs(x = "Year", y = "Temperature (°C)")  #labels for the x and y axis and differentiates between sites 
  
#factewrapping 
plot1 + facet_wrap(.~site,nrow = 5)


#QUESTION 2--------------------------------

#loading packages 
library(ggpubr)

#Loading the data
laminaria <- read_delim("data/laminaria.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)

#viewing the data 
head(laminaria)
glimpse(laminaria)
tail(laminaria)
colnames(laminaria)


#selecting columns of interest

lam_1 <-  laminaria %>% 
  select(site, blade_length, blade_weight, region) %>% 
  filter(region == "FB") 

#plotting 
#laminaria$year <-  as.numeric(laminaria$year)


plot_lam_A <- ggplot(lam_1, aes(x = blade_length, y = blade_weight)) + 
  geom_point(aes(colour = site)) + 
  geom_line(aes(colour = site, group = 1)) +
  scale_colour_brewer(palette = "Accent")+
  scale_x_continuous(breaks = seq(100, 175, 25)) +
  scale_y_discrete(breaks = seq(0, 3, 1)) + 
  labs(x = "Blade length (cm)", y = "Blade mass(kg)")
  
#Facetting  
A <- plot_lam_A + facet_wrap(.~site)

# Question 3 --------------------------------------------------------------

#loading package 
library(ggplot2)

#loading dataset 
TG <- datasets::ToothGrowth

TG_use <- TG %>% 
  group_by(supp, dose) %>% 
  summarise(mean_len = mean(len),
            sd_len = sd(len))

TG_use$dose <- as.numeric(TG_use$dose)

ggplot(TG_use, aes(x=dose, y=mean_len, fill=supp)) + 
  geom_bar(stat="identity", color="black", size = 1,
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_len-sd_len, 
                    ymax=mean_len+sd_len), width=.2,
                position=position_dodge(.5), colour = "black", size = 1) +
  labs(x = "Dose (mg/d)", y = "Tooth length (mm)")

