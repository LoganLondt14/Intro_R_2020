# Summarize stats 
# 29th Jan 2020
# Logan Londt 


# Loading packages 
library(tidyverse)


# Loading data 

laminaria <-read_csv("data/laminaria.csv") 
#doesnt work 

library(readr)
laminaria <- read_delim("data/laminaria.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)
#this code works
lam_sub <- laminaria %>% # Tell R which dataframe we are using and making a file for the two variables 
  select(site, total_length) # Select only specific columns

laminaria %>% # Chose the dataframe
  summarise(avg_bld_wdt = mean(blade_length)) # Calculate mean blade length

laminaria %>% # Tell R that we want to use the 'laminaria' dataframe
  summarise(avg_stp_ln = mean(total_length), # Create a summary of the mean of the total lengths
            sd_stp_ln = sd(total_length)) # Create a summary of the sd of the total lengths

#using the group by function to get summarized data per site 
laminaria %>%
  group_by(site) %>%   # Tell R that we want to use the 'laminaria' dataframe
  summarise(avg_stp_ln = mean(total_length), # Create a summary of the mean of the total lengths
            sd_stp_ln = sd(total_length),
            var_stp_ln = var(total_length),
            median_stp_ln_ = median(total_length))

## Plotting - function ggplot
ggplot(data = laminaria, aes(x = stipe_mass, y = stipe_length)) +
  geom_point(shape = 21, colour = "salmon", fill = "white") +
  labs(x = "Stipe mass (kg)", y = "Stipe length (cm)")

#the x axis is all messed up here 
#have a look at the scale x continous function to fix the x axis 


#Plotting 


ChickWeight <- datasets::ChickWeight

# Create a basic figure showing the relationship with time and weight 
ggplot(data = ChickWeight, aes(x = Time, y = weight)) + #inputting the chickweight dataset
  geom_point() + #creating a point graph 
  geom_line(aes(group = Chick)) #linking this with a line for each chick 

# creating figures containing colours to differntiate betweeen diets
ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) + #same as before , only now we are adding colour to the diets
  geom_point() + #creating a point graph
  geom_line(aes(group = Chick)) #linking this with a line for each chick

# Changing the texture of the line to smooth 
ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm") + #linear model showing line that has the best relationship  
  theme_bw() #adding extra graphics as pasrt of playing around 

#playing around with the colours
ggplot(data = ChickWeight, aes(x = Time, y = weight)) +
  geom_point(colour = "blue") +
  geom_line(aes(group = Chick, colour = "blue"))


#graph with legends indicating difference in weights in relation to diets 
ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point(aes(size = weight)) + # size function to change it into different sizes
  geom_smooth(method = "lm", size = 1.2)

#changing legend position and adding units to variables 
ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Days", y = "Mass (g)", colour = "diet type") + # Change the labels
  theme(legend.position = "bottom") # Change the legend position

#facetting
library(tidyverse)
library(ggpubr)
library(ggplot2)

#Making the panel 
ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm") + # Note the `+` sign here
  facet_wrap(~Diet, ncol = 2) + # This is the line that creates the facets
  labs(x = "Days", y = "Mass (g)")

#making data smaller and easier for plotting 

ChickLast <- ChickWeight %>% #making a new set 
  filter(Time == 21)

#making a line graph 
line_1 <- ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_line(aes(group = Chick)) +
  labs(x = "Days", y = "Mass (g)")
line_1 


lm_1 <- ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) + #making an object
  geom_point() +
  geom_smooth(method = "gam") +
  labs(x = "Days", y = "Mass (g)")

#first run this and then line 116

lm_1


# Running a histogram

histogram_1 <- ggplot(data = ChickLast, aes(x = weight)) +
  geom_histogram(aes(fill = Diet), position = "dodge", binwidth = 100) +
  labs(x = "Final Mass (g)", y = "Count")

histogram_1


#Running a boxplot

box_1 <- ggplot(data = ChickLast, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet)) +
  labs(x = "Diet", y = "Final Mass (g)") 

box_1

#No for the grand finale , Putting it all together 
ggarrange(line_1, lm_1, histogram_1, box_1,
          ncol = 2, nrow = 2, # Set number of rows and columns
          labels = c("A", "B", "C", "D"), # Label each figure
          common.legend = TRUE) # Create common legend






