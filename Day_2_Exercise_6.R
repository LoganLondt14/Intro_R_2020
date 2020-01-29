# Exercise 6 
# 29 Jan 2020
# Logan Londt 

#####Objective
#Select a dataset and plot 2 plots from the datasetand exploring the functions


#Loading packages 
library(tidyverse)
library(ggpubr)
library(ggplot2)

#Plot 1

cars <- datasets::cars
#speed of cars and the distances taken to stop. 

#creating a scatterplot
ggplot(data = cars, aes(x = speed, y = dist)) + #inputting the dataset cars
  geom_point()


#Plot 2

sleep<- datasets::sleep #Selecting a built in dataset 
#Data represents the effect of 2 drugs aimed to increase the hours of sleep

#creating a line graph 
ggplot(data = sleep, aes(x = ID, y = extra)) + #inputting the sleep dataset
  geom_point() +
  geom_line(aes(group = group)) #linking this with a line for each group which in this case is the 2 drugs

#This plot is okay , but it doesnt show us which drug is which


###Fooling around with functions using plot 2

#Changing the colours of lines to differntiate betweeen drugs
ggplot(data = sleep, aes(x = ID, y = extra, colour = group)) + #same as before , only now we are adding colour to the different drug types
  geom_point() + #creating a point graph
  geom_line(aes(group = group)) #linking this with a line for each drug used represented by 'group'

#Changing the theme 
ggplot(data = sleep, aes(x = ID, y = extra, colour = group)) +
  geom_point() +
  geom_line(aes(group = group)) +
  theme_classic2() #using a differnt theme

#playing around with the colours
ggplot(data = sleep, aes(x = ID, y = extra, colour = group)) +
  geom_point(colour = "green") + #this changes the point colours to green
  geom_line(aes(group = group))

#graph with different point size 
ggplot(data = sleep, aes(x = ID, y = extra, colour = group)) +
  geom_point(aes(size = group)) + #Size function to change it into different sizes
  geom_smooth(method = "lm", size = 5.2)

#Changed this a few times but the only thing that chnages is the size under legend
#Besides colour, changing the size of the points could help differntiate betweent the differnt drugs
#I got an error message once i ran this code saying that using size would be a bad idea for discrete variables

#changing legend position and adding units to variables 
ggplot(data = sleep, aes(x = ID, y = extra, colour = group)) +
  geom_point() +
  geom_line(aes(group = group)) +
  labs(x = "student ID", y = "time (hours)", colour = "group") +
  theme(legend.position = "bottom")

#Time in hours represent the extra hours of sleep that students are getting in response to drug 1 and 2 

#Changing the geom function attempt 1

box_1 <-  ggplot(data = sleep, aes(x = ID, y = extra)) +
  geom_boxplot(aes(fill = group)) +
  labs(X = "Student ID", y = "time(hours)")

box_1
#im not sure why this wont work
#cant make data smaller because its already small enough ?

#Changing the geo function attempt 2 
histogram_1 <- ggplot(data = sleep, aes(x = ID)) +
  geom_histogram(aes(fill = group), poistion = "dodge", bindwidth = 100) +
  labs(X = "Student ID", y = "time(hours)")

histogram_1



#Things I struggled with..
#changing the legend name
#geom functions

















