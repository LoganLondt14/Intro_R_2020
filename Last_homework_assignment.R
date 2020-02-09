# Last homework assignment 
# Tidy data
# Author: Logan Londt 



#Section 1 
# 1. Loading built in dataset, BOD 

Bod_data <- datasets::BOD

# Answer: C. BOD is tidy: each row is an observation with two values (time and demand)

# 2. Analyzing which of the following built-in datasets are tidy

BJsales_data <- datasets::BJsales
#Data is tidy , dataset only contains a varable with its own column 

EuStockMarkets_data <- datasets::EuStockMarkets
#Data is tidy , each stock market has a column 

DNase_data <- datasets::DNase
#Data is tidy , each variable has a column

Formaldehyde_data <- datasets::Formaldehyde
#Data is tidy , each variable has a column

Orange_data <- datasets::Orange
#Data is tidy , each variable has a column

UCBAdmissions_data <- datasets::UCBAdmissions
#Data is tidy , each variable has a column


#Section 2 

library(tidyverse)
library(dplyr)
library(dslabs)

# Loading the data , Murder dataset 

data("murders")

#exploring the dataset 

head(murders, 10) #viewing the first 10 rows of dataset
tail(murders) #viewing the last 6 rows of dataset 
glimpse(murders) #overview of dataset 
names(murders) #viewing the column names 

#paragraph on murder dataset 

#There are 51 observations and 5 variables in the dataset , murders. 
#The variables include  state , abrreviation, region , population as well as the associated deaths per state
#Each variable has its own column , making the dataset tidy. 
#This makes it easy to compare certain varibales such as the state, population as well as murders and draw conclusions from these findings. 

#adding a column showing populations in millions

murders <- mutate(murders, population_in_millions = population / 10^6)

#Writing code that ony shows states and population size 

murders_2 <- murders %>% # Tell R which dataframe we are using and making a file for the two variables 
  select(state, population)

#removing rows (Florida)

murders_3 <- murders %>%
  filter(state != "Florida")

#it took me a while to realize the difference between == and =!

#Create a new data frame called no_south that removes states from the South region. How many states are in this
#category?

no_south <-  murders %>%
  filter(region != "South")

#Therefore, there are 34 states 

# (%in%) Using this create a new data frame only showing the data for New York and
#Texas

murders_4 <- murders %>% 
  filter(state %in% c("New York", "Texas")) 

#C stands for combine here to combine the 2 rows of data
#The %in% is for putting things together 

#Calculate the population size of the South and West regionally

SW_total_pop_size <- murders %>%
  filter(region %in% c("South", "West")) %>% 
  group_by(region) %>% #specfic rows just for south and west
  summarise(total_pop_size = sum(population)) #adds a column for sum pop across both regions
           
#Create a new data frame with only the population size of the Northeast region

NE_pop_size <- murders %>%
  filter(region %in% c("Northeast")) %>% 
  group_by(region) %>%
  summarise(total_pop_size = sum(population))

#Create two plots of your choice and explain visible trends

library(ggplot2)
 #plot between pop in millions and abb 

ggplot(data = murders, aes(x = abb, y = population_in_millions)) + #inputting the murder dataset
  geom_point() + #creating a point graph 
  geom_line(aes(group = abb))


  #plot between abb and murders 
ggplot(data = murders, aes(x = abb, y = total)) + #inputting the murder dataset
  geom_point() + 
  labs(x = "states(abb)", y = "murders") + #creating a point graph 
  geom_line(aes(group = abb))

#Compare with population size of the South with the population size of the West

bar_1 <- ggplot(data = SW_total_pop_size, aes(x = region, y = total_pop_size)) +
  geom_bar(stat = "identity") +
  labs(X = "Region", y = "total population size")
bar_1

#here you can clearly see the difference between South and West region
#the population size of the south is much more than the west

#Create a new data frame where the total >20 but <100 and to exclude the value 120

Filter_20_100 <- murders %>% 
  filter(total > 20, total < 100) #didnt include the 120 because we are already excluding 120

#Create an object, containing from 10th to 24th row and 26th row. Hint: consider using the slice() function.

object_1 <- murders %>% 
  slice(10:24, 26)

#Use as_tibble to convert the murders data table into a tibble and save it in an object called murders_tibble.

murders_tibble_1 <- as_tibble(murders)

#Fall under dplyr package 
#Use the group_by function to convert murders into a tibble that is grouped by region.

group_tibble_<- murders_tibble_1 %>% 
  group_by(region)

#Write tidyverse code that is equivalent to this code:

murders_tibble_2<- as.tibble(murders)

#falls under tibble which is in the tidyverse package 


####Section 3 --------

library(dplyr)
library(dslabs)
data(heights)

#Write a paragraph describing the heights dataset

#Heights dataset includes heights and sex reported by students in an class survey.
#Each variable has a column, making it tidy. 
#Because of this, the data is easy to compare the differences and similarities across the different sexes


#Explore the datasets using the various exploring functions. 

glimpse(heights)
head(heights)
tail(heights) 
names(heights)
summary(heights)
filter(heights, sex == "Female")
filter(heights, sex == "Male") 

#Determine the average and standard deviation for males and females. Then calculate the median, minimum and maximum values.


#female
female_group_1 <- heights %>% 
  filter(sex == "Female")

female_group_2 <-summary(female_group_1)

#male 
male_group_1 <- heights %>% 
  filter(sex == "Male")

male_group_2 <-summary(male_group_1)

#1 way to do it , only this way puts the results under values , meaning you would have to convert it into a dataframe 

#female and male
summary_group <- heights %>% 
  group_by(sex) %>% 
  summarise(mean_height = mean(height),
             sd_height = sd(height), 
             min_height = min(height), 
             max_height = max(height), 
             med_height = median(height))

#simpler way of doing it while including both sexes instead of doing it separately 

#SECTION 4----------
#CREATING VECTORS 

x <-  c( 1, 6, 21, 19 , NA, 73, NA)
y <-  c(NA, NA, 3, NA, 13, 24, NA)


#a) Count the number of elements are missing in both x and y

sum(is.na(x)) #Total NA in dataset X 


sum(is.na(y)) #Total NA in dataset y 


#b) Transform the code, used above (a), into a function

Stupid_function <- function(sf) {sum(is.na(sf))} 

#Originally I used the abb of stupid function (sf) and x in the formula and that didnt work. 
#I now substituted Sf in the formula as well in order to use it for ALL datasets 


#c) Create three new vectors and test the function created in (b)

zz <- c(14, 56, NA, 76, 19, NA, NA) #Creating datasetS
yy <- c(12, 15, NA, 63, NA, 23, 45)
jj <- c(10, NA, 25, 19, 21, 96, 36)

#counting the number of elements missing using the new function 

Stupid_function(zz)

Stupid_function(yy)

Stupid_function(jj)

#Section 5-------------

Seasonal_data <- data.frame(year = c(2015, 2016, 2017, 2018),
                            winter = c(41, 39, 47, 40),
                            spring = c(41, 46, 57, 45),
                            summer = c(75, 52, 85, 66),
                            autumn = c(57, 66, 52, 56))

#Using the data above, design an hypothesis

#The temperatures will be at its highest in the Summer seasons and at its lowest in the Winter season


#Tidying the data , making it easier to plot----

SD_tidy <- Seasonal_data %>% 
  gather(winter, spring, summer, autumn, key = "Season", value = "Temp")

#The aim here was to gather winter-summer and place it under 1 variable , "season" 
#the corresponding values were then placed under another variable, "temp"
#This made it easier to plot and easier to compare 

#create two plots and write a paragraph discussing your findings

#Line graph 
ggplot(data = SD_tidy, aes(x = year, y = Temp, colour = Season)) + 
  geom_point() +
  geom_line(aes(group = Season)) #linking this with a line for each season 

#Boxplot
box_1 <- ggplot(data = SD_tidy, aes(x = year, y = Temp)) +
  geom_boxplot(aes(fill = Season)) +
  labs(x = "Year", y = "Temperature") 
box_1


#In both plots, it is evident that Winter has the lowest temperatures across all seasons
#Although in 2015, the temoperatures for Winter and Spring are the same 
#Summer temperatures in 2016 are lower that Autumn in 2016 
#Therefore , the temperatures were the lowest in the winter seasons and the temperatures were the highest for the msot parts in Summer
 
cats_data <- tibble(cats = c("A", "B", "C"),
                   position = c("1-2-3", "3-1-2", "2-3-1"),
                   minutes = c(3, 3, 3),
                   seconds = c(12, 44, 15))
cats_data


#Using the seperate() function split the position column into new three columns. 
#The new column names will be ,first_place, second_place and third_place.

#separating variables 

cats_tidy <- cats_data %>%
  separate(col = position, into = c("first_place", "second_place", "third_place"), sep = "-") %>% 
  unite(minutes, seconds, col = "total_time (min)")

#I want to sort all the 1s under first place , 2s under secon and 3s under third
Attempt_1 <- cats_data %>% 
  sort.default(cats_data, position, decreasing = TRUE)

#Error i got kept saying i needed to use a vector and not a class matrix

Attempt_2 <- cats_data %>% 
 sort.int(cats_data, partial = position, na.last = NA, decreasing = FALSE, 
          method = c("auto", "shell", "quick", "radix"), index.return = FALSE)
#Kept saying position not found 

Attempt_3 <- cats_tidy %>% 
arrange(desc("third_place", "second_place", "first_place"))
# I gave up after this 

#Section 6-----------


#loading untidy dataset

acme <- read_csv("data/acme.csv")

acme_1 <- acme %>% 
 separate(col = month, into = c("month", "year"), sep = "/") #separates the year from the month 

acme_2 <- acme %>% 
arrange(desc(month)) #arranges month column in descending order (recent to oldest)

acme_3 <- acme_1 %>% 
  filter(year %in% c("86", "87")) %>% 
  group_by(year) %>% #groups these years together 
  summarise(av_market = mean(market)) #creates an avergae for the markest across these years 

acme_4 <- acme_1 %>% 
  select(year, market, acme) #Only views the columns selected 

#using seasonal dataset ------

SD_tidy <- Seasonal_data %>% 
  gather(winter, spring, summer, autumn, key = "Season", value = "Temp")

Seasonal_data_conversion <- SD_tidy %>% 
  mutate(kelvin = Temp + 273.15)

#Still confused on how to use spread and join function 
#I attempted the functions but the datasets just ended up looking bad 
#I didnt want to submit crap so i removed those attempts





