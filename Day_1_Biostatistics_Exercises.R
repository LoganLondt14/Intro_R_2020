# Day 1 Biostatistics 
# Exercises 
# Author: Logan Londt 



#Loading packages
library(tidyverse)
library(skimr) #FOR SKIM FUNC
library(DataExplorer)#FOR DATA EXPLORER FUNC
library(scales)#FOR DATA EXPLORER FUNC
library(dplyr) #FOR GLIMPSE FUNC
library(Hmisc) #FOR DESCRIBE, PLOT NUM, FREQ, DF STAT AND PROFILING NUMBER 
library(funModeling) #FOR DESCRIBE, PLOT NUM, FREQ, DF STAT AND PROFILING NUMBER 

#Loading dataset 
chicks <- as_tibble(ChickWeight)

#Exploring the data 

head(chicks) # viewing the FIRST 6 rows of dataset

head(chicks, n=10)# First 10 rows of dataset

head(chicks, n= -10) # All rows but the last 10

tail(chicks) #v iewing the LAST  6 rows of dataset

tail(chicks, n=10) # Last 10 rows

tail(chicks, n= -10) # All rows but the first 10

glimpse(chicks) # overview of dataset

view(chicks) # opening dataset , this will appear in your console and not your environment

names(chicks) #viewing the column names 

summary(chicks) # Provides basic descriptive statistics and frequencies.

edit(chicks) # Open data editor

str(chicks) # Provides the structure of the dataset

chicks[1:10, ] # First 10 rows

chicks[1:10,1:4] # First 10 rows of data of the first 4 variables

colnames(chicks) #Gives the names of the columns in dataset 

length(chicks) #The length of an object can be viewed using the length function

skim(chicks) #It displays most of the numerical attributes from summary, but it also displays missing values

DataExplorer::create_report(chicks) #pull a full data profile of your data frame

dim(chicks) #Displays a preview of all columns as a row so that it's very easy to take in.

plot(chicks) #Produces a matrix of scatterplots which is a correlation matrix of all the columns. 

nrow(chicks) #Give the number of rows 

ncol(chicks) #Gives number of columns

df_status(chicks) #Get a summary for the given data frame

freq(chicks) #Frequency table for categorical variables

profiling_num(chicks) #Metric table with many indicators for all numerical variables, automatically skipping the non-numerical variables

plot_num(chicks) #Plotting numerical data

describe(chicks) #Describes each variable in dataset 



#### Exercise 3.6.1

#using the summary function 

summary(chicks)

#Using summarize func to get the summary of weight in dataset Chicks

chicks %>% 
  summarise(mean_wt = round(mean(weight, na.rm = TRUE), 2),
            med_wt = median(weight, na.rm = TRUE), 
            min_wt = min(weight),
            qrt1_wt = quantile(weight, p = 0.25),
            med_wt = median(weight),
            qrt3_wt = median(weight, p = 0.75),
            max_wt = max(weight))
















            
