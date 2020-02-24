###### CHEAT SHEET ########
# Author: Logan Londt 
# Date: 2020/02/20 


# DAY 1 CODING -------------
# LOADING PACKAGES ---------
library(tidyverse) # This is followed by cntr + enter to activate the packages 

x <- read_csv("data/x") 
#This reads the CSV and the code written in the brackets is where the file is found


#Could also make use of importing the dataset from your files folder 
#In some cases, the data will be messed up 
#This is where you could separate the data via semi colon or comma etc 
#Once this is done , you will see a code appear in the bottom right corner 
#You can copy and paste this code and your data will appear in your environment


## VIEWING THE DATASET ------

head(x) #viewing the FIRST 6 rows of dataset
tail(x) #viewing the LAST  6 rows of dataset 
glimpse(x) #overview of dataset 
view(x) #opening dataset , this will appear in your console and not your environment
names(x) #viewing the column names 

#4.3.3 IN TEXTBOOK --------
# TIDYVERSE -------

#Taking out specific variables and turning it into a dataframe --------

x_ <- x %>% 
  select(var1, var2)

#Heres an example 
lam_sub <- laminaria %>% # making a new df, using the laminaria dataset and then,
  select(site, total_length) #selecting specific columns and placing it into new df

#The code reads from the dataset , make a new df called lam_sub 
#Using the laminaria dataset select variables site and total length and put these variables in new df

#Taking out specific rows from specific variables -------- 

x_slice <- x %>% 
  select(var1, var2) %>% 
  slice(56:78) 

#Heres an example
lam_slice <- laminaria %>% #making a new df and then,
  select(site, total_length) %>% # Selecting specific columns and then,
  slice(56:78) #take out these rows out and place it in a new df. 

#Helpful if you wanted to take a look at certain sites and then place those rows in a new df
#Taking out 1 site within the "site" variable -----------

x_ <- x %>%
  filter(variable == "name of specific row of interest")

#Heres an example 
lam_kom <- laminaria %>%
  filter(site == "Kommetjie")  #processes a data structure (usually a list) in some order to produce a new data structure containing exactly those elements of the original data

#only filters out all rows that relate to kommetjie 

#counting the number of rows for a particular site ------------
x_ <- x %>% 
  filter(variable == "name of specific row of interest") %>% 
  nrow()

#Heres an example
laminaria %>% # Tell R which dataset to use and then, 
  filter(site == "Kommetjie") %>% # Filter out only records from Kommetjie and then, 
  nrow() # Count the number of remaining rows for kommetjie

#SUMMARISING THE DATA AND CREATING A PLOT USING THE VARIABLES OF INTEREST ------------
#Loading packages --------
library(lubridate) # provides tools that make it easier to parse and manipulate dates

#read in the data 
temps <- read_csv("data/SACTN_day_1.csv") 

# summarizing the data  ------------  

mean_temps <- temps %>% #make a df called mean_temps and from the temps dataset , 
  mutate (mon = month(date)) %>% #make another column called mon showing only the month (this info is obtained from date column) and then 
  group_by(site, mon) %>% #group the new df by site and this new column, mon and then,
  summarise(mean_temp = mean(temp, na.rm = TRUE), #find the mean temp using the temp column and remove all NA's and then,  
            sd_temp = sd(temp, na.rm = TRUE)) #find the standard dev using the temp column and remove all NA'S

# creating a graph shwoing the realtionship between variables of interest -------------  

ggplot(data = mean_temps, aes(x = mon, y = mean_temp)) + #create a ggplot using the mean_temps df where mon is on the x axis and mean temps is on the y axis and then,  
  geom_point(aes(col = site)) #plot them as points and use color to differentiate between the sites. 


#DAY 2 CODING -----------------------
#Summarize stats 

# Loading packages 
library(tidyverse)

# Loading data 
laminaria <- read_delim("data/laminaria.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)


#Finding the mean,stand dev, var and median of a variable  -------------
x %>% #
  summarise(name_of_new_col = mean(var),
            name_of_new_col = sd(var),
            name_of_new_col = var(var),
            name_of_new_col = median(var))


#heres an example 
laminaria %>% # take this df and then , 
  summarise(avg_stp_ln = mean(total_length), # use the summarise function to find the mean of the blade length and name this column av_stp_ln
            sd_stp_ln = sd(total_length), # use the summarise function to find the std dev of the blade length and name this column sd_stp_ln 
            var_stp_ln = var(total_length),# use the summarise function to find the var of the blade length and name this column var_stp_ln 
            median_stp_ln_ = median(total_length)) # use the summarise function to find the median of the blade length and name this column med_stp_ln 

#Using the group by function ----------------
x %>%
  group_by(variable) %>%  
  summarise(name_of_new_col = mean(var),  
            name_of_new_col = sd(var),
            name_of_new_col = var(var),
            name_of_new_col = median(var))

#heres an example 
laminaria %>% # take this df and then, 
  group_by(site) %>%  #group the data by site and then , 
  summarise(avg_stp_ln = mean(total_length),  #repeated code from previous calculation
            sd_stp_ln = sd(total_length),
            var_stp_ln = var(total_length),
            median_stp_ln_ = median(total_length))

## Plotting - function ggplot -------------
ggplot(data = x, aes(x = var1, y = var2)) +
  geom_point(shape = 21, colour = "salmon", fill = "white") +
  labs(x = "var1", y = "var2")


#Heres an example 
ggplot(data = laminaria, aes(x = stipe_mass, y = stipe_length)) + #create a ggplot using the lam data and make the x axis the stp mass and the y axis the stp length and then, 
  geom_point(shape = 21, colour = "salmon", fill = "white") + #plot this using points , make the shape of point 21, using salmon and fill these points with the color white and then, 
  scale_x_continuous(breaks = seq(0, 5.6, 0.2)) + #attempted this crap and idk whats happening 
  labs(x = "Stipe mass (kg)", y = "Stipe length (cm)") #label the x axis stipe mass(kg) and the y axis stipe length (cm)


#PLOTTING-------------

ChickWeight <- datasets::ChickWeight
#using "datasets::" you are requesting the built in datasets in R 

#Create a basic figure showing the relationship with 3 variables  -----------
ggplot(data = x, aes(x = var1, y = var2)) + 
  geom_point() + 
  geom_line(aes(group = var3)) 


#Heres an example 
ggplot(data = ChickWeight, aes(x = Time, y = weight)) + #create a ggplot using chickweight data and make the x axis time and teh y axis weight and then, 
  geom_point() + #plot this using points and then, 
  geom_line(aes(group = Chick)) #link this with a line for each chick type 


#Creating figures containing more than 3 variables ----------------------
ggplot(data = x, aes(x = var1, y = var2, colour = var4)) + 
  geom_point() + 
  geom_line(aes(group = var3)) 

#Heres an example 
ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) + #Create a ggplot and make the x axis time and the y axis weight and use colour to differentiate between the different diets and then, 
  geom_point() + #plot this using points and then , 
  geom_line(aes(group = Chick)) #link this with a line for each chick

############# OR ################

ggplot(data = x, aes(x = var1, y = var2, colour = var3)) +
  geom_point(aes(size = var4)) + # size function to change it into different sizes
  geom_smooth(method = "lm", size = 1.2)

#Heres and example 
ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +#Create a ggplot and make the x axis time and the y axis weight and use colour to differentiate between the different diets and then, 
  geom_point(aes(size = weight)) + # plot this using point and  use size function to show the differences in weight amongst chicks and then, 
  geom_smooth(method = "lm", size = 1.2) #use geom smooth to create the smooth line of best fit showing the best relationship (linear model)

#Changing the texture of the line to smooth -----------------------
ggplot(data = x, aes(x = var1, y = var2, colour = var3)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  theme_bw() 

#Heres an example 
ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +#Create a ggplot and make the x axis time and the y axis weight and use colour to differentiate between the different diets and then, 
  geom_point() + #plot this using points and then, 
  geom_smooth(method = "lm") + #use geom smooth to create the smooth line of best fit showing the best relationship (linear model) and then ,   
  theme_bw() #make the them black and white (adding extra graphics as pasrt of playing around) 

#playing around with the colours -----------------------
ggplot(data = x, aes(x = var1, y = var2)) +
  geom_point(colour = "blue") +
  geom_line(aes(group = var3, colour = "blue"))

#Heres an example
ggplot(data = ChickWeight, aes(x = Time, y = weight)) + #Create a ggplot and make the x axis time and the y axis weight and use colour to differentiate between the different diets and then, 
  geom_point(colour = "blue") + #plot this using points and make these points blue and then, 
  geom_line(aes(group = Chick), colour = "black") #link this to each chick type and make this linking line black 


#changing legend position and adding units to variables---------
ggplot(data = x, aes(x = var1, y = var2, colour = var3)) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "var1", y = "var2", colour = "diet type") + 
  theme(legend.position = "bottom") 


#Heres an example
ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +#Create a ggplot and make the x axis time and the y axis weight and use colour to differentiate between the different diets and then, 
  geom_point() + #plot this using points and then, 
  geom_smooth(method = "lm") +#use geom smooth to create the smooth line of best fit showing the best relationship (linear model) and then, 
  labs(x = "Days", y = "Mass (g)", colour = "diet type") + # Use labs function to lable the axis; x is days and y is mass(g) and use colour to show different diet types and then, 
  theme(legend.position = "bottom") # use theme function to move the legend position to the bottom of te figure


# MAKING DIFFERENT TYPES OF PLOTS -------------------------

#facetting
library(tidyverse)
library(ggpubr)
library(ggplot2)

#Making the panel showing same data with different diets -------------------

ggplot(data = x, aes(x = var1, y = var2, colour = var3)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  facet_wrap(~Diet, ncol = 2) + # This is the line that creates the facets, places all graphs together 
  labs(x = "var1", y = "var2")

#Heres an example
ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  facet_wrap(~Diet, ncol = 2) + # Use the facet wrap function to create  facets
  labs(x = "Days", y = "Mass (g)")

#making data smaller and easier for plotting -------------------------
x_ <- x %>% 
  filter(Time == 21)

#Heres an example 
ChickLast <- ChickWeight %>% #making a new set from the original dataset and then ,
  filter(Time == 21) #use the filter function to filter all data related to 21 in time variable 

#making a line graph --------------------
line_1 <- ggplot(data = x, aes(x = var1, y = var2, colour = var3)) +
  geom_point() +
  geom_line(aes(group = var4)) +
  labs(x = "var1", y = "var2")
line_1 

#Heres an example 
line_1 <- ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_line(aes(group = Chick)) +
  labs(x = "Days", y = "Mass (g)")
line_1 

#This is the same with with a smooth line 
lm_1 <- ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) + 
  geom_point() +
  geom_smooth(method = "gam") +
  labs(x = "Days", y = "Mass (g)")
#first run this and then line 116
lm_1


# Running a histogram ----------------
histogram_1 <- ggplot(data = x, aes(x = var1)) +
  geom_histogram(aes(fill = var2), position = "dodge", binwidth = 100) +
  labs(x = "var1", y = "var2")

histogram_1

#Heres an example 
histogram_1 <- ggplot(data = ChickLast, aes(x = weight)) + #create a ggplot using the data chicklast and use the aesthitic function and mak the x axis the weight and then , 
  geom_histogram(aes(fill = Diet), position = "dodge", binwidth = 100) + #use geom_histogram function to crate a histo and provide different colors to the diet and then,
  labs(x = "Final Mass (g)", y = "Count") #use lab func to label the x and y axis using inverted commas so that R recognizes it as a word. 

histogram_1


#Running a boxplot ------------------------
box_1 <- ggplot(data = x, aes(x = var1, y = var2)) +
  geom_boxplot(aes(fill = var3)) +
  labs(x = "var1", y = "var2") 

box_1

#Heres an example
box_1 <- ggplot(data = ChickLast, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet)) +
  labs(x = "Diet", y = "Final Mass (g)") 

box_1

#No for the grand finale , Putting it all together -----------------
ggarrange(line_1, lm_1, histogram_1, box_1,
          ncol = 2, nrow = 2, # Set number of rows and columns
          labels = c("A", "B", "C", "D"), # Label each figure
          common.legend = TRUE) # Create common legend


####### DAY 3 CODING: GGPLOT AND MAPPING 

#loading packages 
library(tidyverse) #activating the packages using the library functions
library(boot) #Built in datasets 

#plotting with boot 

# Load data
urine <- boot::urine #using the assign operator for give dataset a name

# warming up to plot ---------------
# Create a quick scatterplot -----------------
ggplot(data = x, aes(x = var1, y = var2)) +
  geom_point(aes(colour = var3)) +
  labs(x = "var1" , y = "var2")

#Heres an example 
ggplot(data = urine, aes(x = osmo, y = ph)) + #using the ggplot function using dataset urine and use the aes function to make osmo the x variable and ph the y variable and then, 
  geom_point(aes(colour = cond)) + #use the geom point function to create a point or scatterplot and use the aes function to use color variation to differetniate between the differnt conditions and then, 
  labs(x = "osmoregulation" , y = "pH") #Use the labs function to label x and y 


###### Mapping in R ---------------------

# Load libraries
library(tidyverse)
library(ggpubr)

# Load data
load("data/south_africa_coast.RData") 
load("data/sa_provinces.RData")
load("data/rast_annual.RData")
#load("data/MUR.RData")  high resolution dataset that takes a long time to load 
load("data/MUR_low_res.RData") #same data as line 32 just the low res dataset 


#How to make a custom made pallete ------------------
#in the notes under this chapter , you will find a bunch of sites to use to create your own pallette
#Here ive used the first site and the #s correlate to the colors provided by the site. 
#The colour pallette we will use for ocean temperature
cols11 <- c("#004dcd", "#0068db", "#007ddb", "#008dcf", "#009bbc",
            "#00a7a9", "#1bb298", "#6cba8f", "#9ac290", "#bec99a")

#making plot of sa coastline -------------------------
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) + #use the ggplot func and use the data provided called sa coast and use the aes func to put lon on x and lat on y and then, 
  geom_point() #create a scatterpoint using the geom point function.  
#kind of gives a rough sketch of the coast of SA

#making a solid outline aka connecting the dots ---------------
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) + #use the ggplot func and use the data provided called sa coast and use the aes func to put lon on x and lat on y and then, 
  geom_polygon(colour = "black", fill = "grey45", aes(group = group)) #use the geomploygon to create a shape out of the dots , make the line connecting the dots black and fill the shape with a shade of grey aka grey30 andgroup them accroding to th group col in data. 
#basically like adding the land mask to the plot 

# Borders -----------------
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) # Seems like we are adding another set of co-ordinates for the sa provinces
#If you remove the 373 line ,this take that annoying line out of the way 

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) + 
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) # Force lon/lat extent and sets limits to coordinates 

#expand function bring the map more out 
#coord_equal kinda works to remove the grey space on map by limiting the x and the y axis 

# Adding Sea Surface Temp across the coast using the geom raster function ---------------
sst <- MUR_low_res
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) + # using the geom raster function to add The ocean temperature co-ordinates and filling in bins per colour
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) + #geom ploygon to add area of SA, filling it with color and changing the color of the line 
  geom_path(data = sa_provinces, aes(group = group)) + # connnects observations showing provinces 
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) #adding teh limits for x and y 

#colour pallette making it suitable for sea temperature ---------------------
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  scale_fill_manual("Temp. (°C)", values = cols11) + # using the scale fill manual func to give a legend for what the color pallette is used to show
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0)

#final touches -----------------------
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_tile(data = rast_annual, aes(x = lon, y = lat, fill = bins), 
            colour = "white", size = 0.1) + #geom tile uses a dataset at the layer level if you are overriding the plot defaults. 
  scale_fill_manual("Temp. (°C)", values = cols11) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0)


##final mapping touches with extra legend alterations --------------------------------

final_map <- ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_tile(data = rast_annual, aes(x = lon, y = lat, fill = bins), 
            colour = "white", size = 0.1) +
  scale_fill_manual("Temp. (°C)", values = cols11) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) +
  scale_x_continuous(position = "top") + # Put x axis labels on top of figure
  theme(axis.title = element_blank(), # Remove the axis labels
        legend.text = element_text(size = 7), # Change text size in legend
        legend.title = element_text(size = 7), # Change legend title text size
        legend.key.height = unit(0.3, "cm"), # Change size of legend
        legend.background = element_rect(colour = "white"), # Add legend background
        legend.justification = c(1, 0), # Change position of legend
        legend.position = c(1.5, 0.4))# Fine tune position of legend
  
final_map


#####Chapter 10 -----------------

# Load libraries
library(tidyverse)
library(scales) #provide methods for automatically determining breaks and labels for axes and legends.
library(ggsn) #provides North Symbols and Scale Bars for Maps Created with 'ggplot
library(maps) # plot Geographical Maps

# Load Africa map ------------
load("data/africa_map.RData")


#accessing the default maps which in ths case is the globe -----------------
ggplot() +
  borders() + # The global shape file
  coord_equal() # Equal sizing for lon/lat 

#assigning names to the data ------------------
sa_1 <-africa_map

#making maps specifically for South Africa ---------------------
sa_1 <- ggplot() +
  borders(fill = "grey70", colour = "black") + #using the borders function to make the lines thicker around the borders and fills it in with a shade of grey , 
  coord_equal(xlim = c(12, 36), ylim = c(-38, -22), expand = 0) # using the co ordinate equal funct to Force lon/lat extent
sa_1


#adding annotation to your maps such as the different oceans 
sa_2 <- sa_1 +
  annotate("text", label = "Atlantic\nOcean", #name of annotation , word or edits /n means the atlantic in 1 line and ocean in the next 
           x = 15.1, y = -32.0, # this gives the co ordinates of the annotation 
           size = 5.0,  #size of the text 
           angle = 0, #the angle 
           colour = "navy") + #colour of the text 
  annotate("text", label = "Indian\nOcean", 
           x = 31, y = -33, 
           size = 5.0, 
           angle = 360, 
           colour = "red")

#Adding the scale bar -----------------

sa_3 <- sa_2 +
  # scalebar(x.min = 22, x.max = 26, y.min = -36, y.max = -35, # Set location of bar
  #         dist = 200, height = 1, st.dist = 0.8, st.size = 4, # Set particular visualizations 
  #        transform = TRUE, model = "WGS84") + # Set appearance
  north(x.min = 22.5, x.max = 25.5, y.min = -33, y.max = -31, # Set location of symbol
        scale = 1.2, symbol = 16)
sa_3

# showing the full area of study and then showing the zoomed in area or interest ------------
sa_4 <- sa_3 +
  annotation_custom(grob = ggplotGrob(africa_map),
                    xmin = 20.9, xmax = 26.9,
                    ymin = -30, ymax = -24)
sa_4

#### DAY 4 CODING: TIDY DATA 

#loading packages 
library(tidyverse)
library(lubridate) #makes it easier to work with dates and times

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
#Gather function placed all sources together under 1 category called source

#creating tidy data looking at SACTN3-------------

SACTN3_tidy1 <- SACTN3 %>% 
  spread(key = var, value = val) #spread function took corrsponding depth values in nthe value column and placed it under a new col called depth instead of val

#separating variables 

SACTN4a_tidy <- SACTN4a %>% 
  separate(col = index, into = c("site", "src"), sep = "/ ") #sep the source info from the dex col into its own col called source

#separating the year column into its components

SACTN_tidy2 <- SACTN4a %>% 
  separate(col = index, into = c("site", "src"), sep = "/ ") %>% 
  mutate(day = lubridate::day(date),#using mutate function to take teh data from the date col and make a new col only containing the day info and maing that day then,
         month = lubridate::month(date), #same thing here only with dates
         year = lubridate::year(date)) #same thing here only with years

#Doing the opposite, putting the month year and day together 

SACTN4b_tidy <- SACTN4b %>% 
  unite(year, month, day, col = "date", sep = "-") #basically just reverse of the above formula, using the unite function to place the year , month and day togther in 1 called date 


#### DAY 5: TIDY DATA CONTINUED--------


#Using "join" function ---------------------

SACTN4_tidy <- left_join(SACTN4a_tidy, SACTN4b_tidy) #merge operation between two data frames where the merge returns all of the rows from one table (the left side) and any matching rows from the second table. 


############################CHAPTER 12 

# Load libraries ----------

library(tidyverse)
library(lubridate)

# Load the data from a .RData file

load("data/SACTNmonthly_v4.0.RData")
  
  
  # Copy the data as a dataframe with a shorter name -----------

SACTN <- SACTNmonthly_v4.0

# Remove the original ----------------------

rm(SACTNmonthly_v4.0) #remove function removes any files you'd like 

#fILTER FUNCTION ---------------

SACTN %>% 
  filter(site == "Pollock Beach", month(date) == 12 | month(date) == 1) #filters the rows of the data table that meet certain criteria creating a new data subset


#arrange function ---------------

SACTN %>% 
  arrange(depth, temp)#arrange function allows to sort the data frame by column name



SACTN %>% 
  arrange(desc(temp)) #arranges the temp in descending order 


SACTN %>% 
  filter(site == "Humewood", year(date) == 1990)

#filter for this site and in this year 

humewood_90s <- SACTN %>% 
  filter(site == "Humewood", year(date) %in% seq(1990, 1999, 1))

# over series of years 


# Select columns individually by name
SACTN %>% 
  select(site, src, date, temp)

#move it in the order that you want 
#remove ?
#just add a dash e.g. -date 

# Select all columns between site and temp like a sequence
SACTN %>% 
  select(site:temp)

# Select all columns except those stated individually
SACTN %>% 
  select(-date, -depth)

#works when you want to work with smaller datasets

# Select all columns except those within a given sequence
# Note that the '-' goes outside of a new set of brackets
# that are wrapped around the sequence of columns to remove

SACTN %>% 
  select(-(date:depth))


SACTN %>% 
  mutate(kelvin = temp + 273.15)

#uses 1 variable to make anotyher column using that var 
#good for converting certain var

#summarise --------

SACTN %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE))

#any NA in this column, they need to be removed (true)

#CHAPTER 13-----------------


# Create groupings based on temperatures and depth
SACTN_temp_group <- SACTN %>% 
  group_by(round(temp), depth) #used to group the dataframe in R
#ROUNDING OFF THE DEPTH TO 2 DECIMALS 

# Create groupings based on source and date
SACTN_src_group <- SACTN %>% 
  group_by(src, date)

# Create groupings based on date and depth
SACTN_date_group <- SACTN %>% 
  group_by(date, depth)


SACTN %>% 
  rename(source = src) #useful to rename variables 

#helps make the data tidy e.g the k sensitivity of r and spaces , you could rename this with an underscore 















