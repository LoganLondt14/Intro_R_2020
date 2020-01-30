# Day 3 
# Exercise:ggplot and mapping 
# Logan Londt 


### Loading packages 
library(tidyverse)
library(scales)
library(ggsn)
library(maps)
library(ggpubr)
library(ggplot2)

#Activating packages 

# Load data
load("data/africa_map.RData")


##### DIY Mapping and cutting country out of world map 
#accessing the default maps 
ggplot() +
  borders() + # The global shape file
  coord_equal() # Equal sizing for lon/lat

#####Cutting it to one country 
#assigning names to the data 
aussie_1 <-africa_map

#making maps specifically for Austr.
aussie_1 <- ggplot() +
  borders(fill = "grey45", colour = "green") +
  coord_equal(xlim = c(110, 160), ylim = c(-10, -50), expand = 0) # Force lon/lat extent
aussie_1

##adding annotations to surrounding oceans 
aussie_2 <- aussie_1 +
  annotate("text", label = "indian\nOcean", #name of annotation , word or edits /n means the atlantic in 1 line and ocean in the next 
           x = 112, y = -30, # this gives the co ordinates of the annotation 
           size = 4.0,  #size of the text 
           angle = 90, #the angle 
           colour = "red") + #colour of the text 
  annotate("text", label = "Southern\nOcean", 
           x = 135, y = -45, 
           size = 4.0, 
           angle = 360, 
           colour = "navy") + 
  annotate("text", label = "Pacific\nOcean", 
           x = 157, y = -30, # this gives the co ordinates of the annotation 
           size = 4.0,  #size of the text 
           angle = 90, #the angle 
           colour = "blue") + #colour of the text 
  annotate("text", label = "Australia", 
           x = 135, y = -25, 
           size = 6.0, 
           angle = 360, 
           colour = "black") +
  north(x.min = 150, x.max = 158, y.min = -50, y.max = -40, # Set location of symbol
        scale = 0.8, symbol = 16)
aussie_2

#How would we do this if we wanted to cut out a particular province using SA?


######Looking at the Laminaria and Ecklonia data 

#Reading the data 
ecklonia <- read_csv("data/ecklonia.csv")

#PLOT 1 OF ECKLONIA
## Plotting - relationship between stipe length and stipe massof ecklonia 
plot_1 <- ggplot(data = ecklonia, aes(x = stipe_mass, y = stipe_length, color = site)) +
  geom_point(shape = 21, colour = "salmon", fill = "black") +
  labs(x = "Stipe mass (kg)", y = "Stipe length (cm)")

#This shows us the points but doesnt tell us which site the data came from 

#creating figures containing colours to differntiate betweeen diets
plot_2 <- ggplot(data = ecklonia, aes(x = stipe_mass, y = stipe_length, colour = site)) + #same as before , only now we are adding colour to the sites
  geom_point() + #creating a point graph
  geom_line(aes(group = site)) +
  labs(x = "Stipe mass (kg)", y = "Stipe length (cm)") #linking this with a line for each site

#PLOT 2 OF ECKLONIA
#Running a boxplot
box_1 <- ggplot(data = ecklonia, aes(x = stipe_mass, y = stipe_length)) +
  geom_boxplot(aes(fill = site)) +
  labs(x = "Stipe mass(kg)", y = "Stipe length(cm)") 
box_1

#LAMINARIA
#READING THE DATA 
laminaria <- read_delim("data/laminaria.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)
## Plotting - relationship between stipe length and stipe massof ecklonia 
ggplot(data = laminaria, aes(x = stipe_mass, y = stipe_length)) +
  geom_point(shape = 15, colour = "salmon", fill = "white") +
  labs(x = "Stipe mass (kg)", y = "Stipe length (cm)")

#PLOT 3
lm_1 <- ggplot(data = laminaria, aes(x = stipe_mass, y = stipe_length, color = site)) + #making an object
  geom_point() +
  geom_smooth(method = "gam") +
  labs(x = "Stipe mass (kg)", y = "Stipe length (cm)")
lm_1

#I struggled to get the x axis more spaced out

#PLOT 4
line_1 <- ggplot(data = laminaria, aes(x = stipe_mass, y = stipe_length, colour = site)) +
  geom_point() +
  geom_line(aes(group = site)) +
  labs(x = "Stipe mass (kg)", y = "Stipe length (cm)")
line_1 


#PUTTING EVERYTHING TOGETHER 
ggarrange(line_1, lm_1, plot_2, box_1,
          ncol = 2, nrow = 2, # Set number of rows and columns
          labels = c("A(L)", "B(L)", "C(E)", "D(E)"), # Label each figure
          common.legend = TRUE) # Create common legend

#CREATING A PALETTE
LoganPalette <- c("#BCA148", "#86994A", "#568D58", "#327C65", "#27686A",
            "#345363", "#1bb298", "#6cba8f", "#9ac290", "#bec99a")




