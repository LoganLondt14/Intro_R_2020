# Day 3 
# ggplot and mapping
# Date: 30 January 2020
# Logan Londt 

#loading packages 
library(tidyverse) #activating the packages using the library functions
library(boot)

#plotting with boot 

# Load data
urine <- boot::urine #using the assign operator for give dataset a name


# Create a quick scatterplot
ggplot(data = urine, aes(x = osmo, y = ph)) +
  geom_point(aes(colour = cond)) +
  labs(x = "osmoregulation" , y = "pH") #Using the labs function to label x and y 


###### Mapping in R 

# Load libraries
library(tidyverse)
library(ggpubr)

# Load data
load("data/south_africa_coast.RData")
load("data/sa_provinces.RData")
load("data/rast_annual.RData")
#load("data/MUR.RData")  high resolution dataset that takes a long time to load 
load("data/MUR_low_res.RData") #same data as line 32 just the low res dataset 


#custom made pallete
#The colour pallette we will use for ocean temperature
cols11 <- c("#004dcd", "#0068db", "#007ddb", "#008dcf", "#009bbc",
            "#00a7a9", "#1bb298", "#6cba8f", "#9ac290", "#bec99a")

#making plot of sa coastline
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_point()

#making a solid outline
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "grey30", aes(group = group)) # The land mask


# Borders
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) # The province borders

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) + 
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) # Force lon/lat extent and sets limits to coordinates 

#expand function bring the map more out 
#coord_equal kinda works to remove the white space by limiting the x and the y axis 


# Choose which Sea Surface Temp product you would like to use
sst <- MUR_low_res

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) + # The ocean temperatures and filling in bins per colour
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) + #add area of SA
  geom_path(data = sa_provinces, aes(group = group)) + #adds provinces 
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) #adding teh limits for x and y 

#colour pallette making it suitable for sea temperature 
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  scale_fill_manual("Temp. (°C)", values = cols11) + # Set the colour palette
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0)

#final touches 
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_tile(data = rast_annual, aes(x = lon, y = lat, fill = bins), 
            colour = "white", size = 0.1) + # The coastal temperature values
  scale_fill_manual("Temp. (°C)", values = cols11) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0)


##final mapping touches 

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
        legend.position = c(0.55, 0.4) # Fine tune position of legend
  )
final_map


#####Chapter 10 

# Load libraries
library(tidyverse)
library(scales)
library(ggsn)
library(maps)

# Load Africa map
load("data/africa_map.RData")


#accessing the default maps 
ggplot() +
  borders() + # The global shape file
  coord_equal() # Equal sizing for lon/lat 

#assigning names to the data 
sa_1 <-africa_map

#making maps specifically for South Africa
sa_1 <- ggplot() +
  borders(fill = "grey70", colour = "black") +
  coord_equal(xlim = c(12, 36), ylim = c(-38, -22), expand = 0) # Force lon/lat extent
sa_1

#only ran after i assigned a name , but mayne try installing ggplot first and then see? 

#adding annotation to your maps such as the different oceans 
sa_2 <- sa_1 +
  annotate("text", label = "Atlantic\nOcean", #name of annotation , word or edits /n means the atlantic in 1 line and ocean in the next 
           x = 15.1, y = -32.0, # this gives the co ordinates of the annotation 
           size = 3.0,  #size of the text 
           angle = 0, #the angle 
           colour = "navy") + #colour of the text 
  annotate("text", label = "Indian\nOcean", 
           x = 35, y = -36, 
           size = 3.0, 
           angle = 360, 
           colour = "red")

#Adding the scale bar

sa_3 <- sa_2 +
  # scalebar(x.min = 22, x.max = 26, y.min = -36, y.max = -35, # Set location of bar
  #          dist = 200, height = 1, st.dist = 0.8, st.size = 4, # Set particular visualizations 
  #          transform = TRUE, model = "WGS84") + # Set appearance
  north(x.min = 22.5, x.max = 25.5, y.min = -33, y.max = -31, # Set location of symbol
        scale = 1.2, symbol = 16)
sa_3

# showing the full area of study and then showing the zoomed in area or interest 
sa_4 <- sa_3 +
  annotation_custom(grob = ggplotGrob(africa_map),
                    xmin = 20.9, xmax = 26.9,
                    ymin = -30, ymax = -24)
sa_4















