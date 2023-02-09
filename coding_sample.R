################################################################################

##################  Coding Sample - Pre-doctoral Position ######################

# This coding sample is part of a larger project I undertake at LSE. Using the 
# installment of London City Airport (LCY) as an accessibility shock to the finance 
# and insurance labor market in central London, I analyze how firm sorting 
# within cities is affected by proximity considerations and international market 
# access. In doing so, I have created several descriptive visualizations which
# summarize the shift of the finance industry to a certain part of the city
# (Tower Hamlets in eastern London), the increasing relevance of LCY for business
# travel in London, and travel times throughout the city by different means of
# tansport. The coding sample is structured as follows:

# 1.  Shift of the Industry to Tower Hamlets 
#     1.1 Number of employees in finance and insurance positions by borough
#     1.2 Relevance of Tower Hamlets as measured by space occupied by offices 

# 2.  Travel times commencing from LCY 
#     2.1 Uber Data 
################################################################################

### Preliminaries

rm(list = ls())
getwd()
# set WD to _coding_sample folder
setwd("/Users/nickniers/Desktop/Uni/predoc_application/_coding_sample")

# loading required packages
library(tidyverse)
library(sp)
library(leaflet)
library(ggpubr)
library(foreign)
library(readr) 
library(viridis)
library(raster)
library(sf)
library(rgdal)
library(maptools)
library(reshape2)
library(rayshader)
library(grid)
library(png) 


################################################################################

### 1. Shift of the Industry to Tower Hamlets ##################################

### 1.1 Number of employees in finance and insurance positions by borough

# read data and limit to finance and insurance sector + three relevant boroughs
employees = read.csv("RAW/employees.csv")
employees = rbind(employees[employees$Borough == c("City of London"),], employees[employees$Borough == c("Tower Hamlets"),],  employees[employees$Borough == c("Westminster"),])
employees = employees[employees$Sector == "Financial and insurance activities",]
employees = employees[,-2]

# wide to long
employees = melt(employees, id.vars = c("Borough"))
employees$value = gsub(",","", employees$value)
employees$value = as.numeric(employees$value)/1000

# adjust data variable to appropiate format
employees$variable = substring(employees$variable, 2)
employees$variable = as.Date(paste(employees$variable, 1, 1, sep = "-"))

# plot reslut 
employees_plot = ggplot() +
  geom_line(data = employees[employees$Borough == "City of London",], aes(x = variable, y = value, colour = "City of London"), size = 1.5, linetype = "solid")+ 
  geom_point(data = employees[employees$Borough == "City of London",], aes(x = variable, y = value), color="gray40", size=2, shape = 25, fill = "white")+
  geom_line(data = employees[employees$Borough == "Tower Hamlets",], aes(x = variable, y = value, colour = "Tower Hamlets"), size = 1.5, linetype = "solid")+ 
  geom_point(data = employees[employees$Borough == "Tower Hamlets",],aes(x = variable, y = value), color="gray10", size=2, shape = 21, fill = "white") +
  geom_line(data = employees[employees$Borough == "Westminster",], aes(x = variable, y = value, colour = "Westminster"), size = 1.5, linetype = "solid")+ 
  geom_point(data = employees[employees$Borough == "Westminster",],aes(x = variable, y = value), color="gray80", size=2, shape = 22, fill = "white") +
  geom_vline(xintercept = 10960, linetype="dashed", color = "red", size=0.5)+
  scale_color_manual(name = "Borough:", values = c("City of London" = "gray40", "Tower Hamlets" = "gray10", "Westminster" = "gray80"))+
  theme_minimal() + ylab("Financial and insurance activities - No. of workers/1000") + xlab("Year")
employees_plot + theme(text = element_text(size=rel(5)), legend.text = element_text(size=23), legend.position = "bottom")+
  guides(color = guide_legend(override.aes = list(size = 10)))

# save result 
ggsave(filename="employees.jpg", plot=employees_plot, device="jpg",
       path="plots", height=4000, width=6000, units="px", dpi=600)

################################################################################

### 1. Shift of the Industry to Tower Hamlets ##################################

### 1.2 Relevance of Tower Hamlets as measured by space occupied by offices

# read data and limit to "lower super output areas" (LSOA)
fs = read_csv("RAW/floorspace.csv")
fs = fs[fs$geography == "LSOA",c(3:15)]

# adjust data type of relevant columns and NA policy
for (i in 3:13){
  fs[[i]] = as.numeric(fs[[i]])
}
fs[is.na(fs)] = 0

# read in shapefile data
boroughs = st_read("RAW/shapefiles/London_Borough_Excluding_MHW.shp", stringsAsFactors = FALSE)
LSOA = st_read("RAW/shapefiles/LSOA_2011_London_gen_MHW.shp", stringsAsFactors = FALSE)

# merge shapefile data with data from fs dataframe
names(LSOA)[1] = names(fs)[1]
LSOA = merge(LSOA, fs, by = "ons_code", all.x = TRUE)

# define new variable as percentage of floorspace to total area of LSOA 
LSOA$area = st_area(LSOA)
for (i in 16:26){
  LSOA = cbind(LSOA,(LSOA[[i]]/as.numeric(LSOA$area))*100)
  names(LSOA)[12 + i] = paste("ratio", (i-15), sep = "_")
}

# compute centroids of each geometray 
LSOA = st_centroid(LSOA)
LSOA = cbind(LSOA, st_coordinates(LSOA$geometry))

# safe map with borough bordes as a backround png for the actual plot 
backround = ggplot()+
  geom_sf(data = boroughs, aes(fill = "gray98"), colour = "white", fill = "gray98") +
  coord_sf(xlim = c(), ylim = c(), expand = FALSE) +
  theme(legend.position = "none", 
        axis.line=element_blank(), 
        axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.text.y=element_blank(), axis.title.y=element_blank(),
        axis.ticks=element_blank(), 
        panel.background = element_blank())
backround
xlim = ggplot_build(backround)$layout$panel_scales_x[[1]]$range$range
ylim = ggplot_build(backround)$layout$panel_scales_y[[1]]$range$range
ggsave("plots/backround.png", width = diff(xlim)/10, height = diff(ylim)/10, units = "px", limitsize = FALSE)
backround = readPNG("plots/backround.png")  

for (i in 1:11){
  LSOA_plot <- ggplot() +
    background_image(backround)+
    xlim(xlim[1],xlim[2]) + # x-axis Mapping
    ylim(ylim[1],ylim[2]) + # y-axis Mapping
    geom_point(data = LSOA, aes(x = X, y = Y, col = LSOA[[27+i]], size = LSOA[[27+i]]), shape = 15) +
    scale_size_continuous(range = c(0,2)) +
    scale_colour_gradient(high = "violetred4", low = "gray98")+
    coord_sf(xlim = c(), ylim = c(), expand = FALSE) +
    theme(legend.position = "none", 
          axis.line=element_blank(), 
          axis.text.x=element_blank(), axis.title.x=element_blank(),
          axis.text.y=element_blank(), axis.title.y=element_blank(),
          axis.ticks=element_blank(), 
          panel.background = element_blank())
  
  
  LSOA_plot

  
  plot = plot_gg(LSOA_plot, multicore = TRUE, width = 7, height = 7,
        scale = 400, windowsize = c(2000, 2000), zoom = 0.5, phi = 60, theta = 45,
        solid = FALSE, sunangle = 200, solidcolor = "white", solidlinecolor = "white", background = "white",
        triangulate = TRUE
        )
  
  
  render_camera(theta = 45, phi=60,zoom= 0.22, fov= 130)
  render_snapshot(paste("plots/LSOA/fs_", i, ".png"), webshot = TRUE, width = 5000, heigth = 5000, clear = TRUE)
}

