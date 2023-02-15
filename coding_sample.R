################################################################################
################################################################################
##################  Coding Sample - Pre-doctoral Position ######################
################################################################################
################################################################################

### Structure: 

### 1. Intracity Labor Markets: A counterfactual Analysis of LCY
# 1.1. Number of employees in finance and insurance positions by borough
# 1.2. Relevance of Tower Hamlets as measured by space occupied by offices
# 1.3. Distance to London City Airport 

### 2. Republican vote share and COVID-19 related casualties
# 2.1. 2016 General election results in Wisconsin 
# 2.2. Proximity to conservative christian private school 
# 2.3. Spatial distribution of COVID-19-Casualties in Wiscconsin 

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

### Preliminaries

rm(list = ls())
getwd()
# set WD to _coding_sample folder
setwd("/Users/nickniers/Desktop/Uni/predoc_application/_coding_sample/PRE_DOC")

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
# install.packages("devtools")
# devtools::install_github("tylermorganwall/rayshader")
library(rayshader)
library(grid)
library(png) 
# remotes::install_github("pierreroudier/hillshader")
library(hillshader)
library(areal)


################################################################################
################################################################################

### 1. Shift of the Industry to Tower Hamlets ##################################

### 1.1 Number of employees in finance and insurance positions by borough

# read employees data and limit to finance and insurance sector + three relevant boroughs
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

# remove items
rm(employees, employees_plot)

################################################################################
################################################################################

### 1. Shift of the Industry to Tower Hamlets ##################################

### 1.2 Relevance of Tower Hamlets as measured by space occupied by offices

# read floorspace data and limit to "lower super output areas" (LSOA)
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


# prepare final ggplot and subsequent 3D plot for 11 years 
# !depending on the specification of the used PC this process is time consuming
# !all final visualizations can be found in the RESULTS folder 
for (i in 1:11){
  # plot ratio (office floorspace / total area) * 100 with 2D plot
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
  # plot result
  LSOA_plot

  # transfrom into 3D plot
  plot = plot_gg(LSOA_plot, multicore = TRUE, width = 7, height = 7,
        scale = 400, windowsize = c(2000, 2000), zoom = 0.5, phi = 60, theta = 45,
        solid = FALSE, sunangle = 200, solidcolor = "white", solidlinecolor = "white", background = "white",
        triangulate = TRUE
        )
  
  # render result and safe plot
  render_camera(theta = 45, phi=60,zoom= 0.22, fov= 130)
  render_snapshot(paste("plots/LSOA/fs_", i, ".png"), webshot = TRUE, width = 5000, heigth = 5000, clear = TRUE)
}

# remove items
rm(fs, LSOA, backround, LSOA_plot, plot)

################################################################################
################################################################################

### 1. Shift of the Industry to Tower Hamlets ##################################

### 1.3. Distance to London City Airport 

# read uber data and borough shapefile
uber = read_csv("RAW/uber.csv")
boroughs = st_read("RAW/shapefiles/london_MSOA_repaired.shp", stringsAsFactors = FALSE)

# rename variables to obtain common ID variable and merge dataset with shapefile
names(uber)[4] = "ID"
names(boroughs)[11] = "ID"
uber = merge(boroughs, uber, by = "ID", all.x = TRUE)

# load prepared QGis hillshader as backround picture
hillshade = readPNG("plots/london_hillshade.png") 

# plot the travel times commencing from London City Airport
uber_plot <- ggplot() +
  background_image(hillshade)+
  geom_sf(data = uber, aes(fill = uber$'Mean Travel Time (Seconds)'/60), col = "gray95", linewidth = 0.1, alpha = 0.5) +
  scale_fill_gradient2(low = NA, high = "violetred4", na.value = "gray40")+
  coord_sf(xlim = c(), ylim = c(), crs = st_crs("EPSG:27700"), expand = FALSE) +
  theme_void() + theme(legend.position = "none")  # + theme(
  #text = element_text(family = "CMU Serif Roman", color = "#22211d"),
  #legend.background = element_rect(fill = NA, color = NA),
  #legend.direction = "vertical",
  #legend.position = "bottom",
  #legend.margin=margin(0,0,0,0),
  #legend.box.margin=margin(-5,-5,-5,-5)
  #) 
uber_plot
hillshade

# extract legend from plot for post-processing (note in order to extract the legend
# the plot code above has to include the theme lines for the legend)
leg <- get_legend(uber_plot)
as_ggplot(leg)

# save result 
ggsave(filename="uber.jpg", plot=uber_plot, device="jpg",
       path="plots", height=2000, width=3000, units="px", dpi=600)

# remove items
rm(uber, hillshade, backround, uber_plot, leg)

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

### 2. Republican vote share and COVID-19 related casualties ###################

### 2.1 2016 General election results in Wisconsin 

# load shaded relief of Wisconsin for nice plots (pre-processed using QGis)
relief = raster("plots/WI_map.tif")
relief_spdf = as(relief, "SpatialPixelsDataFrame")
relief = as.data.frame(relief_spdf)
relief$WI_map[relief$layer == 0] = 256
rm(relief_spdf)

# load election shapefile containing results of the 2016 US general election for WI
election = st_read("RAW/shapefiles/WI.shp", stringsAsFactors = FALSE)[,c(1,17,48,49,85:88)]

# transform to appropiate projection
election = st_transform(election, 6893)

# generate relative share of people who voted for republican/democratic candidate
election$share_rep = election$PRES16R / (election$PRES16R + election$PRES16D)
election$share_dem = election$PRES16D / (election$PRES16R + election$PRES16D)

# generate dummy based on which share is higher 
election$dummy = ifelse(election$share_rep >= election$share_dem, 1, 0)

# generate magnitude variable to indicate how "strongly" a county is dem or rep
election$mag = ((election$share_rep - 0.5)*election$dummy + (election$share_dem - 0.5)*(election$dummy-1))+0.5

# plot the results 
election_plot <- ggplot() +
  ### tracts
  geom_sf(data = election, aes(fill = mag), size = 0, col = NA) +
  coord_sf(xlim = c(), ylim = c(), expand = FALSE) +
  scale_fill_gradientn(values=c(0, 0.5, 1), colours=c("slateblue4", "gray96", "violetred4")) +
  ### relief
  scale_alpha(name = "", range = c(1, 0), guide = "none") +
  geom_raster(data = relief, aes_string(x = "x", y = "y", alpha = "layer")) +
  theme_void() + theme(legend.position = "none") # + theme(
  #text = element_text(family = "CMU Serif Roman", color = "#22211d"),
  #legend.background = element_rect(fill = NA, color = NA),
  #legend.direction = "vertical",
  #legend.position = "bottom",
  #legend.margin=margin(0,0,0,0),
  #legend.box.margin=margin(-5,-5,-5,-5)
  #) 
election_plot

# extract legend from plot for post-processing (note in order to extract the legend
# the plot code above has to include the theme lines for the legend)
leg <- get_legend(election_plot)
as_ggplot(election_plot)

# save result 
ggsave(filename="election_tracts.jpg", plot=election_plot, device="jpg",
       path="plots", height=2000, width=3000, units="px", dpi=600)

# remove items
rm(election, election_plot)

################################################################################
################################################################################

### 2. Republican vote share and COVID-19 related casualties ###################

### 2.2 Proximity to conservative christian private school 

# load school data 
school = read.spss("RAW/school.sav", to.data.frame=TRUE)

# extract all WI private schools which are labeled as "Other religious, conservative Christian"
school = school[school$PSTANSI == 55,]
school = school[school$TYPOLOGY == 4,]

# extract the coordinates of these schools and transform them to a suited projection
pos = data.frame(cbind(school$LONGITUDE18, school$LATITUDE18))
pos_new = st_as_sf(pos, coords = c('X1', 'X2'))
pos_new = st_set_crs(pos_new, CRS("+init=epsg:4326"))

# read in tracts shapefile (smaller entity than counties)
tracts = st_read("RAW/shapefiles/cb_2018_55_tract_500k.shp", stringsAsFactors = FALSE)
tracts$ID = seq.int(nrow(tracts))

# create indicator which indicates tracts with conservative schools
tracts = st_transform(tracts, 4326)
in_tract = st_within(pos_new,tracts)
in_tract = as.data.frame(in_tract)[,2]
in_tract = cbind(in_tract, pos)
in_tract$Ind = 1
names(in_tract) = c("ID", "X1", "X2", "Ind")

# merge indicator with initial tracts dataset
tracts = merge(tracts, in_tract, by = "ID", all.x = TRUE)
tracts$Ind[is.na(tracts$Ind)] = 0

# determine centroids of each census tract:
tract_pos = st_point_on_surface(tracts)
tract_pos = tract_pos$geometry
tract_pos =  as.data.frame(st_coordinates(tract_pos))
tracts = cbind(tracts, tract_pos)

# transform tracts dataset to final projection (relevant for maps) 
tracts = st_transform(tracts, 6893)

# create vector with the index of tracts with con.chr.pri.schools
index = tracts[tracts$Ind == 1,]
index$names <- rownames(index)
index = as.numeric(index$names)

# add column to data frame with distance to nearest conservative private school
pri_dis = matrix(data = NA, nrow = 1400, ncol = 1400)
for (i in 1:length(tract_pos[,1])){
  pri_dis[,i] = spDistsN1(as.matrix(tract_pos), as.matrix(tract_pos[i,]), longlat = FALSE)
}

# limit dataframe to tracts within the index vector
pri_dis = as.data.frame(pri_dis[c(index),])
rownames(pri_dis) = c(paste(index,sep=" "))

# coordinates to "real" distance transformation
pri_dis = as.data.frame((2 * pi * 6371 * pri_dis)/360)

# find nearest private school for each tract
pri_dis_m = apply(pri_dis,2,min)
names(pri_dis_m) = c("pri_dis_m")

# transform distance to values between 0 and 1
pri_dis_m = ((pri_dis_m - min(pri_dis_m))/(max(pri_dis_m)-min(pri_dis_m)))
tracts = cbind(tracts, pri_dis_m)

school_plot <- ggplot() +
  ### tracts
  geom_sf(data = tracts, aes(fill = pri_dis_m), col = NA) +
  scale_fill_gradientn(values=c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 1), colours=c("dodgerblue4", "dodgerblue3", "dodgerblue2", "dodgerblue1", "deepskyblue2", "deepskyblue1", "gray90"), guide = guide_colourbar(
    barwidth = 15, barheight = 0.5, title.position = 'bottom', reverse = T, title = "distance private school", title.hjust = 0.5,
    label.position = "top")) +
  coord_sf(xlim = c(), ylim = c(), expand = FALSE) +
  ### schools 
  geom_sf(data = pos_new, size = 0.05, colour = "violetred4") +
  ### relief
  scale_alpha(name = "", range = c(1, 0), guide = "none") +
  geom_raster(data = relief, aes_string(x = "x", y = "y", alpha = "layer")) +
  theme_void() + theme(legend.position = "none") # + theme(
  #text = element_text(family = "CMU Serif Roman", color = "#22211d"),
  #legend.background = element_rect(fill = NA, color = NA),
  #legend.direction = "vertical",
  #legend.position = "bottom",
  #legend.margin=margin(0,0,0,0),
  #legend.box.margin=margin(-5,-5,-5,-5)
  #) 
school_plot

# extract legend from plot for post-processing (note in order to extract the legend
# the plot code above has to include the theme lines for the legend)
leg <- get_legend(school_plot)
as_ggplot(school_plot)

# save result 
ggsave(filename="school.jpg", plot=school_plot, device="jpg",
       path="plots", height=2000, width=3000, units="px", dpi=600)
rm(school, school_plot, tract_pos, tracts, in_tract, pos, pos_new, pri_dis, index, pri_dis_m, i)

################################################################################
################################################################################

### 2. Republican vote share and COVID-19 related casualties ###################

### 2.3 Spatial distribution of COVID-19-Casualties in Wisconsin 

# load Covid data
cases = read.csv("RAW/covid.csv")

# extract relevant variables for the time 2020-2021 time period (note that this
# is commented out as the dataset has been pre-modified in order to not exceed a
# certain size)
#cases = cases[-c(3:5,7:18,22:129)]
#cases = cases[cases$Date == "12/31/2021",]
#cases = cases[,c(1,2,5)]
#write.csv(cases, "RAW/covid.csv", row.names=FALSE)


# merge casualties numbers with tract shapefile
tracts = st_read("RAW/shapefiles/cb_2018_55_tract_500k.shp", stringsAsFactors = FALSE)
tracts$ID = seq.int(nrow(tracts))
tracts = merge(tracts, cases, by = "GEOID", all.x = TRUE)





