library(terra)
library(dplyr)
library(sf)
library(stringr)
library(tidyverse)
library(basemaps)
library(rasterVis)
# Link to MACA tifs https://climate.northwestknowledge.net/PATH_TO_TIFS/MACAV2METDATA/TIF/
# WB from Tercek http://screenedcleanedsummaries.s3-website-us-west-2.amazonaws.com/
library(ggplot2)
library(tidyterra)
library(ggthemes)
library(viridis)
library(readxl)
library(gcookbook)
library(lemon)
library(grid)
library(gridExtra)
library(ggpubr)

rm(list=ls())
SiteID <- "POHE"

CF.folders <- "D:/RCF_2024/RCF_opened/"
OutDir <- "C:/Users/arunyon/DOI/CCRP COLLABORATE! - CCRP COLLABORATE!/01 PROJECT Collaboration/Science, Adaptation, Planning/Exposure reports/maps"
tifDir <- "D:/MACA_tiffs/"


CFs <- c("Warm Wet", "Hot Dry")
cols <- c("#2B83BA", "#D7191C")
# CFs <- c("Warm Dry", "Hot Wet")
# cols <- c("#ABDDA4","#FDAE61")

## Get CF info

Tercek <- c("inmcm4","NorESM1-M","MRI-CGCM3","MIROC5","MIROC-ESM-CHEM",
            "IPSL-CM5A-LR","HadGEM2-CC365","GFDL-ESM2G","CanESM2","CSIRO-Mk3-6-0","CNRM-CM5","CCSM4","BNU-ESM")
Tercek.WB <- c(paste0(Tercek,".rcp45"),paste0(Tercek,".rcp85"))

## Extract CFs from SessionInfo
SessionInfo <- read.table(paste0(CF.folders,SiteID,"/SessionInfo.txt"), sep = "^") #for downloaded centroid CFs
# SessionInfo <- read.table(paste0(CF.folders,"/SessionInfo.txt"), sep = "^") #for custom or Koppen CFs

# extract GCMs and CFs from 9th row of SessionInfo - extract between parenthesis and convert to vector
GCMs <- regmatches(SessionInfo[9,], regexpr("\\((.*?)\\)", SessionInfo[9,]))[[1]]; GCMs <- gsub("\\(|\\)", "", GCMs)
GCMs <- strsplit(GCMs,split=", ",fixed=TRUE)[[1]]
CF.list <- tail(regmatches(SessionInfo[9,], gregexpr("\\((.*?)\\)", SessionInfo[9,]))[[1]], 1); CF.list <- gsub("\\(|\\)", "", CF.list)
CF.list <- strsplit(CF.list,split=", ",fixed=TRUE)[[1]]

CF.GCM <- data.frame(CF=CF.list, GCM=GCMs)
CF.GCM <- subset(CF.GCM, CF %in% CFs)
CF1 <- CF.GCM |> filter(CF == CFs[1])
CF2 <- CF.GCM |> filter(CF == CFs[2])

Tercek <- (CF.GCM$GCM %in% Tercek.WB)
## Extract spatial info

# nps_boundary <- sf::st_read('C:/Users/arunyon/3D Objects/Local-files/Git-repos/CCRP_automated_climate_futures/data/general/spatial-data/nps_boundary/nps_boundary.shp')
# park <- filter(nps_boundary, UNIT_CODE == SiteID)
park <- sf::st_read("C:/Users/arunyon/OneDrive - DOI/Documents/GIS/POHE/POHE_Trail_Centerline_2024/POHE_dissolve.shp")
huc8 <- sf::st_read("C:/Users/arunyon/OneDrive - DOI/Documents/GIS/huc8_conus/HUC8_CONUS/HUC8_US.shp")
# huc8 <- st_transform(huc8, crs(park))
p <- st_transform(park,st_crs(huc8))
p <- st_make_valid(p)
h <- st_make_valid(huc8) #not sure what this does, but only way intersects don't return edge error
b<- sf::st_is_within_distance(p,h,10)

buff <- h[b[[1]],]

buff <- sf::st_read("C:/Users/arunyon/OneDrive - DOI/Documents/GIS/POHE/POHE_Trail_Centerline_2024/POHE_HUC8.shp")
ggplot() +
  geom_sf(data = buff) +
  geom_sf(data = p) 

#Get basemaps
x <- basemap_raster(buff, map_service = "esri", map_type = "world_hillshade") #world_street_map great but need simpler; world_imagery too dark;
x_terr <- rast(x)

states <- st_read("C:/Users/arunyon/3D Objects/Local-files/Git-repos/CCRP_automated_climate_futures/data/general/spatial-data/State_Shapefile/States_NAD83_Albers.shp")
states <- st_transform(x=states,crs = crs(buff))
states <- st_crop(states[1],buff)
states_geometry <- st_geometry(states)

# read in files from variable and CF names
#Find file name that contains var and CF | historical

# sub("\\..*", "", CF1$GCM) # Extract part before period
# sub('.*\\.', '', CF1$GCM) # Extract part after period

# function to find 
map.file <- function(GCM.name){
  CF1.file <- map(c(sub('.*\\.', '', GCM.name), sub("\\..*", "", GCM.name)), 
                  str_detect, string = list.files(path=tifDir, pattern = Var)) %>%
    reduce(`&`) %>% 
    magrittr::extract(list.files(path=tifDir, pattern = Var), .)
}

map.file.season <- function(GCM.name,seas){
  CF1.file <- map(c(sub('.*\\.', '', GCM.name), sub("\\..*", "", GCM.name)), 
                  str_detect, string = list.files(path=tifDir, pattern = paste0(Var,"_",seas))) %>%
    reduce(`&`) %>% 
    magrittr::extract(list.files(path=tifDir, pattern = paste0(Var,"_",seas)), .)
}

# Hist plot
historical.plot <- function(rast){
  ggplot() +
    geom_spatraster_rgb(data = x_terr) +
    geom_sf(data = states,color="black",fill=NA) +
    geom_spatraster(data = rast, alpha=.8)+
    geom_sf(data = park[1], aes(), fill = NA,colour="black",linewidth=1.5) +
    # scale_fill_viridis(direction=-1, option = "viridis", oob = scales::squish) + 
    scale_fill_gradientn(colours = div.pal$Hex, na.value=NA) +
    labs(title = "Historical",fill =units) +
    theme_map() +
    theme(legend.position = "bottom",
          legend.key.width = unit(6, "cm"),
          legend.key.height = unit(.3, "cm"),
          legend.justification = "center",
          legend.text=element_text(size=10), legend.title=element_text(size=12),
          # plot.title=element_blank(),
          plot.title=element_text(size=14,face="bold",hjust=0.5),
          plot.background = element_rect(colour = "gray", fill=NA, linewidth=5),
          plot.margin = unit(c(.5,0,0,0), "cm")) + 
    labs(fill =  units)
} 

map.plot.div <- function(data, title,xaxis,metric,col){ #use with divergent palettes, best for absolute metrics
  ggplot() +
    geom_spatraster_rgb(data = x_terr) +
    geom_sf(data = states,color="black",fill=NA) +
    geom_spatraster(data = data, alpha=.8)+
    geom_sf(data = park[1], aes(), fill = NA,colour="black",linewidth=1.5) +
    # scale_fill_viridis(direction=1, option = "mako", 
    #                    limits = c(scale.min, scale.max), oob = scales::squish) + 
    scale_fill_gradientn(colours = div.pal$Hex, rescaler = ~ scales::rescale_mid(.x, mid = 0),
                         limits = c(scale.min, scale.max), na.value=NA) +
    # scale_fill_gradient2(colours = div.pal$Hex,
    #                      low=scale.min, mid=0, high=scale.max, oob = scales::squish) +
    labs(title = title, fill=metric,legend=units) +
    theme_map() +
    theme(legend.position = "bottom",
          legend.key.width = unit(6, "cm"),
          legend.key.height = unit(.3, "cm"),
          legend.justification = "center",
          legend.text=element_text(size=10), legend.title=element_text(size=12),
          legend.title=element_blank(),
          # plot.title=element_blank(),
          plot.title=element_text(size=14,face="bold",hjust=0.5),
          plot.background = element_rect(colour = col, fill=NA, linewidth=5),
          plot.margin = unit(c(.5,0,0,0), "cm"))  
  # labs(fill =metric) +
  # guides(colour = guide_legend(title.position="top", title.hjust = 0.5))
}

map.plot.seq <- function(data, title,xaxis,metric,col){ #use with sequence palettes (best for temp)
  ggplot() +
    geom_spatraster_rgb(data = x_terr) +
    geom_sf(data = states,color="black",fill=NA) +
    geom_spatraster(data = data, alpha=.8)+
    geom_sf(data = park[1], aes(), fill = NA,colour="black",linewidth=1.5) +
    # scale_fill_viridis(direction=1, option = "mako", 
    #                    limits = c(scale.min, scale.max), oob = scales::squish) + 
    scale_fill_gradientn(colours = seq.pal$Hex,
                         limits = c(scale.min, scale.max), na.value=NA) +
    # scale_fill_gradient2(colours = div.pal$Hex,
    #                      low=scale.min, mid=0, high=scale.max, oob = scales::squish) +
    labs(title = title, fill=metric,legend=units) +
    theme_map() +
    theme(legend.position = "bottom",
          legend.key.width = unit(6, "cm"),
          legend.key.height = unit(.3, "cm"),
          legend.justification = "center",
          legend.text=element_text(size=10), legend.title=element_text(size=12),
          # plot.title=element_blank(),
          plot.title=element_text(size=14,face="bold",hjust=0.5),
          plot.background = element_rect(colour = col, fill=NA, linewidth=5),
          plot.margin = unit(c(.5,0,0,0), "cm"))  
  # labs(fill =metric) +
  # guides(colour = guide_legend(title.position="top", title.hjust = 0.5))
}

map.plot.seq.zero <- function(data, title,xaxis,metric,col){ #use with divergent patterns centered on zero (best for wetter/drier palettes)
  ggplot() +
    geom_spatraster_rgb(data = x_terr) +
    geom_sf(data = states,color="black",fill=NA) +
    geom_spatraster(data = data, alpha=.8)+
    geom_sf(data = park[1], aes(), fill = NA,colour="black",linewidth=1.5) +
    scale_fill_gradientn(colours = div.pal$Hex,
                         values = scales::rescale(c(scale.min,0,scale.max)),
                         guide = "colorbar", limits=c(scale.min,scale.max), na.value=NA) +
    labs(title = title, fill=metric,legend=units) +
    theme_map() +
    theme(legend.position = "bottom",
          legend.key.width = unit(6, "cm"),
          legend.key.height = unit(.3, "cm"),
          legend.justification = "center",
          legend.text=element_text(size=10), legend.title=element_text(size=12),
          # plot.title=element_blank(),
          plot.title=element_text(size=14,face="bold",hjust=0.5),
          plot.background = element_rect(colour = col, fill=NA, linewidth=5),
          plot.margin = unit(c(.5,0,0,0), "cm"))
  # labs(fill =metric) +
  # guides(colour = guide_legend(title.position="top", title.hjust = 0.5))
}

map.plot.seq.pos <- function(data, title,xaxis,metric,col){ #use with divergent patterns where all values are positive but just want to show drier side
  ggplot() +
    geom_spatraster_rgb(data = x_terr) +
    geom_sf(data = states,color="black",fill=NA) +
    geom_spatraster(data = data, alpha=.8)+
    geom_sf(data = park[1], aes(), fill = NA,colour="black",linewidth=1.5) +
    scale_fill_gradientn(colours = div.pal$Hex,
                         values = scales::rescale(c(0,scale.min,scale.max)),
                         guide = "colorbar", limits=c(0,scale.max), na.value=NA) +
    labs(title = title, fill=metric,legend=units) +
    theme_map() +
    theme(legend.position = "bottom",
          legend.key.width = unit(6, "cm"),
          legend.key.height = unit(.3, "cm"),
          legend.justification = "center",
          legend.text=element_text(size=10), legend.title=element_text(size=12),
          # plot.title=element_blank(),
          plot.title=element_text(size=14,face="bold",hjust=0.5),
          plot.background = element_rect(colour = col, fill=NA, linewidth=5),
          plot.margin = unit(c(.5,0,0,0), "cm"))
  # labs(fill =metric) +
  # guides(colour = guide_legend(title.position="top", title.hjust = 0.5))
}

######################################################
### For each variable
## Temp
# Var
Var = "tasmean_ANN" #tasmean_ANN, pr_ANN, Deficit
long.title = "Annual Temperature (°F)" # Annual Temperature (°F), Annual Precipitation (in/year), Annual Climatic Water Deficit (in/year)
units = "(°F)" #(°F), (in/year)

#Palettes -- using IPCC palettes -- divergent for hist and sequential for deltas
# temp = temp_div , temp_seq
# prcp = prec_div , prec_seq
# snow = cryo_div , cryo_seq
VarType = "temp" #temp, prec, cryo

inName <- "C:/Users/arunyon/3D Objects/Local-files/IPCC colors for R 2023-01-18/continuous_colormaps.xlsx"
SheetNames <- excel_sheets(inName)    # typ use full path to get sheet names (vs file open)
# SheetNames

# first sheet is only figures of color maps
for(i in 2:length(SheetNames)){
  # cat(str_c("i ->  ", i , "   "))
  d1 <- read_xlsx(inName, sheet = SheetNames[i], col_names = c("R", "G", "B"))
  d1$PalName <- SheetNames[i]
  d1 <- na.omit(d1)
  ifelse(i == 2, PalData <- d1, PalData <- rbind(PalData, d1))
}

# head(PalData)
PalData$Hex <- rgb(PalData$R, PalData$G, PalData$B, maxColorValue = 255)

div.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_div"))
seq.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_seq"))

if(Var=="Deficit") {div.pal$Hex<-rev(div.pal$Hex);seq.pal$Hex<-rev(seq.pal$Hex)}
# 
# # test palettes
# hw_plot <- ggplot(heightweight, aes(x = ageYear, y = heightIn, colour = weightLb)) +
#   geom_point(size = 3)
# hw_plot + scale_color_gradientn(colours = div.pal$Hex)

Hist.file <- str_subset(list.files(path=tifDir, pattern = Var), pattern="historical")
CF1.file <- map.file(CF1$GCM) 
CF2.file <- map.file(CF2$GCM) 

Hist.rast <- rast(paste0(tifDir, Hist.file)) #Read in raster 
CF1.rast <- rast(paste0(tifDir, CF1.file)) #Read in raster 
CF2.rast <- rast(paste0(tifDir, CF2.file)) 

# Need to flip MACA WB data
# Hist.rast <- terra::flip(rast(paste0(tifDir, Hist.file)), direction="vertical") #Read in raster and flip it
# CF1.rast <- terra::flip(rast(paste0(tifDir, CF1.file)), direction="vertical") #Read in raster and flip it
# CF2.rast <- terra::flip(rast(paste0(tifDir, CF2.file)), direction="vertical") #Read in raster and flip it

# # Clip raster to park

Hist.rast <- crop(mask(Hist.rast,buff), buff)
CF1.rast <- crop(mask(CF1.rast,buff), buff)
CF2.rast <- crop(mask(CF2.rast,buff), buff)

# #Tercek plots code -- clip raster to park and make delta files, convert to in
# 
# Hist.rast <- crop(project(Hist.rast, crs(buff)), buff)
# CF1.rast.temp <- crop(project(CF1.rast, crs(buff)), buff)
# CF2.rast.temp <- crop(project(CF2.rast, crs(buff)), buff)
# 
# CF1.rast <- CF1.rast.temp - Hist.rast
# CF2.rast <- CF2.rast.temp - Hist.rast
# 
# Hist.rast <- Hist.rast/25.4
# CF1.rast <- CF1.rast/25.4
# CF2.rast <- CF2.rast/25.4

# Historical plot
Hist.plot <- historical.plot(Hist.rast)
Hist.plot

# Future delta plots
scale.min = min(minmax(CF1.rast)[1],minmax(CF2.rast)[1])
scale.max = max(minmax(CF1.rast)[2],minmax(CF2.rast)[2])

cf1.plot <- map.plot.seq(data=CF1.rast, title=CFs[1],metric=paste0("Change in ",units),col=cols[1])
cf2.plot <- map.plot.seq(data=CF2.rast, title=CFs[2],metric=paste0("Change in ",units),col=cols[2])


# Merge into one plot

maps <- grid_arrange_shared_legend(cf1.plot, cf2.plot,ncol = 2, nrow = 1, position="bottom",
                                   top = textGrob(paste0("Change in ",long.title, " (2040-2069)"),
                                                  gp=gpar(fontface="bold", col="black", fontsize=18)))

hist.fig <- annotate_figure(Hist.plot, top = text_grob(paste0("Historical ", long.title), 
                                                       face = "bold", size = 18))

a <- grid.arrange(hist.fig, maps,nrow = 2,  clip = TRUE)
a

if(grepl("/", long.title, fixed = TRUE)) {long.title=gsub("/", "_", long.title)}
ggsave(paste0(SiteID,"-", long.title,".png"), width = 13, height = 11, path = OutDir,bg="white", a)


### Precip
### For each variable
# Var
Var = "pr_ANN" #tasmean_ANN, pr_ANN, Deficit
long.title = "Annual Precipitation (in/year)" # Annual Temperature (°F), Annual Precipitation (in/year), Annual Climatic Water Deficit (in/year)
units = "(in/year)" #(°F), (in/year)

#Palettes -- using IPCC palettes -- divergent for hist and sequential for deltas
# temp = temp_div , temp_seq
# prcp = prec_div , prec_seq
# snow = cryo_div , cryo_seq
VarType = "prec" #temp, prec, cryo

inName <- "C:/Users/arunyon/3D Objects/Local-files/IPCC colors for R 2023-01-18/continuous_colormaps.xlsx"
SheetNames <- excel_sheets(inName)    # typ use full path to get sheet names (vs file open)
# SheetNames

# first sheet is only figures of color maps
for(i in 2:length(SheetNames)){
  # cat(str_c("i ->  ", i , "   "))
  d1 <- read_xlsx(inName, sheet = SheetNames[i], col_names = c("R", "G", "B"))
  d1$PalName <- SheetNames[i]
  d1 <- na.omit(d1)
  ifelse(i == 2, PalData <- d1, PalData <- rbind(PalData, d1))
}

# head(PalData)
PalData$Hex <- rgb(PalData$R, PalData$G, PalData$B, maxColorValue = 255)

div.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_div"))
seq.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_seq"))

if(Var=="Deficit") {div.pal$Hex<-rev(div.pal$Hex);seq.pal$Hex<-rev(seq.pal$Hex)}
# 
# # test palettes
# hw_plot <- ggplot(heightweight, aes(x = ageYear, y = heightIn, colour = weightLb)) +
#   geom_point(size = 3)
# hw_plot + scale_color_gradientn(colours = div.pal$Hex)

Hist.file <- str_subset(list.files(path=tifDir, pattern = Var), pattern="historical")
CF1.file <- map.file(CF1$GCM) 
CF2.file <- map.file(CF2$GCM) 

Hist.rast <- rast(paste0(tifDir, Hist.file)) #Read in raster 
CF1.rast <- rast(paste0(tifDir, CF1.file)) #Read in raster 
CF2.rast <- rast(paste0(tifDir, CF2.file)) 

# Need to flip MACA WB data
# Hist.rast <- terra::flip(rast(paste0(tifDir, Hist.file)), direction="vertical") #Read in raster and flip it
# CF1.rast <- terra::flip(rast(paste0(tifDir, CF1.file)), direction="vertical") #Read in raster and flip it
# CF2.rast <- terra::flip(rast(paste0(tifDir, CF2.file)), direction="vertical") #Read in raster and flip it

# # Clip raster to park

Hist.rast <- crop(mask(Hist.rast,buff), buff)
CF1.rast <- crop(mask(CF1.rast,buff), buff)
CF2.rast <- crop(mask(CF2.rast,buff), buff)

# #Tercek plots code -- clip raster to park and make delta files, convert to in
# 
# Hist.rast <- crop(project(Hist.rast, crs(buff)), buff)
# CF1.rast.temp <- crop(project(CF1.rast, crs(buff)), buff)
# CF2.rast.temp <- crop(project(CF2.rast, crs(buff)), buff)
# 
# CF1.rast <- CF1.rast.temp - Hist.rast
# CF2.rast <- CF2.rast.temp - Hist.rast
# 
# Hist.rast <- Hist.rast/25.4
# CF1.rast <- CF1.rast/25.4
# CF2.rast <- CF2.rast/25.4

# Historical plot
Hist.plot <- historical.plot(Hist.rast)
Hist.plot

# Future delta plots
scale.min = min(minmax(CF1.rast)[1],minmax(CF2.rast)[1])
scale.max = max(minmax(CF1.rast)[2],minmax(CF2.rast)[2])

cf1.plot <- if(scale.min < 0) {map.plot.seq.zero(data=CF1.rast, title=CFs[1],metric=paste0("Change in\n",units),col=cols[1])} else{
  map.plot.seq.pos(data=CF1.rast, title=CFs[1],metric=paste0("Change in\n",units),col=cols[1])
}
cf2.plot <- if(scale.min < 0) {map.plot.seq.zero(data=CF2.rast, title=CFs[2],metric=paste0("Change in\n",units),col=cols[2])} else{
  map.plot.seq.pos(data=CF2.rast, title=CFs[2],metric=paste0("Change in\n",units),col=cols[2])
}


# Merge into one plot

maps <- grid_arrange_shared_legend(cf1.plot, cf2.plot,ncol = 2, nrow = 1, position="bottom",
                                   top = textGrob(paste0("Change in ",long.title, " (2040-2069)"),
                                                  gp=gpar(fontface="bold", col="black", fontsize=18)))

hist.fig <- annotate_figure(Hist.plot, top = text_grob(paste0("Historical ", long.title), 
                                                       face = "bold", size = 18))

a <- grid.arrange(hist.fig, maps,nrow = 2,  clip = TRUE)
a

if(grepl("/", long.title, fixed = TRUE)) {long.title=gsub("/", "_", long.title)}
ggsave(paste0(SiteID,"-", long.title,".png"), width = 13, height = 11, path = OutDir,bg="white", a)




##########################
# CWD plots # 
##########################
# If both GCMs available in Tercek models
if(FALSE %in% Tercek){
  
  #####################
  ### ClimateToolbox Deficit
  ### For each variable
  # Var
  Var = "deficit_ANN" #tasmean_ANN, pr_ANN, Deficit, deficit_ANN
  long.title = "Annual Climatic Water Deficit (in/year)" # Annual Temperature (°F), Annual Precipitation (in/year), Annual Climatic Water Deficit (in/year)
  units = "(in/year)" #(°F), (in/year)
  
  #Palettes -- using IPCC palettes -- divergent for hist and sequential for deltas
  # temp = temp_div , temp_seq
  # prcp = prec_div , prec_seq
  # snow = cryo_div , cryo_seq
  VarType = "prec" #temp, prec, cryo
  
  inName <- "C:/Users/arunyon/3D Objects/Local-files/IPCC colors for R 2023-01-18/continuous_colormaps.xlsx"
  SheetNames <- excel_sheets(inName)    # typ use full path to get sheet names (vs file open)
  # SheetNames
  
  # first sheet is only figures of color maps
  for(i in 2:length(SheetNames)){
    # cat(str_c("i ->  ", i , "   "))
    d1 <- read_xlsx(inName, sheet = SheetNames[i], col_names = c("R", "G", "B"))
    d1$PalName <- SheetNames[i]
    d1 <- na.omit(d1)
    ifelse(i == 2, PalData <- d1, PalData <- rbind(PalData, d1))
  }
  
  # head(PalData)
  PalData$Hex <- rgb(PalData$R, PalData$G, PalData$B, maxColorValue = 255)
  
  div.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_div"))
  seq.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_seq"))
  
  if(Var=="Deficit"| Var == "deficit_ANN") {div.pal$Hex<-rev(div.pal$Hex);seq.pal$Hex<-rev(seq.pal$Hex)}
  # 
  # # test palettes
  # hw_plot <- ggplot(heightweight, aes(x = ageYear, y = heightIn, colour = weightLb)) +
  #   geom_point(size = 3)
  # hw_plot + scale_color_gradientn(colours = div.pal$Hex)
  
  Hist.file <- str_subset(list.files(path=tifDir, pattern = Var), pattern="historical")
  CF1.file <- map.file(CF1$GCM) 
  CF2.file <- map.file(CF2$GCM) 
  
  Hist.rast <- rast(paste0(tifDir, Hist.file)) #Read in raster 
  CF1.rast <- rast(paste0(tifDir, CF1.file)) #Read in raster 
  CF2.rast <- rast(paste0(tifDir, CF2.file)) 
  
  # Need to flip MACA WB data
  Hist.rast <- terra::flip(rast(paste0(tifDir, Hist.file)), direction="vertical") #Read in raster and flip it
  CF1.rast <- terra::flip(rast(paste0(tifDir, CF1.file)), direction="vertical") #Read in raster and flip it
  CF2.rast <- terra::flip(rast(paste0(tifDir, CF2.file)), direction="vertical") #Read in raster and flip it
  
  # # Clip raster to park
  # 
  # Hist.rast <- crop(Hist.rast, buff)
  # CF1.rast <- crop(CF1.rast, buff)
  # CF2.rast <- crop(CF2.rast, buff)
  
  #Tercek plots code -- clip raster to park and make delta files, convert to in
  
  Hist.rast <- crop(mask(project(Hist.rast, crs(buff)),buff), buff)
  CF1.rast <- crop(mask(project(CF1.rast, crs(buff)),buff), buff)
  CF2.rast <- crop(mask(project(CF2.rast, crs(buff)),buff), buff)
  
  # Historical plot
  Hist.plot <- historical.plot(Hist.rast)
  Hist.plot
  
  # Future delta plots
  scale.min = min(minmax(CF1.rast)[1],minmax(CF2.rast)[1])
  scale.max = max(minmax(CF1.rast)[2],minmax(CF2.rast)[2])
  
  cf1.plot <- if(scale.min < 0) {map.plot.seq.zero(data=CF1.rast, title=CFs[1],metric=paste0("Change in\n",units),col=cols[1])} else{
    map.plot.seq.pos(data=CF1.rast, title=CFs[1],metric=paste0("Change in\n",units),col=cols[1])
  }
  cf2.plot <- if(scale.min < 0) {map.plot.seq.zero(data=CF2.rast, title=CFs[2],metric=paste0("Change in\n",units),col=cols[2])} else{
    map.plot.seq.pos(data=CF2.rast, title=CFs[2],metric=paste0("Change in\n",units),col=cols[2])
  }
  
  
  # Merge into one plot
  
  maps <- grid_arrange_shared_legend(cf1.plot, cf2.plot,ncol = 2, nrow = 1, position="bottom",
                                     top = textGrob(paste0("Change in ",long.title, " (2040-2069)"),
                                                    gp=gpar(fontface="bold", col="black", fontsize=18)))
  
  hist.fig <- annotate_figure(Hist.plot, top = text_grob(paste0("Historical ", long.title), 
                                                         face = "bold", size = 18))
  
  a <- grid.arrange(hist.fig, maps,nrow = 2,  clip = TRUE)
  a
  
  if(grepl("/", long.title, fixed = TRUE)) {long.title=gsub("/", "_", long.title)}
  ggsave(paste0(SiteID,"-", long.title,".png"), width = 13, height = 11, path = OutDir,bg="white", a)
} else {
  
  ### Tercek Deficit
  ### For each variable
  # Var
  Var = "Deficit" #tasmean_ANN, pr_ANN, Deficit
  long.title = "Annual Climatic Water Deficit (in/year)" # Annual Temperature (°F), Annual Precipitation (in/year), Annual Climatic Water Deficit (in/year)
  units = "(in/year)" #(°F), (in/year)
  
  #Palettes -- using IPCC palettes -- divergent for hist and sequential for deltas
  # temp = temp_div , temp_seq
  # prcp = prec_div , prec_seq
  # snow = cryo_div , cryo_seq
  VarType = "prec" #temp, prec, cryo
  
  inName <- "C:/Users/arunyon/3D Objects/Local-files/IPCC colors for R 2023-01-18/continuous_colormaps.xlsx"
  SheetNames <- excel_sheets(inName)    # typ use full path to get sheet names (vs file open)
  # SheetNames
  
  # first sheet is only figures of color maps
  for(i in 2:length(SheetNames)){
    # cat(str_c("i ->  ", i , "   "))
    d1 <- read_xlsx(inName, sheet = SheetNames[i], col_names = c("R", "G", "B"))
    d1$PalName <- SheetNames[i]
    d1 <- na.omit(d1)
    ifelse(i == 2, PalData <- d1, PalData <- rbind(PalData, d1))
  }
  
  # head(PalData)
  PalData$Hex <- rgb(PalData$R, PalData$G, PalData$B, maxColorValue = 255)
  
  div.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_div"))
  seq.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_seq"))
  
  if(Var=="Deficit") {div.pal$Hex<-rev(div.pal$Hex);seq.pal$Hex<-rev(seq.pal$Hex)}
  # 
  # # test palettes
  # hw_plot <- ggplot(heightweight, aes(x = ageYear, y = heightIn, colour = weightLb)) +
  #   geom_point(size = 3)
  # hw_plot + scale_color_gradientn(colours = div.pal$Hex)
  
  Hist.file <- str_subset(list.files(path=tifDir, pattern = Var), pattern="historical")
  CF1.file <- map.file(CF1$GCM) 
  CF2.file <- map.file(CF2$GCM) 
  
  Hist.rast <- rast(paste0(tifDir, Hist.file)) #Read in raster 
  CF1.rast <- rast(paste0(tifDir, CF1.file)) #Read in raster 
  CF2.rast <- rast(paste0(tifDir, CF2.file)) 
  
  # Need to flip MACA WB data
  # Hist.rast <- terra::flip(rast(paste0(tifDir, Hist.file)), direction="vertical") #Read in raster and flip it
  # CF1.rast <- terra::flip(rast(paste0(tifDir, CF1.file)), direction="vertical") #Read in raster and flip it
  # CF2.rast <- terra::flip(rast(paste0(tifDir, CF2.file)), direction="vertical") #Read in raster and flip it
  
  # # Clip raster to park
  # 
  # Hist.rast <- crop(Hist.rast, buff)
  # CF1.rast <- crop(CF1.rast, buff)
  # CF2.rast <- crop(CF2.rast, buff)
  
  #Tercek plots code -- clip raster to park and make delta files, convert to in
  
  Hist.rast <- crop(mask(project(Hist.rast, crs(buff)),buff), buff)
  CF1.rast <- crop(mask(project(CF1.rast, crs(buff)),buff), buff)
  CF2.rast <- crop(mask(project(CF2.rast, crs(buff)),buff), buff)
  
  CF1.rast <- CF1.rast - Hist.rast
  CF2.rast <- CF2.rast - Hist.rast
  
  Hist.rast <- Hist.rast/25.4
  CF1.rast <- CF1.rast/25.4
  CF2.rast <- CF2.rast/25.4
  
  # Historical plot
  Hist.plot <- historical.plot(Hist.rast)
  Hist.plot
  
  # Future delta plots
  scale.min = min(minmax(CF1.rast)[1],minmax(CF2.rast)[1])
  scale.max = max(minmax(CF1.rast)[2],minmax(CF2.rast)[2])
  
  cf1.plot <- if(scale.min < 0) {map.plot.seq.zero(data=CF1.rast, title=CFs[1],metric=paste0("Change in\n",units),col=cols[1])} else{
    map.plot.seq.pos(data=CF1.rast, title=CFs[1],metric=paste0("Change in\n",units),col=cols[1])
  }
  cf2.plot <- if(scale.min < 0) {map.plot.seq.zero(data=CF2.rast, title=CFs[2],metric=paste0("Change in\n",units),col=cols[2])} else{
    map.plot.seq.pos(data=CF2.rast, title=CFs[2],metric=paste0("Change in\n",units),col=cols[2])
  }
  
  
  # Merge into one plot
  
  maps <- grid_arrange_shared_legend(cf1.plot, cf2.plot,ncol = 2, nrow = 1, position="bottom",
                                     top = textGrob(paste0("Change in ",long.title, " (2040-2069)"),
                                                    gp=gpar(fontface="bold", col="black", fontsize=18)))
  
  hist.fig <- annotate_figure(Hist.plot, top = text_grob(paste0("Historical ", long.title), 
                                                         face = "bold", size = 18))
  
  a <- grid.arrange(hist.fig, maps,nrow = 2,  clip = TRUE)
  a
  
  if(grepl("/", long.title, fixed = TRUE)) {long.title=gsub("/", "_", long.title)}
  ggsave(paste0(SiteID,"-", long.title,".png"), width = 13, height = 11, path = OutDir,bg="white", a)
}





# ##################### SEASONAL PLOTS #######################
# ### For each variable
# ## Temp
# # Var
# Var = "tasmean" #tasmean_ANN, pr_ANN, Deficit
# long.title = "Seasonal Temperature (°F)" # Annual Temperature (°F), Annual Precipitation (in/year), Annual Climatic Water Deficit (in/year)
# units = "(°F)" #(°F), (in/year)
# 
# #Palettes -- using IPCC palettes -- divergent for hist and sequential for deltas
# # temp = temp_div , temp_seq
# # prcp = prec_div , prec_seq
# # snow = cryo_div , cryo_seq
# VarType = "temp" #temp, prec, cryo
# 
# inName <- "C:/Users/arunyon/3D Objects/Local-files/IPCC colors for R 2023-01-18/continuous_colormaps.xlsx"
# SheetNames <- excel_sheets(inName)    # typ use full path to get sheet names (vs file open)
# # SheetNames
# 
# # first sheet is only figures of color maps
# for(i in 2:length(SheetNames)){
#   # cat(str_c("i ->  ", i , "   "))
#   d1 <- read_xlsx(inName, sheet = SheetNames[i], col_names = c("R", "G", "B"))
#   d1$PalName <- SheetNames[i]
#   d1 <- na.omit(d1)
#   ifelse(i == 2, PalData <- d1, PalData <- rbind(PalData, d1))
# }
# 
# # head(PalData)
# PalData$Hex <- rgb(PalData$R, PalData$G, PalData$B, maxColorValue = 255)
# 
# div.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_div"))
# seq.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_seq"))
# 
# if(Var=="Deficit") {div.pal$Hex<-rev(div.pal$Hex);seq.pal$Hex<-rev(seq.pal$Hex)}
# 
# CF1.file <- map.file.season(CF1$GCM,"JJA")
# CF2.file <- map.file(CF2$GCM)
# 
# CF1.rast <- rast(paste0(tifDir, CF1.file)) #Read in raster 
# CF2.rast <- rast(paste0(tifDir, CF2.file)) 
# 
# # Need to flip MACA WB data
# # Hist.rast <- terra::flip(rast(paste0(tifDir, Hist.file)), direction="vertical") #Read in raster and flip it
# # CF1.rast <- terra::flip(rast(paste0(tifDir, CF1.file)), direction="vertical") #Read in raster and flip it
# # CF2.rast <- terra::flip(rast(paste0(tifDir, CF2.file)), direction="vertical") #Read in raster and flip it
# 
# # # Clip raster to park
# 
# Hist.rast <- crop(Hist.rast, buff)
# CF1.rast <- crop(CF1.rast, buff)
# CF2.rast <- crop(CF2.rast, buff)
# 
# # #Tercek plots code -- clip raster to park and make delta files, convert to in
# # 
# # Hist.rast <- crop(project(Hist.rast, crs(buff)), buff)
# # CF1.rast.temp <- crop(project(CF1.rast, crs(buff)), buff)
# # CF2.rast.temp <- crop(project(CF2.rast, crs(buff)), buff)
# # 
# # CF1.rast <- CF1.rast.temp - Hist.rast
# # CF2.rast <- CF2.rast.temp - Hist.rast
# # 
# # Hist.rast <- Hist.rast/25.4
# # CF1.rast <- CF1.rast/25.4
# # CF2.rast <- CF2.rast/25.4
# 
# # Historical plot
# Hist.plot <- ggplot() +
#   geom_spatraster_rgb(data = x_terr) +
#   geom_spatraster(data = Hist.rast, alpha=.8)+
#   geom_sf(data = park[1], aes(), fill = NA,colour="black",linewidth=1.5) +
#   # scale_fill_viridis(direction=-1, option = "viridis", oob = scales::squish) + 
#   scale_fill_gradientn(colours = div.pal$Hex) +
#   labs(title = "Historical") +
#   theme_map() +
#   theme(legend.position = "bottom",
#         legend.key.width = unit(6, "cm"),
#         legend.key.height = unit(.3, "cm"),
#         legend.justification = "center",
#         legend.title=element_blank(),
#         # plot.title=element_blank(),
#         plot.title=element_text(size=12,face="bold",hjust=0.5),
#         plot.background = element_rect(colour = "gray", fill=NA, linewidth=5),
#         plot.margin = unit(c(.5,0,0,0), "cm")) + 
#   labs(fill =  paste0(units))
# Hist.plot
# 
# # Future delta plots
# scale.min = min(minmax(CF1.rast)[1],minmax(CF2.rast)[1])
# scale.max = max(minmax(CF1.rast)[2],minmax(CF2.rast)[2])
# 
# cf1.plot <- map.plot(data=CF1.rast, title=CFs[1],metric=paste0("Change in ",units),col=cols[1])
# cf2.plot <- map.plot(data=CF2.rast, title=CFs[2],metric=paste0("Change in ",units),col=cols[2])
# 
# 
# # Merge into one plot
# 
# maps <- grid_arrange_shared_legend(cf1.plot, cf2.plot,ncol = 2, nrow = 1, position="bottom",
#                                    top = textGrob(paste0("Change in ",long.title, " (2040-2069)"),
#                                                   gp=gpar(fontface="bold", col="black", fontsize=18)))
# 
# hist.fig <- annotate_figure(Hist.plot, top = text_grob(paste0("Historical ", long.title), 
#                                                        face = "bold", size = 18))
# 
# a <- grid.arrange(hist.fig, maps,nrow = 2,  clip = TRUE)
# a
# 
# if(grepl("/", long.title, fixed = TRUE)) {long.title=gsub("/", "_", long.title)}
# ggsave(paste0(SiteID,"-", long.title,".png"), width = 11, height = 11, path = OutDir,bg="white", a)
# 
# 
# 
# ##################### EXTRA PLOTS ###########################
# ### ClimateToolbox Wind
# ### For each variable
# # Var
# Var = "was_ANN" #tasmean_ANN, pr_ANN, Deficit, deficit_ANN
# long.title = "Annual Average Wind Speed (mi/hr)" # Annual Temperature (°F), Annual Precipitation (in/year), Annual Climatic Water Deficit (in/year)
# units = "(mi/hr)" #(°F), (in/year)
# 
# #Palettes -- using IPCC palettes -- divergent for hist and sequential for deltas
# # temp = temp_div , temp_seq
# # prcp = prec_div , prec_seq
# # snow = cryo_div , cryo_seq
# # wind = wind_div , wind_div
# VarType = "wind" #temp, prec, cryo
# 
# inName <- "C:/Users/arunyon/3D Objects/Local-files/IPCC colors for R 2023-01-18/continuous_colormaps.xlsx"
# SheetNames <- excel_sheets(inName)    # typ use full path to get sheet names (vs file open)
# # SheetNames
# 
# # first sheet is only figures of color maps
# for(i in 2:length(SheetNames)){
#   # cat(str_c("i ->  ", i , "   "))
#   d1 <- read_xlsx(inName, sheet = SheetNames[i], col_names = c("R", "G", "B"))
#   d1$PalName <- SheetNames[i]
#   d1 <- na.omit(d1)
#   ifelse(i == 2, PalData <- d1, PalData <- rbind(PalData, d1))
# }
# 
# # head(PalData)
# PalData$Hex <- rgb(PalData$R, PalData$G, PalData$B, maxColorValue = 255)
# 
# div.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_div"))
# seq.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_seq"))
# 
# if(Var=="Deficit"| Var == "deficit_ANN") {div.pal$Hex<-rev(div.pal$Hex);seq.pal$Hex<-rev(seq.pal$Hex)}
# # 
# # # test palettes
# # hw_plot <- ggplot(heightweight, aes(x = ageYear, y = heightIn, colour = weightLb)) +
# #   geom_point(size = 3)
# # hw_plot + scale_color_gradientn(colours = div.pal$Hex)
# 
# Hist.file <- str_subset(list.files(path=tifDir, pattern = Var), pattern="historical")
# 
# CF1.file <- map(c(sub('.*\\.', '', CF1$GCM), sub("\\..*", "", CF1$GCM)), 
#                 str_detect, string = list.files(path=tifDir, pattern = Var)) %>%
#   reduce(`&`) %>% 
#   magrittr::extract(list.files(path=tifDir, pattern = Var), .)
# 
# CF2.file <- map(c(sub('.*\\.', '', CF2$GCM), sub("\\..*", "", CF2$GCM)), 
#                 str_detect, string = list.files(path=tifDir, pattern = Var)) %>%
#   reduce(`&`) %>% 
#   magrittr::extract(list.files(path=tifDir, pattern = Var), .)
# 
# Hist.rast <- rast(paste0(tifDir, Hist.file)) #Read in raster 
# CF1.rast <- rast(paste0(tifDir, CF1.file)) #Read in raster 
# CF2.rast <- rast(paste0(tifDir, CF2.file)) 
# 
# # # Need to flip MACA WB data
# # Hist.rast <- terra::flip(rast(paste0(tifDir, Hist.file)), direction="vertical") #Read in raster and flip it
# # CF1.rast <- terra::flip(rast(paste0(tifDir, CF1.file)), direction="vertical") #Read in raster and flip it
# # CF2.rast <- terra::flip(rast(paste0(tifDir, CF2.file)), direction="vertical") #Read in raster and flip it
# 
# # # Clip raster to park
# # 
# # Hist.rast <- crop(Hist.rast, buff)
# # CF1.rast <- crop(CF1.rast, buff)
# # CF2.rast <- crop(CF2.rast, buff)
# 
# #Tercek plots code -- clip raster to park and make delta files, convert to in
# 
# Hist.rast <- crop(project(Hist.rast, crs(buff)), buff)
# CF1.rast <- crop(project(CF1.rast, crs(buff)), buff)
# CF2.rast <- crop(project(CF2.rast, crs(buff)), buff)
# 
# # Historical plot
# Hist.plot <- ggplot() +
#   geom_spatraster_rgb(data = x_terr) +
#   geom_spatraster(data = Hist.rast, alpha=.8)+
#   geom_sf(data = park[1], aes(), fill = NA,colour="black",linewidth=1.5) +
#   # scale_fill_viridis(direction=-1, option = "viridis", oob = scales::squish) + 
#   scale_fill_gradientn(colours = div.pal$Hex) +
#   labs(title = "Historical") +
#   theme_map() +
#   theme(legend.position = "bottom",
#         legend.key.width = unit(6, "cm"),
#         legend.key.height = unit(.3, "cm"),
#         legend.justification = "center",
#         legend.title=element_blank(),
#         # plot.title=element_blank(),
#         plot.title=element_text(size=12,face="bold",hjust=0.5),
#         plot.background = element_rect(colour = "gray", fill=NA, linewidth=5),
#         plot.margin = unit(c(.5,0,0,0), "cm")) + 
#   labs(fill =  paste0(units))
# Hist.plot
# 
# # Future delta plots
# scale.min = min(minmax(CF1.rast)[1],minmax(CF2.rast)[1])
# scale.max = max(minmax(CF1.rast)[2],minmax(CF2.rast)[2])
# 
# map.plot <- function(data, title,xaxis,metric,col){
#   ggplot() +
#     geom_spatraster_rgb(data = x_terr) +
#     geom_spatraster(data = data, alpha=.8)+
#     geom_sf(data = park[1], aes(), fill = NA,colour="black",linewidth=1.5) +
#     # scale_fill_viridis(direction=1, option = "mako", 
#     #                    limits = c(scale.min, scale.max), oob = scales::squish) + 
#     scale_fill_gradientn(colours = seq.pal$Hex, 
#                          limits = c(scale.min, scale.max), oob = scales::squish) +
#     labs(title = title, fill=metric) +
#     theme_map() +
#     theme(legend.position = "bottom",
#           legend.key.width = unit(6, "cm"),
#           legend.key.height = unit(.3, "cm"),
#           legend.justification = "center",
#           legend.title=element_blank(),
#           # plot.title=element_blank(),
#           plot.title=element_text(size=12,face="bold",hjust=0.5),
#           plot.background = element_rect(colour = col, fill=NA, linewidth=5),
#           plot.margin = unit(c(.5,0,0,0), "cm"))  
#   # labs(fill =metric) +
#   # guides(colour = guide_legend(title.position="top", title.hjust = 0.5))
# }
# 
# cf1.plot <- map.plot(data=CF1.rast, title=CFs[1],metric=paste0("Change in ",units),col=cols[1])
# cf2.plot <- map.plot(data=CF2.rast, title=CFs[2],metric=paste0("Change in ",units),col=cols[2])
# 
# 
# # Merge into one plot
# 
# maps <- grid_arrange_shared_legend(cf1.plot, cf2.plot,ncol = 2, nrow = 1, position="bottom",
#                                    top = textGrob(paste0("Change in ",long.title, " (2040-2069)"),
#                                                   gp=gpar(fontface="bold", col="black", fontsize=18)))
# 
# hist.fig <- annotate_figure(Hist.plot, top = text_grob(paste0("Historical ", long.title), 
#                                                        face = "bold", size = 18))
# 
# a <- grid.arrange(hist.fig, maps,nrow = 2,  clip = TRUE)
# a
# 
# if(grepl("/", long.title, fixed = TRUE)) {long.title=gsub("/", "_", long.title)}
# ggsave(paste0(SiteID,"-", long.title,".png"), width = 11, height = 11, path = OutDir,bg="white", a)
