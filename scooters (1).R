##############################################################
#                      Introduction                          #
##############################################################
#This code was written by Henry Amestoy and last updated on 3/18/18
#This code was written for the ENVS 117 Final Project which examines e-scooter
#data in San Jose. The page is split up into sections on importing and 
#cleaning the data, then into sections on which part of the research project
#is being addressed

##############################################################
#               Dependencies and Set up                      #
##############################################################

#Importing required packages---------------------------------
library(sf)
library(jsonlite)
library(tidyverse)
library(tmap)
library(lwgeom)
library(tmaptools)
library(tigris)
library(tidycensus)
library(shinyjs)
library(leaflet)
library(ggplot2)
library(shiny)
library(dplyr)
library(maps)
library(ggthemes)
library(transformr)
library(gganimate)
library(lubridate)
library(units)
library(htmlwidgets)

#setting working directory-----------------------------------
setwd("Z:/GIS_project_scripts")

##############################################################
#  Downloading and Preparing SJ City Data                    #
##############################################################


#Downloading and Preparing Zoning Data------------------------
#Downloading san jose city boundry shapefile
cities <- st_read("Cities2015.shp")
san_jose <- filter(cities, NAME == "San Jose")[2,]
#Downloading san jose city zoning shapefile
sj_zoning <- st_read("ZONING.shp")
sj_zoning <- st_transform(sj_zoning, crs = st_crs(san_jose))

#Downloading and Preparing Census Data------------------------
census_api_key("d5ce31aacbd309fcce494f105239ca1c99d3488f")
v17 <- load_variables(2017, "acs5", cache = TRUE)
medianHouseholdIncome <- get_acs(geography = "block group", 
                                 state = "CA", 
                                 county = "Santa Clara", 
                                 variable = "B19013_001", 
                                 output = "wide", 
                                 geometry = TRUE)
#setting crs for consistency
medianHouseholdIncome <- st_transform(medianHouseholdIncome, crs = st_crs(san_jose))
#cropping spatial extent to only San Jose
medianHouseholdIncome <- st_crop(medianHouseholdIncome, san_jose)

#Downloading and Preparing BART Station Data--------------------
rail_stations <- st_read("Rail_Sta_13.shp")
#setting crs for consistency
rail_stations <- st_transform(rail_stations, crs = st_crs(san_jose))
#cropping spatial extent to only San Jose
sj_rail_stations <- st_intersection(rail_stations, san_jose)

#Downloading and Preparing Road Data----------------------------
roads <- st_read("Trknet2018.shp")
#setting crs for consistency
roads <- st_transform(roads, crs = st_crs(san_jose))
#cropping spatial extent to only San Jose
sj_roads <- st_intersection(roads, san_jose)

#saving cleaned version of files-------------------------------
save(san_jose, file = "san_jose.RData")
save(sj_zoning, file = "sj_zoning.RData")
save(sj_rail_stations, file =  "sj_rail_stations.RData")
save(medianHouseholdIncome, file =  "medianHouseholdIncome.RData")
save(sj_roads, file =  "sj_roads.RData")

##############################################################
#        Downloading and Preparing Scooter Json Files        #
##############################################################

#Declaring a utility function for reading in many files----------
#function conbines a particular amount (bounds) of json files with the same name (string)
#returns a data frame of scooters
create_frame <- function(string,bounds){
  i = 1
  ret_frame <- (fromJSON(paste(string,i,".json",sep = ""), flatten=TRUE)$vehicles)
  i = i + 1
  while(i <= bounds){
    ret_frame <- rbind(ret_frame,(fromJSON(paste(string,i,".json",sep = ""), flatten=TRUE)$vehicles))
    i = i + 1
  }
  return(ret_frame)
}

#Creating Data Frame for 3/8/18-------------------------------------
#creating a data frame for 3/8/18 8:30am
string_3_8_830 = "vehicles3.8.830."
bounds_3_8_830 = 38
scooters_3_8_830 = cbind(create_frame(string_3_8_830,bounds_3_8_830),data.frame("Time" = "8:30am"))
#creating a data frame for 3/8/18 12:00pm
string_3_8_12 = "vehicles3.8.12."
bounds_3_8_12 = 30
scooters_3_8_12 = cbind(create_frame(string_3_8_12,bounds_3_8_12),data.frame("Time" = "12:00pm"))
#creating a data frame for 3/8/18 5:00pm
string_3_8_5 = "vehicles.3.8.5."
bounds_3_8_5 = 38
scooters_3_8_5 = cbind(create_frame(string_3_8_5,bounds_3_8_5),data.frame("Time" = "5:00pm"))
#combining, geocoding, and limiting the spatial extent the data
all_scooters_3_8 = rbind(scooters_3_8_830,scooters_3_8_12,scooters_3_8_5)
all_scooters_3_8_sf <- st_as_sf(all_scooters_3_8, coords = c("longitude","latitude"), crs = st_crs(san_jose))
sj_all_scooters_3_8_sf <- st_intersection(all_scooters_3_8_sf, san_jose)

#Creating Data Frame for 3/16/18---------------------------------
#creating a data frame for 3/16/18 8:30am
string_3_16_830 = "vehicles3.16.830."
bounds_3_16_830 = 30 
scooters_3_16_830 = cbind(create_frame(string_3_16_830,bounds_3_16_830),data.frame("Time" = "8:30am"))
#creating a data frame for 3/16/18 12:00pm
string_3_16_12 = "vehicles3.16.12."
bounds_3_16_12 = 20
scooters_3_16_12 = cbind(create_frame(string_3_16_12,bounds_3_16_12),data.frame("Time" = "12:00pm"))
#creating a data frame for 3/16/18 5:00pm
string_3_16_5 = "vehicles3.16.5."
bounds_3_16_5 = 25
scooters_3_16_5 = cbind(create_frame(string_3_16_5,bounds_3_16_5),data.frame("Time" = "5:00pm"))
#combining, geocoding, and limiting the spatial extent the data
all_scooters_3_16 = rbind(scooters_3_16_830,scooters_3_16_12,scooters_3_16_5)
all_scooters_3_16_sf <- st_as_sf(all_scooters_3_16, coords = c("longitude","latitude"), crs = st_crs(san_jose))
sj_all_scooters_3_16_sf <- st_intersection(all_scooters_3_16_sf, san_jose)

#saving cleaned version of files----------------------------------
save(sj_all_scooters_3_8_sf,file = "sj_all_scooters_3_8_sf.RData")
save(sj_all_scooters_3_16_sf,file = "sj_all_scooters_3_16_sf.RData")

##############################################################
#           Investigating Scooter Movement                  #
##############################################################

#loading required data----------------------------------------
sj_all_scooters_3_8_sf <- get(load("sj_all_scooters_3_8_sf.RData"))
sj_all_scooters_3_16_sf <- get(load("sj_all_scooters_3_16_sf.RData"))
sj_roads <- get(load("sj_roads.RData"))
san_jose <- get(load("san_jose.RData"))

#creating animated maps of scooter movement-------------------
#creating basemap
sj <- ggplot() + geom_sf(data = san_jose,
                              fill = 'black',
                              color = 'grey') +
                 geom_sf(data = sj_roads,
                          fill = "black",
                          color= "white",
                          size = 0.01
                          ) + 
                 theme(axis.text.x = element_text(color = "white"),
                       axis.text.y = element_text(color = "white"),
                       axis.line = element_blank(),
                       panel.grid.major = element_line(color= "white"),
                       panel.grid.minor = element_line(color = "white"),
                       panel.border = element_blank(),
                       panel.background = element_blank(),
                       axis.ticks = element_blank())
sj
#creating map of 3/8 scooter data
map3_8 <- 
  sj +
  geom_sf(data = sj_all_scooters_3_8_sf,
             colour = 'dark grey',
             size = 0.2)+
  ylab(NULL) +
  xlab(NULL) 
#creating map of 3/16 scooter data
map3_16 <- 
  sj +
  geom_sf(data = sj_all_scooters_3_16_sf,
          colour = 'dark grey',
          size = 0.1)+
  ylab(NULL) +
  xlab(NULL)
#creating animation of 3/8 scooter data
anim3_8 <- map3_8 +
  transition_states(
    Time,
    transition_length = 2, 
    state_length = 4
  ) +
  ggtitle("Scooter Locations on 3/8/18") +
  labs(subtitle = "{closest_state}") +
  ease_aes('cubic-in-out') +
  theme(
    plot.title = element_text(color="black", size=20, face="bold"),
    plot.subtitle = element_text(color="black", size=14, face="bold"))
#creating animation of 3/16 scooter data
anim3_16 <- map3_16 +
  transition_states(
    Time,
    transition_length = 2, 
    state_length = 4
  ) +
  ggtitle("Scooter Locations on 3/16/18") +
  labs(subtitle = "{closest_state}") +
  ease_aes('cubic-in-out') +
  theme(
    plot.title = element_text(color="black", size=20, face="bold"),
    plot.subtitle = element_text(color="black", size=14, face="bold"))
#saving gifs
anim_save(file = "scooters3.8.gif",anim3_8,nframes = 150, width = 700, height = 700)
anim_save(file = "scooters3.16.gif",anim3_16,nframes = 150, width = 700, height = 700)

##############################################################
#        Investigating  "Last Mile Problem"                  #
##############################################################

#loading required data---------------------------------------
sj_all_scooters_3_8_sf <- get(load("sj_all_scooters_3_8_sf.RData"))
sj_rail_stations <- get(load("sj_rail_stations.RData"))

#creating interactive map-------------------------------------
LMP_interactive <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = sj_rail_stations, 
              col = "dark blue",
              fillOpacity = 0.75,
              radius = 20,
              group = "San Jose Rail Stations",
              popup = paste(sj_rail_stations$STATION," RAIL STATION")) %>%
  addMarkers(data = filter(sj_all_scooters_3_8_sf, Time == "8:30am"),
             clusterOptions = markerClusterOptions(), group = "Scooter Locations at 8:30am")%>% 
  addMarkers(data = filter(sj_all_scooters_3_8_sf, Time == "12:00pm"),
             clusterOptions = markerClusterOptions(), group = "Scooter Locations at 12:00pm")%>% 
  addMarkers(data = filter(sj_all_scooters_3_8_sf, Time == "5:00pm"),
             clusterOptions = markerClusterOptions(), group = "Scooter Locations at 5:00pm")%>% 
  addLayersControl(
    baseGroups = c("Scooter Locations at 8:30am", "Scooter Locations at 12:00pm", "Scooter Locations at 5:00pm"),
    options = layersControlOptions(collapsed = FALSE))
#saving html
saveWidget(LMP_interactive, file="LMP_interactive.html")

#creating bargraphs of scooter amounts------------------------
#converting crs to get proper units for buffer
ft_sj_rail_stations <- st_transform(sj_rail_stations, 2228)
ft_sj_all_scooters_3_8_sf <- st_transform(sj_all_scooters_3_8_sf, 2228)
#buffering stations by 1/4 mile
buf_sj_rail_stations <- st_buffer(ft_sj_rail_stations, set_units(set_units(0.25, mi), ft))
#intersecting scooters with buffered stations to measure scooters near stations
LMP_sj_scooters_3_8<- st_intersection(ft_sj_all_scooters_3_8_sf, buf_sj_rail_stations)
#graphing amount of scooters near stations over time
LMP_bars <- ggplot(LMP_sj_scooters_3_8, aes(Time)) +
  geom_bar(col = "white",
           fill="black", 
           size=.1) + 
  labs(title="Amount of Scooters Within 1/4 Mile of Rail Stations") +
  theme(
    plot.title = element_text(color="black", size=20, face="bold"),
    plot.subtitle = element_text(color="black", size=14, face="bold"),
    axis.title = element_text(color="black", size=12, face="bold"),
    axis.text = element_text(color="black", size=10)) +
  xlab("Time") +
  ylab("Number of Scooters")
LMP_bars

##############################################################
#         Investigating  Scooter Equity                      #
##############################################################

#loading required data----------------------------------------
sj_all_scooters_3_8_sf <- get(load("sj_all_scooters_3_8_sf.RData"))
medianHouseholdIncome <- get(load("medianHouseholdIncome.RData"))

#creating interactive map-------------------------------------
#setting color bins to display census income data
bins <- c(0,50000,100000,150000,200000,250000,Inf)
pal <- colorBin("Blues", domain = medianHouseholdIncome$B19013_001E, bins = bins)
#creating the interactive map
Equity_interactive <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = medianHouseholdIncome, 
              fillColor = ~pal(B19013_001E),
              fillOpacity = 0.75,
              weight = 0) %>%
  addMarkers(data = filter(sj_all_scooters_3_8_sf, Time == "8:30am"),
             clusterOptions = markerClusterOptions(), group = "Scooter Locations at 8:30am")%>% 
  addMarkers(data = filter(sj_all_scooters_3_8_sf, Time == "12:00pm"),
             clusterOptions = markerClusterOptions(), group = "Scooter Locations at 12:00pm")%>% 
  addMarkers(data = filter(sj_all_scooters_3_8_sf, Time == "5:00pm"),
             clusterOptions = markerClusterOptions(), group = "Scooter Locations at 5:00pm")%>% 
  addLegend(pal = pal, values = medianHouseholdIncome$B19013_001E, opacity = 0.7, title = "Median Household Income",
            position = "bottomright",
            labFormat = labelFormat(prefix = "$")) %>%
  addLayersControl(
    baseGroups = c("Scooter Locations at 8:30am", "Scooter Locations at 12:00pm", "Scooter Locations at 5:00pm"),
    options = layersControlOptions(collapsed = FALSE))
Equity_interactive
#saving html
saveWidget(Equity_interactive, file = "Equity_interactive.html")

#creating graph of scooter density-----------------------------
#joining scooters and census data on income
MHI_sj_all_scooters_3_8_sf <- st_join(sj_all_scooters_3_8_sf, medianHouseholdIncome)
#calculating total areas of each income level in sj
m_0<- sum(st_area(filter(medianHouseholdIncome, B19013_001E <= 50000)))
m_50<-sum(st_area(filter(medianHouseholdIncome, B19013_001E <= 100000 & B19013_001E > 50000)))
m_100<-sum(st_area(filter(medianHouseholdIncome, B19013_001E <= 150000 & B19013_001E > 100000)))
m_150<-sum(st_area(filter(medianHouseholdIncome, B19013_001E <= 200000 & B19013_001E > 150000)))
m_200<-sum(st_area(filter(medianHouseholdIncome, B19013_001E <= 250000 & B19013_001E > 200000)))
m_250<-sum(st_area(filter(medianHouseholdIncome, B19013_001E > 250000)))
#calculating number of scooters in each income level
num_scooters_0<- nrow(filter(MHI_sj_all_scooters_3_8_sf, B19013_001E <= 50000))
num_scooters_50<-nrow(filter(MHI_sj_all_scooters_3_8_sf, B19013_001E <= 100000 & B19013_001E > 50000))
num_scooters_100<-nrow(filter(MHI_sj_all_scooters_3_8_sf, B19013_001E <= 150000 & B19013_001E > 100000))
num_scooters_150<-nrow(filter(MHI_sj_all_scooters_3_8_sf, B19013_001E <= 200000 & B19013_001E > 150000))
num_scooters_200<-nrow(filter(MHI_sj_all_scooters_3_8_sf, B19013_001E <= 250000 & B19013_001E > 200000))
num_scooters_250<-nrow(filter(MHI_sj_all_scooters_3_8_sf, B19013_001E > 250000))
#calculating the density of scooters in each income level, multiplying by 1000,000 to convert m^2 to km^2
scooter_density_0 <- as.numeric(num_scooters_0/m_0) * 1000000
scooter_density_50 <- as.numeric(num_scooters_50/m_50)  * 1000000
scooter_density_100 <- as.numeric(num_scooters_100/m_100)  * 1000000
scooter_density_150 <- as.numeric(num_scooters_150/m_150)  * 1000000
scooter_density_200 <- as.numeric(num_scooters_200/m_200)  * 1000000
scooter_density_250 <- as.numeric(num_scooters_250/m_250)  * 1000000
#preping data to be graphed
income <- c("  $50,000 or Less"," $50,000-$100,000","$100,000-$150,000","$150,000-$200,000","$200,000-$250,000", "$250,000 or More")
density <- c(scooter_density_0,scooter_density_50,scooter_density_100,scooter_density_150,scooter_density_200,scooter_density_250)
scooter_density <- data.frame(density,income)
#plotting scooter density by income level
density_bars <- ggplot(scooter_density, aes(x=income, y= density)) +
  geom_bar(stat = "identity",
          col = "white",
           fill="black", 
           size=.1) + 
  labs(title="Density of Scooters by Income Level") +
  theme(
    plot.title = element_text(color="black", size=20, face="bold"),
    plot.subtitle = element_text(color="black", size=14, face="bold"),
    axis.title = element_text(color="black", size=12, face="bold"),
    axis.text = element_text(color="black", size=10)) +
  xlab("Income Level") +
  ylab("Number of Scooters per Square Kilometer")
density_bars

#creating animated histogram of scooter amounts-------------------------
MHI_bars <- ggplot(MHI_sj_all_scooters_3_8_sf, aes(B19013_001E)) +
    geom_histogram(bins= 5,
                   col= "white",
                 fill="black", 
                 size=.1) + 
    labs(title="Scooter Availabilty by Income Level", 
       subtitle="{closest_state}") +
    transition_states(
      Time,
      transition_length = 2, 
      state_length = 4
    ) +
   ease_aes('cubic-in-out') +
   theme(
      plot.title = element_text(color="black", size=20, face="bold"),
      plot.subtitle = element_text(color="black", size=14, face="bold"),
      axis.title = element_text(color="black", size=12, face="bold"),
      axis.text = element_text(color="black", size=10)) +
  xlab("Median Household Income") +
  ylab("Number of Scooters")
MHI_bars
#saving gif
anim_save(file= "MHI_bars.gif", MHI_bars)




##############################################################
#         Investigating  Scooters and Zoning                 #
##############################################################

#loading required data----------------------------------------
sj_all_scooters_3_8_sf <- get(load("sj_all_scooters_3_8_sf.RData"))
sj_zoning <- get(load("sj_zoning.RData"))
sj_roads <- get(load("sj_roads.RData"))
san_jose <- get(load("san_jose.RData"))

#Processing data to get broader categories for analysis----------
zoningna_sj_all_scooters_3_8_sf <- st_join(sj_all_scooters_3_8_sf, sj_zoning)
#dropping NA values, likely because these are scooters on roadways
zoning_sj_all_scooters_3_8_sf <- zoningna_sj_all_scooters_3_8_sf %>% drop_na(ZONING)
#grouping zoning by realted zones
scooters_zoned_other <- filter(zoning_sj_all_scooters_3_8_sf, ZONING == "Main Street Gro" | ZONING =="Public/Quasi-Pu" | ZONING =="Agriculture")
scooters_zoned_res <- filter(zoning_sj_all_scooters_3_8_sf, ZONING == "Two-Family Resi" | ZONING == "Planned Develop" | ZONING == "Multiple Reside" | ZONING =="Single-Family R" | ZONING =="Mobilehome Park")
scooters_zoned_com <-filter(zoning_sj_all_scooters_3_8_sf, ZONING == "Commercial Offi" | ZONING == "Commercial Gene" | ZONING =="Commercial Pede" | ZONING =="Downtown Primar" | ZONING =="Commercial Neig")
scooters_zoned_ind <- filter(zoning_sj_all_scooters_3_8_sf, ZONING == "Light Industria" | ZONING == "Heavy Industria" | ZONING =="Industrial Park")
scooters_zoned_other$Zone = "Other"
scooters_zoned_res$Zone = "Residential"
scooters_zoned_com$Zone = "Commercial"
scooters_zoned_ind$Zone = "Industrial"
scooters_zoned <- rbind(scooters_zoned_other,scooters_zoned_res,scooters_zoned_com,scooters_zoned_ind)

#creating bargraphs of scooter amounts-------------------------
zoning_bars <- ggplot(scooters_zoned, aes(Zone))  +
  geom_bar(col="white",
           fill = "black",
                 size=.1) + 
  labs(title="Scooters by Zone", 
       subtitle="{closest_state}")+
  transition_states(
    Time,
    transition_length = 2, 
    state_length = 4
  ) +
  ease_aes('cubic-in-out') +
  theme(
    plot.title = element_text(color="black", size=20, face="bold"),
    plot.subtitle = element_text(color="black", size=14, face="bold"),
    axis.title = element_text(color="black", size=12, face="bold"),
    axis.text = element_text(color="black", size=10)) +
  xlab("Zone") +
  ylab("Number of Scooters")
zoning_bars
#saving gif
anim_save(file= "zoning_bars.gif", zoning_bars)

