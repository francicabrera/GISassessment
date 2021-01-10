---
#   title: "GIS. Final Assessment"
# author: "Francina Cabrera Fermin"
# date: "11/01/2021"
# output: html_document
# ---
# 
# # How fast can medical assistance get to an emergency event in a congested city? 
# An exploratory analysis of the response times of ambulances in the Distrito Nacional, Dominican Republic.
# 
# ## INSTRUCTIONS
# 
# ## 1.	Introduction
# 
# This document offers the instructions to process, analyse and visualise data to examine the response times of the Emergency Medical Services (EMS) in a area of the capital of the Dominican Republic: the Distrito Nacional.
# 
# ## 2.	Description of the data
# 
# 1. Emergency cases collected by the Sistema Nacional de Atencion a Emergencias y Seguridad 9-1-1 of the Dominican Republic (.csv). This dataset only includes the health-related events solved during August 2019 that required the assistance of an ambulance in the Distrito Nacional and the surrounding provinces. To comply with the confidentiality of the information some fields were eliminated from the original data set.
# 2. Response Zones data of the Distrito National collected by the 9-1-1 EMS of the Dominican Republic (.shp). This dataset includes operational areas for health-related emergencies.
# 3. Ambulances prepositioning collected by the 9-1-1 EMS of the Dominican Republic (.csv). To comply with the confidentiality of the 
# 4. Dominican Republic's population counts per borough (.shp) collected in the 2010 Population Census by National Statistics Office of the Dominican Republic.
# 5. Distrito Nacional’s sub-boroughs (.shp) collected in the 2010 Population Census by National Statistics Office of the Dominican Republic.
# 6. Dominican Republic’s provinces (.shp) collected in the 2010 Population Census by National Statistics Office of the Dominican Republic. Downloaded from: https://www.one.gob.do/informaciones-cartograficas/shapefiles
# 7. For visualisation only:
# Dominican Republic’s roads collected by Open Street map and downloaded from https://download.geofabrik.de/central-america.html
# 
# 
# The data is stored in an online github repository: https://github.com/francicabrera/GISassessment
# ## 3. Instructions
# ### 3.1. Install packages and load libraries


#install.packages("mapboxapi", dependencies = TRUE)
library(mapboxapi)
library(here)
library(maptools)
library(sf)
library(tmap)
library(ggplot2)
library(plotly)
library(tidyverse)
library(grid)
library(data.table)
library(ggspatial)
library(ggsn)

### 3.1. Load and Prepare of the data

provinces <- st_read(here("Data","geo","ShapeFilesCenso2010",
                          "PROVCenso2010.shp")) %>% 
  # Project to EPSG:32619. This is the projected coordinate system for the Dominican Republic.
  st_transform(.,32619)

# Distrito Nacional shapefile
DistritoNacional <- provinces %>% 
  # Filter by the Distrito Nacional (DN)
  filter(., TOPONIMIA=="DISTRITO NACIONAL")
# Quick look of the feature
qtm(DistritoNacional)

# Borough shapefile
boroughs <- st_read(here("Data","geo","ShapeFilesCenso2010",
                         "POBL_NACIONAL_BP.shp")) %>%  
  # Project to EPSG:32619.
  st_transform(.,32619) %>% 
  #filter by the Distrito Nacional (DN)
  filter(., PROV=="01")
# Quick look of the feature
qtm(boroughs)

# Subboroughs of DN shapefile
subboroDN <- st_read(here("Data","geo","ShapeFilesCenso2010",
                          "SUBBARRIOS_DN.shp")) %>% 
  # Project to EPSG:32619
  st_transform(.,32619)
# Quick look of the feature
qtm(subboroDN)

# Response Emergency Zones
RespZones <- st_read(here("Data","geo","ResponseZones",
                          "SALUD_DN.shp"))%>% 
  # Project to EPSG:32619
  st_transform(.,32619)

# Quick look of the feature
qtm(RespZones)

# As we are going to work we an specific Response Zone, let's create the sf that will be used later
ZoneSA1 <- RespZones %>%
  dplyr::filter(str_detect(ZONA, "S-A1"))%>%
  # Project to EPSG:32619
  st_transform(., 32619)

# Roads shapefile
# We will use this only for visualisation
roads <- st_read(here("Data","geo","roads_OSM",
                      "gis_osm_roads_free_1.shp")) %>%
  # Project to EPSG:32619
  st_transform(.,32619)

# Clip Roads data to  provinces
#This can take a while.
RoadsProv <- roads[provinces,]

# Clip Roads data to DN 
RoadsDN <- roads[DistritoNacional,]

#Clip Roads to S-A1
RoadsA1 <- roads[ZoneSA1,]

# Emergency units allocations
# Read data from csv
EmerUnits<- read.csv("Data/Units.csv", 
                     header = TRUE, 
                     encoding = "latin1") %>% 
  # Create sf with Emergency Units allocations
  st_as_sf(., coords = c("Long", "Lat"),
           crs = 4326) %>%
  # Project to EPSG:32619
  st_transform(., 32619)

# Clip the sf data to DN
EmerUnitsDN <- EmerUnits[DistritoNacional,]

# Clip the sf data to S-A1
EmerUnitsSA1 <- EmerUnits[ZoneSA1,]

# 911 Cases from August 2019
# Read data from csv
Cases <- read.csv("Data/Cases.csv", 
                  header = TRUE, 
                  encoding = "latin1")
#Check the data
summary(Cases)

# Drop nas in longitude and latitude fields
Cases <- Cases %>%
  drop_na(longitude, latitude, dispatch_date_to_onscene_date_mins) %>% 
  # Remove any duplicate cases.
  distinct(., primary_unitid, zone, longitude, latitude, dispatch_date_to_onscene_date_mins)

summary(Cases)

# Add a column with the range of the response times.


#Response Times Range
Cases$RTrange <- cut(Cases$dispatch_date_to_onscene_date_mins,5,include.lowest=TRUE)

# Create sf with all Cases data
CasesAll <- Cases %>% 
  st_as_sf(., coords = c("longitude", "latitude"),
           crs = 4326) %>%
  # Project to EPSG:32619
  st_transform(., 32619)

#Clip cases data to DN.
CasesDN <- CasesAll[DistritoNacional,]

#Clip cases data to S-A1.
CasesA1 <- CasesAll[ZoneSA1,]

# Join the cases data with boroughs and subboroughs

#Boroughs 
boro_CasesDN <- boroughs %>% 
  st_join(CasesAll) %>% 
  group_by(., TOPONIMIA) %>%
  #add a count column to have the number of cases per borough
  count(count=n())

#Subboroughs
subboro_CasesDN <- subboroDN %>% 
  st_join(CasesAll)

#Subboroughs
subboro_CasesDN2 <- subboro_CasesDN %>%
  group_by(., COD_ONE) %>%
  #add a count column to have the number of cases per subborough
  count(count=n())

####Cases with high Response Time
#We will need also the cases with RT over 10 minutes.


CasesHRT <- CasesDN %>% 
  filter(dispatch_date_to_onscene_date_mins > 10.0)

#Clip cases data to DN.
CasesHRTDN <- CasesHRT[DistritoNacional,]

#Clip cases data to S-A1.
CasesHRTA1 <- CasesHRT[ZoneSA1,]

# Join cases with High RT to boroughs and subboroughs
#Boroughs 
boro_CasesHRTDN <- boroughs %>% 
  st_join(CasesHRT) %>% 
  group_by(., CODIGO) %>%
  #add a count column to have the number of cases per borough
  count(count=n())

#Subboroughs
subboro_CasesHRTDN <- subboroDN %>% 
  st_join(CasesHRT)

### 3.2. Descriptive Statistics

# Obtain the descriptive statistics for the population and cases. Use the data from the boroughs and cases. 


# Population in Distrito Nacional
sum(boroughs$pobl)
mean(boroughs$pobl)
sd(boroughs$pobl)

# Population in Zone S-A1
sum(boroughs[ZoneSA1,]$pobl)
mean(boroughs[ZoneSA1,]$pobl)
sd(boroughs[ZoneSA1,]$pobl)

# Cases in Distrito Nacional
sum(boro_CasesDN$count)
mean(boro_CasesDN$count)
sd(boro_CasesDN$count)

# Cases with HRT in Distrito Nacional
sum(boro_CasesHRTDN$count)
mean(boro_CasesHRTDN$count)
sd(boro_CasesHRTDN$count)


### 3.3. Exploration of the data

# Create a choropleth map with the ranges of RT by subborough (to obtain a more precise visualisation) to observe the distribution of the cases.
# Overlap the population density by borough.

# Modify the subboroughs of the DN with the density for Response Times (RTs)
subboro_CasesDN<- subboro_CasesDN %>%                    
  group_by(COD_ONE) %>%         
  summarise(density = first(RTrange),
            SUBboro= first(NOM_SUB))

# Plot
Map1 <- tm_shape(RespZones) +
  tm_polygons(col = NA,
              alpha = 0.1,
              border.col = "#8856a7",
              lwd = 1.5)+
  tm_shape(subboro_CasesDN) +
  tm_polygons("density",
              style="jenks",
              palette="GnBu",
              border.col = "white",
              lwd=0.5,
              midpoint=NA,
              title="Response Times (minutes) \nper subborough",
              legend.hist=TRUE,
              legend.is.portrait=FALSE)+
  tm_shape(boroughs) +
  tm_bubbles("pobl", 
             "#8c96c6",
             alpha = 0.7,
             border.col = "white", 
             border.lwd=0.1, 
             size.lim = c(0, 60000), 
             breaks=c(-Inf, seq(0, 6, by=2), Inf),
             title.size="Population per borough",
             legend.size.is.portrait = FALSE) +
  #Roads are used for visualisation purpose. 
  #It can take a while to run. Un-comment to plot it.
  # tm_shape(RoadsProv)+ 
  # tm_lines(lwd=0.1,
  #          palette = "Greys",
  #          legend.show = FALSE)+
  tm_shape(boroughs)+
  tm_polygons(col="white",
              alpha = 0.1,
              border.col = "#8c96c6",
              lwd=1)+
  tm_shape(RespZones) +
  tm_polygons(col = "ZONA",
              palette = "Greys",
              alpha = 0.05,
              border.col = "#8856a7",
              lwd = 1.5,
              title="Response Zones",
              legend.is.portrait=FALSE)+
  tm_text("ZONA",
          col = "#8856a7") +
  tm_compass(north = 0,
             type = "arrow",
             text.size = 0.8,
             show.labels = 1,
             cardinal.directions = c("N", "E", "S", "W"),
             lwd = 1, 
             position = c("right","top"),
             color.light="#f0f0f0",
             color.dark="#636363",
             text.color="#636363")+
  tm_scale_bar(position=c("left", "bottom"),
               color.light="#f0f0f0",
               color.dark="#636363",
               text.color="#636363") +
  tm_layout(inner.margin=c(0.1,0.04,0.04,0.04),
            legend.outside=TRUE,
            legend.outside.position = "right",
            legend.text.size = 0.5,
            legend.height = 0.5)

# Inset Map of the studied area
inset <- tm_shape(provinces) +
  tm_polygons(col = "#cccccc",
              border.col = "white",
              lwd=0.5)+
  tm_layout(frame=FALSE,
            bg.color = "transparent")+
  tm_shape(DistritoNacional) +
  tm_polygons(col = "#8856a7") +
  tm_text("TOPONIMIA", 
          col = "black",
          xmod=0.8,
          ymod=-0.5,
          size = 0.32)

# This will save the map in the same path as the project.
tmap_save(Map1,
          insets_tm = inset,
          insets_vp=viewport(0.35, 0.22, width = 0.15, height = 0.15),
          filename="Map1.png",
          dpi=600)


### 3.3.1 Point density analysis


# Point Density Visualisation of Cases with High RTs
# Create new sf with CRS 4326 and X and Y variables for Cases in Zone S-A1
CasesHRTA1_pd <- CasesHRTA1 %>%
  st_transform(4326) %>% # transform to same CRS 
  cbind(st_coordinates(.)) # get X and Y coordinates

# Create new sf with CRS 4326 and X and Y variables for Cases in Zone S-A1
EmerUnitsA1_pd <- EmerUnitsSA1 %>%
  st_transform(4326) %>% # transform to same CRS 
  cbind(st_coordinates(.)) # get X and Y coordinates


# Plot
Map2 <- ggplot() +
  geom_bin2d(data = CasesHRTA1_pd,
             aes(X, Y),
             binwidth = c(0.005, 0.005)) + 
  geom_sf(data = RoadsA1,
          col="#636363",
          size=0.07) +
  theme_minimal() +
  scale_fill_distiller(palette = "GnBu",
                       direction = 1, 
                       name= "Count of cases with High RTs")+
  theme(legend.position="bottom")+
  labs(x="",
       y="")+
  geom_sf(data = EmerUnitsA1_pd,
          col="#8856a7",
          size=1,)+ 
  geom_text(data = EmerUnitsA1_pd,
            aes(X, Y, label = Ficha),
            colour = "#404040",
            size=3,
            vjust = -1)+
  annotation_scale(bar_cols=c("#f0f0f0","#636363"))+
  north(data=CasesHRTA1_pd,
        location= "bottomright",
        symbol = 3)



Map2


# This will save the map in the same path as the project.
ggsave("Map2.png",
       plot=Map2,
       dpi = 600)


### 3.3.2 Cases with RT > 10 mins in A1


# Change name of column 'primary_unitid' to 'Ficha'.
CasesHRTA1 <- CasesHRTA1 %>% 
  rename(Ficha="primary_unitid")


##Cases in A1
CasesinA1 <-  CasesHRTA1 %>% 
  filter(Ficha %in% EmerUnitsSA1$Ficha) #filter by emergency units assigned to S-A1


#Subset cases not in A1
CasesNOTA1 <- subset(CasesHRTA1, !(Ficha %in% EmerUnitsSA1$Ficha)) #filter by emergency units not assigned to S-A1


# Plot
tmap_mode("plot")
Map3 <- tm_shape(RoadsA1) +
  tm_lines(lwd=0.2) +
  #Roads are used for visualisation purpose. 
  #It can take a while to run. Un-comment to plot it.
  # tm_shape(RoadsProv %>% filter(fclass == "primary"|fclass == "residential")) +
  # tm_lines(lwd=0.1) +
  tm_shape(ZoneSA1)+
  tm_fill(col = "white",
          alpha = 0.3,
          border.lwd = 0.2,
          border.col = "#8856a7",
          show.legend = FALSE)+
  tm_shape(provinces) +
  tm_polygons(col = "white",
              border.lwd = 0.2,
              border.col = "#737373",
              alpha = 0.1)+
  tm_text("TOPONIMIA",
          col = "#737373",
          size = 0.7) +
  tm_shape(CasesinA1) +
  tm_dots(col = "#a8ddb5")+
  tm_add_legend(title="Emergency Cases > 10 mins",
                type = "symbol",
                labels = "Solved by emergency units from S-A1",
                col = "#a8ddb5")+
  tm_shape(CasesNOTA1) +
  tm_dots(col = "#43a2ca",
          size = 0.1)+
  tm_add_legend(type = "symbol",
                labels = "Solved by emergency units not from S-A1",
                col = "#43a2ca")+
  tm_shape(ZoneSA1) +
  tm_polygons(col = "ZONA",
              palette = "white",
              border.lwd = 0.2,
              border.col = "#8856a7",
              alpha = 0.1,
              title="Response Zone")+
  tm_text("ZONA",
          col = "#8856a7",
          size = 0.9)+
  tm_layout(outer.margins = 0, 
            panel.label.bg.color = 'white',
            panel.label.size = 1.2,
            legend.outside=TRUE,
            legend.outside.position = "right",
            title.snap.to.legend = T)+
  tm_compass(north = 0,
             type = "arrow",
             text.size = 0.8,
             show.labels = 1,
             cardinal.directions = c("N", "E", "S", "W"),
             lwd = 1, 
             position = c("right","top"),
             color.light="#f0f0f0",
             color.dark="#636363",
             text.color="#636363")+
  tm_scale_bar(position=c("left", "bottom"),
               color.light="#f0f0f0",
               color.dark="#636363",
               text.color="#636363")
Map3


# Save the plot
tmap_save(Map3,
          filename="Map3.png",
          dpi=600)

## 4. Service area analysis of emergency units in zones S-A1
# 
# To run this part of the analysis you must create an account on Mapbox and use your token:
#   https://www.mapbox.com
# 
# This part of the analysis uses code from this source: https://npalomin.github.io/CASA_seminar_2020/casa0005_seminar1.html
# 
# Check also the Mapbox documentation for more information:
#   https://docs.mapbox.com/api/navigation/isochrone/
  
  

# Mapbox token
 my_token <- "INSERT YOUR MAPBOX TOKEN HERE"

# Count High RTs cases by Emergency Units
HRTperEU <- as.data.frame(table(CasesHRTA1$Ficha)) %>% 
  drop_na(Var1) %>% 
  rename(Ficha="Var1")


# Merge to obtain the count of High RTs cases by Emergency Units
EmerUnitsSA1 <- EmerUnitsSA1 %>%
  merge(.,HRTperEU) %>% 
  rowid_to_column(., "sid") #Create a unique id variable "sid"


# Isochrones (for 8 and 10 minutes) for S- A1 Emergency Units 
D_isoA1 <- mb_isochrone(EmerUnitsSA1, 
                        "driving", 
                        time = c(8,10), 
                        id_column = "sid", 
                        access_token = my_token)


# Get variables from EmerUnits sf to join with isochrones
euHRTsA1_m <- EmerUnitsSA1 %>%
  st_drop_geometry()


# Join
D_isoA1 <- D_isoA1 %>%
  merge(., euHRTsA1_m,
        by.x="id", 
        by.y="sid")


qtm(D_isoA1)


Map4.1 <- tm_shape(D_isoA1) +
  tm_fill(col = "time",
          palette="GnBu",
          alpha = 0.9,
          title="Drive time Isochrones (mins)",
          legend.is.portrait = F) +
  tm_facets(by="time",
            ncol = 2) +
  #Roads are used for visualisation purpose. 
  #It can take a while to run. Un-comment to plot it.
  # tm_shape(RoadsProv %>% filter(fclass == "primary"|fclass == "residential")) +
  # tm_lines(lwd=0.1) +
  tm_shape(RoadsA1) +
  tm_lines(lwd=0.2) +
  tm_shape(provinces) +
  tm_polygons(col = "white",
              border.lwd = 0.2,
              border.col = "#737373",
              alpha = 0.1)+
  tm_text("TOPONIMIA",
          col = "#737373",
          size = 0.7) +
  tm_shape(ZoneSA1) +
  tm_polygons(col = "ZONA",
              palette = "white",
              border.lwd = 0.2,
              border.col = "#8856a7",
              alpha = 0.1,
              title="Response Zone")+
  tm_text("ZONA",
          col = "#8856a7",
          size = 0.5)+
  tm_shape(EmerUnitsSA1) +
  tm_dots(col = "Ficha",
          palette = "#8856a7",
          size = 0.3,
          border.col = "white",
          border.lwd = 0.5,
          legend.show = FALSE)+
  tm_add_legend(title="Emergency Unit allocation",
                type = "symbol",
                labels = " Emergency Unit",
                col = "#8856a7")+
  tm_layout(outer.margins = 0, 
            panel.label.bg.color = 'white',
            panel.label.size = 1.2,
            legend.outside=TRUE,
            legend.outside.position = "bottom",
            title.snap.to.legend = T)+
  tm_compass(north = 0,
             type = "arrow",
             text.size = 0.8,
             show.labels = 1,
             cardinal.directions = c("N", "E", "S", "W"),
             lwd = 1, 
             position = c("right","top"),
             color.light="#f0f0f0",
             color.dark="#636363",
             text.color="#636363")+
  tm_scale_bar(position=c("left", "bottom"),
               color.light="#f0f0f0",
               color.dark="#636363",
               text.color="#636363")


# This will save the map in the same path as the project.
tmap_save(Map4.1,
          filename="Map4.1.png",
          dpi=600)
Map4.1


### 4.1.Analysis of the service area.
#In this part will analyse how many events with high RT were reachable by the ambulances in 8-10 minutes.

# Join cases with isochrones
CasesHRTA1b <- CasesHRTA1 %>% 
  st_transform(4326) %>% # 1. project to the same crs as the isochrones
  st_join(., D_isoA1) %>% # 2. Join
  na.omit()

# Count the cases by the common field (Ficha) to get the number of 
# events by ambulances
CasesHRTA1bcount <- CasesHRTA1b %>% 
  group_by(id, Ficha.y) %>%
  summarise(n=n())

CasesHRTA1bcount

# Here we create a single point per ambulance.
CasesHRTA1bcount_centroid <- st_centroid(CasesHRTA1bcount)

# Plot the result
Map4.2 <- tm_shape(RoadsA1) +
  tm_lines(lwd=0.2) +
  #Roads are used for visualisation purpose. 
  #It can take a while to run. Un-comment to plot it.
  # tm_shape(RoadsProv %>% filter(fclass == "primary"|fclass == "residential")) +
  # tm_lines(lwd=0.1) +
  tm_shape(ZoneSA1)+
  tm_fill(col = "white",
          alpha = 0.3,
          border.lwd = 0.2,
          border.col = "#8856a7",
          show.legend = FALSE)+
  tm_shape(provinces) +
  tm_polygons(col = "white",
              border.lwd = 0.2,
              border.col = "#737373",
              alpha = 0.1)+
  tm_text("TOPONIMIA",
          col = "#737373",
          size = 0.7) +
  tm_shape(ZoneSA1) +
  tm_polygons(col = "ZONA",
              palette = "white",
              border.lwd = 0.2,
              border.col = "#8856a7",
              alpha = 0.1,
              title="Response Zone")+
  tm_text("ZONA",
          col = "#8856a7",
          size = 0.8,
          ymod = -0.6)+
  tm_shape(CasesHRTA1bcount_centroid) +
  tm_bubbles(col = "#8856a7",
             size = "n",
             alpha = 0.4,
             border.lwd = 0.3,
             style = "kmeans",
             title.size="Count of cases \nreachable by S-A1 emergency units") +
  tm_text("Ficha.y",
          col = "black",
          size = 0.5,
          ymod = -0.6) +
  tm_layout(outer.margins = 0, 
            panel.label.bg.color = 'white',
            panel.label.size = 1.2,
            legend.outside=TRUE,
            legend.outside.position = "right",
            title.snap.to.legend = T)+
  tm_compass(north = 0,
             type = "arrow",
             text.size = 0.8,
             show.labels = 1,
             cardinal.directions = c("N", "E", "S", "W"),
             lwd = 1, 
             position = c("right","top"),
             color.light="#f0f0f0",
             color.dark="#636363",
             text.color="#636363")+
  tm_scale_bar(position=c("left", "bottom"),
               color.light="#f0f0f0",
               color.dark="#636363",
               text.color="#636363")

# This will save the map in the same path as the project.
tmap_save(Map4.2,
          filename="Map4.2.png",
          dpi=600)
Map4.2


### 4.3.Isochrones for emergency units with more cases of high RT.


# Ambulances with Highest RTs
euHRTs <- EmerUnitsSA1 %>%
  slice_max(EmerUnitsSA1$Freq, n = 4)


# Isochrones (for 8 and 10 minutes) for Emergency Units with most cases with highest RTs
D_isoA1b <- mb_isochrone(euHRTs, 
                         "driving", 
                         time = c(8,10), 
                         id_column = "sid", 
                         access_token = my_token)


# Get variables from EmerUnits sf to join with isochrones
euHRTs_mb <- euHRTs %>%
  st_drop_geometry()


# Join
D_isoA1b <- D_isoA1b %>%
  merge(., euHRTs_mb,
        by.x="id", 
        by.y="sid")


# Change name of column 'primary_unitid' to 'Ficha'
CasesDNA1b <- CasesA1 %>% 
  rename(Ficha="primary_unitid") %>% 
  #Filter cases by Emergency Units with most cases with highest response times
  filter(Ficha %in% euHRTs$Ficha)


# Plot
tmap_mode("plot")
Map4.3 <- tm_shape(D_isoA1b) +
  tm_polygons(col = "time",
              palette="GnBu",
              border.lwd = 0.3,
              border.col = "white",
              alpha = 0.9,
              title="Drive time Isochrones (mins)",
              legend.is.portrait = F) +
  tm_facets(by="Ficha",
            ncol = 2) +
  tm_shape(RoadsA1) +
  tm_lines(lwd=0.2) +
  tm_shape(ZoneSA1) +
  tm_polygons(col = "ZONA",
              palette = "white",
              border.lwd = 0.2,
              border.col = "#8856a7",
              alpha = 0.1,
              title="Response Zone")+
  tm_shape(CasesDNA1b) +
  tm_dots(col = "RTrange",
          palette = "#e34a33",
          legend.show = FALSE)+
  tm_facets(by="Ficha", 
            ncol = 2)+
  tm_add_legend(title="Emergency Cases",
                type = "symbol",
                labels = " > 10 mins",
                col = "#e34a33")+
  tm_shape(euHRTs) +
  tm_dots(col = "Ficha",
          palette = "#8856a7",
          size = 0.5,
          border.col = "white",
          border.lwd = 0.5,
          legend.show = FALSE)+
  tm_add_legend(title="Emergency Unit allocation",
                type = "symbol",
                labels = " Emergency Unit",
                col = "#8856a7")+
  tm_facets(by="Ficha",
            ncol = 2)+
  tm_layout(outer.margins = 0, 
            panel.label.bg.color = 'white',
            panel.label.size = 1.2,
            legend.outside=TRUE,
            legend.outside.position = "right",
            title.snap.to.legend = T)+
  tm_compass(north = 0,
             type = "arrow",
             text.size = 0.8,
             show.labels = 1,
             cardinal.directions = c("N", "E", "S", "W"),
             lwd = 1, 
             position = c("right","top"),
             color.light="#f0f0f0",
             color.dark="#636363",
             text.color="#636363")+
  tm_scale_bar(position=c("left", "bottom"),
               color.light="#f0f0f0",
               color.dark="#636363",
               text.color="#636363")
Map4.3


# Save the plot
tmap_save(Map4.3,
          filename="Map4.3.png",
          dpi=600)

# Events that fall inside the Isochrones. 
# Now we will count the events whit high RT that fall inside the calculated isochrones.

# Project to EPSG:32619.
D_isoA1c  <- D_isoA1b %>%
  st_transform(., 32619)



Cases_inIso <- CasesDNA1b[D_isoA1c,] %>% 
  group_by(., Ficha) %>% 
  count(count=n())

Cases_inIso

## 5. Optimal Route and Estimated Time of Arrival (ETA)

# Here, we will calculate a route from a random Emergency Unit Allocation (from the 4 analysed before) to a random Emergency Case that had a high RT. The result will be a sf with a route, directions and ETA.
# 
# To run this part of the analysis you will use your Mapbox token. Go to this link to create it:https://www.mapbox.com
# 
# Check also the Mapbox documentation for more information:
#   https://docs.mapbox.com/api/navigation/directions/
  
  
# Create new data frame for cases without geometry
CasesHRTA15 <- CasesHRTA1 %>% 
  #Coordinates must be in WGS84
  st_transform(4326) %>% 
  #Obtain XY coordinates
  cbind(st_coordinates(.)) %>% 
  #Drop the geometry
  st_drop_geometry()


# Create new data frame for Emergency Units without geometry
euHRTA15 <- euHRTs %>% 
  #Coordinates must be in WGS84
  st_transform(4326) %>% 
  #Obtain XY coordinates
  cbind(st_coordinates(.)) %>% 
  #Drop the geometry
  st_drop_geometry()


# Dataframe with origin and destination coordinates
dfCasesHRTmerge <- merge(CasesHRTA15,euHRTA15,by="Ficha")


# Create an object to select a random row from the data frame
Random_index <- floor(runif(1,min = 0,max = nrow(dfCasesHRTmerge)))


# Define the Origin and destination list from the data frame
Origin <- list(toString(dfCasesHRTmerge[Random_index,]['X.y']),toString(dfCasesHRTmerge[Random_index,]['Y.y']))
Destination <- list(toString(dfCasesHRTmerge[Random_index,]['X.x']),toString(dfCasesHRTmerge[Random_index,]['Y.x']))


# Create the optimal route with the Mapbox Api
D_dir <- mb_directions(origin = Origin,
                       destination = Destination,
                       #set the profile to "driving-traffic" 
                       #this consults traffic data from the Mapbox API
                       profile="driving-traffic",
                       output = "sf",
                       access_token = my_token,
                       steps=TRUE)


# Let's have a look at the results
D_dir


# Obtain the time (minutes) and distance (kilometers) of the trip
sum(D_dir$duration)
sum(D_dir$distance)


# Create an sf for the Origin and Destination data frame
Origin_sf <- as.data.frame(Origin, col.names = c("longitude", "latitude")) %>% 
  data.frame(., type = "Origin") %>% 
  st_as_sf(., coords = c("longitude", "latitude"), crs = 4326)


Destination_sf <- as.data.frame(Destination, col.names = c("longitude", "latitude")) %>%
  data.frame(., type = "Destination") %>% 
  st_as_sf(., coords = c("longitude", "latitude"), crs = 4326)


#Combine both sf into one.
OriDes <- rbind(Origin_sf,Destination_sf)


# Plot
tmap_mode("plot")
Map5 <- tm_shape(D_dir)+
  tm_lines(col = "#7bccc4",
           lwd=6)+
  tm_add_legend(title="Leyend",
                type = "line",
                labels = " Route",
                col = "#7bccc4",
                lwd = 2)+
  tm_shape(boroughs)+
  tm_polygons(col = "#f0f0f0")+
  tm_text("TOPONIMIA",
          size = 0.8, 
          col = "#969696")+
  tm_shape(D_dir)+
  tm_lines(col = "#7bccc4",
           lwd=6)+
  tm_shape(RoadsA1 %>% filter(fclass == "primary"))+
  tm_lines(col = "white",
           lwd=0.6)+
  tm_shape(RoadsA1)+
  tm_lines(lwd=0.4,
           palette = "white",
           legend.show = FALSE)+
  tm_shape(OriDes)+
  tm_dots(col = "type",
          palette = "BuPu",
          size = 1,
          title = "Type")+
  tm_compass(north = 0,
             type = "arrow",
             text.size = 0.8,
             show.labels = 1,
             cardinal.directions = c("N", "E", "S", "W"),
             lwd = 1, 
             position = c("right","top"),
             color.light="#f0f0f0",
             color.dark="#636363",
             text.color="#636363",
             size = 3)+
  tm_scale_bar(position=c("right", "bottom"),
               color.light="#f0f0f0",
               color.dark="#636363",
               text.color="#636363")+
  tm_layout(legend.position = c("left", "bottom"),
            legend.bg.color = "white",
            legend.bg.alpha	= 0.7,
            legend.width = 0.6,
            title.snap.to.legend = T,
            inner.margin=c(0.12,0.10,0.10,0.12))
Map5


# Save the plot
tmap_save(Map5,
          filename="Map5.png",
          dpi=600)

# This is the end of this document.


