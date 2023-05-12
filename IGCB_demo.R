#######################################################
#######################################################
# Project Name: IGCB GIS in R Workshop
#
# Script Name: Data Exercise
#
# Script Description:
#
# Author: Peter S. Larson
# Copyright (c) Peter S. Larson, 2023
# Email:  anfangen@umich.edu
#
# Date: 2023-05-11
#
#
# Notes:
#
#######################################################


### Resources #####

## A good introduction to tmap (and handbook) can be found here. It covers not only the package's
## functionality but also basic concepts of GIS and spatial analysis.
## https://r-tmap.github.io/tmap-book/geodata.html

## A nice text for spatial analysis in R is:
## Brundson and Comber 2nd edition
# https://www.amazon.com/Introduction-Spatial-Analysis-Mapping-Analytics/dp/1526428504/ref=pd_lpo_sccl_1/142-7213978-1499735?pd_rd_w=gzDI1&content-id=amzn1.sym.116f529c-aa4d-4763-b2b6-4d614ec7dc00&pf_rd_p=116f529c-aa4d-4763-b2b6-4d614ec7dc00&pf_rd_r=WJVNVK47KGSVDGWSMHSX&pd_rd_wg=hbLrG&pd_rd_r=5227deb7-30ba-424c-a2a3-3a67399baf11&pd_rd_i=1526428504&psc=1

## All of the code for the book is here:
## https://bookdown.org/lexcomber/brunsdoncomber2e/

## Data models (Vector (Points, lines, polygons), raster)
## Spatial representations (the sf revolution)
## Coordinate systems and projections

### GIS in R ####

#### Libraries ####

### Get the libraries we need
library(sf)
library(sp)
library(maptools)
library(tmap)
library(rgeos)
library(haven)
library(readr)
library(dplyr)
library(tidyverse)
library(tmaptools)
library(compareGroups)

#### Intro to tmap ####

### Tmap is really, really easy to use. Let's make a map of the world
library(tmap)
data("World")

### Make a basic map of the carbon footprint (I think) of each country around the world
tm_shape(World) +
  tm_polygons("footprint")

### We can change the way the colors are displayed
tm_shape(World) +
  tm_polygons("footprint", style="quantile")

### We can add a title to the legend
tm_shape(World) +
  tm_polygons("footprint", style="quantile", title="Carbon footprint") 

## We can add a title to the map
tm_shape(World) +
  tm_polygons("footprint", style="quantile", title="Carbon footprint") +
  tm_layout("The Carbon Footprint of the World")


## We can add a title to the map and put it outside
tm_shape(World) +
  tm_polygons("footprint", style = "quantile", title = "Carbon footprint") +
  tm_layout("The Carbon Footprint of the World",
            main.title.position = "center")

## We can create interactive maps 
tmap_mode("view")
tm_shape(World) +
  tm_polygons("footprint", style = "quantile", title = "Carbon footprint") +
  tm_layout("The Carbon Footprint of the World",
            main.title.position = "center")

## We can do different styles of maps and put maps into different panels
tmap_mode("view")
tm_shape(World) +
  tm_polygons(c("HPI", "economy")) +
  tm_facets(sync = TRUE, ncol = 2)

### More facet type maps
tmap_mode("plot")
## tmap mode set to plotting

data(NLD_muni)
NLD_muni$perc_men <- NLD_muni$pop_men / NLD_muni$population * 100

tm_shape(NLD_muni) +
  tm_polygons("perc_men", palette = "viridis", title = "Percent men",style = "quantile") +
  tm_facets(by = "province")

## Adding basemaps 
tmap_mode("view")
tm_basemap("Stamen.Watercolor") +
  tm_shape(World) +
  tm_polygons("footprint", style = "quantile", title = "Carbon footprint") +
  tm_layout("The Carbon Footprint of the World",
            main.title.position = "center")


## You can plot multiple things on the same map.
tmap_mode("plot")

data(World, metro, rivers, land)

tmap_mode("plot")
## tmap mode set to plotting
tm_shape(land) +
  tm_raster("elevation", palette = terrain.colors(10)) +
  tm_shape(World) +
  tm_borders("white", lwd = .5) +
  tm_text("iso_a3", size = "AREA") +
  tm_shape(metro) +
  tm_symbols(col = "red", size = "pop2020", scale = .5) +
  tm_legend(show = FALSE)


## save an image ("plot" mode)
foot <- tm_shape(World) +
  tm_polygons("footprint", style = "quantile", title = "Carbon footprint") +
  tm_layout("The Carbon Footprint of the World",
            main.title.position = "center")

tmap_save(foot, filename = "Footprint_map.png")

## save as stand-alone HTML file ("view" mode)
tmap_save(tm, filename = "world_map.html")


#### Part 3: putting things together ####

# Load in the malaria case data
ETH_malaria_data <-
  read.csv(
    "https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/course_materials/week1/Lab_files/Data/mal_data_eth_2009_no_dups.csv",
    header = T
  ) %>% # Case data
  dplyr::mutate(parasite_presence = ifelse(pf_pos > 0, "Positive", "Negative")) ## Create a binary outcome.

## Set coordinates
coordinates(ETH_malaria_data) <- ~ longitude + latitude

## convert to sf object
ETH_malaria_data <- st_as_sf(ETH_malaria_data)

## Get populationss
pops <- read_csv("GIS_data/Ethiopia/admin/eth_pops.csv")

## Get some admin boundaries and population at the same time.
ETH_Adm_2 <-
  st_as_sf(raster::shapefile("GIS_data/Ethiopia/Admin/Ethiopia_adm2_uscb_2020.shp")) %>% # get admin boundaries and make it an sf object at the same time
  dplyr::left_join(pops %>% dplyr::select(GEO_MATCH, BTOTL_2000)) %>% #join the population data to this using GEO_MATCH as a common identifier
  dplyr::rename(pops = BTOTL_2000) ## rename BTOTL_2000 to pops, just ot make it easier for us to use

## Set these so they are the same crs, really not necessary to make a map, but necessary for the data extraction step.
st_crs(ETH_malaria_data) <- st_crs(ETH_Adm_2)

## Make a nice map to show ourselves that we aren't crazy
tm_shape(ETH_Adm_2) +
  tm_polygons(
    "pops",
    title = "Population",
    palette = get_brewer_pal(palette = "Greys", n = 5, plot = FALSE),
    alpha = .5
  ) +
  tm_shape(ETH_malaria_data) +
  tm_symbols(
    "parasite_presence",
    size = .25,
    border.col = "black",
    border.lwd = 1,
    palette = c("white", "red"),
    title.size = "Parasite presence"
  ) +
  tm_layout(main.title = "Malaria data set") +
  tm_credits("World Bank, etc")

#### Get some other data that we can works with ###

# Remember, we want to try and test for associations between spatial risk factors and disease.

# To do this, we have to get some data from other GIS layers.

# We've already got data for population from the polygon data.
# Let's also get a line file for rivers and an elevation raster.

## Elevation raster
elev <-
  raster::raster("GIS_data/Ethiopia/Elevation/ETH_msk_alt.gri")

## Rivers (lines)
rivers <-
  raster::shapefile("GIS_data/Ethiopia/Rivers/ETH_water_lines_dcw.shp")
rivers <- st_as_sf(rivers)

## Let's plot them so we can convince ourselves that we have the correct data
tm_shape(elev) +
  tm_raster(
    title = "Elevation",
    palette = get_brewer_pal(palette = "Greys", n = 5, plot = FALSE),
    legend.hist = TRUE
  ) +
  tm_shape(rivers, alpha = .5) +
  tm_lines("gray39") +
  tm_layout(main.title = "Elevation and rivers in Ethiopia", legend.outside = TRUE)



#### Data extraction ####

## Now let's do some data extraction. We'll be doing two things:
## 1) Extracting population to points (polygons to points, every point will get the feature infor for each polygon it sits in)
## 2) Getting distance to the nearest river (distance from point to nearest lines).

## Population

## Get population numbers for each point, actually get all of the data from the population/adm 2 data set. We can do this using st_intersection()
## Polygon to points
ETH_malaria_data <- st_intersection(ETH_Adm_2, ETH_malaria_data)

## Let's look at it to see if it worked.
head(ETH_malaria_data)

## Looks like it did. Now let's get the other variables.

## Elevation
ETH_malaria_data$elevation <-
  raster::extract(elev, ETH_malaria_data)

## Get distance to river
distances <- st_distance(ETH_malaria_data, rivers) * 0.001
ETH_malaria_data$distance_to_river <-
  (as.vector(apply(distances, 1, min)))

## Now let's look at a summary of the data
summary(ETH_malaria_data)




#### Regression models ###

## Our goal is to test for associations between presence of malaria and population, elevation and distance to river.

## We can do that using a regression model as before. Plus, we'll make some cool plots.

## Our outcome is binary, which means we can't use a regular scatterplot. We'll get a little fancy.

## First we have to convert the parasite presence to a number
ETH_malaria_data <- ETH_malaria_data %>%
  dplyr::mutate(parasite_01 = ifelse(parasite_presence == "Positive", 1, 0))

ggplot(data = ETH_malaria_data, aes(x = pops, y = parasite_01)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Malaria and population") +
  xlab("Population") +
  ylab("Parasite presence")

ggplot(data = ETH_malaria_data, aes(x = elevation, y = parasite_01)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Malaria and elevation") +
  xlab("Elevation") +
  ylab("Parasite presence")

ggplot(data = ETH_malaria_data, aes(x = distance_to_river, y = parasite_01)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Malaria and distance to river") +
  xlab("Distance to river") +
  ylab("Parasite presence")

### We'll also use compareGroups to get a nice table
table_malaria <-
  compareGroups(parasite_presence ~ pops + elevation + distance_to_river,
                data = as.data.frame(ETH_malaria_data))
table_malaria <-
  createTable(
    table_malaria,
    show.p.overall = FALSE,
    show.ratio = TRUE,
    show.all = TRUE
  )
export2md(table_malaria)

## Not very convincing, but we'll make a model anyway
fit_malaria <-
  glm(parasite_01 ~  pops + elevation + distance_to_river ,
      data = ETH_malaria_data,
      family = "binomial")
summary(fit_malaria)
par(mfrow = c(2, 2))
plot(fit_malaria)
par(mfrow = c(1, 1))

## Add back to our data set
ETH_malaria_data$predictions <-
  exp(predict(fit_malaria)) / (1 + exp(predict(fit_malaria)))
ETH_malaria_data$residuals <-
  fit_malaria$residuals - min(fit_malaria$residuals) ## Hack to get the plot to work. I will fix and upload a corrected version of the script.

## Make a couple of maps
predictions <- tm_shape(ETH_Adm_2) +
  tm_polygons(col = "white") +
  tm_shape(ETH_malaria_data) +
  tm_bubbles("predictions", style = "quantile", scale = .75) +
  tm_layout(main.title = "Probability of malaria")

residuals <- tm_shape(ETH_Adm_2) +
  tm_polygons(col = "white") +
  tm_shape(ETH_malaria_data) +
  tm_bubbles("residuals") +
  tm_layout(main.title = "Residuals")

current.mode <- tmap_mode("plot")
tmap_arrange(predictions, residuals, widths = c(.5, .5))
tmap_mode(current.mode)
