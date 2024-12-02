library(sf)
sf_use_s2(FALSE)
library(stars)
library(tidyverse)
library(leaflet)
library(leafem)
library(paletteer)

grid <- read_sf("~/Downloads/Chris/5km_grid_v8_1.shp") %>% st_transform(4326)
data <- readxl::read_xls("~/Downloads/Chris/MASTER_HPAI_MAX_GRID.xls")

bbox <- st_bbox(grid %>% st_transform(4326)) %>% st_set_crs(4326)
crds <- st_coordinates(grid %>% st_centroid())


MaxCounts <- grid %>% mutate(TARGET_FID = 1:nrow(.)) %>%
  left_join(data) %>%
  filter(Max_of_Max>0) %>% st_centroid() %>% st_transform(4326) %>%
  mutate(lng = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>% st_as_sf(coords = c("lng", "lat"), crs = 4326)


grd_stars <- st_as_stars(bbox, dx = length(unique(crds[,1])), ny = length(unique(crds[,2])), values = NA, crs = 4326)
kmMin       <- st_rasterize(MaxCounts %>% dplyr::select(Max_of_Max), grd_stars)

cls  <- rev(paletteer_c("ggthemes::Red-Green Diverging", 6))
brks <- c(0, 100, 1000, 5000, 10000, 50000, 2000000) 

leaflet() %>%
  addProviderTiles(providers$Esri.WorldShadedRelief, group = "map") %>%
  addProviderTiles(providers$CartoDB.VoyagerOnlyLabels, group = "label") %>%
  addGeoRaster(
    kmMin,
    opacity = 0.85,
    colorOptions = colorOptions(
      breaks = brks,
      palette = cls,
      na.color = "transparent"
    )) %>%
  addCircleMarkers(lng = st_coordinates(grid %>% st_centroid() %>% st_transform(4326))[,1],
                   lat = st_coordinates(grid %>% st_centroid() %>% st_transform(4326))[,2], color = "black")
    



kmMed <- st_rasterize(MaxCounts %>% dplyr::select(Max_of_Max), 
                     st_as_stars(bbox, nx = round(length(unique(crds[,1]))/10, 0), 
                                 ny = round(length(unique(crds[,2]))/10, 0), values = NA, crs = 4326))

kmMax <- st_rasterize(MaxCounts %>% dplyr::select(Max_of_Max), 
                      st_as_stars(bbox, nx = round(length(unique(crds[,1]))/25, 0), 
                                  ny = round(length(unique(crds[,2]))/25, 0), values = NA, crs = 4326))


starsAggr <- list(Max = kmMin, Med = kmMed, min = kmMax)
save(starsAggr, file = "data/starsAggr.rda")


### Shorebird Areas
sba <- read_sf("~/Downloads/for simeon/SBIRD_AREA_260924/SBIRD_AREA_260924.shp") %>%
  dplyr::select(OBJECTID, SBIRD_AREA) %>% st_simplify(dTolerance = 50)

save(sba, file = "data/sba.rdata")
