library(sf)
sf_use_s2(FALSE)
library(stars)
library(tidyverse)
library(leaflet)
library(leafem)
library(paletteer)

grid <- read_sf("~/Downloads/ChrisPurnell_Latest/grid/5km_grid_v9.shp") %>% dplyr::select(TARGET_FID)
data <- readxl::read_xlsx("~/Downloads/ChrisPurnell_Latest/MASTER_HPAI_GRID2.xlsx")

grid_stars <- st_as_stars(st_bbox(grid), dx = 5000, dy = 5000, values = NA, crs = st_crs(grid))

cls  <- rev(paletteer::paletteer_d("RColorBrewer::RdYlGn")[c(TRUE, FALSE)])
brks <- c(0, 100, 1000, 5000, 10000, 50000, 2000000) 

densTab <- grid %>% left_join(data %>% dplyr::select(TARGET_FID, Max_of_Max), by = "TARGET_FID") %>%
  dplyr::select(TARGET_FID, Max_of_Max) %>% st_centroid()


grid_detail <- densTab %>% st_transform(4326) %>%
  mutate(lon   = st_coordinates(.)[,1], 
         lat   = st_coordinates(.)[,2],
         color = cls[cut(Max_of_Max, brks, labels = FALSE)]) %>%
  st_drop_geometry()

grid_middle <- st_rasterize(densTab %>% dplyr::select(Max_of_Max), 
                            st_as_stars(st_bbox(grid), dx = 15000, dy = 15000, values = NA, crs = st_crs(grid))) %>%
  st_as_sf() %>% setNames(c("Max_of_Max", "geometry")) %>% st_centroid() %>% st_transform(4326)%>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>%
  mutate(color = cls[cut(Max_of_Max, brks, labels = FALSE)]) %>% st_drop_geometry()

grid_large <- st_rasterize(densTab %>% dplyr::select(Max_of_Max), 
                       st_as_stars(st_bbox(grid), dx = 25000, dy = 25000, values = NA, crs = st_crs(grid))) %>%
  st_as_sf() %>% setNames(c("Max_of_Max", "geometry")) %>% st_centroid() %>% st_transform(4326)%>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>%
  mutate(color = cls[cut(Max_of_Max, brks, labels = FALSE)]) %>% st_drop_geometry()

gridDens <- list(detail = grid_detail, middle = grid_middle, grid_outer = grid_large)
save(gridDens, file = "data/distributionsGrid.rda")

labelText <- glue::glue("<b>Special Bird Area: </b> {sba$SBIRD_AREA} <br> Click for information (not implemented)")

leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  addProviderTiles(providers$Esri.WorldShadedRelief, group = "map") %>%
  addProviderTiles(providers$CartoDB.VoyagerOnlyLabels, group = "label") %>%
  addCircles(data = gridDens[[1]],
             lng = ~lon,
             lat = ~lat, 
             stroke = FALSE,
             fillColor = ~color, fillOpacity = 0.8, radius = 2000, group = 'MaxZoom') %>%
  addCircles(data = gridDens[[2]],
             lng = ~lon,
             lat = ~lat, 
             stroke = FALSE,
             fillColor = ~color, fillOpacity = 0.8, radius = 7000, group = 'MidZoom') %>%
  addCircles(data = gridDens[[3]],
             lng = ~lon,
             lat = ~lat, 
             stroke = FALSE,
             fillColor = ~color, fillOpacity = 0.8, radius = 12500, group = 'MinZoom') %>%
  addPolygons(data = sba %>% st_transform(4326),
              color = "black", weight = 2,
              fillColor = "orange", fillOpacity = 0.3,
              label = lapply(labelText, htmltools::HTML),
              labelOptions = labelOptions(noHide = F, direction = "top"),
              group = "MaxZoom") %>%
  groupOptions("MaxZoom", zoomLevels = 8:20) %>%
  groupOptions("MidZoom", zoomLevels = 5:7) %>%
  groupOptions("MinZoom", zoomLevels = 1:4) %>%
  addLegend("bottomright", 
            colors = cls,
            labels = c("1-100", "100-1,000", "1,000-5,000", "5,000-10,000", "10,000-50,000", "50,000-2,000,000"),
            title = "Abundance",
            opacity = 1) %>%
  onRender(
    "function(el, x) {
          L.control.zoom({
            position:'topright'
          }).addTo(this);
        }")



### Shorebird Areas
sba <- read_sf("~/Downloads/for simeon/SBIRD_AREA_260924/SBIRD_AREA_260924.shp") %>%
  dplyr::select(OBJECTID, SBIRD_AREA) %>% st_simplify(dTolerance = 50)

save(sba, file = "data/sba.rdata")
