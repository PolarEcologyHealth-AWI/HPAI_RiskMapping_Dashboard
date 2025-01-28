library(tidyverse)

load("~/Downloads/WOAH_PI_Rdata")

Is <- PI %>% dplyr::select(c("Dead","Species" ,"is_wild","Date","Longitude","Latitude","sero_sub_genotype_eng")) %>%
  filter(complete.cases(.)) %>%
  mutate(H5 = grepl("H5", sero_sub_genotype_eng),
         markerCol = ifelse(is_wild,"chocolate","blue"))

Is_sf <- Is %>% st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% filter(H5)

templ <- st_as_stars(st_bbox(c(xmin = -180, xmax = 180, ymin = -90, ymax = 90), crs = 4326), 
                     dx = 0.5, dy = 0.5, values = 0) %>% st_as_sf() %>% st_intersection(Is_sf %>% st_buffer(1) %>% st_union())

Is_sf_grid <- Is_sf %>% 
  mutate(grd = apply(st_intersects(., templ, sparse = FALSE), 1, function(x) ifelse(any(!is.na(x)), which(x), NA)))

outbreakDat <- Is_sf_grid %>% mutate(Lon = st_coordinates(.)[,1],
                                       Lat = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>% group_by(Year = as.numeric(format(Date, "%Y")), Grid = grd, is_wild) %>%
  summarise(sample = n(), lon = median(Lon), lat = median(Lat)) %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)

save(outbreakDat, file = "HPAIoutbreak.rda")
