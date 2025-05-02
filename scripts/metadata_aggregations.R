# library(sf)
# sf_use_s2(FALSE)
# library(stars)
# library(tidyverse)
# library(leaflet)
# library(leafem)
# library(paletteer)
# 
# # load("data/sba.rda")
# # sba <- sba %>% st_transform(4326)
# # save(sba, file = "data/sba_trans.rda")
# load("data/sba_trans.rda")
# 
# grid <- read_sf("/Volumes/SLis_SSD/Backup_022025/Downloads/ChrisPurnell_Latest/grid/5km_grid_v9.shp") %>% dplyr::select(TARGET_FID)
# data <- readxl::read_xlsx("/Volumes/SLis_SSD/Backup_022025/Downloads/ChrisPurnell_Latest/MASTER_HPAI_GRID2.xlsx")
# 
# cls  <- rev(paletteer::paletteer_d("RColorBrewer::RdYlGn")[c(TRUE, FALSE)])
# brks <- c(0, 100, 1000, 5000, 10000, 50000, 2000000)
# 
# densTab <- grid %>% left_join(data %>% dplyr::select(TARGET_FID, Max_of_Max), by = "TARGET_FID") %>%
#   dplyr::select(TARGET_FID, Max_of_Max) %>% st_centroid() %>%
#   st_transform(4326) %>%
#   mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2],
#          color = cls[cut(Max_of_Max, brks, labels = FALSE)]) %>% st_drop_geometry()
# 
# 
# leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
#   addProviderTiles(providers$Esri.WorldShadedRelief, group = "map") %>%
#   addProviderTiles(providers$CartoDB.VoyagerOnlyLabels, group = "label") %>%
#   addCircles(data = densTab,
#              lng = ~lon,
#              lat = ~lat,
#              stroke = FALSE,
#              fillColor = ~color, fillOpacity = 0.8, radius = 2000)
# 
# save(densTab, file = "data/densTab.rda")
# 
# ### Cell info
# smAggrData <- data %>% dplyr::select('TARGET_FID', 'Max_of_Max', 'SPECIES COUNT', names(data)[-c(1:15)]) %>%
#   setNames(c('TARGET_FID', 'Max_of_Max', 'SPECIES_COUNT', names(data)[-c(1:15)])) %>%
#   filter(!is.na(SPECIES_COUNT)) %>%
#   left_join(densTab %>% st_drop_geometry() %>% dplyr::select(TARGET_FID, lon, lat, color), by = "TARGET_FID") %>%
#   rename(RID = TARGET_FID)
#   
# save(smAggrData, file = "data/smAggrData.rda")


# grid_detail <- densTab %>% st_transform(4326) %>%
#   mutate(lon   = st_coordinates(.)[,1],
#          lat   = st_coordinates(.)[,2],
#          color = cls[cut(Max_of_Max, brks, labels = FALSE)]) %>%
#   st_drop_geometry()
 

# grid_minimal  <- st_rasterize(densTab %>% dplyr::select(Max_of_Max) %>% st_transform(4326),
#                             st_as_stars(st_bbox(grid %>% st_transform(4326)), nx = 1662, ny = 939, values = NA, crs = 4326))
# 
# grid_middle <- st_rasterize(densTab %>% dplyr::select(Max_of_Max) %>% st_transform(4326),
#                             st_as_stars(st_bbox(grid %>% st_transform(4326)), nx = round(1662/3,0), ny = round(939/3,0), values = NA, crs = 4326))
# 
# grid_large <- st_rasterize(densTab %>% dplyr::select(Max_of_Max) %>% st_transform(4326),
#                             st_as_stars(st_bbox(grid %>% st_transform(4326)), nx = round(1662/6,0), ny = round(939/6,0), values = NA, crs = 4326))
# 
# grid_TARGET_FID <- st_rasterize(densTab %>% dplyr::select(TARGET_FID) %>% st_transform(4326),
#                                 st_as_stars(st_bbox(grid %>% st_transform(4326)), nx = 1662, ny = 939, values = NA, crs = 4326))
# 
# 
# # gridDens <- list(detail = grid_detail, grid_inner = grid_minimal, middle = grid_middle, grid_outer = grid_large)
# # # # save(gridDens, file = "data/distributionsGrid.rda")
# # # 
# # # labelText <- glue::glue("<b>Special Bird Area: </b> {sba$SBIRD_AREA} <br> Click for information (not implemented)")
# # # 
# # # leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
# # #   addProviderTiles(providers$Esri.WorldShadedRelief, group = "map") %>%
# # #   addProviderTiles(providers$CartoDB.VoyagerOnlyLabels, group = "label") %>%
# # #   addCircles(data = gridDens[[1]],
# # #              lng = ~lon,
# # #              lat = ~lat,
# # #              stroke = FALSE,
# # #              fillColor = ~color, fillOpacity = 0.8, radius = 2000, group = 'MaxZoom') %>%
# # #   leafem:::addGeoRaster(
# # #             gridDens[[2]],
# # #             opacity = 0.6,
# # #             colorOptions = colorOptions(
# # #               breaks = brks,
# # #               palette = cls
# # #             )) %>%
# # #   # addCircles(data = gridDens[[2]],
# # #   #            lng = ~lon,
# # #   #            lat = ~lat,
# # #   #            stroke = FALSE,
# # #   #            fillColor = ~color, fillOpacity = 0.8, radius = 7000, group = 'MidZoom') %>%
# # #   # addCircles(data = gridDens[[3]],
# # #   #            lng = ~lon,
# # #   #            lat = ~lat,
# # #   #            stroke = FALSE,
# # #   #            fillColor = ~color, fillOpacity = 0.8, radius = 12500, group = 'MinZoom') %>%
# # #   # addPolygons(data = sba %>% st_transform(4326),
# # #   #             color = "black", weight = 2,
# # #   #             fillColor = "orange", fillOpacity = 0.3,
# # #   #             label = lapply(labelText, htmltools::HTML),
# # #   #             labelOptions = labelOptions(noHide = F, direction = "top"),
# # #   #             group = "MaxZoom") %>%
# # #   groupOptions("MaxZoom", zoomLevels = 8:20) %>%
# # #   groupOptions("MidZoom", zoomLevels = 5:7) %>%
# # #   groupOptions("MinZoom", zoomLevels = 1:4) %>%
# # #   addLegend("bottomright",
# # #             colors = cls,
# # #             labels = c("1-100", "100-1,000", "1,000-5,000", "5,000-10,000", "10,000-50,000", "50,000-2,000,000"),
# # #             title = "Abundance",
# # #             opacity = 1) %>%
# # #   onRender(
# # #     "function(el, x) {
# # #           L.control.zoom({
# # #             position:'topright'
# # #           }).addTo(this);
# # #         }")
# # # 
# # # 
# # # 
# # # ### Shorebird Areas
# # # sba <- read_sf("~/Downloads/for simeon/SBIRD_AREA_260924/SBIRD_AREA_260924.shp") %>%
# # #   dplyr::select(OBJECTID, SBIRD_AREA) %>% st_simplify(dTolerance = 50)
# # # save(sba, file = "data/sba.rdata")
# 
# 
## Cell data
# smAggrData <- data %>% dplyr::select('TARGET_FID', 'Max_of_Max', 'SPECIES COUNT', names(data)[-c(1:15)]) %>%
#   setNames(c('TARGET_FID', 'Max_of_Max', 'SPECIES_COUNT', names(data)[-c(1:15)])) %>%
#   filter(!is.na(SPECIES_COUNT))
 
# ### Aggregate same cells
# duplVec  <- as.factor(apply(smAggrData[,2:10], 1, paste, collapse = "-"))
# aggrSplt <- split(smAggrData, duplVec)
# # table(sapply(aggrSplt, function(x) nrow(x)))
# 
# grid_TARGET_FID_sf <- grid_TARGET_FID %>% st_as_sf()
# 
# gr <- parallel::mclapply(aggrSplt, function(x) {
#   grid_TARGET_FID_sf %>% filter(TARGET_FID%in%x$TARGET_FID) %>% 
#     st_union() %>% st_sf() %>% bind_cols(x[1,-1])
# }, mc.cores = 5) %>% Reduce("rbind", .)
# 
# gr <- gr %>% mutate(RID = 1:nrow(gr))
# save(gr, file = "data/gr.rda")
# 
# grps   <- readxl::read_xlsx("data/SpGroups.xlsx")
# grpsSM <- gr %>% st_drop_geometry() %>%
#   bind_cols(
#     parallel::mclapply(1:nrow(gr), function(x) {
#       gr[x,] %>% st_drop_geometry() %>% dplyr::select(grps$Group) %>%
#       pivot_longer(cols = everything(), names_to = 'Group', values_to = 'Count') %>%
#       left_join(grps, by = "Group") %>% group_by(GroupNew) %>% summarise(Count = sum(Count, na.rm = T)) %>%
#       pivot_wider(names_from = GroupNew, values_from = Count)
#     },mc.cores = 6) %>% Reduce("rbind",.)
#   ) %>% dplyr::select(c(RID, Max_of_Max, SPECIES_COUNT, grps$GroupNew, grps$Group))

# grpsSM$sba <- unlist(parallel::mclapply(1:nrow(gr), function(x) {
#   out <- sba$SBIRD_AREA[apply(gr[x,] %>% st_intersects(sba %>% st_transform(st_crs(gr)), sparse = FALSE), 1, which)]
#   ifelse(length(out)==0, NA, out[1])
#   }, mc.cores = 6))
# 
# cls  <- rev(paletteer_c("ggthemes::Red-Green Diverging", 6))
# brks <- c(0, 100, 1000, 5000, 10000, 50000, 2000000) 
# 
# gr$Color <- cls[cut(gr$Max_of_Max, breaks = brks, labels = F)]
# 
# birdAggr <- list(BirdSummary = grpsSM, GridCount = gr %>% dplyr::select(RID, Max_of_Max, Color),
# grid_inner = grid_minimal, middle = grid_middle, grid_outer = grid_large)
# save(birdAggr, file = "data/birdAggr.rda")
