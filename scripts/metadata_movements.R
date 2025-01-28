# library(tidyverse)
# library(sf)
# sf_use_s2(FALSE)
# library(stars)
# library(spatstat)
# library(RColorBrewer)
# 
### Flyway ###
##############
# 
# plot(rnaturalearthdata::coastline50 %>% st_as_sf() %>% st_shift_longitude() %>% st_geometry())
# pts <- locator()
# 
# poly <- list(matrix(c(pts$x[length(pts$x)], pts$x, pts$y[length(pts$x)], pts$y), ncol = 2, byrow = F)) %>% st_polygon() %>%
#   st_sfc() %>% st_set_crs(4326)
#   
# flyway <- smoothr::smooth(poly, method = "ksmooth")
# plot(flyway, add = T, col = "red")
# save(flyway, file = "data/flyway.rda")
# load("data/flyway.rda")



### Species ############
#################### 

meta <- tibble(species = 
                 c("All",
                   "Bar-tailed Godwit",
                   "Black-tailed Godwit",
                   "Curlew Sandpiper",
                   "Great Knot",
                   "Greater Sandplover",
                   "Grey-tailed Tattler",
                   "Red-necked Stint",
                   "Red Knot",
                   "Ruddy Turnstone",
                   "Sanderling",
                   "Sharp-tailed Sandpiper",
                   "Terek Sandpiper",
                   "Short-tailed Shearwater",
                   "Wedge-tailed Shearwater"
                 ), 
               SCINAME = 
                 c("All",
                   "Limosa lapponica",
                   "Limosa limosa",
                   "Calidris ferruginea",
                   "Calidris tenuirostris",
                   "Anarhynchus leschenaultii",
                   "Tringa brevipes",
                   "Calidris ruficollis",
                   "Calidris canutus",
                   "Arenaria interpres",
                   "Calidris alba",
                   "Calidris acuminata",
                   "Xenus cinereus",
                   "Ardenna tenuirostris",
                   "Puffinus pacificus"),
               TrackName = 
                 c("All",
                   "Godwit",
                   NA,
                   "CurlewSandpiper",
                   "GreatKnot",
                   NA,
                   NA,
                   "RedNeckedStint",
                   NA,
                   "Turnstone",
                   "Sanderling",
                   NA,
                   NA,
                   "ShortTailedShearwaters",
                   "WedgeTailedShearwaters"))

# save(meta, file = "data/metadata.rda")




### Distribution #######
#####################

# cols = tibble(SEASONAL = c(1:5),
#               CODE    = c('Resident', 'Breeding season', 'Non-breeeding season', 'Passage', 'Seasonal occurance uncertain'),
#               COLOR   = c('#5DD135', '#e27c0f', '#0788D2', '#8fc9cb', '#E3ACFA'))
# 
# dist00 <- st_read("~/Google Drive/My Drive/GeoDat/BirdLife_Distributions/Waders/Wader_2.shp") %>%
#   filter(SCINAME%in%meta$SCINAME) %>% dplyr::select(SCINAME, SEASONAL) %>% left_join(cols, by = 'SEASONAL')
# 
# dist01 <- dist00 %>% st_intersection(st_bbox(c(xmin = 50, xmax = 180, ymin = -85, ymax = 85), crs = 4326) %>% st_as_sfc()) %>%
#   st_shift_longitude()
# dist02 <- dist00 %>% st_intersection(st_bbox(c(xmin = -180, xmax = -100, ymin = -85, ymax = 85), crs = 4326) %>% st_as_sfc()) %>%
#   st_shift_longitude()
# 
# dist <- rbind(dist01, dist02) %>% mutate(AREA = as.numeric(st_area(.))) %>% arrange(AREA) %>% st_intersection(flyway)
# plot(dist)
# 
# ### All
# all <- dist %>% group_by(SEASONAL) %>% summarise(geometry = st_union(geometry), COLOR = unique(COLOR)) %>%
#        suppressMessages()
# tm <- all %>% mutate(SCINAME = "All", SEASONAL = all$SEASONAL, CODE = cols$CODE[all$SEASONAL], COLOR = cols$COLOR[all$SEASONAL],
#                      AREA = as.numeric(st_area(.))) %>% dplyr::select(names(dist))
# 
# dist <- dist %>% bind_rows(tm)
# 
# # save(dist, file = "data/distributions.rda")
# # load("data/distributions.rda")
# 
# dist <- dist %>% st_simplify(dTolerance = 0.05) %>% filter(CODE != "Seasonal occurance uncertain")
# save(dist, file = "data/distributions_small.rda")


### Leg-flags ##########
########################

## Australia polygon
# plot(rnaturalearthdata::coastline50 %>% st_as_sf() %>% st_shift_longitude() %>% st_geometry())
# pts <- locator()
# 
# poly <- list(matrix(c(pts$x[length(pts$x)], pts$x, pts$y[length(pts$x)], pts$y), ncol = 2, byrow = F)) %>% st_polygon() %>%
#   st_sfc() %>% st_set_crs(4326)
# 
# oz <- smoothr::smooth(poly, method = "ksmooth")
# plot(oz, add = T, col = "red")
# save(oz, file = "data/oz.rda")
# load("data/oz.rda")

## Grid
# grd <- st_make_grid(flyway %>% st_transform("+proj=laea + lon_0=145"), cellsize = 250000, square = FALSE) %>%
#   st_transform(4326) %>% st_shift_longitude()
# # plot(grd, axes = T)
# # 
# load("~/Downloads/my_work_space.RData")
# flagsDB <- Y %>% filter(SpeName%in%meta$species) %>%
#   dplyr::select(SpeName, lon, lat, Date) %>%
#   mutate(Date = as.POSIXct(Date)) %>% filter(!is.na(Date), !is.na(lon), !is.na(lat), lon> -180, lon<180) %>%
#   mutate(Month = as.numeric(format(Date, "%m"))) %>%
#   st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
#   mutate(Grid = unlist(apply(st_intersects(., grd, sparse = FALSE), 1, function(x) ifelse(all(is.null(x)), NA, which(x))))) %>%
#   filter(!is.na(Grid)) %>%
#   st_drop_geometry() %>%
#   left_join(tibble(Grid = 1:length(grd),
#                    lon = st_coordinates(grd %>% st_centroid())[,1],
#                    lat = st_coordinates(grd %>% st_centroid())[,2]), by = 'Grid') %>%
#   st_as_sf(coords = c("lon", "lat"), crs = 4326)  %>%
#   mutate(Australia = st_intersects(., oz, sparse = FALSE)[,1], .after = 'Grid')


# save(flagsDB, file = "data/flagsDB.rda")
load("data/flagsDB.rda")


#### Densities
{
# library(spatstat)
# centre <- flyway %>% st_centroid() %>% st_coordinates()
# prj    <- sprintf("+proj=laea +lon_0=%s +lat_0=%s", centre[1], centre[2])
# 
# spDens <- lapply(unique(flagsDB$SpeName), function(x) {
# 
#   sf_points  <- flagsDB %>% filter(SpeName==x) %>% st_geometry()
#   # plot(sf_points)
#   ppp_points <- as.ppp(sf_points %>% st_transform(prj))
#   Window(ppp_points) <- as.owin(flyway %>% st_transform(prj))
#   density_spatstat <- density(ppp_points, adjust = 0.25)
#   # plot(density_spatstat)
#   density_stars <- stars::st_as_stars(density_spatstat) %>%
#     st_set_crs(prj) %>% setNames("dens") %>%
#     mutate(dens = scales::rescale(dens, c(0,1)))
# 
#   q <- quantile(density_stars[[1]], na.rm = T, probs = seq(0.5, 0.99, length = 5))
# 
#   st_contour(density_stars, na.rm = T,
#                      breaks = q)[-1,] %>%
#     mutate(species = x, quantile = seq(0.5, 0.99, length = 5)[1:nrow(.)], month = NA) %>%
#     dplyr::select(species, month, quantile) %>% bind_rows(
#       lapply(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), function(m) {
#         sf_points  <- flagsDB %>% filter(SpeName==x) %>%
#           mutate(Month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")[Month]) %>%
#           filter(Month == m) %>%
#           st_geometry()
#         if(length(sf_points)>10) {
#         ppp_points <- as.ppp(sf_points %>% st_transform(prj))
#         Window(ppp_points) <- as.owin(flyway %>% st_transform(prj))
#         density_spatstat <- density(ppp_points, adjust = 0.25)
#         # plot(density_spatstat)
#         density_stars <- stars::st_as_stars(density_spatstat) %>%
#           st_set_crs(prj) %>% setNames("dens") %>%
#           mutate(dens = scales::rescale(dens, c(0,1)))
# 
#         q <- quantile(density_stars[[1]], na.rm = T, probs = seq(0.5, 0.99, length = 5))
# 
#         st_contour(density_stars, na.rm = T,
#                    breaks = q)[-1,] %>%
#           mutate(species = x, quantile = seq(0.5, 0.99, length = 5)[1:nrow(.)], month = m) %>%
#           dplyr::select(species, month, quantile)
#         } else NULL
#       }) %>% Reduce("rbind", .)
#     )
# 
# ggplot() +
#   # geom_sf(data = flyway %>% st_transform(prj)) +
#   geom_sf(data = cont %>% st_transform(4326) %>% st_shift_longitude(), mapping = aes(geometry = geometry, fill = dens)) +
#   scale_fill_manual(values = brewer.pal(5, "RdPu"))
# 
# }) %>% Reduce("rbind", .)
# 
# alDens <- lapply("All", function(x) {
#   sf_points  <- flagsDB %>% st_geometry()
#   ppp_points <- as.ppp(sf_points %>% st_transform(prj))
#   Window(ppp_points) <- as.owin(flyway %>% st_transform(prj))
#   density_spatstat <- density(ppp_points, adjust = 0.25)
#   # plot(density_spatstat)
#   density_stars <- stars::st_as_stars(density_spatstat) %>%
#     st_set_crs(prj) %>% setNames("dens") %>%
#     mutate(dens = scales::rescale(dens, c(0,1)))
#
#   q <- quantile(density_stars[[1]], na.rm = T, probs = seq(0.5, 0.99, length = 5))
#
#   st_contour(density_stars, na.rm = T,
#              breaks = q)[-1,] %>%
#     mutate(species = x, quantile = seq(0.5, 0.99, length = 5)[1:nrow(.)], month = NA) %>%
#     dplyr::select(species, month, quantile) %>% bind_rows(
#     lapply(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), function(m) {
#       sf_points  <- flagsDB %>%
#         mutate(Month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")[Month]) %>%
#         filter(Month == m) %>%
#         st_geometry()
#       if(length(sf_points)>10) {
#         ppp_points <- as.ppp(sf_points %>% st_transform(prj))
#         Window(ppp_points) <- as.owin(flyway %>% st_transform(prj))
#         density_spatstat <- density(ppp_points, adjust = 0.25)
#         # plot(density_spatstat)
#         density_stars <- stars::st_as_stars(density_spatstat) %>%
#           st_set_crs(prj) %>% setNames("dens") %>%
#           mutate(dens = scales::rescale(dens, c(0,1)))
#
#         q <- quantile(density_stars[[1]], na.rm = T, probs = seq(0.5, 0.99, length = 5))
#
#         st_contour(density_stars, na.rm = T,
#                    breaks = q)[-1,] %>%
#           mutate(species = x, quantile = seq(0.5, 0.99, length = 5)[1:nrow(.)], month = m) %>%
#           dplyr::select(species, month, quantile)
#       } else NULL
#     }) %>%
#       Reduce("rbind", .)
#     )
# }) %>% Reduce("rbind",.)
#
# flagDens <- bind_rows(alDens, spDens)
# rownames(flagDens) <- 1:nrow(flagDens)
#
# ## Color
# flagDens <- flagDens %>%
#   left_join(tibble(quantile = seq(0.5, 0.99, length = 5), color = RColorBrewer::brewer.pal(5, "RdPu")), by = "quantile") %>%
#   st_transform(4326) %>% st_shift_longitude()
#
#
#
# save(flagDens, file = "data/flagDens.rda")
# load("data/flagDens.rda")
# 
# flagPts <- lapply(unique(flagsDB$SpeName), function(x) {
#   flagsDB %>% filter(SpeName==x) %>%
#     mutate(Month = as.character(month(ymd(010101) + months(Month-1),label=TRUE,abbr=TRUE))) %>%
#     group_by(Grid) %>% summarise(Sample = n()) %>%
#     mutate(Weight = approx(c(1, 329539), c(5, 400), Sample)$y,
#            Species = x,
#            Month   = NA) %>% dplyr::select(Species, Grid, Sample, Weight, Month, geometry) %>% suppressMessages() %>%
#     bind_rows(
#       lapply(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), function(month) {
#       flagsDB %>% filter(SpeName==x) %>%
#         mutate(Month = as.character(month(ymd(010101) + months(Month-1),label=TRUE,abbr=TRUE))) %>%
#         filter(Month %in% month) %>%
#         group_by(Grid) %>% summarise(Sample = n()) %>%
#         mutate(Weight = approx(c(1, 329539), c(5, 800), Sample)$y,
#                Species = x,
#                Month   = month) %>% dplyr::select(Species, Grid, Sample, Weight, Month, geometry)
#       }) %>% Reduce("rbind", .) %>%     suppressMessages()
#     )
# }) %>% Reduce("rbind",.) %>% bind_rows(
#   flagsDB %>%
#     mutate(Month = as.character(month(ymd(010101) + months(Month-1),label=TRUE,abbr=TRUE))) %>%
#     group_by(Grid) %>% summarise(Sample = n()) %>%
#     mutate(Weight = approx(c(1, 329539), c(5, 100), Sample)$y,
#            Species = "All",
#            Month   = NA) %>% dplyr::select(Species, Grid, Sample, Weight, Month, geometry) %>% suppressMessages() %>%
#     bind_rows(
#       lapply(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), function(month) {
#         flagsDB %>%
#           mutate(Month = as.character(month(ymd(010101) + months(Month-1),label=TRUE,abbr=TRUE))) %>%
#           filter(Month %in% month) %>%
#           group_by(Grid) %>% summarise(Sample = n()) %>%
#           mutate(Weight = approx(c(1, 329539), c(5, 250), Sample)$y,
#                  Species = "All",
#                  Month   = month) %>% dplyr::select(Species, Grid, Sample, Weight, Month, geometry)
#       }) %>% Reduce("rbind", .) %>%     suppressMessages()
#     )
# )
# 
# flagPts <- flagPts %>% mutate(Color = c("#3390de", "#2e37fc")[as.numeric(st_intersects(flagPts, oz, sparse = F)[,1])+1]) %>%
#   dplyr::select(Species, Month, Grid, Sample, Color)
# 
# save(flagPts, file = "data/flagPts.rda")
# load("data/flagPts.rda")

# flegPhen <- lapply(unique(flagsDB$SpeName), function(x) {
#   flagsDB %>% st_drop_geometry() %>% filter(SpeName == x) %>%
#     mutate(day = as.numeric(format(Date, "%j"))) %>%
#     group_by(Month, Australia) %>% summarise(Count = n()) %>% pivot_wider(names_from = Australia, values_from = Count) %>% 
#     setNames(c("Month", "Flyway", "Australia")) %>% mutate(Species = x, .before = Month)
# }) %>% Reduce("rbind", .) %>%
#   bind_rows(
#     flagsDB %>% st_drop_geometry() %>% 
#       mutate(day = as.numeric(format(Date, "%j"))) %>%
#       group_by(Month, Australia) %>% summarise(Count = n()) %>% pivot_wider(names_from = Australia, values_from = Count) %>% 
#       setNames(c("Month", "Flyway", "Australia")) %>% mutate(Species = "All", .before = Month)
#   )
# 
# save(flegPhen, file = "data/flegPhen.rda")
# load("data/flegPhen.rda")
}
# 
# 
# 
### Geolocator Data #### 
########################
# 
# wd <- "/Users/slisovsk/Library/CloudStorage/GoogleDrive-simeon.lisovski@gmail.com/My Drive/Science/Projects/OptimalMigChange/Data/Tracks/"
# 
# trk_list <- tibble(path = list.files(wd, recursive = T)) %>%
#   mutate(species = sapply(strsplit(path, "/"), function(x) x[1]),
#          ID = sapply(strsplit(path, "/"), function(x) sapply(strsplit(x[2], "_"), function(y) y[1]))) %>%
#   filter(species %in% meta$TrackName) %>% left_join(meta, by = join_by(species == TrackName)) %>%
#   dplyr::select(path, ID, species.y) %>% rename(species = species.y) %>%
#   group_split(species)
# 
# sf_tracks <- lapply(1:6, function(r) {
#   x <- trk_list[[r]]
#   lapply(1:nrow(x), function(y) {
#     
#     if(r%in%c(2,4,5)) {
#       tmp <- read.csv(paste0(wd, x$path[y]))
#       if(any(tmp$Type==2)) {
#         out<- tmp %>%
#           mutate(ID         = x$ID[y],
#                  Species    = x$species[y],
#                  Date       = as.numeric(format(as.POSIXct(StartTime), "%j")),
#                  Site       = 1:nrow(.)) %>%
#           dplyr::select(Species, ID, Site, Date, Days, Type, Lon.50., Lat.50.)
#         if(out$Type[1]==0) out$Date[1] = 1 
#         
#         join <- full_join(out %>% group_by(Date) %>% 
#                             summarise(Lon = median(Lon.50.), Lat = median(Lat.50.), Type = min(Type), Site = min(Site)), 
#                           tibble(ID = out$ID[1], Species = out$Species[1], Date = 1:365), by = 'Date') %>%
#           arrange(Date) %>% dplyr::select(Species, ID, Site, Date, Type, Lon, Lat) %>%
#           fill(Type, Site, Lon, Lat)
#         
#         join$Days = group_split(join, Site) %>% 
#           lapply(function(x) ifelse(x$Type==0, rep(1, nrow(x)), cumsum(rep(1, nrow(x))))) %>% Reduce("c", .)    
#         
#         join %>% st_as_sf(coords = c("Lon", "Lat"), crs = 4326) %>% 
#           mutate(Lon = st_coordinates(.)[,1],
#                  Lat = st_coordinates(.)[,2]) %>%
#           group_split(ID) %>%
#           lapply(function(i) {
#             split <- round(mean(which(i$Lat==max(i$Lat))),0)
#             i$Split <- 1
#             i$Split[(split+1):nrow(i)] <- 2
#             i
#           }) %>% Reduce("rbind", .) %>%
#           mutate(Color = ifelse(Split==1, "#14668c", "#cd930d"),
#                  Color = ifelse(Type==0, "black", Color)) %>%
#           dplyr::select(Species, ID, Date, Type, Days, Split, Color, Lon, Lat)
#         
#       }
#     } else if(r==1) {
#       tmp <- read.csv(paste0(wd, x$path[y]))
#       if(any(tmp$Type==2)) {
#         out<- tmp %>%
#           mutate(ID         = x$ID[y],
#                  Species    = x$species[y],
#                  Date1      = as.numeric(format(as.POSIXct(Arrival.50.), "%j")),
#                  Date2      = as.numeric(format(as.POSIXct(Departure.50.), "%j")),
#                  Site       = site) %>%
#           pivot_longer(cols = c(Date1, Date2)) %>%
#           rename(Date = value)  %>%
#           dplyr::select(Species, ID, Site, Date, Type, Lon.50., Lat.50.)
#         if(out$Type[1]==0) out$Date[1] = 1 
#         
#         join <- full_join(out %>% group_by(Date) %>% 
#                             summarise(Lon = median(Lon.50.), Lat = median(Lat.50.), Type = min(Type), Site = min(Site)), 
#                           tibble(ID = out$ID[1], Species = out$Species[1], Date = 1:365), by = 'Date') %>%
#           arrange(Date) %>% dplyr::select(Species, ID, Site, Date, Type, Lon, Lat) %>%
#           fill(Type, Site, Lon, Lat)
#         
#         join$Days = group_split(join, Site) %>% 
#           lapply(function(x) ifelse(x$Type==0, rep(1, nrow(x)), cumsum(rep(1, nrow(x))))) %>% Reduce("c", .)    
#         
#         join %>% st_as_sf(coords = c("Lon", "Lat"), crs = 4326) %>% 
#           mutate(Lon = st_coordinates(.)[,1],
#                  Lat = st_coordinates(.)[,2]) %>%
#           group_split(ID) %>%
#           lapply(function(i) {
#             split <- round(mean(which(i$Lat==max(i$Lat))),0)
#             i$Split <- 1
#             i$Split[(split+1):nrow(i)] <- 2
#             i
#           }) %>% Reduce("rbind", .) %>%
#           mutate(Color = ifelse(Split==1, "#14668c", "#cd930d"),
#                  Color = ifelse(Type==0, "black", Color)) %>%
#           dplyr::select(Species, ID, Date, Type, Days, Split, Color, Lon, Lat)
#       }
#     } else if(r%in%c(3,6)) {
#       tmp <- read.csv(paste0(wd, x$path[y]))
#       if(any(tmp$type==2)) {
#         out<- tmp %>% rename(Type = type) %>%
#           mutate(ID         = x$ID[y],
#                  Species    = x$species[y],
#                  Date1      = as.numeric(format(as.POSIXct(Arrival), "%j")),
#                  Date2      = as.numeric(format(as.POSIXct(Departure), "%j")),
#                  Site       = 1:nrow(.)) %>%
#           pivot_longer(cols = c(Date1, Date2)) %>%
#           rename(Date = value)  %>%
#           dplyr::select(Species, ID, Site, Date, Type, Lon, Lat) %>%
#           filter(!is.na(Date))
#         if(out$Type[1]==0) out$Date[1] = 1 
#         
#         join <- full_join(out %>% group_by(Date) %>% 
#                           summarise(Lon = median(Lon), Lat = median(Lat), Type = min(Type), Site = min(Site)), 
#                           tibble(ID = out$ID[1], Species = out$Species[1], Date = 1:365), by = 'Date') %>%
#           arrange(Date) %>% dplyr::select(Species, ID, Site, Date, Type, Lon, Lat) %>%
#           fill(Type, Site, Lon, Lat)
#         
#         join$Days = group_split(join, Site) %>% 
#           lapply(function(x) ifelse(x$Type==0, rep(1, nrow(x)), cumsum(rep(1, nrow(x))))) %>% Reduce("c", .)    
#         
#         join %>% st_as_sf(coords = c("Lon", "Lat"), crs = 4326) %>% 
#           mutate(Lon  = st_coordinates(.)[,1],
#                  Lat  = st_coordinates(.)[,2]) %>%
#           group_split(ID) %>%
#           lapply(function(i) {
#             split <- round(mean(which(i$Lat==max(i$Lat))),0)
#             i$Split <- 1
#             i$Split[(split+1):nrow(i)] <- 2
#             i
#           }) %>% Reduce("rbind", .) %>%
#           mutate(Color = ifelse(Split==1, "#14668c", "#cd930d"),
#                  Color = ifelse(Type==0, "black", Color)) %>%
#           dplyr::select(Species, ID, Date, Type, Days, Split, Color, Lon, Lat)
#       }
#     }
#     
#     }) %>% Reduce("rbind",.)
# }) %>% lapply(function(x) {
#   x %>% mutate(
#     spWeight = approx(range(Days), c(0.5, 40), Days)$y, .before = "geometry"
#   )
# }) %>% Reduce("rbind", .) %>%
#   mutate(allWeight = approx(range(Days), c(0.5, 40), Days)$y, .before = "geometry")
# 
# save(sf_tracks, file = "data/sf_tracks.rda")
load("data/sf_tracks.rda")


#### Shearwaters
# fls <- tibble(path    = list.files("~/Downloads/Marcel Klassen Files", pattern = ".csv", full.names = T),
#               species = ifelse(sapply(strsplit(path, "-"), function(x) x[[2]]) == "STSW", "Short-tailed Shearwater", "Wedge-tailed Shearwater"),
#               id      = sapply(strsplit(path, "-"), function(x) gsub("Track.csv","", x[[length(x)]])))
# 
# 
# sw_tracks <- lapply(1:nrow(fls), function(i) {
#   read_csv(fls$path[i]) %>% suppressMessages() %>% st_as_sf(coords = c("Lon.mean", "Lat.mean"), crs = 4326) %>%
#     mutate(Species = fls$species[i], ID = fls$id[i], Date = as.numeric(format(Time, "%j")), Type = NA, Days = NA, 
#            Split = NA, Color = "black", Lon = st_coordinates(.)[,1], Lat = st_coordinates(.)[,2], spWeight = NA, allWeight = NA) %>%
#     dplyr::select(names(sf_tracks))
# }) %>% Reduce("rbind",.)
# 
# sf_tracks <- sf_tracks %>% bind_rows(sw_tracks)
# save(sf_tracks, file = "data/sf_tracks.rda")
