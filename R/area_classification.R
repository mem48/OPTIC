# Area Classifications

# Ideas for classifications

# Shopping area
# out of town shopping centre
# Offices
# Car parks  or park and ride
# Residential
# Hotel
# University
# Tram/train station
# civic centre (conferecne offices)

library(osmextract)
library(sf)
library(dplyr)
library(tmap)
library(tidyr)
library(cluster)
library(ggplot2)
tmap_mode("view")
tmap_options(check.and.fix = TRUE)

# gm_points <- oe_get("Greater Manchester", 
#              layer = "points",
#              quiet = FALSE)
# 
# gm_lines <- oe_get("Greater Manchester", 
#                     layer = "lines",
#                       quiet = FALSE)
# 
# gm_mulilines <- oe_get("Greater Manchester", 
#                     layer = "multilinestrings",
#                       quiet = FALSE)

gm_poly <- oe_get("Greater Manchester", 
                    layer = "multipolygons",
                      quiet = FALSE)

# gm_other <- oe_get("Greater Manchester", 
#                     layer = "other_relations",
#                       quiet = FALSE)

points <- read.csv("D:/OneDrive - University of Leeds/Data/OPTIC/TFGM Assets_reduced.csv")
points <- st_as_sf(points, coords = c("Lon","Lat"), crs = 4326)
points <- st_transform(points, 27700)

gm_poly <- st_transform(gm_poly, 27700)
#gm_points <- st_transform(gm_points, 27700)
#gm_lines <- st_transform(gm_lines, 27700)
#gm_mulilines <- st_transform(gm_mulilines, 27700)


buff <- st_buffer(points, 250)

ev_poly <- gm_poly[buff,]

# Classify polygons
nms <- c("type","aeroway","amenity","admin_level", "barrier","boundary",
         "building","craft","geological","historic","land_area","landuse",
         "leisure","man_made","military","natural","office","place","shop",
         "sport","tourism")

for(i in nms){
  sub <- ev_poly[[i]]
  sub2 <- paste(i,ev_poly[[i]], sep = ":")
  sub2 <- ifelse(is.na(sub), NA, sub2)
  ev_poly[[i]] <- sub2
}


ev_poly$classification <- apply( st_drop_geometry(ev_poly)[ , nms ] , 1 , paste , collapse = " ")
ev_poly$classification <- gsub("NA","",ev_poly$classification, ignore.case = FALSE)
ev_poly$classification <- gsub("type:multipolygon","",ev_poly$classification, ignore.case = FALSE)
ev_poly$classification <- gsub("building:yes","",ev_poly$classification, ignore.case = FALSE)
ev_poly$classification <- trimws(gsub("\\s+", " ", ev_poly$classification))

ev_poly <- ev_poly[st_is_valid(ev_poly),] # Remove invalid geometry

ev_poly <- ev_poly[,c("osm_id","osm_way_id","name","classification","geometry")]

# Drop Invalid Polygons
invalid <- c("boundary:town","boundary:public_transport","type:boundary",
             "natural:water","place:neighbourhood","place:suburb",
             "natural:wood","natural:scrub","natural:scree","natural:sand",
             "natural:mud","natural:grassland","land_area:administrative",
             "landuse:wasteland","landuse:railway","building:roof",
             "historic:archaeological_site","boundary:census","building:disused",
             "building:construction")

ev_poly <- ev_poly[!grepl(paste(invalid,collapse="|"), ev_poly$classification),]
ev_poly <- ev_poly[!is.na(ev_poly$classification),]
ev_poly <- ev_poly[ev_poly$classification != "",]
ev_poly <- ev_poly[ev_poly$classification != "building:yes",]
ev_poly <- ev_poly[ev_poly$classification != "landuse:grass",]
ev_poly <- ev_poly[ev_poly$classification != "landuse:flowerbed",]
ev_poly <- ev_poly[ev_poly$classification != "landuse:forest",]
ev_poly <- ev_poly[ev_poly$classification != "landuse:meadow",]

buff_union <- st_buffer(st_union(buff),0)
ev_poly <- st_intersection(ev_poly, buff_union)

classes <- read.csv("landuses.csv")
classes <- classes[,c("type","group")]

ev_poly <- left_join(ev_poly, classes, by = c("classification" = "type"))

ev_poly <- ev_poly %>%
  group_by(group) %>%
  summarise()

ev_poly <- ev_poly[!is.na(ev_poly$group),]
ev_poly <- ev_poly[ev_poly$group != "",]

# Loop of points and summarise
res <- list()
for(i in 1:nrow(points)){
  pt <- points[i,]
  pt_buff <- st_buffer(pt, 250)
  sub <- st_intersection(ev_poly, pt_buff)
  sub$area <- as.numeric(st_area(sub))
  sub <- st_drop_geometry(sub)
  sub$area <- sub$area / sum(sub$area) * 100
  sub <- sub[,c("Charge.Point.ID","group","area")]
  res[[i]] <- sub
}

res <- bind_rows(res)
res$area <- round(res$area, 2)

land_summary <- pivot_wider(res, names_from = "group", values_from = "area")
land_summary[is.na(land_summary)] <- 0

clust4 <- kmeans(land_summary[,2:ncol(land_summary)], centers = 4)
clust5 <- kmeans(land_summary[,2:ncol(land_summary)], centers = 5)
clust6 <- kmeans(land_summary[,2:ncol(land_summary)], centers = 6)
clust7 <- kmeans(land_summary[,2:ncol(land_summary)], centers = 7)

land_summary$clust4 <- clust4$cluster
land_summary$clust5 <- clust5$cluster
land_summary$clust6 <- clust6$cluster
land_summary$clust7 <- clust7$cluster

land_summary <- left_join(land_summary, points, by = c("Charge.Point.ID"))
land_summary <- st_as_sf(land_summary)

land_summary$clust4 <- as.character(land_summary$clust4)
land_summary$clust5 <- as.character(land_summary$clust5)
land_summary$clust6 <- as.character(land_summary$clust6)
land_summary$clust7 <- as.character(land_summary$clust7)
qtm(land_summary, dots.col = "clust7", scale = 1.5)

clus_centre <- as.data.frame(clust7$centers)
clus_centre$cluster <- 1:nrow(clus_centre)
clus_centre <- pivot_longer(clus_centre, cols = names(clus_centre)[names(clus_centre) != "cluster"])

ggplot(clus_centre, aes(x = cluster, y = value, fill = name)) +
  geom_bar(position="dodge", stat="identity") +
  scale_x_continuous(breaks=seq(1:7))

st_write(land_summary, "EV_points_clustered.geojson")







