library(sf)
library(rgdal)

australia <- st_read("data/Australia/Australia_proj.shp")
australia

plot(australia)

plot(australia[2])

aus <- readOGR("data/Australia/Australia_proj.shp")

plot(australia$geometry)

plot(st_geometry(australia), axes = TRUE)


koala <- read.csv("data/koala.csv")
head(koala)
koala_sf <- st_as_sf(koala, 
                     coords = c("Longitude", "Latitude"),
                     crs = 4326)
plot(koala_sf)


# load from packages ------------------------------------------------------
library(maps)
library(tidyverse)

data(world.cities)
world.cities

cities <- st_as_sf(world.cities, 
                   coords = c("long", "lat"),
                   crs = 4326)

cities_aus <- filter(cities, country.etc == "Australia")


cities <- st_as_sf(world.cities, 
                   coords = c("long", "lat"),
                   crs = 4326) %>% 
  filter(country.etc == "Australia") %>% 
  filter(pop > 1e6)


plot(cities["pop"], cex = cities$pop / 1e6)


# projection --------------------------------------------------------------

plot(australia$geometry)
plot(koala_sf, add = TRUE)

st_crs(australia)
st_crs(koala_sf)

koala_proj <- st_transform(koala_sf, crs = st_crs(australia))

plot(australia$geometry, axes = TRUE)
plot(koala_proj, add = TRUE)


# clipping maps -----------------------------------------------------------
st_bbox(koala_proj)

aus <- st_crop(australia, st_bbox(koala_proj))
plot(aus)

library(raster)

plot(australia$geometry, axes = TRUE)

ext <- drawExtent()
aus <- st_crop(australia, ext)
plot(aus$geometry)


# buffers -----------------------------------------------------------------

buf <- st_buffer(koala_proj, dist = 1.5e5)

plot(st_geometry(australia))
plot(koala_proj, add = TRUE, pch = 16, cex = 0.5)

plot(st_geometry(buf), add = TRUE, border = "red")

buf <- st_buffer(koala_proj, dist = 1.5e5) %>% 
  st_union()

plot(st_geometry(australia))
plot(koala_proj, add = TRUE, pch = 16, cex = 0.5)
plot(st_geometry(buf), add = TRUE, border = "red")

# intersection ------------------------------------------------------------
koala_sts <- st_intersection(koala_proj, australia)
plot(koala_sts["STATE"])

table(koala_sts$STATENAME)


# distance ----------------------------------------------------------------
costline <- st_union(australia) %>% 
  st_cast("MULTILINESTRING")

plot(costline)

koala_proj$dist <- as.numeric(st_distance(koala_proj, costline))

plot(costline$geometry)
plot(koala_proj["dist"], add = TRUE)

# self-distance 
koala_dist <- st_distance(koala_proj, koala_proj)
head(koala_dist)

mindist <- function(x) {
  mindis <- vector(mode = "numeric", length = nrow(x))
  for(i in 1:nrow(x)){
    mindis[i] <- min(x[i, -i])
  }
  return(mindis)
}

koala_proj$ndist <- mindist(koala_dist)

plot(koala_proj["ndist"])


# writing spatial ---------------------------------------------------------

st_write(koala_proj, "data/new_koala.gpkg")

st_read("data/new_koala.gpkg")



##############     Rasters in R      ################
library(rgdal)
library(raster)
library(tidyverse)
library(sf)
library(maps)
library(rgeos)

####################

r <- matrix(0,nrow =3 , ncol=3)
r[,1] <- c(1,2,3)
r[,2] <- c(5,6,7)
r[,3] <- c(8:10)


rr <- raster(r)
plot(rr)

res(rr)
ncol(rr)
nrow(rr)
ncell(rr)
crs(rr)
dim(rr)
range(values(rr))
cellStats(rr,mean)


#### read in a raster
au <- raster("data/australia.tif")
plot(au)

rasterlist_t <- list.files("data/wc_tavg",
                           pattern = ".tif$",
                           full.names = TRUE)
t_stack <- stack(rasterlist_t)
plot(t_stack)

rasterlist_p <- list.files("data/wc_prec",
                           pattern = ".tif$",
                           full.names = TRUE)
p_stack <- stack(rasterlist_p)
plot(p_stack)

t_stack_crop <- crop(t_stack, au)
plot(t_stack_crop[[1]])
t_stack_crop_mask <- mask(t_stack_crop,au)
plot(t_stack_crop_mask[[1]])

t_stack_au <- crop(t_stack, au) %>%
  mask(au)
p_stack_au <- crop(p_stack, au) %>%
  mask(au)
plot(p_stack_au[[1]])
plot(t_stack_au[[1]])

pmean <- mean(p_stack_au)
tmean <- mean(t_stack_au)

plot(pmean)

writeRaster(tmean, "tmean.tif",
            format = "GTiff",
            overwrite = TRUE)

test <- rr + rr
plot(test)

tcat <- matrix(0, nrow = 4, ncol = 3)
cellStats(tmean, quantile)
tcat[,1] <- c(4,19,22,25)
tcat[,2] <- c(19,22,25,30)
tcat[,3] <- c(1:4)

tcat_au <- reclassify(tmean, 
                      tcat, 
                      right=FALSE,
                      overwrite=TRUE)
plot(tcat_au)

pcat <- matrix(0, nrow = 2, ncol = 3)
cellStats(pmean, max)
pcat[,1] <- c(0,40)
pcat[,2] <- c(40,380)
pcat[,3] <- c(1:2)

pcat_au <- reclassify(pmean, 
                      pcat, 
                      right=FALSE,
                      overwrite=TRUE)
plot(pcat_au)

###### make categories combinations

climate_au <- tcat_au + (pcat_au *10)
plot(climate_au)



###### interactions

states <- st_read("data/AU_states.shp")
states1 <- as(states, "Spatial") #readOGR("data/AU_states.shp")

head(koala)
crs(koala_sf)
crs(climate_au)
crs(states)

crs(climate_au) <- crs("+proj=longlat +datum=WGS84 +no_defs")
st_crs(koala_sf) <- crs("+proj=longlat +datum=WGS84 +no_defs")

koala_climate <- raster::extract(climate_au,
                                 koala_sf,
                                 na.rm = TRUE)
hist(koala_climate)
table(koala_climate)


states_pr <- st_transform(states, 
                          crs(climate_au))
crs(states_pr)

state_climate <- raster::extract(climate_au,
                                 states_pr, 
                                 factors = TRUE,
                                 df=TRUE,
                                 na.rm = TRUE)


class.counts <- table(state_climate$ID, 
                    state_climate$layer) %>% 
    as.matrix.data.frame() %>%
    as.data.frame() %>%
    setNames(paste0("class", c(11,12,13,14,21,22,23,24)))

class.counts$OBJECTID <- as.numeric(rownames(class.counts))

states_pr <- left_join(states_pr, class.counts, by = "OBJECTID")
plot(states_pr["class12"])

### plotting
par(mar = c(2,2,2,2))
plot(climate_au, legend = FALSE)
colors <- c("white", "yellow", "lightgreen", "darkgreen",
  "lightblue", "blue", "purple", "red")
breaks <- c(10,11,12,13,14,21,22,23,24)
plot(climate_au, col = colors, breaks= breaks)
plot(states1, add = TRUE, border = "red", col = NA )
plot(koala_sf, add = TRUE, pch = 10)
maps::map.scale(120,-40, relwidth = 0.25, 
                metric = TRUE, ratio = false,
                cex = 0.6)
par(xpd = NA)
leg <- c("dry cold", "dry cool", "dry warm", "dry hot",
      "wet cold", "wet cool", "wet warm", "wet hot")

legend(155, y=-15, legend = leg, fill = colors)

# ggplot plotting --------------------------------------------------------
library(tidyverse)
library(viridis)

ncell(tmean)

sam <- sampleRegular(tmean, 5e5, asRaster = TRUE) %>% 
  as.data.frame(xy = TRUE, na.rm = TRUE)
head(sam)

ggplot(data = sam, aes(x = x, y = y, fill = layer)) +
  geom_raster() +
  scale_fill_viridis(option = "A") +
  labs(x = "Longitude", y = "Latitude", 
       fill = "Temperature") +
  coord_sf(crs = 4326) +
  theme_bw()

ggplot() +
  geom_sf(data = australia) +
  geom_sf(data = koala_proj, alpha = 0.5, 
          aes(fill = as.factor(Species)), show.legend = "point") +
  labs(x = "Longitude", y = "Latitude", fill = "Koala") +
  theme_bw() + 
  theme(panel.background = element_rect(fill = "deepskyblue3"))

esquisse::esquisser()

library(mapview)

mapview(koala_sf)

library(mapedit)

editMap(mapview(koala_sf))


ggplot(data = wmap) +
  geom_sf()
