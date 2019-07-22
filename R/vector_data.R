library(sf)
library(rworldmap)
library(tidyverse)

wmap <- rworldmap::getMap(resolution = "low") %>% 
  st_as_sf()

plot(wmap["BIODIVERSITY"])

par(bty="l")

st_multipoint(rbind(c(1, 1.0), c(1.5, 2), c(2, 2), c(2.5, 1.5))) %>% 
  plot(axes = TRUE, ylim = c(0.5, 2), xlim = c(0.5, 3))

st_polygon(list(rbind(c(1, 1.0), c(1.5, 2), c(2, 2), c(2.5, 1.5), c(1, 1.0))))

plot(axes = TRUE, ylim = c(0.5, 2), xlim = c(0.5, 3), main = "Polygon", col = "lightblue")

library(maps)
data(world.cities)
head(world.cities)

cities <- st_as_sf(world.cities, coords = c("long", "lat"), crs = 4326) %>% 
  filter(country.etc == "Australia")
plot(australia_clip$geometry)
plot(cities["pop"], cex = cities$pop / 1e6, lwd = 2, add = TRUE)

plot(states$geometry)

simp_asu <- st_simplify(states)
plot(simp_asu$geometry)


