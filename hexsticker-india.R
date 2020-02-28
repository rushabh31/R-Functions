library(tidyverse)
library(raster)
library(sf)
ind <- getData("GADM", country = "IND", level = 0) %>%
  disaggregate() %>%
  geometry()

ggplot() + 
  geom_sf(data = st_as_sf(ind))

hex_points <- ind %>%
  spsample(type = "hexagonal", cellsize = 2)

as_tibble(hex_points@coords)

ind_hex <- HexPoints2SpatialPolygons(hex_points, dx = 2)

ggplot() + 
  geom_sf(data = st_as_sf(ind)) + 
  geom_sf(data = st_as_sf(ind_hex), colour = "blue", fill = NA)


ind_plot <- hexwall(
  path="C:/Users/58675/Documents/hex-stickers-master/hex-stickers-master/SVG",
  sticker_width = 500,
  coords = hex_points@coords,
  sort_mode = "colour"
)
