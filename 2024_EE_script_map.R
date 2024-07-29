### INTRODUCTION #####

# Welcome. For the description of the project please visit: https://github.com/diekei/2024_EE_diekei_host_shift
# Article is available at: 


## LIBRARY ####
library(sf)
library(elevatr)
library(terra)
library(whitebox)
library(tidyterra)
library(giscoR)
library(ggnewscale)
library(ggblend)
library(remotes)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(grid)
library(tidyverse)
library(raster)
library(rgdal)
library(readxl)
library(ggrepel)
library(scales)
library(ggspatial)
library(svglite)


## MAP SULAWESI ####
#map built based on tutorial in https://dominicroye.github.io/en/2022/hillshade-effects/

#getting area of interest
data.indonesia <- ne_states(country = "Indonesia", returnclass = "sf")
plot(st_geometry(data.indonesia))

data.sulawesi <- data.indonesia[data.indonesia$name %in% c("Sulawesi Utara", "Sulawesi Tengah", "Sulawesi Selatan", 
                                                           "Sulawesi Tenggara", "Gorontalo", "Sulawesi Barat"), ]
plot(st_geometry(data.sulawesi))

data.sulawesi.comb <- st_union(data.sulawesi)
plot(st_geometry(data.sulawesi.comb))

#data.sulawesi.comb <- st_make_valid(data.sulawesi.comb)
#data.sulawesi.comb <- st_buffer(data.sulawesi.comb, 0)


#getting body of water
data.sulawesi.lakes <- st_read("data/Sulawesi_lakes_50k.shp")
data.sulawesi.lakes <- st_intersection(data.sulawesi.lakes, data.sulawesi)

data.sulawesi.rivers <- st_read("data/Sulawesi_rivers_outline_50k.shp")
data.sulawesi.rivers <- st_intersection(data.sulawesi.rivers, data.sulawesi)

plot(st_geometry(data.sulawesi.comb), col = "lightgrey")
plot(st_geometry(data.sulawesi.lakes), col = "#c6dbef", add = TRUE)
plot(st_geometry(data.sulawesi.rivers), col = "#c6dbef", add = TRUE)


#getting DEM of the region
data.sulawesi.dem <- get_elev_raster(data.sulawesi, z = 10)
plot(data.sulawesi.dem)

data.sulawesi.dem <- rast(data.sulawesi.dem) %>% mask(vect(data.sulawesi.comb))
plot(data.sulawesi.dem)

data.sulawesi.dem2 <- project(data.sulawesi.dem, crs(data.sulawesi.lakes))
data.sulawesi <- st_transform(data.sulawesi, st_crs(data.sulawesi.lakes))
plot(data.sulawesi.dem2)

#create simple relief map
data.sulawesi.dem.reduced <- aggregate(data.sulawesi.dem, fact = 2, fun = mean)
data.sulawesi.dem.df <- as.data.frame(data.sulawesi.dem.reduced, xy = TRUE)
names(data.sulawesi.dem.df)[3] <- "alt"

test <- ggplot() +
  geom_raster(data = data.sulawesi.dem.df, aes(x, y, fill = alt)) +
  geom_sf(data = data.sulawesi.lakes, fill = "#c6dbef", colour = NA) +
  scale_fill_hypso_tint_c(breaks = c(180, 250, 500, 1000,
                                     1500, 2000, 2500,
                                     3000, 3500, 4000)) +
  guides(fill = guide_colorsteps(barwidth = 20,
                                 barheight = .5,
                                 title.position = "right")) +
  labs(fill = "m") +
  coord_sf() +
  theme_void() +
  theme(legend.position = "bottom")


#calculating hillshade
data.sulawesi.slope <- terrain(data.sulawesi.dem2, "slope", unit = "radians")
plot(data.sulawesi.slope)

data.sulawesi.orientation <- terrain(data.sulawesi.dem2, "aspect", unit = "radians")
plot(data.sulawesi.orientation)

data.sulawesi.hillshade <- shade(data.sulawesi.slope, data.sulawesi.orientation, angle = 45, direction = 300, normalize= TRUE)
plot(data.sulawesi.hillshade, col = grey(1:100/100))

#combine relief and shadow
data.sulawesi.hillshade.reduced <- aggregate(data.sulawesi.hillshade, fact = 2, fun = mean)
data.sulawesi.hillshade.df <- as.data.frame(data.sulawesi.hillshade.reduced, xy = TRUE)

plot.sulawesi.single <- ggplot() +
  geom_raster(data = data.sulawesi.hillshade.df, aes(x, y, fill = hillshade), show.legend = FALSE) +
  scale_fill_distiller(palette = "Greys") +
  new_scale_fill() +
  geom_raster(data = data.sulawesi.dem.df, aes(x, y, fill = alt), alpha = .7) +
  scale_fill_hypso_tint_c(breaks = c(180, 250, 500, 1000,
                                     1500,  2000, 2500,
                                     3000, 3500, 4000)) +
  geom_sf(data = data.sulawesi.lakes, fill = "#c6dbef", colour = NA) +
  guides(fill = guide_colorsteps(barwidth = 20, barheight = .5, title.position = "right")) +
  labs(fill = "m") +
  coord_sf() +
  theme_void() +
  theme(legend.position = "bottom")

plot.sulawesi.single


#multidirection hillshade version
data.sulawesi.hillshade.mult <- map(c(270, 15, 60, 330), function(dir){ 
  shade(data.sulawesi.slope, data.sulawesi.orientation, angle = 45, direction = dir, normalize= TRUE)})

data.sulawesi.hillshade.mult <- rast(data.sulawesi.hillshade.mult) %>% sum()
plot(data.sulawesi.hillshade.mult, col = grey(1:100/100))

data.sulawesi.hillshade.mult.reduced <- aggregate(data.sulawesi.hillshade.mult, fact = 2, fun = mean)
data.sulawesi.hillshade.mult.df <- as.data.frame(data.sulawesi.hillshade.mult.reduced, xy = TRUE)

ggplot() +
  geom_raster(data = data.sulawesi.hillshade.mult.df, aes(x, y, fill = sum), show.legend = FALSE) +
  scale_fill_distiller(palette = "Greys") +
  new_scale_fill() +
  geom_raster(data = data.sulawesi.dem.df, aes(x, y, fill = alt), alpha = .7) +
  scale_fill_hypso_tint_c(breaks = c(180, 250, 500, 1000,
                                     1500,  2000, 2500,
                                     3000, 3500, 4000)) +
  geom_sf(data = data.sulawesi.lakes, fill = "#c6dbef", colour = NA) +
  guides(fill = guide_colorsteps(barwidth = 20, barheight = .5, title.position = "right")) +
  labs(fill = "m") +
  coord_sf() +
  theme_void() +
  theme(legend.position = "top")

plot.sulawesi.mult <- ggplot() +
  geom_raster(data = data.sulawesi.hillshade.mult.df, aes(x, y, fill = sum), show.legend = FALSE) +
  scale_fill_distiller(palette = "Greys") +
  new_scale_fill() +
  geom_raster(data = data.sulawesi.dem.df, aes(x, y, fill = alt), alpha = .7) +
  scale_fill_hypso_tint_c(
    colours = c('#656D4F', '#9B9B84', '#FBEDDB', '#E1D0BC', '#C9A88D', '#775B45', '#482216'),
    breaks = c(180, 250, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000)
  ) +
  geom_sf(data = data.sulawesi.lakes, fill = "#c6dbef", colour = NA) +
  guides(fill = guide_colorsteps(barwidth = 20, barheight = .5, title.position = "right")) +
  labs(fill = "m") +
  coord_sf() +
  theme_void() +
  theme(legend.position = "top")

diverging_palette <- colorRampPalette(c("#acc6b5", "#FBEDDB", "#C9A88D", "#482216"))(7)
palette_df <- data.frame(
  x = 1:7,
  color = diverging_palette
)
ggplot(palette_df, aes(x = x, y = 1, fill = color)) +
  geom_tile() +
  scale_fill_identity() +
  theme_void()

plot.sulawesi.mult <- ggplot() +
  geom_raster(data = data.sulawesi.hillshade.mult.df, aes(x, y, fill = sum), show.legend = FALSE) +
  scale_fill_distiller(palette = "Greys") +
  new_scale_fill() +
  geom_raster(data = data.sulawesi.dem.df, aes(x, y, fill = alt), alpha = .5) +
  scale_fill_hypso_tint_c(
    colours = c('#DDDED8', '#C3C3C3', '#ABABAB', '#939393', '#7F7F7D', '#5B5F62', '#212121'),
    breaks = c(180, 250, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000)
  ) +
  geom_sf(data = data.sulawesi.lakes, fill = "white", colour = NA) +
  guides(fill = guide_colorsteps(barwidth = 20, barheight = .5, title.position = "right")) +
  labs(fill = "m") +
  coord_sf() +
  theme_void() +
  theme(legend.position = "top")

plot.sulawesi.mult
ggsave("Plots/leu_map_base2.png", 
       width = 10, height = 10, unit = "in",
       device = png, dpi = 1200)


## MAP SEASIA ####

data.seasia <- ne_countries(scale = "large", returnclass = "sf")
data.seasia <- data.seasia[data.seasia$admin %in% c("Indonesia", "East Timor", "Singapore", "Malaysia", "Brunei", 
                                                    "Thailand", "Cambodia", "Laos", "Myanmar", "Vietnam", 
                                                    "Philippines", "Papua New Guinea", "Australia"), ]
data.seasia.comb <- st_union(data.seasia)
plot(st_geometry(data.seasia.comb))

data.seasia.dem <- get_elev_raster(test2, z = 5)
plot(data.seasia.dem.130)

# Create a mask for areas above -130 meters
data.seasia.dem.130 <- data.seasia.dem >= -130
data.seasia.dem.130 <- mask(data.seasia.dem, data.seasia.dem.130, maskvalue = 0)
data.seasia.dem.130.df <- as.data.frame(data.seasia.dem.130, xy = TRUE)
names(data.seasia.dem.130.df)[3] <- "alt"

plot.seasia <- ggplot() +
  geom_raster(data = data.seasia.dem.130.df, aes(x = x, y = y, fill = alt), na.rm = TRUE) +
  scale_fill_gradient(low = "grey90", high = "grey90", na.value = NA) +
  geom_sf(data = data.seasia.comb, fill = "grey70", color = "NA") +
  geom_sf(data = data.sulawesi.comb, fill = "red", color = "NA") +
  coord_sf(xlim = c(95, 145), ylim = c(-15, 12), expand = FALSE) +
  labs(x = "\nLongitude", y = "Latitude\n") +
  scale_x_continuous(breaks = seq(100, 140, by = 20)) +
  scale_y_continuous(breaks = seq(-10, 10, by = 10)) +
  annotation_scale(location = "bl", 
                   width_hint = 0.2, 
                   bar_cols = c("black", "black")) +
  theme_bw() +
  theme(panel.border = element_rect(color = "black", size = .5,fill = NA),
        axis.text = element_text(size = 16), 
        axis.title = element_text(size = 16),
        legend.position = 'null')

png("Plots/leu_map_seasia.png", height = 5, width = 5, units = "in", res = 1200)
plot.seasia
dev.off()

ggsave("Plots/leu_map_seasia.png", 
       width = 5, height = 5, unit = "in",
       device = png, dpi = 1200)

ggsave("Plots/leu_map_seasia.svg", 
       width = 5, height = 5,
       device = svg)


## MAP SAMPLING ####

data.sampling <- read_csv("2024_EE_data_map.csv")

convert_dms_to_dd <- function(dms) {
  # Split the DMS string into components
  dms_split <- unlist(strsplit(dms, split = "[°'\" ]"))
  # Remove any empty strings from splitting
  dms_split <- dms_split[dms_split != ""]
  
  # Convert to numeric values
  degrees <- as.numeric(dms_split[1])
  minutes <- as.numeric(dms_split[2])
  seconds <- as.numeric(dms_split[3])
  direction <- dms_split[4]
  
  # Calculate the decimal degrees
  decimal_degrees <- degrees + (minutes / 60) + (seconds / 3600)
  
  # Handle negative values for South and West coordinates
  if (direction %in% c("S", "W")) {
    decimal_degrees <- -decimal_degrees
  }
  
  return(decimal_degrees)
}

data.sampling.cvc <- data.frame(
  lat_dd = sapply(data.sampling$lat, convert_dms_to_dd),
  lon_dd = sapply(data.sampling$lon, convert_dms_to_dd)
)

data.sampling <- cbind(data.sampling, data.sampling.cvc[, c("lat_dd", "lon_dd")])

data.sampling.getalt <- st_as_sf(data.sampling, coords = c("lon_dd", "lat_dd"), crs = 4326)
data.sampling.alt <- get_elev_point(data.sampling.getalt, prj = st_crs(4326), src = "aws")

data.sampling <- cbind(data.sampling, data.sampling.alt[, "elevation"])
data.sampling$host <- factor(data.sampling$host, levels = c("Mikania", "Leucas", "Coleus", "no"))

size_trans <- function() {
  trans_new(name = 'identity200',
            transform = function(x) x * 200,
            inverse = function(x) x / 200)
}

plot.sampling <- ggplot() +
  geom_raster(data = data.sulawesi.hillshade.mult.df, aes(x, y, fill = sum), show.legend = FALSE) +
  scale_fill_distiller(palette = "Greys") +
  new_scale_fill() +
  geom_raster(data = data.sulawesi.dem.df, aes(x, y, fill = alt), alpha = .5) +
  scale_fill_hypso_tint_c(
    colours = c('#DDDED8', '#C3C3C3', '#ABABAB', '#939393', '#7F7F7D', '#5B5F62', '#212121'),
    breaks = c(180, 250, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000)
  ) +
  #geom_sf(data = data.sulawesi.comb, fill = "grey", colour = NA) +
  geom_sf(data = data.sulawesi.lakes, fill = "white", colour = NA) +
  guides(fill = guide_colorsteps(barwidth = .5, barheight = 20, title.position = "right")) +
  labs(fill = "m") +
  coord_sf(xlim = c(118.5, 126), ylim = c(-6, 2), expand = FALSE) +
  theme_void() + 
  new_scale_fill() +
  new_scale_colour() +
  geom_point(data = data.sampling %>% filter(type == "host" & confirmed == "no"), 
             aes(x = lon_dd, y = lat_dd, shape = host), colour = "grey40", fill = "grey40", alpha = 0.5) +
  scale_shape_manual(values = c(21, 24, 25, 1),
                     labels = c("Mikania", "Leucas", "Coleus", "NA")) +
  new_scale_fill() +
  new_scale_colour() +
  geom_point(data = data.sampling %>% filter(type == "host" & confirmed == "yes"),
             aes(x = lon_dd, y = lat_dd, shape = host, colour = host, fill = host)) +
  scale_colour_manual(values = c("#8DD3C7", "#FDB462", "#FCCDE5"),
                      labels = c("Mikania", "Leucas", "Coleus")) +
  scale_fill_manual(values = c("#8DD3C7", "#FDB462", "#FCCDE5"),
                    labels = c("Mikania", "Leucas", "Coleus")) +
  new_scale_fill() +
  new_scale_colour() +
  geom_text_repel(data = data.sampling %>% filter(label == "yes"),
                  aes(x = lon_dd, y = lat_dd, label = pop, colour = beetle), size = 3, max.overlaps = 50) +
  scale_colour_manual(values = c("grey40", "black"),
                      labels = c("w/o beetles", "w/ beetles")) +
  new_scale_fill() +
  new_scale_colour() +
  geom_point(data = data.sampling %>% filter(type == "beetle"), 
             aes(x = lon_dd, y = lat_dd, shape = host, fill = host, size = count), colour = "grey40") +
  geom_rect(aes(xmin = 120.12, xmax = 120.175, ymin = -0.821, ymax = -0.77), color = "red", fill = NA) +
  scale_fill_manual(values = c("#8DD3C7", "#FDB462", "#FCCDE5"),
                    labels = c("Mikania", "Leucas", "Coleus")) +
  scale_size_continuous(trans = size_trans(), breaks = c(5, 10, 15, 20), labels = c(5, 10, 15, 20)) +
  annotation_scale(location = "br", 
                   width_hint = 0.15, 
                   bar_cols = c("black", "black")) +
  theme(legend.position = "right",
        legend.title = element_blank()) +
  guides(fill = F, colour = F)

plot.sampling

ggsave("Plots/leu_map_sampling.png", 
       width = 10, height = 10, unit = "in",
       device = png, dpi = 1200)

ggsave("Plots/leu_map_sampling.svg", 
       width = 10, height = 10,
       device = svg)

ggsave("Plots/leu_map_sampling.pdf", 
       width = 10, height = 10,
       device = pdf)


plot.sampling.inset <- ggplot() +
  geom_sf(data = data.sulawesi.lakes, fill = "white", colour = NA) +
  coord_sf(xlim = c(120.12, 120.175), ylim = c(-0.821, -0.77), expand = FALSE) +
  theme_bw() +
  geom_point(data = data.sampling %>% dplyr::filter(type == "beetle"), 
             aes(x = lon_dd, y = lat_dd, shape = host, fill = host, size = count), colour = "grey40") +
  geom_point(data = data.sampling %>% dplyr::filter(type == "host" & confirmed == "no"), 
             aes(x = lon_dd, y = lat_dd, shape = host), colour = "grey40", fill = "grey40", alpha = 0.5) +
  geom_point(data = data.sampling %>% dplyr::filter(type == "host" & confirmed == "yes"),
             aes(x = lon_dd, y = lat_dd, shape = host, colour = host, fill = host)) +
  scale_shape_manual(values = c(21, 24, 25, 1)) +
  scale_colour_manual(values = c("#8DD3C7", "#FDB462", "#FCCDE5", "grey40", "black")) +
  scale_fill_manual(values = c("#8DD3C7", "#FDB462", "#FCCDE5")) +
  new_scale_fill() +
  new_scale_colour() +
  geom_text_repel(data = data.sampling,
                  aes(x = lon_dd, y = lat_dd, label = pop, colour = beetle), size = 3, max.overlaps = 50) +
  scale_colour_manual(values = c("grey40", "black"),
                      labels = c("w/o beetles", "w/ beetles")) +
  scale_size_continuous(breaks = c(5, 10, 15, 20), labels = c(5, 10, 15, 20)) +
  scale_x_continuous(breaks = seq(120.12, 120.16, by = 0.02)) +
  scale_y_continuous(breaks = seq(-0.811, -0.771, by = 0.02)) +
  annotation_scale(location = "tr", 
                   width_hint = 0.3, 
                   bar_cols = c("black", "black")) +
  theme(
    legend.position = 'none',
    axis.title = element_blank(), 
    axis.ticks = element_blank(),
    axis.text = element_text(size = 10),
    plot.background = element_rect(fill = NA, color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

plot.sampling.inset

ggsave("Plots/leu_map_sampling_inset.png", 
       width = 3, height = 3, unit = "in",
       device = png, dpi = 1200)

ggsave("Plots/leu_map_sampling_inset.svg", 
       width = 3, height = 3,
       device = svg)

ggsave("Plots/leu_map_sampling_inset.pdf", 
       width = 3, height = 3,
       device = pdf)

#png("Plots/leu_map_combined.png", height = 10, width = 10, units = "in", res = 1200)
plot.sampling
print(plot.sampling.inset, vp = viewport(0.28, 0.859, width = 0.25, height = 0.25))
print(plot.seasia, vp = viewport(0.28, 0.859, width = 0.25, height = 0.25))
#dev.off()

