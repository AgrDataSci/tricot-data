# .............................................
# Write the map with available datasets #####
# first run: September 2025
# updated: September 2025
# K de Sousa

# load required packages
library("geodata")
library("sf")

# read trial coordinates from csv file
xy = read.csv("docs/trial-xy.csv")

# load world administrative boundaries
adm = world(path = "docs")

# convert to simple features format
adm = st_as_sf(adm)

# remove antarctica to clean the map
adm = adm[adm$NAME_0 != "Antarctica", ]

# select relevant columns: longitude, latitude, and crop name
coord = xy[c("longitude", "latitude","crop_name")]

# remove rows with missing values
coord = na.omit(coord)

# create map of trial locations
trialmap =
  ggplot() +
  geom_sf(adm$geometry, mapping = aes(), 
          colour = "#4d4d4d", 
          fill = "#f7fbff") +         # draw world map
  geom_jitter(data = coord, aes(x = longitude,
                                y = latitude, 
                                colour = crop_name)) + # add trial points
  theme_void() + 
  scale_color_brewer(palette = "Set1") + # set color palette
  theme(legend.position = c(0.05, 0.65),
        legend.text = element_text(size = 12),
        legend.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white"),
        plot.margin = unit(c(1,1,1,1), "mm"),
        legend.title = element_blank()) 

# display map
trialmap

# save map to file as png
ggsave("docs/trial-locations.png",
       plot = trialmap,
       width = 36,
       height = 18,
       units = "cm",
       dpi = 400)
