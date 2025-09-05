library("geodata")
library("sf")


adm = world(path = "docs")

adm = st_as_sf(adm)

adm = adm[adm$NAME_0 != "Antarctica", ]

coord = xy[c("longitude", "latitude","crop_name")]

coord = na.omit(coord)

ggplot() +
  geom_sf(adm$geometry, mapping = aes(), 
          colour = "#4d4d4d", 
          fill = "#f7fbff") +
  geom_jitter(data = coord, aes(x = longitude,
                                y = latitude, 
                                colour = crop_name)) +
  theme_void() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size = 12),
        panel.background = element_blank(),
        plot.margin = unit(c(1,1,1,1), "mm"),
        legend.title = element_blank()) 

write.csv()

