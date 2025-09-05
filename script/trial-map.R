library("geodata")
library("sf")

xy = read.csv("docs/trial-xy.csv")

xy = xy[!duplicated(xy$block_id),]

adm = world(path = "docs")

adm = st_as_sf(adm)

adm = adm[adm$NAME_0 != "Antarctica", ]

coord = xy[c("longitude", "latitude","crop_name")]

coord = na.omit(coord)

trialmap =
  ggplot() +
  geom_sf(adm$geometry, mapping = aes(), 
          colour = "#4d4d4d", 
          fill = "#f7fbff") +
  geom_jitter(data = coord, aes(x = longitude,
                                y = latitude, 
                                colour = crop_name)) +
  theme_void() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = c(0.05, 0.65),
        legend.text = element_text(size = 12),
        legend.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white"),
        plot.margin = unit(c(1,1,1,1), "mm"),
        legend.title = element_blank()) 

trialmap

ggsave("docs/trial-locations.png",
       plot = trialmap,
       width = 36,
       height = 18,
       units = "cm",
       dpi = 200)

