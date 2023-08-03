# gather from tidyr is like reshape -----
# load packages ----
{
  library(cowplot)
  library(dplyr)
  library(forcats)
  library(ggplot2)
  library(ggspatial)
  library(ggrepel)
  library(ggh4x)
  library(here)
  library(lubridate)
  library(patchwork)
  library(readr)
  library(sf)
}

# ---- bring in data ---- 


p_map <- st_read(dsn = here::here("Shapefiles",
                                  "Lake Papineau Shapefile", 
                                  "."),
                 layer = "plake_edit_wo_link")

spawning_hab <- st_read(dsn = here::here("Shapefiles",
                                         "Spawning Habitat Shapefile", 
                                         "."),
                        layer = "lt_spawn_sites")


# ---- map ---- 

p <- ggplot() + 
  geom_sf(data = p_map, alpha = 0.5) + 
  geom_sf(data = spawning_hab,
          fill = "#CB3E59", 
          colour = "black"
          # alpha = 0.5
  ) +
  scale_x_continuous(breaks = seq(-74.80, -74.74, by = 0.02)) +
  # annotation_north_arrow(style = north_arrow_nautical,
  #                        # height = unit(1.5, "cm"), width = unit(1.5, "cm"),
  #                        # pad_x = unit(2.5, "in"), pad_y = unit(5.75, "in"),
  #                        location = "tl",
  #                        which_north = "true") + 
  annotation_scale() +
  coord_sf(
    # ylim = c(45.764, 45.9125), 
    # xlim = c(-74.805, -74.725)
    # xlim = c(-74.798, -74.67)
  ) +
  theme_bw() +
  # theme_void() + 
  theme(
    panel.grid = element_blank(), 
    strip.background = element_blank(),
    plot.background = element_blank(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank()
    # axis.text.x = element_text(angle = 90)
  ) + 
  labs(
    x = "", 
    y = ""
  )

# p


inlays <- 
  # ggplotGrob(
  ggplot() + 
  geom_sf(data = p_map, alpha = 0.5) + 
  geom_sf(data = spawning_hab,
          fill = "#CB3E59", 
          colour = "black"
  ) + 
  geom_label_repel(data = spawning_hab, 
                   aes(label = site, geometry = geometry),
                   stat = "sf_coordinates", size = 4,
                   # label.size = 4,
                   force = 5.0, 
                   nudge_y = 0.0035, 
                   nudge_x = 0.003,
                   arrow = arrow(angle = 30,  
                                 length = unit(0.20, "cm")), 
                   point.padding = 0.65
                   ) +
  coord_sf(
    ylim = c(45.777, 45.825), 
    xlim = c(-74.78, -74.735)
  ) +
  annotation_north_arrow(style = north_arrow_nautical,
                         location = "tr",
                         which_north = "true") +
  ggspatial::annotation_scale(width_hint = 0.1, 
                              text_pad = unit(0.05, "cm")) +
  theme_bw( 
    # base_size = 17
  ) + 
  theme(
    axis.text = element_text(colour = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    # axis.text.y = element_text(angle = 90), 
    legend.title.align = 0.5,
    legend.position = c(0.8195, 0.165), 
    legend.key.size = unit(0.01, 'cm'),
    # legend.title = element_text(size = 8.5),
    # legend.text = element_text(size = 7.5),
    legend.background = element_blank(), 
    # plot.margin = grid::unit(c(0,0,0,0), "mm"), 
    plot.background = element_rect(colour = "transparent"),
    panel.background = element_rect(colour = "transparent"),
    panel.grid = element_blank(),
    strip.background = element_blank(), 
  ) +  
  labs(x = 
         "Longitude",
       y = 
         "Latitude"
  )

# inlays






p4 <- inlays + inset_element(p, 
                             left = 0.7, right = 1.0,
                             top = 0.98, bottom = 0.46,
                             # align_to = "plot",
                             clip = TRUE
)


# p4






ggsave(
  filename = here("Plots", 
                  "Spawning Locations", 
                  "pap_lake_spawning_location_inset_number_edit.png"), 
  height = 11, width = 8.5
)
ggsave(
  filename = here("Plots", 
                  "Spawning Locations", 
                  "pap_lake_spawning_location_inset_number_edit.pdf"), 
  height = 11, width = 8.5
)


p1 <- ggplot() + 
  geom_sf(data = p_map, alpha = 0.5) + 
  geom_sf(data = spawning_hab,
          fill = "#CB3E59", 
          colour = "black"
          # alpha = 0.5
  ) +
  scale_x_continuous(breaks = seq(-74.80, -74.74, by = 0.02)) +
  annotation_north_arrow(style = north_arrow_nautical,
                         # height = unit(1.5, "cm"), width = unit(1.5, "cm"),
                         # pad_x = unit(2.5, "in"), pad_y = unit(5.75, "in"),
                         location = "tl",
                         which_north = "true") +
  annotation_scale() + 
  coord_sf(
    # ylim = c(45.764, 45.9125), 
    # xlim = c(-74.805, -74.725)
    # xlim = c(-74.798, -74.67)
  ) +
  theme_bw(base_size = 15) +
  # theme_void() + 
  theme(
    panel.grid = element_blank(), 
    strip.background = element_blank(),
    plot.background = element_blank()
    # axis.text.x = element_text(angle = 90)
  ) + 
  labs(
    x = "Longitude", 
    y = "Latitude"
  )

p1
