# gather from tidyr is like reshape -----
# load packages ----
{
  library(dplyr)
  library(forcats)
  library(ggplot2)
  library(ggh4x)
  library(here)
  library(lubridate)
  library(patchwork)
  library(readr)
  library(sf)
}

# ---- bring in data ---- 

dat <- read_rds(
  here("saved data", 
       "lt_fall.rds")
)

p_map <- st_read(dsn = here::here("Shapefiles",
                                  "Lake Papineau Shapefile", 
                                  "."),
                 layer = "plake_edit_wo_link")




# --- fall movment between 1-5-23 ---

channel <- dat %>% 
  filter(name %in% c(1, 5, 23)) %>% 
  mutate(
    fish_basin = factor(
      stringr::str_remove(fish_basin, " Basin"), 
      levels = c("East", "West", "North")
    )
  )


glimpse(channel)


n_obs <- channel %>% 
  group_by(name, month, year, long_mean, lat_mean) %>% 
  summarise(
    n = n_distinct(floy_tag)
  ) %>% 
  ungroup() %>% 
  st_as_sf(., coords = c("long_mean", "lat_mean"), 
           crs = st_crs(p_map))

n_obs

n_obs_basin <- channel %>% 
  group_by(name, month, year, long_mean, lat_mean, fish_basin) %>% 
  summarise(
    n = n_distinct(floy_tag)
  ) %>% 
  ungroup() %>% 
  st_as_sf(., coords = c("long_mean", "lat_mean"), 
           crs = st_crs(p_map))

n_obs_basin



# ---- bubble plot ----

p <- ggplot() +
  geom_sf(data = p_map) + 
  geom_sf(data = n_obs, aes(size = n)) +
  facet_grid(year ~ month) +
  scale_size(name = "Number of Fish") + 
  theme_void() + 
  theme_bw(base_size = 15) + 
  theme(
    panel.grid = element_blank(), 
    strip.background = element_blank(), 
    axis.text.x = element_text(angle = 90)
  ) + 
  labs(
    x = "Longitude", 
    y = "Latitude"
  )

p  
p1 <- ggplot() +
  geom_sf(data = p_map) + 
  geom_sf(data = n_obs_basin %>% 
            filter(fish_basin == "East"), aes(size = n),
          colour = "#2D0089", 
          # alpha = 0.5
  ) +
  facet_grid(year ~ month) +
  scale_size_continuous(name = "Frequency", 
                        breaks = seq(2, 8, 2), 
                        range = c(2, 6)) + 
  theme_void(base_size = 15) + 
  # theme_bw(base_size = 15) + 
  theme(
    panel.grid = element_blank(), 
    strip.background = element_blank(), 
    legend.position = c(1.35, 0.525)
    # axis.text.x = element_text(angle = 90)
  ) + 
  labs(
    x = "Longitude", 
    y = "Latitude"
  )
p2 <- ggplot() +
  geom_sf(data = p_map) + 
  geom_sf(data = n_obs_basin %>% 
            filter(fish_basin == "West"), aes(size = n),
          colour = "#CB3E59", 
          # alpha = 0.5
  ) +
  facet_grid(year ~ month) +
  scale_size_continuous(name = "Frequency", 
                        breaks = seq(2, 8, 2),
                        range = c(2, 6)) + 
  theme_void(base_size = 15) + 
  # theme_bw(base_size = 15) + 
  theme(
    panel.grid = element_blank(), 
    strip.background = element_blank(), 
    legend.position = c(1.35, 0.525)
    # axis.text.x = element_text(angle = 90)
  ) + 
  labs(
    x = "Longitude", 
    y = "Latitude"
  )

p3 <- ggplot() +
  geom_sf(data = p_map) + 
  geom_sf(data = n_obs_basin %>% 
            filter(fish_basin == "North"), aes(size = n),
          colour = "#F9942B", 
          # alpha = 0.5
  ) +
  facet_grid(year ~ month) +
  scale_size_continuous(name = "Frequency", 
                        breaks = seq(0, 2, 1), range = c(2, 4)
  ) + 
  theme_void(base_size = 15) + 
 
  # theme_bw(base_size = 15) + 
  theme(
    panel.grid = element_blank(), 
    strip.background = element_blank(), 
    legend.position = c(1.35, 0.5475)
    # axis.text.x = element_text(angle = 90)
  ) + 
  labs(
    x = "Longitude", 
    y = "Latitude"
  )

# p3

p4 <- p1 + p2 + p3

# p4


ggsave(filename = here("Plots", 
                       "Bubble Maps", 
                       "fall_bubble_map.png"), 
       plot = p4, height = 8.5 * 1.12, width = 11 * 2.1)

p5 <- ggplot() +
  geom_sf(data = p_map) + 
  geom_sf(data = n_obs_basin, 
          aes(size = n, fill = fish_basin), 
          # alpha = 0.5, 
          shape = 21,
          colour = "black"
  ) +
  facet_nested(year ~ month + fish_basin) +
  scale_size_continuous(name = "Frequency", 
                        # breaks = seq(2, 8, 2), 
                        range = c(2, 6)) + 
   scale_fill_manual(
    name = "Basin", 
    values = c("#2D0089", "#CB3E59",  "#F9942B")
    
  ) + 
  scale_x_continuous(breaks = seq(-74.80, -74.74, by = 0.02)) +
  # theme_void(base_size = 15) + 
  theme_bw(base_size = 15) +
  guides(fill = guide_legend(override.aes = list(size = 6))) + 
  theme(
    panel.grid = element_blank(), 
    strip.background = element_blank(), 
    # legend.position = c(1.15, 0.525),
    axis.text.x = element_text(angle = 90)
  ) + 
  labs(
    x = "Longitude", 
    y = "Latitude"
  )

# p5

ggsave(filename = here("Plots", 
                       "Bubble Maps", 
                       "fall_bubble_map_nested.png"), 
       plot = p5, height = 8.5 * 2, width = 11 * 1.5)
