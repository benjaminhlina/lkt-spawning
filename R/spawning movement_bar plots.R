# gather from tidyr is like reshape -----
# load packages ----

library(dplyr)
library(forcats)
library(ggplot2)
library(ggh4x)
library(here)
library(lubridate)
library(readr)

# bring in downloaded data exported from VUE ----

lt <- read_rds(here::here("Saved data", 
                          "kenauk lake trout 2017 - 2020.rds"))
# mutate(
#   letter_name = fct_rev(letter_name)
# )


glimpse(lt)


# filter for 2017 and 2018 ----
# 
# lt_oct_2017 <- lt %>% 
#   filter(season_year == "Fall 2017") %>% 
#   filter(month == "October") %>% 
#   filter(sensor_unit %in% c("m", NA, "m/s²"))
# 
# lt_oct_2018 <- lt %>% 
#   filter(season_year == "Fall 2018") %>% 
#   filter(month == "October") %>% 
#   filter(sensor_unit %in% c("m", NA, "m/s²"))
# 
# lt_oct_2019 <- lt %>% 
#   filter(season_year == "Fall 2019") %>% 
#   filter(month == "October") %>% 
#   filter(sensor_unit %in% c("m", NA, "m/s²"))


lt_oct <- lt %>% 
  filter(month == "October" & sensor_unit %in% c("m", NA, "m/s²"))

# determine rec_frequences ----

lt_rec_freq <- lt_oct %>% 
  group_by(year, name) %>% 
  summarise(
    n = n()
  ) %>% 
  ungroup()


lt_rec_freq
# lt_rec_2017 <- data.frame(table(lt_oct_2017$name))
# 
# 
# lt_rec_2018 <- data.frame(table(lt_oct_2018$name))
# 
# 
# lt_rec_2019 <- data.frame(table(lt_oct_2019$name))
# 
# lt_rec_2017
# lt_rec_2018
# lt_rec_2019


# determine how many fish were heard on both 5 and 1 -----

rec_5_1 <- lt_oct %>%
  filter(name %in% c(1, 5)) %>%
  arrange(detection_timestamp_utc) 
# rec_5_1 <- lt_oct_2018 %>%
#   filter(name %in% c(1, 5)) %>% 
#   arrange(detection_timestamp_utc)
# 
# 
# rec_5_1_2019 <- lt_oct_2019 %>%
#   filter(name %in% c(1, 5)) %>% 
#   arrange(detection_timestamp_utc)

unique(basin_move$floy_tag)
tail(basin_move)
basin_move <- rec_5_1 %>% 
  group_by(floy_tag, fish_basin, transmitter_serial, year) %>% 
  summarise(counts = n_distinct(name)) %>% 
  ungroup() %>% 
  mutate(
    fish_basin = factor(
      stringr::str_remove(fish_basin, " Basin"), 
      levels = c("East", "West", "North")
    )
  ) %>% 
  arrange(
    fish_basin, counts
  ) %>% 
  mutate(
    floy_tag = factor(
      floy_tag, 
      levels = 
        c("05550", "05809", "07044", "07478", "07969",
          "1155", "1156",  "1161", "1162", "1163",
          "1170", "05812", "05781", "05816", "07046",
          "1158", "1165",  "1657", "1659", "1669",  
          "1670", "05804", "05817", "07001", "07025",
          "1652", "1654", "1655", "1658", "07975",
          "1157"))
  )

basin_move
# basin_move_2019 <- rec_5_1_2019 %>% 
#   group_by(transmitter_serial) %>% 
#   summarise(counts = n_distinct(name)) %>% 
#   ungroup()
# 
# basin_move_2019

basin_move_rec_both <- basin_move %>% 
  filter(counts == 2) %>% 
  group_by(fish_basin, year) %>% 
  summarise(
    n = n()
  ) %>% 
  ungroup()

basin_move_rec_both



fish_move_basin <- rec_5_1 %>% 
  filter(transmitter_serial %in% basin_move_rec_both$transmitter_serial) %>% 
  arrange(transmitter_serial, detection_timestamp_utc)

# fish_move_basin_2019 <- rec_5_1_2019 %>% 
#   filter(transmitter_serial %in% basin_move_rec_both_2019$transmitter_serial) %>% 
#   arrange(transmitter_serial, detection_timestamp_utc)




# look at movement within a day -----

m <- unique(fish_move_basin$transmitter_serial)

m

fish_move_basin$days <- day(fish_move_basin$detection_timestamp_utc) 




glimpse(fish_move_basin)




between_b <- fish_move_basin %>% 
  group_by(days, transmitter_serial, year) %>%
  summarise(rec_count = n_distinct(name)) %>% 
  ungroup()





between_b <- between_b %>% 
  filter(rec_count == 2)




between_b


# individuals_between <- between_b %>% 
#   group_by(days) %>% 
#   summarise(d = ) %>% 
#   ungroup()

# individuals_between

unique(between_b$transmitter_serial)
# between_bs <- fish_move_basin %>% 
#   group_by(days) %>%
#   summarise(rec_count = n_distinct(name)) %>% 
#   ungroup()
# between_bs

# 
# openxlsx::write.xlsx(between_b, here::here("Fish and tagging data",
#                                            "Receiver Downloads", 
#                                            "spawning movement", 
#                                            "between_basin_days_oct.xlsx"))

# plot these both in frequency plot as well as a bubble plot and map -----

p <- ggplot(data = lt_oct) + 
  stat_count(aes(x = letter_name, fill = receiver_basin), 
             colour = "black") + 
  facet_wrap(. ~ year)
p




# look at unique fish id at recs 
glimpse(lt_oct)

tf <- lt_oct %>%
  group_by(letter_name, receiver_basin, year) %>%
  summarize(rcount = n_distinct(transmitter_serial)) %>%
  ungroup()
tf





p3 <- ggplot(data = tf) + 
  geom_col(aes(x = letter_name, y = rcount, fill = receiver_basin), 
           colour = "black") + 
  facet_wrap(. ~ year)
p3






lt_fall <- lt %>% 
  filter(season %in% "Fall" & sensor_unit %in% c("m", NA, "m/s²"))

# write_rds(file = here("saved data", 
#                "lt_fall.rds"), x = lt_fall)


p5 <- ggplot(data = lt_oct,
             aes(x = factor(transmitter_serial), 
                 y = letter_name)) + 
  geom_abline(aes(intercept = name_order,
                  slope = 0,
                  colour = receiver_basin), linewidth  = 1 ) +
  geom_count(aes(colour = fish_basin)) +
  facet_wrap(.~ year) + 
  coord_cartesian(ylim = c(0, 2))
scale_color_discrete(name = "Receiver Basin") + 
  scale_size_continuous(name = "Detection Count", 
                        breaks = seq(0, 15000, 2500)) + 
  theme_classic() +  
  theme(axis.text.x = element_text(size = 8, angle = 90, hjust = 1,
                                   colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 15, face = "bold"), 
        strip.background = element_blank(),
        strip.text = element_text(size = 15, face = "bold"),
        legend.text.align = 0.5) + 
  labs(x = "Transmitter ID", 
       y = "Reciever Number")

p5



basin_move





p7 <- ggplot(data = basin_move) + 
  geom_col(aes(x = floy_tag, y = counts, 
               fill = 
                 # factor(counts)
                 fish_basin
  ), 
  colour = "black",
  alpha = 0.75) +
  # scale_fill_viridis_d(
  #   begin = 0.15, 
  #   end = 0.85, 
  #   option = "E",
  #   direction = -1,
  #   alpha = 0.35,
  #   name = "Frequency") + 
  scale_fill_manual(
    name = "Basin", 
    values = c("#2D0089", "#CB3E59",
               "#F9942B"
    )
    
  ) + 
  scale_y_continuous(breaks = seq(0, 2, 1), 
                     expand = expansion(mult = c(0, 0.05))) +
  facet_wrap(. ~ year, scale = "free_x") + 
  theme_bw(base_size = 15) + 
  # coord_cartesian(ylim = c(0, 2)) + 
  theme(
    panel.grid = element_blank(), 
    strip.background = element_blank(), 
    # legend.position = "none", 
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) + 
  labs(
    x = "Fish ID", 
    y = "Number of Receivers Detected On"
  )


p7


ggsave(here::here(
  "Plots", 
  "Frequency Between East and West", 
  "frequency_plot_for_oct_movment.png"
), width = 11, height = 8.5)




p8 <- ggplot(data = lt_fall,
             aes(x = factor(transmitter_serial), 
                 y = letter_name)) + 
  geom_abline(aes(intercept = name_order,
                  slope = 0,
                  colour = receiver_basin), size  = 1 ) +
  geom_count(aes(colour = fish_basin)) +
  
  scale_color_viridis_d(name = "Receiver Basin", 
                        option = "G", 
                        begin = 0.25,
                        end = 0.65
  ) + 
  scale_size_continuous(name = "Detection Count", 
                        breaks = seq(0, 15000, 2500)) + 
  facet_grid(year ~ month) + 
  theme_classic() +  
  theme(axis.text.x = element_text(size = 8, angle = 90, hjust = 1,
                                   colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 15, face = "bold"), 
        strip.background = element_blank(),
        strip.text = element_text(size = 15, face = "bold"),
        legend.text.align = 0.5) + 
  labs(x = "Transmitter ID", 
       y = "Reciever Number")



# p8



# save plots -----
ggsave(here::here(
  "Plots", 
  "Bubble Plots", 
  "bubble_plot_for_oct_movment.png"
), width = 11* 1.5, height = 8.5 * 1.5)



  
  

basin_move_fall <- lt_fall %>%
  filter(name %in% c(1, 5)) %>%
  arrange(detection_timestamp_utc) %>% 
  group_by(floy_tag, fish_basin, transmitter_serial, month, year) %>% 
  summarise(counts = n_distinct(name)) %>% 
  ungroup() %>% 
  mutate(
    fish_basin = factor(
      stringr::str_remove(fish_basin, " Basin"), 
      levels = c("East", "West", "North")
    )
  ) %>% 
  arrange(
    fish_basin, counts
  ) %>% 
    mutate(floy_tag = factor(
      floy_tag, 
      levels = c("05550", "05809", "07044", "07478", "07969",
                 "1155",  "1156",  "1161",  "1162",  "1163",
                 "1170",  "05812", "05781", "07046", "1158",  
                 "1167",  "1657",  "1659",  "1669",  "1670",  "2",  
                 "05804", "05816", "05817", "07001", "07025", "1165",  
                 "1652",  "1654",  "1655",  "1658", "07975", "1157"
      )
    )
    )

basin_move_fall    
p9 <- ggplot(data = basin_move_fall) + 
  geom_col(aes(x = floy_tag, y = counts, 
               fill = 
                 # factor(counts)
                 fish_basin
  ), 
  colour = "black",
  alpha = 0.75) +
  # scale_fill_viridis_d(
  #   begin = 0.15, 
  #   end = 0.85, 
  #   option = "E",
  #   direction = -1,
  #   alpha = 0.35,
  #   name = "Frequency") + 
  scale_fill_manual(
    name = "Basin", 
    values = c("#2D0089", "#CB3E59",
               "#F9942B"
    )
    
  ) + 
  scale_y_continuous(breaks = seq(0, 2, 1), 
                     expand = expansion(mult = c(0, 0.05))) +
  facet_nested(year ~ month + fish_basin, scale = "free_x", space = "free_x") + 
  theme_bw(base_size = 15) + 
  # coord_cartesian(ylim = c(0, 2)) + 
  theme(
    panel.grid = element_blank(), 
    strip.background = element_blank(), 
    # legend.position = "none", 
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) + 
  labs(
    x = "Fish ID", 
    y = "Number of Receivers Detected On"
  )
# p9

ggsave(here::here(
  "Plots", 
  "Frequency Between East and West", 
  "frequency_plot_fall_movement.png"
), width = 11* 1.5, height = 8.5 * 1.5, plot = p9)



