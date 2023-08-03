# gather from tidyr is like reshape -----
# load packages ----

library(dplyr)
library(forcats)
library(ggplot2)
library(here)
library(lubridate)
library(magrittr)
library(readr)

# default map colours -----
water <- c("#E0FFFF", "#FFFFFF")


kud_colours <- c("#E0FFFF", "#ff5050", "#FFFFFF")

# plot papineau map ----



# bring in downloaded data exported from VUE ----

lt <- read_rds(here::here("Saved data", 
                          "kenauk lake trout 2017 - 2020.rds"))


glimpse(lt)

unique(lt$month)
# filter for 2017 and 2018 ----

lt_nov_2017 <- lt %>% 
  filter(seasons == "Fall 2017") %>% 
  filter(month == "November") %>% 
  filter(sensor_unit %in% c("m", NA, "m/s²"))

lt_nov_2018 <- lt %>% 
  filter(seasons == "Fall 2018") %>% 
  filter(month == "November") %>% 
  filter(sensor_unit %in% c("m", NA, "m/s²"))



lt_nov_2019 <- lt %>% 
  filter(season_year == "Fall 2019") %>% 
  filter(month == "November") %>% 
  filter(sensor_unit %in% c("m", NA, "m/s²"))
# determine rec_frequences ----

lt_rec_2017 <- data.frame(table(lt_nov_2017$name))


lt_rec_2018 <- data.frame(table(lt_nov_2018$name))

lt_rec_nov_2019 <- data.frame(table(lt_nov_2019$name))

lt_rec_2017
lt_rec_2018

lt_nov_2017$letter_name %<>%
  fct_rev()


lt_nov_2018$letter_name %<>%
  fct_rev()


# determine how many fish were heard on both 5 and 1 -----

rec_5_1 <- lt_nov_2018 %>%
  filter(name %in% c(1, 5)) %>% 
  arrange(detection_timestamp_utc)



basin_move <- rec_5_1 %>% 
  group_by(transmitter_serial) %>% 
  summarise(counts = n_distinct(name)) %>% 
  ungroup()

basin_move_rec_both <- basin_move %>% 
  filter(counts == 2) %>% 
  arrange(transmitter_serial)



fish_move_basin <- rec_5_1 %>% 
  filter(transmitter_serial %in% basin_move_rec_both$transmitter_serial) %>% 
  arrange(transmitter_serial, detection_timestamp_utc)



# look at movement within a day -----

m <- unique(fish_move_basin$transmitter_serial)

m

fish_move_basin$days <- day(fish_move_basin$detection_timestamp_utc) 



glimpse(fish_move_basin)





between_b <- fish_move_basin %>% 
  group_by(days, transmitter_serial) %>%
  summarise(rec_count = n_distinct(name)) %>% 
  ungroup()

between_b %<>%
  filter(rec_count == 2)


between_b

individuals_between <- between_b %>% 
  group_by(days) %>% 
  summarise(d = ) %>% 
  ungroup()

individuals_between 

unique(between_b$transmitter_serial)


# between_bs <- fish_move_basin %>% 
#   group_by(days) %>%
#   summarise(rec_count = n_distinct(name)) %>% 
#   ungroup()
# between_bs

# plot these both in frequency plot as well as a bubble plot and map -----

p <- ggplot(data = lt_nov_2017) + 
  stat_count(aes(x = letter_name, fill = receiver_basin), 
             colour = "black") 
p

p1 <- ggplot(data = lt_nov_2018) + 
  stat_count(aes(x = letter_name, fill = receiver_basin), 
             colour = "black")

p1



# look at unique fish id at recs 
glimpse(lt_nov_2017)

tf <- lt_nov_2017 %>%
  group_by(letter_name, receiver_basin) %>%
  summarize(rcount = n_distinct(transmitter_serial)) %>%
  ungroup()
tf


tf_2018 <-  lt_nov_2018 %>%
  group_by(letter_name, receiver_basin) %>%
  summarize(rcount = n_distinct(transmitter_serial)) %>%
  ungroup()




p3 <- ggplot(data = tf) + 
  geom_col(aes(x = letter_name, y = rcount, fill = receiver_basin), 
           colour = "black")
p3


p4 <- ggplot(data = tf_2018) + 
  geom_col(aes(x = letter_name, y = rcount, fill = receiver_basin),
           colour = "black")
p4

tf_2018

glimpse(lt_nov_2017)


# switch order again ------
lt_nov_2017$letter_name %<>%
  fct_rev()


lt_nov_2018$letter_name %<>%
  fct_rev()


# plot bubbles ----- 


p5 <- ggplot(data = lt_nov_2017,
             aes(x = factor(transmitter_serial), 
                 y = letter_name)) + 
  geom_abline(aes(intercept = name_order,
                  slope = 0,
                  colour = receiver_basin), size  = 1 ) +
  geom_count(aes(colour = fish_basin)) +
  
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




p6 <- ggplot(data = lt_nov_2018,
             aes(x = factor(transmitter_serial), 
                 y = letter_name)) + 
  geom_abline(aes(intercept = name_order,
                  slope = 0,
                  colour = receiver_basin), size  = 1 ) +
  geom_count(aes(colour = fish_basin)) +
  
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

p6


p7 <- ggplot(data = basin_move) + 
  geom_col(aes(x = transmitter_serial, y = counts, fill = factor(counts)), 
           colour = "black") +
  scale_fill_viridis_d(begin = 0.3, end = 0.8) + 
  scale_y_continuous(breaks = seq(0, 2, 1))


p7
# save plots -----


ggsave(plot = p, filename = here::here("Fish and tagging data",
                                       "Receiver Downloads", 
                                       "spawning movement", 
                                       "rec_freq_nov_2017.pdf"), 
       height = 4.37 * 1.25, 
       width = 8.34 * 1.25)



ggsave(plot = p1, filename = here::here("Fish and tagging data",
                                        "Receiver Downloads", 
                                        "spawning movement", 
                                        "rec_freq_nov_2018.pdf"), 
       height = 4.37 * 1.25, 
       width = 8.34 * 1.25)



ggsave(plot = p3, filename = here::here("Fish and tagging data",
                                        "Receiver Downloads", 
                                        "spawning movement", 
                                        "unique_fish_per_rec_nov_2017.pdf"), 
       height = 4.37 * 1.25, 
       width = 8.34 * 1.25)

ggsave(plot = p4, filename = here::here("Fish and tagging data",
                                        "Receiver Downloads", 
                                        "spawning movement", 
                                        "unique_fish_per_rec_nov_2018.pdf"), 
       height = 4.37 * 1.25, 
       width = 8.34 * 1.25)



ggsave(plot = p5, filename = here::here("Fish and tagging data",
                                        "Receiver Downloads", 
                                        "spawning movement", 
                                        "bubble_unique_fish_nov_2017.pdf"), 
       height = 4.37 * 1.25, 
       width = 8.34 * 1.25)

ggsave(plot = p6, filename = here::here("Fish and tagging data",
                                        "Receiver Downloads", 
                                        "spawning movement", 
                                        "bubble_unique_fish_nov_2018.pdf"), 
       height = 4.37 * 1.25, 
       width = 8.34 * 1.25)

ggsave(plot = p7, filename = here::here("Fish and tagging data",
                                        "Receiver Downloads", 
                                        "spawning movement", 
                                        "rec_1_5_movement_nov_2018.pdf"), 
       height = 4.37 * 1.25, 
       width = 8.34 * 1.25)


