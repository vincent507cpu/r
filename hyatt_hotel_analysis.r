###########################################################################################
# I wrote these codes to analyze Hyatt hotels,
# published the analysis result in my blog,
# https://vincent507.wordpress.com/2018/07/16/full-guide-of-world-of-hyatt/
###########################################################################################
library(tidyverse)
setwd("~/Desktop")
chart <- read_csv("Hyatt_Properties_Mar_2016.csv", col_names = TRUE)
chart$Category <- str_replace_all(chart$Category, "CAT", "")

China <- chart %>%
  filter(Country == "China", !str_detect(.$Property, "Hong Kong|Macau")) %>%
  select(Property, Category) %>%
  filter(str_detect(.$Property, "Grand|Park|Andaz")) %>%
  filter(Category != "5")

CAT34 <- chart %>%
  filter(Category %in% c(4,3), Country %in% c("US", "China")) %>%
  filter(str_detect(.$Property, "Park Hyatt|Grand Hyatt|Andaz")) %>%
  select(Property)

CAT4 <- chart %>%
  filter(Category == 4, str_detect(.$Property, "Park Hyatt")) %>%
  select(Property)

CAT34_new <- rbind(CAT34, CAT4)

CAT34_new$Property

CAT1 <- chart %>%
  filter(Category == 1, Country %in% c("China"), !str_detect(Property, "Airport")) %>%
  select(Property)

CAT7 <- chart %>%
  filter(Category == "7") %>%
  select(Property)
unlist(CAT7$Property)

park <- chart %>%
  filter(str_detect(.$Property, "Park Hyatt")) %>%
  group_by(Category) %>%
  summarise('Park Hyatt' = n())

grand <- chart %>%
  filter(str_detect(.$Property, "Grand Hyatt")) %>%
  group_by(Category) %>%
  summarise('Grand Hyatt' = n())

regency <- chart %>%
  filter(str_detect(.$Property, "Hyatt Regency")) %>%
  group_by(Category) %>%
  summarise('Hyatt Regency' = n())

andaz <- chart %>%
  filter(str_detect(.$Property, "Andaz")) %>%
  group_by(Category) %>%
  summarise('Andaz' = n())

place <- chart %>%
  filter(str_detect(.$Property, "Hyatt Place")) %>%
  group_by(Category) %>%
  summarise('Hyatt Place' = n())

house <- chart %>%
  filter(str_detect(.$Property, "Hyatt House")) %>%
  group_by(Category) %>%
  summarise('Hyatt House' = n())

centric <- chart %>%
  filter(str_detect(.$Property, "Hyatt Centric")) %>%
  group_by(Category) %>%
  summarise('Hyatt Centric' = n())

residence <- chart %>%
  filter(str_detect(.$Property, "Residence")) %>%
  group_by(Category) %>%
  summarise('Hyatt Residence Club' = n())

unbound <- chart %>%
  filter(str_detect(.$Property, "Unbound")) %>%
  group_by(Category) %>%
  summarise('The Unbound Collection' = n())

table <-park %>%
  full_join(grand, by = "Category") %>%
  full_join(regency, by = "Category") %>%
  full_join(andaz, by = "Category") %>%
  full_join(place, by = "Category") %>%
  full_join(house, by = "Category") %>%
  full_join(centric, by = "Category") %>%
  full_join(residence, by = "Category") %>%
  arrange(Category) %>%
  t()
