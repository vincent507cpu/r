###########################################################################################
# I wrote these codes to analyze Hilton hotels,
# published the analysis result in my blog,
# https://vincent507.wordpress.com/2018/07/16/full-guide-of-hilton-honors/
###########################################################################################
library(tidyverse)
setwd("~/Desktop")
Hilton <- read_csv("Hilton-MPL-2017-04-20-LoyaltyLobby.csv", col_names  = TRUE)

Hilton[1,] <- NA

high <- Hilton %>%
#  as.integer(New_Max_Points) %>%
  filter(New_Max_Points >= 80000) %>%
  filter(str_detect(`Hotel Name / Property Link`, "Conrad|Waldorf")) %>%
  select(`Hotel Name / Property Link` )

unlist(high$`Hotel Name / Property Link`)

low <- Hilton %>%
  filter(`New
Max
Points` <= 10000, Country %in% c("US", "China")) %>%
  select(`Hotel Name / Property Link`)
