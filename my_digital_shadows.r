library(tidyverse)
library(lubridate)
library(ggrepel)
options(scipen = 999)


####################################################################################################
# AmEx MR part
mr <- read_csv("~/Desktop/ActivityDetails.csv")
mr[47, 4] <- NA
mr <- mr[-c(3, 12, 31, 32, 48), ]
mr <- mr[seq(dim(mr)[1],1),]
# https://stackoverflow.com/questions/39975317/how-to-reverse-the-order-of-a-dataframe-in-r

mr <- mr %>%
  mutate(Description = str_replace_all(Description, ".+ SPND", "Authorized User Offer")) %>%
  mutate(Description = str_replace_all(Description, ".+\\/.+", "Sign-up Bonus")) %>%
  mutate(Description = str_replace_all(Description, "Retention.+", "Retention")) %>%
  mutate(Description = str_replace_all(Description, "Targeted Spend & Get Offers", "Targeted Spend Offers")) %>%
  mutate(Description = str_replace_all(Description, "20% Extra Points Apr 2018", "Spend Reward")) %>%
  mutate(Description = str_replace_all(Description, ".+\\-\\d+", "Spend Reward")) %>%
  mutate(Description = str_replace_all(Description, "Amazon 3X Membership Rewards", "Spend Reward")) %>%
  mutate(Description = str_replace_all(Description, "Amex Express Checkout Offer", "Spend Reward")) %>%
  mutate(Description = str_replace_all(Description, "Points Adjustment", "Spend Reward")) %>%
    mutate(Date = str_replace_all(mdy(Date), "(\\d+-\\d+)-\\d+", "\\1"))

mr2 <- mr %>%
  group_by(Description) %>%
  summarise("Total Points" = sum(Points, na.rm = T))

png(file = "p1_piechart.jpg")
pie(mr2$`Total Points`, labels = mr2$`Total Points`, main = "Total Points Earned", col = rainbow(length(mr2$`Total Points`)))
legend("topright", mr2$Description, cex = 0.8, fill = rainbow(length(mr2$`Total Points`)))
dev.off()
# https://www.tutorialspoint.com/r/r_pie_charts.htm

png(filename = "p1_lineplot.jpg")
mr %>%
  group_by(Date) %>%
  summarise(Points = sum(Points, na.rm = T)) %>%
  ggplot(aes(Date, cumsum(Points), group = 1)) +
    geom_line(color = "red") +
    geom_point(color = "blue") +
    geom_text_repel(aes(Date, cumsum(Points), label = paste("Earn Points:", Points, "\nTotal Points:", cumsum(Points)), vjust = -1)) +
    theme(panel.grid.minor.y = element_blank(), panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey95"), panel.grid.minor.x = element_blank(), panel.grid.major.x = element_line(color = "grey95")) +
    ylab("Total Points") + coord_cartesian(ylim = c(0, 450000)) +
  ggtitle(label = "The Points Accumulation")
dev.off()
#https://stackoverflow.com/questions/15844919/cumulative-plot-using-ggplot2

png(filename = "p1_barplot.jpg")
mr %>%
  filter(Description == "Spend Reward", !is.na(multiple)) %>%
  group_by(multiple) %>%
  summarise(`Multiple Earn` = sum(points), `General Earn` = sum(Points, na.rm = T) - `Multiple Earn`) %>%
  gather(-multiple, key = `earn rate`, value = `points earned`) %>%
  ggplot(aes(multiple, `points earned`, fill = `earn rate`)) +
    geom_col(position = "dodge") +
    geom_label_repel(aes(multiple, `points earned`, label = paste(`points earned`, "points")), show.legend = F) +
  ylab("Points Earned") + xlab("Rate") + guides(fill=guide_legend(title="Earning Rate")) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  ggtitle(label = "Comparison Between Different Earning Rate")
dev.off()
# https://stackoverflow.com/questions/14622421/how-to-change-legend-title-in-ggplot
####################################################################################################

