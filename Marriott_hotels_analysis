#####################################################################################################
# I wrote these codes to analyze Marriott hotels,
# published the analysis result in my blog,
# https://vincent507.wordpress.com/2018/07/16/redemption-strategy-for-new-marriott-frequent-program/
# https://vincent507.wordpress.com/2018/07/18/full-guide-of-marriott-reward/
# https://vincent507.wordpress.com/2018/07/18/full-guide-of-marriott-free-night-certificate-usage/
#####################################################################################################
library(tidyverse)
setwd("~/Desktop")
chart <- read_csv("chart.csv", col_names = TRUE)

graph <- chart %>%
  group_by(Category) %>%
  summarise(Number = n()) %>%
  ggplot() +
  geom_col(aes(y = Number, x = Category), fill = "black", color = "black") +
  geom_text(aes(label = Number, x = Category, y = Number),vjust=-0.2)

mytheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)))

print(graph + mytheme + labs(title = "Number of hotels under each category", x = "Category", y = "Number") + scale_x_continuous(breaks = seq(1, 8, 1)))

table1 <- chart %>%
  group_by(Brand) %>%
  summarise(n = n())

table2 <- chart %>%
  group_by(Difference) %>%
  summarise(n())
  
AC <- chart %>%
  filter(Brand == "AC Hotels by Marriott") %>%
  group_by(Category) %>%
  summarise("AC Hotels" = n())

Aloft <- chart %>%
  filter(Brand == "Aloft") %>%
  group_by(Category) %>%
  summarise("Aloft" = n())

Autograph <- chart %>%
  filter(Brand == "Autograph Collection") %>%
  group_by(Category) %>%
  summarise("Autograph Collection" = n())

Courtyard <- chart %>%
  filter(Brand == "Courtyard") %>%
  group_by(Category) %>%
  summarise(Courtyard = n())

Delta <- chart %>%
  filter(Brand == "Delta") %>%
  group_by(Category) %>%
  summarise(Delta = n())

Design <- chart %>%
  filter(Brand == "Design Hotels") %>%
  group_by(Category) %>%
  summarise("Design Hotels" = n())


Edition <- chart %>%
  filter(Brand == "Edition") %>%
  group_by(Category) %>%
  summarise(Edition = n())

Element <- chart %>%
  filter(Brand == "Element") %>%
  group_by(Category) %>%
  summarise(Element = n())

Fairfield <- chart %>%
  filter(Brand == "Fairfield Inn") %>%
  group_by(Category) %>%
  summarise("Fairfield Inn" = n())

FP <- chart %>%
  filter(Brand == "Four Points") %>%
  group_by(Category) %>%
  summarise("Four Points" = n())

Gaylord <- chart %>%
  filter(Brand == "Gaylord Hotels") %>%
  group_by(Category) %>%
  summarise("Gaylord Hotels" = n())

JW <- chart %>%
  filter(Brand == "JW Marriott") %>%
  group_by(Category) %>%
  summarise("JW Marriott" = n())

LM <- chart %>%
  filter(Brand == "Le Meridien") %>%
  group_by(Category) %>%
  summarise("Le Meridien" = n())

Luxury <- chart %>%
  filter(Brand == "Luxury Collection") %>%
  group_by(Category) %>%
  summarise("Luxury Collection" = n())

Marriott <- chart %>%
  filter(Brand == "Marriott") %>%
  group_by(Category) %>%
  summarise("Marriott" = n())

Club <- chart %>%
  filter(Brand == "Marriott Vacation Club Intl") %>%
  group_by(Category) %>%
  summarise("Marriott Vacation Club Intl" = n())

Moxy <- chart %>%
  filter(Brand == "Moxy Hotels" ) %>%
  group_by(Category) %>%
  summarise("Moxy Hotels"  = n())

Protea <- chart %>%
  filter(Brand == "Protea Hotels"  ) %>%
  group_by(Category) %>%
  summarise("Protea Hotels"   = n())

Renaissance <- chart %>%
  filter(Brand == "Renaissance") %>%
  group_by(Category) %>%
  summarise(Renaissance = n())

Residence <- chart %>%
  filter(Brand == "Residence Inn") %>%
  group_by(Category) %>%
  summarise("Residence Inn" = n())

Ritz <- chart %>%
  filter(Brand == "Ritz-Carlton") %>%
  group_by(Category) %>%
  summarise("Ritz-Carlton" = n())

Sheraton <- chart %>%
  filter(Brand == "Sheraton") %>%
  group_by(Category) %>%
  summarise(Sheraton = n())

SpringHill <- chart %>%
  filter(Brand == "SpringHill Suites") %>%
  group_by(Category) %>%
  summarise("SpringHill Suites" = n())

Regis <- chart %>%
  filter(Brand == "St. Regis") %>%
  group_by(Category) %>%
  summarise("St. Regis" = n())

TownPlace <- chart %>%
  filter(Brand == "TownePlace Suites") %>%
  group_by(Category) %>%
  summarise("TownePlace Suites" = n())

Tribute <- chart %>%
  filter(Brand == "Tribute Portfolio") %>%
  group_by(Category) %>%
  summarise("Tribute Portfolio" = n())

W <- chart %>%
  filter(Brand == "W Hotels") %>%
  group_by(Category) %>%
  summarise("W Hotels" = n())

Westin <- chart %>%
  filter(Brand == "Westin") %>%
  group_by(Category) %>%
  summarise("Westin" = n())

Category <- AC %>%
  full_join(Aloft, by = "Category") %>%
  full_join(Autograph, by = "Category") %>%
  full_join(Courtyard, by = "Category") %>%
  full_join(Delta, by = "Category") %>%
  full_join(Design, by = "Category") %>%
  full_join(Edition, by = "Category") %>%
  full_join(Fairfield, by = "Category") %>%
  full_join(FP, by = "Category") %>%
  full_join(Gaylord, by = "Category") %>%
  full_join(JW, by = "Category") %>%
  full_join(LM, by = "Category") %>%
  full_join(Luxury, by = "Category") %>%
  full_join(Marriott, by = "Category") %>%
  full_join(Moxy, by = "Category") %>%
  full_join(Protea, by = "Category") %>%
  full_join(Regis, by = "Category") %>%
  full_join(Renaissance, by = "Category") %>%
  full_join(Residence, by = "Category") %>%
  full_join(Ritz, by = "Category") %>%
  full_join(Sheraton, by = "Category") %>%
  full_join(SpringHill, by = "Category") %>%
  full_join(TownPlace, by = "Category") %>%
  full_join(Tribute, by = "Category") %>%
  full_join(W, by = "Category") %>%
  full_join(Westin, by = "Category")

Difference <- chart %>%
  arrange(desc(Difference)) %>%
  filter(Difference >= 14000) %>%
  select(Name, Difference, Current, New, Brand) %>%
  filter(Current != 70000) %>%
  select(Name)

Difference2 <- chart %>%
  filter(New == 85000)  %>%
  select(Name)

Difference3 <- chart %>%
  arrange(desc(Difference)) %>%
  filter(Difference <= -10000) %>%
  select(Name, Difference, Current, New) %>%
  filter(New >= 50000) %>%
  select(Name)

Difference4 <- chart %>%
  arrange(Difference) %>%
  select(Name, Difference, Current, New) %>%
  filter(New >= 50000) %>%
  filter(Difference <= -10000)
 
US50000 <- chart %>%
  filter(New == 50000, Country == "USA", Brand %in% c(
    "Marriott", "W Hotels", "Sheraton", "Westin", "Luxury Collection", "JW Marriott", 
    "Autograph Collection", "Ritz-Carton"))

CN <- chart %>%
  filter(Country == "China", New %in% c(35000, 50000, 60000), !str_detect(Name, "Airport")) %>%
  select(Name, New)

LA <- chart %>%
  filter(str_detect(Name, "Los Angeles|Hollywood|Beverly Hills"), !str_detect(Name, "Airport"), New %in% c(35000, 50000, 60000)) %>%
  select(Name, New)

SF <- chart %>%
  filter(str_detect(Name, "San Francisco"), !str_detect(Name, "Airport"), New %in% c(35000, 50000, 60000)) %>%
  select(Name, New)

NY <- chart %>%
  filter(str_detect(Name, "New York"), !str_detect(Name, "Airport"), New %in% c(35000, 50000, 60000)) %>%
  select(Name, New)

FL <- chart %>%
  filter(str_detect(Name, "Orlando|Miami|Key|Tampa|Fort"), !str_detect(Name, "Airport"), New %in% c(35000, 50000, 60000)) %>%
  select(Name, New)

HI <- chart %>%
  filter(str_detect(Name, "Kauai|Waikiki|Oahu|Honolulu
                    |Maui|Kaiulani|Keauhou|Hapuna|Princeville
                    |Ka'anapali|Kona|Wailokoloa|Mauna|Kailua|Lahania"), !str_detect(Name, "Airport"), New %in% c(35000, 50000, 60000)) %>%
  select(Name, New)

TX <- chart %>%
  filter(str_detect(Name, "Dallas|Houston|San Antonio|Austin"), !str_detect(Name, "Airport"),
        New %in% c(35000, 50000, 60000)) %>%
  select(Name, New)

SEA <- chart %>%
  filter(str_detect(Name, "Seattle"), !str_detect(Name, "Airport"),
         New %in% c(35000, 50000, 60000)) %>%
  select(Name, New)

Paris <- chart %>%
  filter(Country == "France", str_detect(Name, "Paris"), !str_detect(Name, "Airport"), New %in% c(35000, 50000, 60000)) %>%
  select(Name, New)

london <- chart %>%
  filter(str_detect(Name, "London"), !str_detect(Name, "Airport"), New %in% c(35000, 50000, 60000)) %>%
  select(Name, New)

Italy <- chart %>%
  filter(str_detect(Name, "Milan|Roma|Venezia|Firenze|Siena"), !str_detect(Name, "Airport"),
         New %in% c(35000, 50000, 60000)) %>%
  select(Name, New)

cat1 <- chart %>%
  filter(Category == 1, Country %in% c("China", "USA")) %>%
  select(Name)

beijing <- chart %>%
  filter(str_detect(Name, "Beijing"))

JP <- chart %>%
  filter(Country == "Japan", New %in% c(35000, 50000, 60000))

DC <- chart %>%
  filter(str_detect(Name, "Washington"), !str_detect(Name, "Airport"), New %in% c(35000, 50000, 60000)) %>%
  select(Name, New)

cnus <- chart %>%
  filter(Country %in% c("USA", "China"), New == 25000) %>%
  select(Name)

cn <- chart %>%
  filter(Country == "China", New == 25000) %>%
  select(Name)

us <- chart %>%
  filter(Country == "USA", New == 25000) %>%
  select(Name)
