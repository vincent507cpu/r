#This script is for a project titled "PROJECT: SCOUT YOUR ATHLETICS FANTASY TEAM" on DataCamp.

# Load the tidyverse package
library(tidyverse)
# Import the full dataset
data <- read_csv("datasets/athletics.csv")

# Select the results of interest: women's javelin
javelin <- data %>%
    filter(Male_Female == "Female", Event == "Javelin") %>%
    select(-Male_Female, -Event)
 
# Give yourself a snapshot of your data 
head(javelin)
summary(javelin)

# Assign the tidy data to javelin_long
javelin_long <- gather(javelin, Flight, Distance, -c("Athlete","EventID"))

# Make Flight a numeric
javelin_long$Flight <- as.numeric(gsub("Flight", "", javelin_long$Flight))

# Examine the first 6 rows
head(javelin_long)

javelin_totals <- javelin_long %>%
    filter(Distance > 0) %>%
    group_by(Athlete, EventID) %>%
    summarize(TotalDistance = sum(Distance),
             StandardDev = round(sd(Distance), 3),
             Success = n())

# View 10 rows of javelin_totals
head(javelin_totals, n = 10)

javelin <- javelin %>%
    mutate(early = Flight1 + Flight2 + Flight3,
          late = Flight4 + Flight5 + Flight6,
          diff = late - early)

# Examine the last ten rows
tail(javelin, n = 10)

javelin_totals <- javelin_totals %>%
    left_join(javelin, by = c("Athlete", "EventID")) %>%
    select(Athlete, TotalDistance, StandardDev, Success, diff)

# Examine the first ten rows
head(javelin_totals, n = 10)

norm <- function(result) {
    (result - min(result)) / (max(result) - min(result))
}
aggstats <- c("TotalDistance", "StandardDev", "Success", "diff")
javelin_norm <- javelin_totals %>%
    ungroup() %>%
    mutate_at(aggstats, norm) %>%
    group_by(Athlete) %>%
    summarize_all(mean)

head(javelin_norm)

weights <- c(4, 1, 3, 2)
javelin_team <- javelin_norm %>%
    mutate(TotalScore = TotalDistance * weights[1] + StandardDev * weights[2] +
           Success * weights[3] + diff * weights[4]) %>%
    arrange(desc(TotalScore)) %>%
    slice(1:5) %>%
    select(Athlete, TotalScore)

javelin_team

team_stats <- javelin_totals %>% 
    filter(Athlete %in% javelin_team$Athlete) %>%
    summarize_all(mean)

pool_stats <- data.frame(do.call('cbind', sapply(javelin_totals, function(x) if(is.numeric(x)) c(max(x), mean(x)))))
pool_stats$MaxAve <- c("Maximum", "Average")
pool_stats <- pool_stats %>%
    gather(key="Statistic", value="Aggregate", -MaxAve)
                                                 
# Examine team stats
team_stats

p <- team_stats %>%
    gather(Stastistic, Aggregate, -Athlete) %>%
    ggplot(aes(x = Athlete, y = Aggregate, fill = Athlete)) +
    geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Statistic, nrow = 2, ncol = 2, scales="free_y") +
  geom_hline(data=pool_stats, aes(yintercept=Aggregate, group=Statistic, color=MaxAve), size=1) +
   labs(title="Athlete Club: Women's Javelin", color="Athlete pool maximum / average") +
  scale_fill_hue(l=70) +
  scale_color_hue(l=20) +
  theme_minimal() +
  theme(axis.text.x=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())
  
p

home <- c(2, 5, 1)
(away <- sample(1:nrow(javelin_totals), 3, replace=FALSE)) #I get 76, 43, 39

HomeTeam <- round(sum(team_stats$TotalDistance[home]),2)
AwayTeam <- round(sum(javelin_totals$TotalDistance[c(76,43,39)]),2)

print(paste0("Javelin match, Final Score: ", HomeTeam, " - ", AwayTeam))
ifelse(HomeTeam > AwayTeam, print("Win!"), print("Sometimes you just have to take the L."))
