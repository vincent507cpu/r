#This script is for a project titled "PROJECT: WHO IS DRUNK AND WHEN IN AMES, IOWA?" on DataCamp.

# load the tidyverse suite of packages 
library(tidyverse)

# read the data into your workspace
ba_data <- read_csv("datasets/breath_alcohol_ames.csv")

# quickly inspect the data
head(ba_data)

# obtain counts for each year 
ba_year <- ba_data %>%
    count(year)
    
# use count to tally up the totals for each department
pds <- ba_data %>%
    group_by(location) %>%
    count()
    
# count by hour and arrange by descending frequency
hourly <- ba_data %>%
    group_by(hour) %>%
    count() %>%
    arrange(desc(n))

# use a geom_ to create the appropriate bar chart
ggplot(hourly, aes(hour, n)) + geom_bar(stat = "identity")

# count by month and arrange by descending frequency
monthly <- ba_data %>%
    group_by(month) %>%
    count() %>%
    arrange(desc(n))

# make month a factor
monthly$month <- as.factor(monthly$month)

# use a geom_ to create the appropriate bar chart
ggplot(monthly, aes(month, n)) + geom_bar(stat = "identity")

# count by gender 
ba_data %>%
    group_by(gender) %>%
    count()

# create a dataset with no NAs in gender 
clean_gender <- ba_data %>%
    filter(!is.na(gender))

# create a mean test result variable and save as mean_bas
mean_bas <- clean_gender %>%
    group_by(gender) %>%
    mutate(meanRes = (Res1 + Res2) / 2)

# create side-by-side boxplots to compare the mean blood alcohol levels of men and women
ggplot(mean_bas, aes(gender, meanRes)) + geom_boxplot()

# Filter the data
duis <- ba_data %>%
    filter(Res1 > 0.08 | Res2 > 0.08)

# proportion of tests that would have resulted in a DUI
p_dui <- nrow(duis) / nrow(ba_data)

library(lubridate) 

# Create date variable using paste() and ymd()
ba_data <- ba_data %>% 
    mutate(date = ymd(paste(year, month, day, sep = "-")))

# Create a week variable using week()
ba_data <- ba_data %>% mutate(week = week(date))

# create the weekly data set 
weekly <- ba_data %>%
    group_by(week, year) %>%
    count()

# uncomment and run the following line
weekly <- weekly %>% ungroup() # ungroup is necessary for the plot later

# make year a factor
weekly <- weekly %>% mutate(year = as.factor(year))

# create the time series plot with one line for each year
ggplot(weekly, aes(x = week, y = n)) + 
  geom_line() + 
  geom_point(aes(color = year)) +  # included to make the plot more readable 
  scale_x_continuous(breaks = seq(0,52,2))  # to make the x-axis more readable 
  
## Run this code to create the plot 
ggplot() + 
  geom_point(data = weekly, aes(x = week, y = n, color = year)) + 
  geom_line(data = weekly, aes(x = week, y = n, color = year)) +  # included to make the plot more readable 
  geom_segment(data = NULL, arrow = arrow(angle = 20, length = unit(0.1, "inches"),
                                          ends = "last", type = "closed"), 
               aes(x = c(20,20), xend = c(15.5,16), y = c(21, 20), yend = c(21, 12.25))) + 
  geom_text(data = NULL, aes(x = 23, y = 20.5, label = "VEISHEA Weeks"), size = 3) + 
  scale_x_continuous(breaks = seq(0,52,2)) 

## Make a decision about VEISHEA. TRUE or FALSE?  
cancelling_VEISHEA_was_right <- TRUE
