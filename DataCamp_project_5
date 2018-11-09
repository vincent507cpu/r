#This script is for a project titled "PROJECT: LEVEL DIFFICULTY IN CANDY CRUSH SAGA" on DataCamp.

# This sets the size of plots to a good default.
options(repr.plot.width = 5, repr.plot.height = 4)

# Loading in packages
library(tidyverse)

# Reading in the data
data <- read_csv("datasets/candy_crush.csv")

# Printing out the first couple of rows
head(data)

print("Number of players:")
length(unique(data$player_id))

print("Period for which we have data:")
range(data$dt)

# Calculating level difficulty
difficulty <- data %>%
    group_by(level) %>%
    summarise(p_win = sum(num_success) / sum(num_attempts))

# Printing out the level difficulty
difficulty$p_win

# Plotting the level difficulty profile
library(scales)
ggplot(difficulty, aes(x = level, y = p_win)) +
    geom_line() +
    scale_y_continuous(limits = c(0, 1))
    
# Adding points and a dashed line
library(scales)
ggplot(difficulty, aes(x = level, y = p_win)) +
    geom_line() +
    scale_y_continuous(limits = c(0, 1)) +
    geom_point() +
    geom_hline(yintercept = 0.1)

# Computing the standard error of p_win for each level
data <- data %>%
    group_by(level) %>%
    summarise(attempts = sum(num_attempts))
difficulty <- difficulty %>%
    left_join(data, by = "level") %>%
    mutate(error = sqrt(p_win * (1 - p_win) / attempts))
    
# Adding standard error bars
library(scales)
ggplot(difficulty, aes(x = level, y = p_win)) +
    geom_line() +
    scale_y_continuous(limits = c(0, 1)) +
    geom_point() +
    geom_hline(yintercept = 0.1) +
    geom_errorbar(aes(ymax = p_win + error, ymin = p_win - error))
    
# The probability of completing the episode without losing a single time
p <- prod(difficulty$p_win)

# Printing it out
p

# Should our level designer worry about that a lot of 
# players will complete the episode in one attempt?
should_the_designer_worry = FALSE
