#This script is for a project titled "PROJECT: EXPLORE 538'S HALLOWEEN CANDY RANKINGS" on DataCamp.

# Load all the packages we need
library(tidyverse)
library(broom)
library(corrplot)
library(fivethirtyeight)

# Load the candy_rankings dataset from the fivethirtyeight package
data(candy_rankings)

# Take a glimpse() at the dataset
glimpse(candy_rankings)

# gather() the categorical variables to make them easier to plot
candy_rankings_long <- gather(candy_rankings, feature, value, chocolate:pluribus)

# Make a bar plot showing the distribution of each variable
ggplot(candy_rankings_long, aes(value)) +
    geom_bar() +
    facet_wrap(~feature)

# Make a lollipop chart of pricepercent
ggplot(candy_rankings, aes(x = reorder(competitorname, pricepercent),y = pricepercent)) +
    geom_segment(aes(xend=reorder(competitorname, pricepercent), yend = 0)) +
    geom_point() +
    coord_flip()

# Plot a histogram of winpercent
ggplot(candy_rankings, aes(winpercent)) +
    geom_histogram()

# Make a lollipop chart of winpercent
ggplot(candy_rankings, aes(x = reorder(competitorname, winpercent),y = winpercent)) +
    geom_segment(aes(xend=reorder(competitorname, winpercent), yend = 0)) +
    geom_point() +
    coord_flip()

# Plot the correlation matrix using corrplot()
candy_rankings %>%
    select(-competitorname) %>%
    cor() %>%
    corrplot()

# Fit a linear model of winpercent explained by all variables 
# except competitorname
win_mod <- lm(winpercent ~ . - competitorname, candy_rankings)

# Take a look at the summary
summary(win_mod)

# Plot the residuals vs the fitted values
ggplot(augment(win_mod),aes(.fitted, .resid)) +
    geom_point() +
    geom_hline(yintercept = 0)

# Fit a glm() of chocolate
choc_mod <- glm(chocolate ~ . - competitorname, candy_rankings, family = "binomial")

# Print the summary
summary(choc_mod)

# Make a dataframe of predictions
preds <- augment(choc_mod, type.predict = "response") %>%
    mutate(prediction = .fitted > .5)

# Create the confusion matrix
(conf_mat <- preds  %>%
    select(chocolate, prediction) %>%
    table())

# Calculate the accuracy
(accuracy <- sum(diag(conf_mat)) / sum(conf_mat))
