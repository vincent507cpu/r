#####################################################################################################
# I wrote this script to make a plan for a trip of San Diego.
# blog post: 
# https://vincent507.wordpress.com/2018/12/09/practice-what-ive-learned-a-plan-for-trip-of-san-diego/
#####################################################################################################

# Recommended way to install ggmap package:
# if(!requireNamespace("devtools")) install.packages("devtools")
# devtools::install_github("dkahle/ggmap", ref = "tidyup") 

library(tidyverse)
# Set up data set
usgrant <- list(name = "THE US GRANT, a Luxury Collection Hotel, San Diego", type = "hotel", short = "Luxury Collection")
auto <- list(name = "Hotel Republic San Diego, Autograph Collection", type = "hotel", short = "Autograph Collection")
cy <- list(name = "Courtyard San Diego Airport/Liberty Station", type = "hotel", short = "Courtyard")
bal <- list(name = "Balboa Park, san diego, ca", hour = 4, type = "place", short = "Balboa Park", abbr = "bal")
midway <-list(name = "USS Midway, san diego, ca", hour = 2, type = "place", short = "USS Midway", abbr = "midway")
coro <- list(name = "Coronado, san diego, ca", hour = 4, type = "place", short = "Coronado", abbr = "coro")
cab <- list(name = "Cabrillo National Monument, san diego, ca", hour = 2, type = "place", short = "Cabrillo", abbr = "cab")
ljc <- list(name = "La Jolla Cove, san diego, ca", hour = 2, type = "place", short = "La Jolla", abbr = "ljc")
ot <- list(name = "4002 Wallace St, San Diego, CA 92110", hour = 4, type = "place", short = "Old Town", abbr = "ot") #Old Town
ap <- list(name = "san diego airport", type = "airport", short = "airport")
# Complete the data preparation
place <- bind_rows(bal, midway, coro, cab, ljc, ot, auto, usgrant, cy, ap)

library(ggmap)
register_google(key = "my_google_key_here") #use your key!
library(ggrepel)
# Plot the map of San Diego with targeted places marked
geoc <- geocode(place$name)
place <- bind_cols(place, geoc)
ggmap(get_map(location = 'san diego', zoom = 11)) +
  geom_label_repel(data = place, aes(label = short, color = type), force = 8, nudge_x = 1) +
  geom_point(data = place, aes(lon, lat, color = type))
# https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html

# Algorithm: Suppose I'll visit these places in either day 1 or day 2,
# so set the value of these places either 1 or 2. Permutate to get the combination of visit plan, 
# if the value of a place is 1, retrieve the visiting hours from "place" matrix. 
# Sum up the hours, it is the hour I will spend in day 1. Do the same thing for day 2. 
# Filter out the hours more than 10 or less than 8, the remainings are feasible plans.

library(gtools)

lst1 <- c("bal", "midway", "coro", "cab", "ljc", "ot")
lst2 <- c(lst1, "auto", "lux", "cy", "ap")
plan <- permutations(2, 6, v=c(1, 2), repeats.allowed = T) # generate combinations
# https://stackoverflow.com/questions/53604144/unordered-combination-and-store-the-result-in-a-matrix-in-r
colnames(plan) <- lst1 # name the column names
rownames(place) <- lst2 # name the row names
day1 <- c(rep(0, nrow(plan))) # create a column to store total hours in day 1
names(day1) <- "day1" # name the column of day 1
day2 <- c(rep(0, nrow(plan))) # create a column to store total hours in day 2
names(day2) <- "day2" # name the column of day 2
plan <- as.data.frame(plan) # has to convert from matrix to data frame
plan <- cbind(plan, day1, day2) # bind three data frames

# Calculate the sum of hours for each day
for(i in 1:nrow(plan)){
  for (j in 1:6) {
    if(plan[i, j] == 1){
      plan[i, 7] <- plan[i, 7] + place[colnames(plan)[j], 2]
    }
    if(plan[i, j] == 2){
      plan[i, 8] <- plan[i, 8] + place[colnames(plan)[j], 2]
    }
  }
} 
nrow(plan) # 64
plan <- plan %>%
  filter(day1 >= 8 & day1 <= 10 & day2 >= 8 & day2 <= 10) # Filter out unfeasible plan
nrow(plan) # 24

# Algorithm: read each row in plan, make all of possible route combinations for each day, 
# replace value of 1 and 2 by abbr value in place.
day_1 <- list()
day_2 <- list()
for (i in 1:nrow(plan)) {
  j <- sum(plan[i, ] == 1)
  day_1[[i]] <- data.frame(permutations(j, j, 
           v = names(plan)[which(plan[i, ] == 1, arr.ind=T)[, "col"]]), stringsAsFactors = FALSE)
  # https://stackoverflow.com/questions/36960010/get-column-name-that-matches-specific-row-value-in-dataframe
  k <- sum(plan[i, ] == 2)
  day_2[[i]] <- data.frame(permutations(k, k, 
           v = names(plan)[which(plan[i, ] == 2, arr.ind=T)[, "col"]]), stringsAsFactors = FALSE)
}
# https://stackoverflow.com/questions/53625126/how-to-create-data-frames-not-just-one-at-once-in-r

# Replace abbr by the real place name
for (i in 1:length(day_1)) {
  for (j in 1:length(day_1[[i]])) {
    for (k in 1:length(day_1[[i]][,j])) {
      day_1[[i]][, j][k] <- case_when(day_1[[i]][, j][k] == "bal" ~ "Balboa Park, san diego, ca",
                                      day_1[[i]][, j][k] == "midway" ~ "USS Midway, san diego, ca",
                                      day_1[[i]][, j][k] == "coro" ~ "Coronado, san diego, ca",
                                      day_1[[i]][, j][k] == "cab" ~ "Cabrillo National Monument, san diego, ca",
                                      day_1[[i]][, j][k] == "ljc" ~ "La Jolla Cove, san diego, ca",
                                      day_1[[i]][, j][k] == "ot" ~ "4002 Wallace St, San Diego, CA 92110")
    }
  }
}
for (i in 1:length(day_2)) {
  for (j in 1:length(day_2[[i]])) {
    for (k in 1:length(day_2[[i]][, j])) {
      day_2[[i]][, j][k] <- case_when(day_2[[i]][, j][k] == "bal" ~ "Balboa Park, san diego, ca",
                                      day_2[[i]][, j][k] == "midway" ~ "USS Midway, san diego, ca",
                                      day_2[[i]][, j][k] == "coro" ~ "Coronado, san diego, ca",
                                      day_2[[i]][, j][k] == "cab" ~ "Cabrillo National Monument, san diego, ca",
                                      day_2[[i]][, j][k] == "ljc" ~ "La Jolla Cove, san diego, ca",
                                      day_2[[i]][, j][k] == "ot" ~ "4002 Wallace St, San Diego, CA 92110")
    }
  }
}

# Algorithm: calculate the time of driving in each route.
start <- place$name[7]
end1 <- place$name[8]
end2 <- place$name[9]

######################################################################################
# Credit of the following code in this box to Erin, although I modified it a little :P
compute_route_time = function(row, start, end) {
  # get a vector of stops that you can loop through
  stops = unlist(row)

  # distance from start to first stop
  time = mapdist(from = start, to = stops[1])$seconds
  # iterate through the rest of the route
  n_stops = length(stops)
  for (i in 1:(n_stops-1)) {
    time = time + mapdist(from = stops[i], to = stops[i+1])$seconds
  }
  # finish rout
  time = time + mapdist(from = stops[n_stops], to = end)$seconds

  # collect row as a data frame to preserve old columns
  row = as.data.frame(row)
  # add time column
  row$time = time
  return(row)
}

for (i in 1:length(day_1)) {
  day_1[[i]] <- day_1[[i]] %>%
    rowwise() %>%
    do(compute_route_time(., start = start, end = end1))
}
for (i in 1:length(day_2)) {
  day_2[[i]] <- day_2[[i]] %>%
   rowwise() %>%
    do(compute_route_time(., start = end1, end = end2))
}
# https://stackoverflow.com/questions/53662127/non-numeric-argument-to-binary-operator-in-ggmap
##################################################################################

# Find out the shortest time. This route will be the best traveling route.
# Find the shortest route, overwrite the list, only keep the best route for each plan
route_1 <- list()
route_2 <- list()
total_time <- NULL # empty vector for storing times for each plan
for (i in 1:length(day_1)) {
  route_1[[i]] <- day_1[[i]][which.min(day_1[[i]]$time),]
  route_2[[i]] <- day_2[[i]][which.min(day_2[[i]]$time),]
  total_time <- c(total_time, route_1[[i]]$time + route_2[[i]]$time)
}

(max <- which.min(total_time)) # The best route is No. 1

# Transform route information to get route and plot later
plan_1 <- data.frame(matrix(ncol = 2, nrow = length(route_1[[max]]) + 1)) # Create an empty data frame
colnames(plan_1) <- c("id", "location") # Set up the column name
# Fill location info and sequence of traveling
for (i in 1:(length(route_1[[max]]) + 1)) {
  if(i == 1){
    plan_1$id[1] <- 1
    plan_1$location[1] <- "Hotel Republic San Diego, Autograph Collection"
  }
  if(i > 1 & i < length(route_1[[max]]) + 1){
    plan_1$id[i] <- i
    plan_1$location[i] <- route_1[[max]][i - 1]
  }
  if(i == length(route_1[[max]]) + 1){
    plan_1$id[i] <- length(route_1[[max]]) + 1
    plan_1$location[i] <- "THE US GRANT, a Luxury Collection Hotel, San Diego"
  }
}

plan_2 <- data.frame(matrix(ncol = 2, nrow = length(route_2[[max]]) + 1))
colnames(plan_2) <- c("id", "location")
for (i in 1:(length(route_2[[max]]) + 1)) {
  if(i == 1){
    plan_2$id[1] <- 1
    plan_2$location[1] <- "THE US GRANT, a Luxury Collection Hotel, San Diego"
  }
  if(i > 1 & i < length(route_2[[max]]) + 1){
    plan_2$id[i] <- i
    plan_2$location[i] <- route_2[[max]][i - 1]
  }
  if(i == length(route_2[[max]]) + 1){
    plan_2$id[i] <- length(route_2[[max]]) + 1
    plan_2$location[i] <- "Courtyard San Diego Airport/Liberty Station"
  }
}

# Get route info
legs_df_1 <- list()
for (i in 1:(nrow(plan_1) - 1)) {
  legs_df_1[[i]] <- route(from = as.character(plan_1$location[i]), to = as.character(plan_1$location[i + 1]), alternatives = FALSE, mode = "driving", structure = "leg")
  legs_df_1[[i]]$id <- i
}
for (i in 1:length(legs_df_1)) {
  if(i == 1){
    final_1 <- legs_df_1[[1]][which.min(legs_df_1[[1]]$seconds),]
  }
  if(i != 1){
    final_1 <- bind_rows(final_1, legs_df_1[[i]][which.min(legs_df_1[[i]]$seconds),])
  }
}

legs_df_2 <- list()
for (i in 1:(nrow(plan_2) - 1)) {
  legs_df_2[[i]] <- route(from = as.character(plan_2$location[i]), to = as.character(plan_2$location[i + 1]), alternatives = FALSE, mode = "driving", structure = "leg")
  legs_df_2[[i]]$id <- i
}
for (i in 1:length(legs_df_2)) {
  if(i == 1){
    final_2 <- legs_df_2[[1]][which.min(legs_df_2[[1]]$seconds),]
  }
  if(i != 1){
    final_2 <- bind_rows(final_2, legs_df_2[[i]][which.min(legs_df_2[[i]]$seconds),])
  }
}

#####################################################################################################
# The final plan:
# Day 1: departs from Hotel San Diego Republic, 
# then visit USS Midway Museum, Coronado Island and Balboa Park, return to US Grant to end day 1;
#
# Day 2: starts from US Grant, visit La Jolla Cove, San Diego Oldtown and Cabrillo National Monument, 
# return to Courtyard as the end of day 2.
#####################################################################################################

# Plot
ggmap(get_map(location = 'san diego', zoom = 13)) +
  geom_label_repel(data = place, aes(label = short, color = type), force = 5, nudge_x = 1) +
  geom_point(data = place, aes(lon, lat, color = type)) +
  geom_leg(aes(x = startLon, y = startLat, xend = endLon, yend = endLat),
               alpha = 3/4, size = 2, data = legs_df_1[[1]], color = "red") +
  geom_leg(aes(x = startLon, y = startLat, xend = endLon, yend = endLat),
               alpha = 3/4, size = 2, data = legs_df_1[[2]], color = "blue") +
  geom_leg(aes(x = startLon, y = startLat, xend = endLon, yend = endLat),
               alpha = 3/4, size = 2, data = legs_df_1[[3]], color = "green") +
  geom_label_repel(aes(x = startLon, y = startLat), data = final_1, label = final_1$id) +
  ggtitle("Day 1") +
  theme(plot.title = element_text(colour = "red", size = 20, face = "bold"))

ggmap(get_map(location = 'san diego', zoom = 11)) +
  geom_label_repel(data = place, aes(label = short, color = type), force = 5, nudge_x = 1) +
  geom_point(data = place, aes(lon, lat, color = type)) +
  geom_leg(aes(x = startLon, y = startLat, xend = endLon, yend = endLat),
               alpha = 3/4, size = 2, data = legs_df_2[[1]], color = "red") +
  geom_leg(aes(x = startLon, y = startLat, xend = endLon, yend = endLat),
               alpha = 3/4, size = 2, data = legs_df_2[[2]], color = "blue") +
  geom_leg(aes(x = startLon, y = startLat, xend = endLon, yend = endLat),
               alpha = 3/4, size = 2, data = legs_df_2[[3]], color = "green") +
  geom_leg(aes(x = startLon, y = startLat, xend = endLon, yend = endLat),
               alpha = 3/4, size = 2, data = legs_df_2[[4]], color = "purple") +
  geom_leg(aes(x = startLon, y = startLat, xend = endLon, yend = endLat),
               alpha = 3/4, size = 2, data = legs_df_2[[5]], color = "black") +
  geom_label_repel(aes(x = startLon, y = startLat), data = final_2, label = final_2$id) +
  ggtitle("Day 2") +
  theme(plot.title = element_text(colour = "red", size = 20, face = "bold"))
