#This script is for a project titled "PROJECT: PHYLLOTAXIS: DRAW FLOWERS USING MATHEMATICS" on DataCamp.

# This sets plot images to a nice size.
options(repr.plot.width = 4, repr.plot.height = 4)

# Loading in the ggplot2 package
library(ggplot2)

t <- seq(0, 2*pi, length.out = 50)
x <- sin(t)
y <- cos(t)
df <- data.frame(t, x, y)

# Make a scatter plot of points in a circle
p <- ggplot(df, aes(x, y))
p + geom_point()

# Defining the number of points
points <- 500

# Defining the Golden Angle
angle <- pi * (3 - sqrt(5))

t <- (1:points) * angle
x <- sin(t)
y <-cos(t)
df <- data.frame(t, x, y)

# Make a scatter plot of points in a spiral
p <- ggplot(df, aes(x*t, y*t))
p + geom_point()

df <- data.frame(t, x, y)

# Make a scatter plot of points in a spiral
p <- ggplot(df, aes(x*t, y*t))
p + geom_point() + theme_bw() +
theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
     title = element_blank(), axis.text = element_blank(),
     panel.grid = element_blank())
     
p <- ggplot(df, aes(x*t, y*t))
p +  geom_point(size = 8, alpha = 0.5, color = "darkgreen") + theme_bw() +
theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
     title = element_blank(), axis.text = element_blank(),
     panel.grid = element_blank())
     
p <- ggplot(df, aes(x*t, y*t, size = t))
p + geom_point(alpha = 0.5, color = "black", pch = 8) + theme_bw() +
theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
     title = element_blank(), axis.text = element_blank(),
     panel.grid = element_blank(), legend.position="none")
     
p <- ggplot(df, aes(x*t, y*t))
p + geom_point(alpha = 0.5, color = "yellow", pch = 17) + 
theme(panel.background = element_rect(fill = "lightblue"), axis.text.x = element_blank(),axis.text.y = element_blank(),
     title = element_blank(), axis.text = element_blank(),
     panel.grid = element_blank(), legend.position="none")
     
angle <- pi*(3-sqrt(5))
points <- 1000
angle <- 2.0

t <- (1:points)*angle
x <- sin(t)
y <- cos(t)

df <- data.frame(t, x, y)

p <- ggplot(df, aes(x*t, y*t))
p + geom_point(alpha = 0.5, color = "yellow", pch = 17) + 
theme(panel.background = element_rect(fill = "lightblue"), axis.text.x = element_blank(),axis.text.y = element_blank(),
     title = element_blank(), axis.text = element_blank(),
     panel.grid = element_blank(), legend.position="none")
     
angle <- pi*(3-sqrt(5))
points <- 2000
angle <- 13*pi/180

t <- (1:points)*angle
x <- sin(t)
y <- cos(t)

df <- data.frame(t, x, y)

p <- ggplot(df, aes(x*t, y*t))
p + geom_point(alpha = 0.1, color = "yellow", pch = 1) + 
theme(panel.background = element_rect(fill = "white"), axis.text.x = element_blank(),axis.text.y = element_blank(),
     title = element_blank(), axis.text = element_blank(),
     panel.grid = element_blank(), legend.position="none")
