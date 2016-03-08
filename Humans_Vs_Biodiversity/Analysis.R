library("rnbn")
library("dplyr")
library("ggplot2")
library("ggmap")
library("tidyr")


dipshef <- getOccurrences(tvks = 'NBNSYS0000166512', gridRef = 'Sk38', silent = TRUE, acceptTandC = TRUE)
dipshef

#this seperates the startDate column into seperate columns for years, months and days so that we 
# can just make a year on year map
dipshef2 <- separate(dipshef, startDate, c("y", "m", "d"))
dipshef2

# change y column to num from char
dipshef3 <- mutate(dipshef2, y = as.numeric(y)) %>%
  mutate(decade = floor(y/10)*10)

#get a google map for sheffield

map <- get_map(location = 'Sheffield, United Kingdom', zoom = 13)
map

dipshef3

# make a map of dippers
mapPoints <- ggmap(map) +
  geom_point(aes(x = longitude, y = latitude, color = factor(decade)), size = 5, alpha = .3, data = dipshef3)

mapPoints