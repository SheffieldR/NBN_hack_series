library("rnbn")
library("dplyr")
library("ggplot2")
library("ggmap")
library("tidyr")
source("population.R")

pop.history <- sheffield.population()

dipshef <- getOccurrences(gridRef = 'SK38', silent = TRUE, acceptTandC = TRUE, startYear = 2000)
dipshef

#this seperates the startDate column into seperate columns for years, months and days so that we 
# can just make a year on year map
dipshef2 <- separate(dipshef, startDate, c("year", "m", "d"))
dipshef2

# change y column to num from char
dipshef3 <- mutate(dipshef2, year = as.numeric(year)) %>%
  mutate(decade = floor(year/10)*10)

#get a google map for sheffield

map <- get_map(location = 'Sheffield, United Kingdom', zoom = 13)
map

dipshef3

# make a map of dippers
mapPoints <- ggmap(map) +
  geom_point(aes(x = longitude, y = latitude, color = factor(decade)), size = 5, alpha = .3, data = dipshef3)

mapPoints


dipshef2 %>% 
  filter(year >= min(pop.history$year)) %>% 
  group_by(year) %>% 
  summarise(count=length(unique(pTaxonVersionKey))) %>%
  left_join(pop.history) %>% ggplot(aes(population, count)) + geom_point() + geom_smooth(method="lm") + geom_text(aes(label=year))

dipshef2 %>%
  filter(pTaxonVersionKey == 'NHMSYS0000530307') %>%
  group_by(year) %>%
  summarise(count=n()) %>%
  ggplot(aes(year, count)) + geom_point()