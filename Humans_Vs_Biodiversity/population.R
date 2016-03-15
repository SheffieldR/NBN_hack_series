library(dplyr)

library(reshape2)
library(tidyr)
population.data <- read.csv("MYEB3_summary_components_of_change_series_UK_(0214).csv", stringsAsFactors=F) %>%
  melt(id.vars = 1:3) %>%
  extract(variable, into=c("measurement","year"), regex="(.*)_(\\d{4})")
#  mutate(year = )

sheffield.population <- population.data %>% 
  filter(lad2014_name == "Sheffield") %>%
  filter(measurement  == "population") %>%
  select(year, population=value)