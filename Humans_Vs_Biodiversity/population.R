library(dplyr)

library(reshape2)
library(tidyr)

sheffield.population <- function(){
  population.data <- read.csv("MYEB3_summary_components_of_change_series_UK_(0214).csv", stringsAsFactors=F) %>%
    melt(id.vars = 1:3) %>%
    extract(variable, into=c("measurement","year"), regex="(.*)_(\\d{4})")
  #  mutate(year = )

  population.data %>% 
    filter(lad2014_name == "Sheffield") %>%
    filter(measurement  == "population") %>%
    transform(year = as.numeric(year)) %>%
    select(year, population=value)
}