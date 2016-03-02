load('shef_data.rdata')

# what resoutions are there?
table(shef_data$resolution)

# only 1k or better
shef_data_1k_or_better <- shef_data[shef_data$resolution %in% c('100m', "1km"),] 

# This package is on github 'google "sparta biological records centre"'
library(sparta)

# ":::" gives access to internal functions in R packages
shef_data_1k_or_better$oneK <- sparta:::reformat_gr(shef_data_1k_or_better$location, 1000)

# check these are sensible
unique(shef_data_1k_or_better$oneK)

# how many species? 2177
unique(shef_data_1k_or_better$pTaxonName)

# Which species have more that 1000 records?
good_species <- names(sort(table(shef_data_1k_or_better$pTaxonName), TRUE)[sort(table(shef_data_1k_or_better$pTaxonName), TRUE)>1000])

# Keep only the 'good species'
telferData <- shef_data_1k_or_better[shef_data_1k_or_better$pTaxonName %in% good_species, ]

# Get common names - something like this might work
library(rnbn)

# loop here?
name <- getTVKQuery(query = good_species[1], species_only = T, top = T)
