load('Team_Temporal/shef_data.rdata')

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

# loop here? (when I tried server was down)
name <- getTVKQuery(query = good_species[1], species_only = T, top = T)

# I'm going to assume that we get the english names and 
# put them in a column called name. For now I will use the latin
# names
telferData$name <- telferData$pTaxonName

# Now for the Telfer analysis we just need what where and when date
# so we keep the name column
# the oneK column (this is the 1km grid ref)
# and the start and end date columns (some reccords come from a date range
# rather than a specific date)
telferData_sub <- telferData[,colnames(telferData) %in% c('name', 'oneK', 'startDate', 'endDate')]

# Check this looks correct
head(telferData_sub)
# looks good

# Now we need to work out what year we want to use to 
# split our data into two halves
# this could be a year of biological significance, eg
# when polution got below a certain level or local protected
# areas were created. Here I just eyeball the data so roughly 
# half is before and half after the split year

# Create a vector of all the years (i use startDate just for convienience)
years <- as.numeric(format(telferData_sub$startDate, "%Y"))

# Plot this
hist(years)

### errrr why dont we have data before 2005?
# damn the original script to get data is set to only get data between 2005-10
# My bad, I am going to modify this script to get all of the public data then
# we shuld have a bit more to play with. I'll add all this to the Git Repo

# Going ahead with the data we have for now
# Here is the help for the function we want to use
?telfer

# we need to create a time period column
?date2timeperiod

# Use different years when we have all the data
TP <- date2timeperiod(Date = telferData_sub[,1:2],
                      time_periods = data.frame(start = c(2005, 2009),
                                                end = c(2008, 2010)))

# Make sure these numbers are sort of balanced
table(TP)

# and now we are good to go with the analysis
telfer_results <- telfer(taxa = telferData_sub$name,
                         site = telferData_sub$oneK,
                         time_period = TP)

# this function automatically removes duplicates
# ie 2 records of the same species in the same square in teh same time period
# It is worth having a look at the paper referenced in the function to 
# see how the method works:
# Telfer, M.G., Preston, C.D., & Rothery, P. (2002) A general method for
# measuring relative change in range size from biological atlas data.
# Biological Conservation, 107, 99-109

# Lets have a look in decending order
sorted_results <- telfer_results[order(telfer_results$Telfer_1_2, decreasing = TRUE), ]
sorted_results

# Let's save this as a csv
write.csv(sorted_results, file = 'Team_Temporal/Telfer_results.csv')

# we need to read the paper and work out what these values mean
# is -1 a big decline or a small decline?

#### IMPORTANT ####
# When we come to presenting to the results we need to acknowledge all of the
# organisations that have contributed data. To do this look at the csv output
# from the script that gets data shef_providers.csv. Note there might be duplicates
# here as providers will be written once per hectad. read in and then use unique() 
# on the dataframe to get rif of the duplicates