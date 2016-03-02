# This script will get all the public
# data for the hectad listed in the csv.

# This csv has the all the hectads in a 7x7 10km grid centered on Sheffield.
# This entire grid is therefore 70km x 70km.
# you could alternativly use the south west yorkshire squares but Sheffield
# is right at the south of the county

rm(list = ls())
library(rnbn)

# Which hectads are around Sheffield 
shef_hec_tab <- read.csv('Team_Temporal/shef_city_hectads.csv',
                         stringsAsFactors = FALSE)
shef_hec <- shef_hec_tab$x
shef_hec

# Function for doing data retrieval
multi_hectad <- function(x){
  
  # read in
  # for this to work you will need to create this
  # r data file with your login details
  # or comment out this line and type in your username
  # and password in the nbnLogin function below
  load(file = 'Team_Temporal/login_details.rdata')
  
  # login
  nbnLogin(username = login_details$un,
           password = login_details$pwd,
           verbose =  TRUE)
  
  # get data
  occ <- getOccurrences(gridRef = x)
  
  # write out a csv copy
  # (we will also create a .rdata copy for faster loading)
  colnd <- ifelse(file.exists('Team_Temporal/shef_table.csv'), FALSE, TRUE)
  write.table(x = occ, file = 'Team_Temporal/shef_table.csv',
              sep = ',', append = TRUE, col.names = colnd,
              row.names = FALSE)
  
  # write out data about data contributors
  colnp <- ifelse(test = file.exists('Team_Temporal/shef_providers.csv'), FALSE, TRUE)
  write.table(x = attr(occ, which = 'providers'),
              file = 'Team_Temporal/shef_providers.csv',
              sep = ',', append = TRUE, col.names = colnp,
              row.names = FALSE)
  
  # log progress
  write.table(x = x, file = 'Team_Temporal/log.txt',
              sep = ',', append = TRUE, col.names = FALSE)
  
  return(occ)
  
}

### Run in parallel
# this speeds things up if you have multiple cores
# Load libraries for parallelising
library(parallel)
library(snowfall)

# Start our (very mini) cluster (I need a new laptop)
# This uses all your cores (prepare for your computer to go slooooow)
sfInit(parallel = TRUE, type = 'SOCK', cpus = detectCores() - 1)

# Send all our parameters and our function to the cluster
sfExportAll()

# Send the rnbn package to the cluster
sfLibrary(rnbn)

# Running in parallel is silent - nothing will appear in your console
time_taken <- system.time({
  
  shef_data <- do.call(rbind, sfClusterApplyLB(shef_hec, multi_hectad))
  
})

print(time_taken)

# save the rdata file
save(shef_data, file = 'Team_Temporal/shef_data.rdata')

# Close the cluster
sfStop()
