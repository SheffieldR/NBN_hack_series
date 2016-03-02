# This script will get all the public
# data for the hectad listed in the csv.

# This csv has the hectads of south west yorkshire

rm(list = ls())
library(rnbn)

# Which hectads are around Sheffield 
shef_hec_tab <- read.csv('south-west-yorkshire-10km-squares.csv', stringsAsFactors = FALSE)
shef_hec <- shef_hec_tab$Square
shef_hec

# Function for doing a few hectads
# Beefed up a bit
multi_hectad <- function(x){
  
  # read in
  # for this to work you will need to create this
  # r data file with your login details
  # or comment out this line and type in your username
  # and password in teh nbnLogin function
  load(file = 'login_details.rdata')
  
  # login
  nbnLogin(username = login_details$un,
           password = login_details$pwd)
  # get data
  occ <- getOccurrences(gridRef = x)
  
  # write out a csv copy
  # (we will also create a .rdata copy for faster loading)
  write.table(x = occ, file = 'shef_table.csv', sep = ',', append = TRUE, col.names = TRUE)
  
  return(occ)
  
}


### Run in parallel
# Load libraries for parallelising
library(parallel)
library(snowfall)

# Start our (very mini) cluster (I need a new laptop)
# This uses all your cores (prepare for your computer to go slooooow)
sfInit(parallel = TRUE, type = 'SOCK', cpus = detectCores())

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



