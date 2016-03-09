
## FUNCTIONS #########################################################################

library(rgdal)
library(rgeos)
library(maptools)
ogrListLayers(dsn)



# Function for doing data retrieval
multi_hectad <- function(tvks, grid){
  
  # read in
  # for this to work you will need to create this
  # r data file with your login details
  # or comment out this line and type in your username
  # and password in the nbnLogin function below
  
  source("Habitats_Vs_Biodiversity/setup.R")
  
  
  # get data
  occ <- getOccurrences(tvks = tvks, gridRef = grid)
  
  # write out a csv copy
  # (we will also create a .rdata copy for faster loading)
  #colnd <- ifelse(file.exists('Team_Temporal/shef_table.csv'), FALSE, TRUE)
  
  #write.table(x = occ, file = 'Team_Temporal/shef_table.csv',
              #sep = ',', append = TRUE, col.names = colnd,
              #row.names = FALSE)
  
  # write out data about data contributors
  #colnp <- ifelse(test = file.exists('Team_Temporal/shef_providers.csv'), FALSE, TRUE)
  #write.table(x = attr(occ, which = 'providers'),
              #file = 'Team_Temporal/shef_providers.csv',
              #sep = ',', append = TRUE, col.names = colnp,
              #row.names = FALSE)
  
  # log progress
  #write.table(x = grid, file = 'Team_Temporal/log.txt',
              #sep = ',', append = TRUE, col.names = FALSE)
  
  return(occ)
  
}


## WORKFLOW #########################################################################


setwd("/Users/Anna/Documents/workflows/NBN_hack_series/")
source("Habitats_Vs_Biodiversity/setup.R")


# Load polygon habitat data
dsn <- "~/Google Drive/Sheffield_R/Hack events/NBN hack/data/Sheff & Roth LL Updated May 2015_shapefile/Sheff & Roth LL Updated May 2015_region.shp"
#shp <- readOGR(dsn, ogrListLayers(dsn))
shp <- readShapePoly(dsn)

# check plot
plot(shp)


# assume input object from ui and parametarise
input <- c()
input$spp <- "Turdus_merula"
tvks <- getTVKQuery(input$spp)[1,"ptaxonVersionKey"]


# Which hectads are around Sheffield 
shef_hec_tab <- read.csv('Team_Temporal/shef_city_hectads.csv',
                         stringsAsFactors = FALSE)
shef_hec <- shef_hec_tab$x

occ <-multi_hectad(tvks, grid = shef_hec[1])

all.occ <- lapply(shef_hec, FUN = multi_hectad, tvks = tvks)


       
