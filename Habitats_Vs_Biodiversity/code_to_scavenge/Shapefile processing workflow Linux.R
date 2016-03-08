rm(list=ls())

#____________________________________________________________________________
#.....DEPENDENCIES

# define dependencies
depend <- c("rgeos", "rgdal","sp","maptools", "spatstat",
            "data.table", "stringr", "ggplot2", "scales",
            "compiler", "parallel", "plyr", "foreach", 
            "doParallel", "doMC")


# install dependencies
# install.packages(depend)

# load dependencies
lapply(depend, require, character.only = TRUE)

#____________________________________________________________________________
#.....FUNCTIONS

#mac
setwd("~/Documents/workflows/Sex Roles in Birds/birds/")
# linux
setwd("~/Documents/scripts/")


source("Shapefile processing functions.R")

#____________________________________________________________________________
#.....SETTINGS

options(stringsAsFactors = F)

eres <- 10
input.folder <- "~/Documents/Range Match Data/"
bird.folder <- "BirdLife Data/Shapefiles/"
env.folder <-paste("BioCLIM Data/bil",eres,"/", sep="")
output.folder <- "Output/"
  
wd.bird <-paste(input.folder, bird.folder, sep = "")
wd.env <-paste(input.folder, env.folder, sep = "")
wd.output <- paste(input.folder, output.folder, sep = "")
dir.create(path = paste(wd.output, "Matched Shapefiles/", sep = ""), showWarnings = F)
dir.create(path = paste(wd.output, "range dat/", sep = ""), showWarnings = F)

  
bird.files <- list.files(wd.bird, pattern = ".shp")
env.files <- list.files(wd.env, pattern = ".bil")

bios <- c("alt",paste("bio", 1:19, sep=""))
#____________________________________________________________________________
#.....WORKFLOW

#____________________________________________________________________________

cores <- detectCores()  

#.....CLUSTER METHOD..............................................................................
#...set up cluster

cl <- makeCluster(cores - 1)

#clusterExport(cl, varlist =c("wd.bird", "wd.env", "wd.output", "bios", "getSppRow", "fixholes",
                             #"getBioRow", "bioDataTable", "areaDataTable", "latWts"))

#...MULTICORE METHOD..............................................................................
#...register cores

registerDoMC(cores=cores)

#____________________________________________________________________________

#...1001:end............
          
min <- 1
max <- length(bird.files)

bird.dat.parallel <- foreach(x = bird.files[min:max], .combine = rbind,
                             .inorder = F, .errorhandling = "remove") %dopar%{
                               depend <- c("rgeos", "rgdal","sp","maptools", "spatstat",
                                           "data.table", "stringr", "ggplot2", "scales",
                                           "compiler", "parallel", "plyr", "foreach", 
                                           "doParallel")
                               lapply(depend, require, character.only = TRUE)
                     
                               getSppRow(x, wd.bird, wd.env, wd.output, bios, 
                                         input.folder, overwrite = F)}

save(bird.dat.parallel, file = paste(wd.output, "bird.dat.Rdata",sep = ""))
write.csv(bird.dat.parallel, file = paste(wd.output, "bird.dat.csv", sep = ""))

stopCluster(cl)




#____________________________________________________________________________
#.....Compile resolutions
md2.5 <- read.csv("~/Documents/Range Match Data/Output/bird.dat.csv", stringsAsFactors = F)[,-1]

shp <- list.files(paste(wd.output, "Matched Shapefiles/", sep = ""))
rg <- list.files(paste(wd.output, "range dat/", sep = ""))


bf.id <- unlist(lapply(gsub(".Rdata", "", rg), FUN = function(x, bird.files){grep(x, bird.files, value = T)}, 
                bird.files = bird.files)) 
bf.id <- bf.id[-which(duplicated(bf.id))]


bird.compile <- c()

for(i in 1:length(bf.id)){
bird.compile <- rbind(bird.compile, getSppRow(bf.id[i], wd.bird, wd.env, wd.output, bios, 
                input.folder, overwrite = F))}

bird.compile$observer <- "res10"
md2.5$observer <- "res2.5"

bio <- rbind(bird.compile, md2.5)
bio$ref <- "Hijmans, R.J., S.E. Cameron, J.L. Parra, P.G. Jones and A. Jarvis, 2005. Very high resolution interpolated climate surfaces for global land areas. International Journal of Climatology 25: 1965-1978."
names(bio)[names(bio) == "spp"] <- "species"
write.csv(bio, "~/Google Drive/Sex Roles in Birds Data Project/Inputs/Anna workflow/data in/standardised csv data/BioClim.csv",
          row.names = F)

bio <- read.csv("~/Google Drive/Sex Roles in Birds Data Project/Inputs/Anna workflow/data in/standardised csv data/BioClim.csv")




