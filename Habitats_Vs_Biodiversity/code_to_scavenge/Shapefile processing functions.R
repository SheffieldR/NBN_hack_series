

#____________________________________________________________________________
#.....FUNCTIONS

#____________________________________________________________________________

# Produces appropriate error message and saves error report if required
processError <- function(type, spp, wd.output, check = NA, xt = NULL, overwrite = overwrite){
  
  dir.create(path = paste(wd.output, "error reports/", sep = ""), showWarnings = F)
  
  err.df <- data.frame(data = 1:4, error = c("no presence",
                                             "no polys",
                                             "no data in polys",
                                             "no data in any polys"))
  
  print(paste(spp, if(type %in% c(1:2,4)){"....................NA.............."}else{
    "Warning: .............."}, err.df[type, "error"], 
              if(any(is.na(check))){NULL}else{str_c(paste(check), collapse = ", ")}))
  

  
  error.rep <- data.frame(species = spp, error = err.df[type, "error"],
                          area.lost = if(type %in% c(1:2,4)){1}else{1 - (gArea(xt[-check,])/gArea(xt))},
                          polys = if(any(is.na(check))){NA}else{str_c(paste(check), 
                                                                      collapse = ", ")},
                          season = if(any(is.na(check))){NA}else{str_c(paste(xt$SEASONAL[check]), 
                                                                       collapse = ", ")})
  error.ls <- list.files(paste(wd.output, "error reports/", sep = ""))
  if(overwrite == F & paste(spp,".csv", sep = "") %in% error.ls){
    pr.error <- read.csv(paste(wd.output, "error reports/", spp, ".csv", sep = ""), stringsAsFactors = F)
    error.rep <- rbind(pr.error, error.rep)
  }

  write.csv(error.rep, paste(wd.output, "error reports/", 
                             spp, ".csv", sep = ""), row.names = F)}


# fixholes in Spatial Polygon (SP) object
fixholes <- function(sp.obj) {
  require(rgeos)
  require(stringr)
  if(!inherits(sp.obj, "SpatialPolygons")) stop("Input object must be of class SpatialPolygons")
  pls = slot(sp.obj, "polygons")
  pls1 = lapply(pls, checkPolygonsHoles)
  slot(sp.obj, "polygons") = pls1
  return(sp.obj)
}


# Calculate latitudinal weights for environmental variables. Used to correct
# calculation of region wide statistics
latWts <- function(lats, e){cell <- e@grid@cellsize[1]
                            
                            l <- cos((lats-cell/2)*pi/180)^2 *cos(cell*pi/180)
                            l0 <- cos(mean(lats-cell/2)*pi/180)^2 *cos(cell*pi/180)
                            l/l0}


# Calculate total area of resident and breeding range (SEASONAL 1:2) and proportion of range utilised under
# seasonal occupancies 1-4. Compiles into range.dat dataframe
areaDataTable<- function(xt){  
  
  data <- NULL
  a <- gArea(xt)/10^6 #calculate area over presence 1:2 and seasonality 1:4
  data <- a
  
  # if seasonality 1:2 polygons exist, calculate area over only those
  if(all(!xt$SEASONAL %in% 1:2)){a.sub <- NA}else{
  a.sub <- gArea(xt[xt$SEASONAL %in% 1:2,])/10^6}
  data <- cbind(data, a.sub)
  
  # calculate the proportion of the range where presence 1:2 occupied at seasonality 1:4
  for(seasonal in 1:4){
    
    xs <-try(xt[xt$SEASONAL == seasonal,], silent = T)
    if(class(xs) == "try-error"){ap <- NA}else{
      as <- gArea(xs)/10^6
      ap <- as/a}
    data <- cbind(data, ap)}
  data <- as.data.frame(data)
  colnames(data) <- c('area.t', 'area', paste("area.s", 1:4, sep = ""))
  return(data)}

# Calculates area and latitudinally corrected mean, min, max and variance of bioclim
# variable (bio) for each polygon region (each row of x@data). Creates data frame of single 
# bio column and rows equal to the data table of the SP file.
bioDataTable <- function(x, xt, bio, e, wd.env, calc.dat){
  
  season.id <- xt$SEASONAL %in% 1:2
  
  # means calculated using lat corrected values
  e.dat <- lapply(calc.dat, FUN = function(x,e){x[,1] <- e@data[as.numeric(rownames(x)),1]}, e)
  coo.polys <- lapply(calc.dat, FUN = function(x,e){coordinates(e)[as.numeric(rownames(x)),2]}, e)
  coo.wts <- lapply(coo.polys, FUN =latWts, e)
  
  mean.regions <- data.frame(mapply(e.dat,coo.wts, FUN = function(x, coo.wts){weighted.mean(x, wt =coo.wts)}))
  names(mean.regions) <- paste(bio, "m", sep=".")
  
  # max, min & var calculated on absolute values
  max.regions <- data.frame(unlist(lapply(e.dat, FUN = function(x){max(x)})))
  names(max.regions) <- paste(bio, "max", sep=".")
  
  min.regions <- data.frame(unlist(lapply(e.dat, FUN = function(x){min(x)})))
  names(min.regions) <- paste(bio, "min", sep=".")
  
  var.regions <- data.frame(mapply(e.dat, coo.wts, FUN = function(x, coo.wts){weighted.var(x, coo.wts)}))
  names(var.regions) <- paste(bio, "var", sep=".")
  
  # Restrict calculation of overall variance to resident (SEASONAL == 1) and breeding grounds (SEASONAL == 2)
  if(any(season.id)){
  coo.polys <- coo.polys[season.id]
  e.dat <- e.dat[season.id]
  
  wts <- latWts(unlist(coo.polys), e)
  var.all <- data.frame(rep(weighted.var(unlist(e.dat), wts),  dim(var.regions)[1]))
  }else{
    var.all <- data.frame(rep(NA,  dim(var.regions)[1]))  
  }
  
  names(var.all) <- paste(bio, "varall", sep=".")
  
  if(bio == "alt"){ area <- gArea(xt, byid  = T)/10^6
                    bio.dat <- data.frame(area, mean.regions, max.regions, min.regions, var.regions, var.all)}else{
                      bio.dat <- data.frame(mean.regions, max.regions, min.regions, var.regions, var.all)}
  return(bio.dat)
}

# Compiles the polygon region level bioclim data in processed SP file to 
# range wide statistics
getBioRow <- function(x, bios){
  
  if(all(!x$SEASONAL %in% 1:2)){return(NULL)}else{x[x$SEASONAL %in% 1:2,]}
  
  
  if(!paste(bios[1], "m", sep=".") %in% names(x@data)){
  bio.dat.nm <- paste(rep(bios, each = 4), c(".m", ".max", ".min", ".var"), sep = "")
  bio.dat <-data.frame(matrix(NA, ncol = length(bio.dat.nm), nrow=1))
  names(bio.dat) <- bio.dat.nm}else{
  
  for(bio in bios){
    
    mean <- weighted.mean(x@data[,paste(bio, "m", sep=".")], wt = x@data[,"area"])
    max <- max(x@data[,paste(bio, "max", sep=".")])
    min <- min(x@data[,paste(bio, "min", sep=".")])
    var <- unique(x@data[,paste(bio, "varall", sep=".")])
    
    if(bio == "alt"){bio.dat<- data.frame(mean, max, min, var)
                     names(bio.dat) <- paste(bio, c(".m", ".max", ".min", ".var"), sep = "")}else{
                       dat<- data.frame(mean, max, min, var)
                       names(dat) <- paste(bio, c(".m", ".max", ".min", ".var"), sep = "")
                       bio.dat <- data.frame(bio.dat, dat)
                     }
  }}
  return(bio.dat)}



getSppRow <- function(bird.file, wd.bird, wd.env, wd.output, bios, 
                       input.folder = input.folder, overwrite = F){
  
  
  
  t0 <- Sys.time()
  
  spp <- sub("_[0-9].*$", "", bird.file)     
  #load(file=paste(input.folder, "bird.dat.colnames.Rdata", sep=""))
  
  error.ls <- list.files(paste(wd.output, "error reports/", sep = ""))
  if(overwrite == F & paste(spp,".csv", sep = "") %in% error.ls){return(NULL)}
  done.ls <- list.files(paste(wd.output, "Matched Shapefiles/", sep = ""))
  if(!paste(spp, ".Rdata", sep="") %in% done.ls | overwrite == T){load.bio <- F}else{load.bio <- T}
  range.ls <- list.files(paste(wd.output, "range dat/", sep = ""))
  if(paste(spp, ".Rdata", sep = "") %in% range.ls & overwrite == F){load.range <- T}else{load.range <- F}
  
  
  
  if(!load.bio | !load.range){
  # Load Shapefile
  #x <- readOGR(dsn, ogrListLayers(dsn))
  dsn <- paste(wd.bird, bird.file, sep="")
  x <- readShapeSpatial(dsn, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

  
  if(all(!x$PRESENCE %in% 1:2)){
  processError(type = 1, spp, wd.output, check = NA, xt = NULL, overwrite = overwrite)
    return(NULL)}
  
  x<- x[x$PRESENCE %in% 1:2 & x$SEASONAL %in% 1:4,]
  x<- fixholes(x)
  }
  
  if(load.range){load(paste(wd.output, "range dat/", spp, ".Rdata", sep = ""))}else{
    xt <- spTransform(x, CRS = CRS("+proj=laea +lon_0=0.001 +lat_0=0.001 +ellps=sphere"))
  }
    
  if(!load.bio){
    
    # Extract BIOClim data and update spdf
    
    
    for(env.file in paste(bios, ".bil", sep="")){
      
      # import and set bio
      bio <- gsub(".bil", "", env.file)
      e <- readGDAL(paste(wd.env, env.file, sep = ""))
      if(bio %in% paste("bio", c(1:2, 4:11), sep = "")){e@data <- e@data/10}
      
      
      if(env.file == "alt.bil"){
        # Make polys
        polys <- SpatialPolygons(x@polygons, proj4string = CRS(summary(e)$proj4string))
        #OVERLAY - 
        calc.dat <- over(polys,e , returnList = T)
        if(length(unlist(calc.dat)) == 0){
          processError(type = 2, spp, wd.output, check = NA, xt = NULL, overwrite = overwrite)
          return(NULL)
        }
        
        
        check <- which(unlist(lapply(calc.dat, FUN = function(x){dim(x[1])[1] == 0})))
          
            if(length(check)>=1){
            if(length(check) == length(calc.dat)){
              processError(type = 4, spp, wd.output, check = check, xt = NULL, overwrite = overwrite)
              return(NULL)
            }
            
            processError(type = 3, spp, wd.output, check = check, xt = xt, overwrite = overwrite)
            x <- x[-check,]
            if(!load.range){xt <- xt[-check,]}
            calc.dat <- calc.dat[-check]
            }
      }
      
      
      x@data <- data.frame(x@data, bioDataTable(x, xt, bio, e, wd.env, calc.dat))
    }
    
    
    # save spdf    
    save(x, file = paste(wd.output, "Matched Shapefiles/", spp,".Rdata", sep=""))
    

  }else{load(file = paste(wd.output, "Matched Shapefiles/", spp,".Rdata", sep=""))}
  
  
  if(!load.range){
    t2 <- Sys.time()
    # Calculate range attribute data
    range.dat <- areaDataTable(xt)
  
    save(range.dat, file = paste(wd.output, "range dat/", spp, ".Rdata", sep = ""))
    t3 <- Sys.time()
    print(t3-t2)}
  


  # calculate range bio data
  bio.dat <- getBioRow(x, bios)

  if(is.null(bio.dat)){processError(type = 2, spp, wd.output, check = NA, xt = NULL, overwrite = overwrite)

    return(NULL)}
  
  
  print(paste("COMPLETE...........................",spp))
  t1 <- Sys.time()
  print(t1-t0)
  
  row.dat <- data.frame(spp, range.dat, bio.dat)
  return(row.dat)}



aggregateDuplicates <- function(dat, wd.bird, bird.file, bio.nm, bios, wd.output, x){
  
  if(any(duplicated(dat[,bio.nm]))){
    
  spp <- sub(".Rdata", "", bird.file)

  dsn <- paste(wd.bird, list.files(wd.bird, pattern = ".shp")[grep(spp, 
                                                                   list.files(wd.bird, pattern = ".shp"))], 
                                                              sep="")
  # Load Shapefile
  xt <- readOGR(dsn, ogrListLayers(dsn))
  xt <- xt[xt$PRESENCE %in% 1:2,]
  
  if(any(!xt$SEASONAL %in% dat$SEASONAL)){
    dat <- dat[xt$PRESENCE %in% dat$PRESENCE & xt$SEASONAL %in% dat$SEASONAL,]
    
    areas <- dat$area/sum(dat$area)
    for(bio in bios){
      dat[, paste(bio, ".varall", sep = "")] <- weighted.mean(dat[, paste(bio, ".var", sep = "")],
                                                              areas)}
    
    x <- x[xt$PRESENCE %in% dat$PRESENCE & xt$SEASONAL %in% dat$SEASONAL,]
    x@data <- dat
    
    #save corrected bio data
    save(x, file = paste(wd.output,"Matched Shapefiles/", spp, ".Rdata", sep=""))
    
    #load and save corrected range data
    load(file = paste(wd.output,"range dat/", spp, ".Rdata", sep=""))
    range.dat[which(!(1:4 %in% unique(dat$SEASONAL)))+1] <- NA
    range.dat[2:5] <- range.dat[2:5]/sum(range.dat[2:5], na.rm = T)
    range.dat$area <- sum(dat$area)
    
    save(range.dat, file = paste(wd.output,"range dat/", spp, ".Rdata", sep=""))
  }}
  
  dup <-  dat$SEASONAL[duplicated(dat$SEASONAL)]
  if(length(dup) == 0){return(dat)}
  seasons <- unique(dat$SEASONAL)
  nondup <- seasons[!seasons %in% dup]
  
  
  areas <- dat$area/sum(dat$area)
  
  new.dat <- data.frame(matrix(NA, nrow = length(seasons), ncol = length(bio.nm) + 1))
  names(new.dat) <- c("SEASONAL", bio.nm)
  new.dat$SEASONAL <- seasons
  
  for(i in 1:length(dup)){
    
    for(j in 1:length(bios)){    
      mu <- dat[dat$SEASONAL == dup[i], bio.nm[grep(paste(bios[j],".m$", sep = ""), bio.nm)]]
      sigma <- dat[dat$SEASONAL == dup[i], bio.nm[grep(paste(bios[j],".var$", sep = ""), bio.nm)]]
      
      
      new.dat[new.dat$SEASONAL == dup[i], bio.nm[grep(paste(bios[j],".m$", sep = ""), bio.nm)]] <-
        weighted.mean(mu, areas[dat$SEASONAL == dup[i]])
      
      new.dat[new.dat$SEASONAL == dup[i], bio.nm[grep(paste(bios[j],".var$", sep = ""), bio.nm)]] <- 
        sqrt(weighted.mean(mu^2 + sigma^2, 
                           areas[dat$SEASONAL == dup[i]]) - weighted.mean(mu, 
                                                                          areas[dat$SEASONAL == dup[i]])^2)
      
      new.dat[new.dat$SEASONAL == dup[i], bio.nm[grep(paste(bios[j],".max", sep = ""), bio.nm)]] <-
        max(dat[dat$SEASONAL == dup[i], bio.nm[grep(paste(bios[j],".max", sep = ""), bio.nm)]])
      
      new.dat[new.dat$SEASONAL == dup[i], bio.nm[grep(paste(bios[j],".min", sep = ""), bio.nm)]] <-
        min(dat[dat$SEASONAL == dup[i], bio.nm[grep(paste(bios[j],".min", sep = ""), bio.nm)]])
    }}
  
  if(length(nondup) != 0){
    for(j in 1:length(nondup)){
      new.dat[new.dat$SEASONAL == nondup[j], bio.nm] <- 
        dat[dat$SEASONAL == nondup[j], bio.nm]}}
  
  return(new.dat)}

compileSeasonRangeRow <- function(bird.file, df.blank, wd.output, wd.bird, bio.nm, bios){
  
  df.row <- df.blank
  
  spp <- sub(".Rdata", "", bird.file)     
  df.row[,"spp"] <- spp
  
  # Load matched shapefile & range characteristics data
  load(file = paste(wd.output,"Matched Shapefiles/", spp, ".Rdata", sep=""))
  
  dat <- x@data[x@data$PRESENCE < 3 & x@data$SEASONAL < 5,]
  
  if(nrow(dat) == 0 | !"alt.m" %in% names(dat)){next}
  
  if(any(duplicated(dat$SEASONAL))){
    
    dat <- aggregateDuplicates(dat, wd.bird, bird.file, bio.nm, bios, wd.output, x)}
  
  for(i in 1:nrow(dat)){
    
    season <- paste("S",dat$SEASONAL[i], sep = "")
    df.row[,paste(season, bio.nm, sep = "")] <- dat[dat$SEASONAL[i] , bio.nm]
  }
  
  load(file = paste(wd.output,"range dat/", spp, ".Rdata", sep=""))
  df.row[, names(range.dat)] <- range.dat
  
  return(df.row)}


