#______master.cpr<-cpr.overlap.dat


#__Restart?
#__X<-master.cpr

#Set up
rm(list = ls())

#CPR prep__________________________________________________________________________________________
load(file="/Users/annakrystalli/Documents/CPR/DATA/RAW DATA/SeaWiFS/r files/master cpr data.RData")
X<-cpr.overlap.dat

#Files
data.index<-read.csv("/Users/annakrystalli/Documents/SATELLITE/data type index.csv")

save.folder<-"/Users/annakrystalli/Documents/TRAINING DATA"

match.which<-1:length(data.index[,1])


#--MATCH FROM SCRTACH_______________________________________________________________________________    

#...Monthly match

for (i in match.which){
  
  dir<-paste("/Users/annakrystalli/Documents/SATELLITE/monthly/", data.index[i,1],sep="")
  
  
  
  #...get match folder info
  match.folder.info<-matchFolderInfo(dir)
  
  #...set tem.label for searching
  if(match.folder.info$temp=="W"){ tem.label<-".w"
  temp.index<-unique(X$w.index)
  temp.id<-X$w.index}else{tem.label<-".m"
  temp.index<-unique(X$m.index)
  temp.id<-X$m.index}
  
  #...Create new column if required 
  if(length(which((colnames(X)==paste(match.folder.info$data.type, tem.label, sep=""))==TRUE))==0){
    #__Create new variable column?
    X<-createVarCol(X,s,match.folder.info=match.folder.info)  
  }
  
  
  #...Allocate column to match data to and clear column with NA 
  var.col<-which((colnames(X)==paste(match.folder.info$data.type, tem.label, sep=""))==TRUE)
  X[,var.col]<-NA
  
  #_______________________________________________________________________________________________      
  for(j in 1:length(temp.index)){
    
    temp.index.i<-temp.index[j]
    
    #Produce index of data rows temporally matching temporal index
    match.id<-which(temp.id==temp.index.i)
    
    
    #Test whether sat image with temporal match exists - otherwise "NTP"
    if (length(
      try(
        sat.file<-match.folder.info$filenames[grep(temp.index.i, match.folder.info$filenames,
                                                   value=FALSE)] , 
        silent=TRUE)
    )!=0){load(paste(dir,"/r files/",sat.file, sep=""))}else{X[match.id,var.col]<-"NTP"
    rm(sat.file)} 
    
    
    #If sat.file created find space match. 1=land, 0=cloud, need to remove at end   
    if (testObject(sat.file)==TRUE){
      
      s<-img
      
      #...extract var values
      X[match.id,var.col]<-diag(s[X$r.ind[match.id],X$c.ind[match.id]])
      
      #...remove sat.file    
      rm(sat.file)
      rm(s)
      rm(img)
      
    }
    
    
    print(temp.index.i)
  }
  
  
  
  print(dir)
  
  
  
  #...once column has run through all cpr samples, save as latest version  
  save(X, file=paste(save.folder, "/Latest training save.RData", sep="")) 
  
  
  
  
}   



#...Weekly match____________________________________________________________________________

for (i in match.which){
  dir<-paste("/Users/annakrystalli/Documents/SATELLITE/weekly/", data.index[i,1],sep="")
  
  
  
  #...get match folder info
  match.folder.info<-matchFolderInfo(dir)
  
  #...set tem.label for searching
  if(match.folder.info$temp=="W"){ tem.label<-".w"
  temp.index<-unique(X$w.index)
  temp.id<-X$w.index}else{tem.label<-".m"
  temp.index<-unique(X$m.index)
  temp.id<-X$m.index}
  
  #...Create new column if required 
  if(length(which((colnames(X)==paste(match.folder.info$data.type, tem.label, sep=""))==TRUE))==0){
    #__Create new variable column?
    X<-createVarCol(X,s,match.folder.info=match.folder.info)  
  }
  
  
  #...Allocate column to match data to  
  var.col<-which((colnames(X)==paste(match.folder.info$data.type, tem.label, sep=""))==TRUE)
  X[,var.col]<-NA
  
  #_______________________________________________________________________________________________      
  
  
  for(k in 1:length(temp.index)){
    
    temp.index.i<-temp.index[k]
    
    #Produce index of data rows temporally matching temporal index
    match.id<-which(temp.id==temp.index.i)
    
    
    #Test whether sat image with temporal match exists - otherwise "NTP"
    if (length(
      try(
        sat.file<-match.folder.info$filenames[grep(temp.index.i, match.folder.info$filenames,
                                                   value=FALSE)] , 
        silent=TRUE)
    )!=0){load(paste(dir,"/r files/",sat.file, sep=""))}else{X[match.id,var.col]<-"NTP"
    rm(sat.file)} 
    
    
    #If sat.file created find space match. 1=land, 0=cloud, need to remove at end   
    if (testObject(sat.file)==TRUE){
      
      s<-img
      
      #...extract var values
      X[match.id,var.col]<-diag(s[X$r.ind[match.id],X$c.ind[match.id]])
      
      #...remove sat.file    
      rm(sat.file)
      rm(s)
      rm(img)
      
    }
    
    
    print(temp.index.i)
  }
  
  
  
  print(dir)
  
  
  
  #...once column has run through all cpr samples, save as latest version  
  save(X, file=paste(save.folder, "/Latest training save.RData", sep="")) 
  
} 

#Reset directory   
setwd("/Users/annakrystalli/")  

#____________________________________________________________END MATCH FROM SCRATCH_______________________________________________________________________________    




#---MATCH BATHYMETRY...................................................................

#...LOAD Bathymetry map
load(file="~/Documents/SATELLITE/Associated data/r files/Bathymetry.RData")


#Load latest trainind dataset
load(file="/Users/annakrystalli/Documents/TRAINING DATA/Latest training save.RData")

#Create bathymetry column and assign appropriate values from bathymetry map
X$bath<-NA
X$bath<-bath.m[cbind(X$r.ind, X$c.ind)]

#...Save as latest version  
save(X, file=paste(save.folder, "/Latest training save.RData", sep=""))



#---MATCH NAO...................................................................

load(file="~/Documents/SATELLITE/Associated data/r files/NAO.RData")

#Create data columns
X$NAO<-NA
X$wNAO<-NA

NAO.index<-NAO$m.index[which(NAO$m.index%in%X$m.index)]

#...match m.index to m.index
for(i in 1:length(NAO.index)){
  X$NAO[X$m.index==NAO.index[i]]<-NAO$NAO[NAO$m.index==NAO.index[i]]
  X$wNAO[X$m.index==NAO.index[i]]<-NAO$wNAO[NAO$m.index==NAO.index[i]]
}

#...Save as latest version  
save(X, file=paste(save.folder, "/Latest training save.RData", sep=""))

