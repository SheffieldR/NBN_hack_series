#Set up
rm(list = ls())

#Packages
require(png)
require(tiff)
require(stringr)

#Files
data.index<-read.csv("/Users/annakrystalli/Documents/SATELLITE/data type index.csv")
#___________________________________________________________________________________________________ 

#SET DIRECTORY

#Set filename directory from which to batch open
dir<-"/Users/annakrystalli/Documents/SATELLITE/monthly/fcomp_dens"


#Get filename info to loop processing
folder.info<-batchOpen(dir)
dir.create(paste(folder.info$output.file, "images",sep=""))



#LOOP THROUGH IMAGES________________________________________________________________________________  
for (i in 1: length(folder.info$filenames)){
  
  file.name<-folder.info$filenames[i]
  
  
  
  imgProcess(file.name, data.type=folder.info$data.type, temp=folder.info$temp)
  
  
}
setwd("/Users/annakrystalli/")






#_____________________________________________________________________________________________________________________________________
#......Attach temp index to image filenames and save as r matrices in r files folder

#Set input directory

#...Single dir selection
dir<-"/Users/annakrystalli/Documents/SATELLITE/monthly/SeaWiFS_nLw_555_8day_fronts_composites"

#...Compile list of all 
dir.index.m<-paste("/Users/annakrystalli/Documents/SATELLITE/monthly/",data.index[,1], sep="")
dir.index.w<-paste("/Users/annakrystalli/Documents/SATELLITE/weekly/",data.index[,1], sep="")

#...Subset  
dir.index<-dir.index.m[4:5]
dir.index.w<-dir.index.w[c(-1,-3:-9)]

#...merge monthly & weekly dir indices        
dir.index<-c(dir.index.m, dir.index.w)




#Run function on entire variable folder
for(i in 1:length(dir.index)){
  batchFolderRename(dir=dir.index[i])   
}

#Reset directory   
setwd("/Users/annakrystalli/")  







