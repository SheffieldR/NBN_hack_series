
rm(list=ls())

library(Reol); library(xml2)

load("/Users/chriscooney/Documents/Workflows/NBN_hack_series/")

# head(shef_data)
# 
# dataset <- shef_data[shef_data$resolution == "100m",]
# 
# plot(dataset$latitude ~ dataset$longitude)
# 
# spp <- dataset$pTaxonName[1]

spp <- "Turdus merula"

sppdat <- MatchTaxatoEOLID(spp, exact = F)

data1 <- DownloadEOLpages(sppdat$eolPageNumbers, to.file = FALSE)
DataObjectOverview(data1)

#PageProcessing(data1[1])

myxml <- read_xml(data1[[1]])

mediaURLs <- xml_find_all(myxml, ".//d1:mediaURL", ns=xml_ns(myxml))
url.show(xml_text(mediaURLs[2]))

str(data1)

plot(data1)

