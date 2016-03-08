
rm(list=ls())

library(shiny); library(Reol); library(xml2); library(rnbn)

#taxo <- read.csv("/Users/chriscooney/Google Drive/CRCStorage/Datasets/TaxonomicData/Edited checklists/BLIOCPhyloMasterTax_2015_05_06.csv")

nbnLogin(username = "drcrc", password = "Turdusmerula")

load("/Users/chriscooney/Documents/Workflows/NBN_hack_series/BirdBingo/shiny_app/shef_data.rdata")
 
birdTVKs <- getGroupSpeciesTVKs("bird")
shef_data <- shef_data[shef_data$pTaxonVersionKey %in% birdTVKs,]

spp <- table(shef_data$pTaxonName)
spp <- spp[order(spp, decreasing = T)]

spp.names <- names(spp)[1:25]

sppdat <- MatchTaxatoEOLID(spp.names, exact = T)
sppdat <- sppdat[!is.na(sppdat$eolPageNumbers),]

spp <- spp[names(spp) %in% sppdat$ListOfTaxa]

spp.list <- as.numeric(sample(sppdat$eolPageNumbers, size = 10, prob = spp, replace=F))

chosen.names <- sppdat$ListOfTaxa[match(spp.list, sppdat$eolPageNumbers)]

myEOL <- DownloadEOLpages(spp.list, to.file = FALSE)
DataObjectOverview(myEOL)

#PageProcessing(data1[1])

url.list <- as.list(rep(NA, length(myEOL)))

for (i in 1:length(myEOL)) {
    furls <- c()
    myxml <- read_xml(myEOL[[i]])
    mediaURLs <- xml_find_all(myxml, ".//d1:mediaURL", ns=xml_ns(myxml))
    #url.show(xml_text(mediaURLs[1]))
    for (j in 1:length(mediaURLs)) {
        text <- xml_text(mediaURLs[j])
        if (any(grep(".jpg", text, fixed=T))) {
            furls <- c(furls, xml_text(mediaURLs[j]))
        }
    }
    url.list[[i]] <- sample(furls, 4)
}


# # # UI # # #

img.height <- "200px"
img.width <- "150px"

ui <- fluidPage(
    # Application title
    titlePanel("Bird Bingo!"),
    hr(),
    fluidRow(
        column(2, h5(chosen.names[1])),
        column(2, h5(chosen.names[2])),
        column(2, h5(chosen.names[3])),
        column(2, h5(chosen.names[4])),
        column(2, h5(chosen.names[5])),
        column(2, h5(chosen.names[6]))
        ),
    fluidRow(
        column(2,
               img(src=url.list[[1]][1], width=img.width),
               img(src=url.list[[1]][2], width=img.width),
               img(src=url.list[[1]][3], width=img.width),
               img(src=url.list[[1]][4], width=img.width)
               ),
        column(2,
               img(src=url.list[[2]][1], width=img.width),
               img(src=url.list[[2]][2], width=img.width),
               img(src=url.list[[2]][3], width=img.width),
               img(src=url.list[[2]][4], width=img.width)
        ),
        column(2,
               img(src=url.list[[3]][1], width=img.width),
               img(src=url.list[[3]][2], width=img.width),
               img(src=url.list[[3]][3], width=img.width),
               img(src=url.list[[3]][4], width=img.width)
        ),
        column(2,
               img(src=url.list[[4]][1], width=img.width),
               img(src=url.list[[4]][2], width=img.width),
               img(src=url.list[[4]][3], width=img.width),
               img(src=url.list[[4]][4], width=img.width)
        ),
        column(2,
               img(src=url.list[[5]][1], width=img.width),
               img(src=url.list[[5]][2], width=img.width),
               img(src=url.list[[5]][3], width=img.width),
               img(src=url.list[[5]][4], width=img.width)
        ),
        column(2,
               img(src=url.list[[6]][1], width=img.width),
               img(src=url.list[[6]][2], width=img.width),
               img(src=url.list[[6]][3], width=img.width),
               img(src=url.list[[6]][4], width=img.width)
        )
    ),
    hr(),
    fluidRow(
        column(2, checkboxInput("Seen1", label = "Seen it?", value = FALSE)),
        column(2, checkboxInput("Seen2", label = "Seen it?", value = FALSE)),
        column(2, checkboxInput("Seen3", label = "Seen it?", value = FALSE)),
        column(2, checkboxInput("Seen4", label = "Seen it?", value = FALSE)),
        column(2, checkboxInput("Seen5", label = "Seen it?", value = FALSE)),
        column(2, checkboxInput("Seen6", label = "Seen it?", value = FALSE))
        )
)

server <- function(input, output, session) {
    
}

shinyApp(ui, server)
