

dat <- read.csv("data/data.csv",
                stringsAsFactors = F)

metadata <- read.csv("data/metadata.csv", 
                     stringsAsFactors = FALSE)

vars <- as.list(names(dat)[!names(dat) %in% c("species","order","family")])
names(vars) <- paste(metadata$cat[match(vars, metadata$ms.vname)], 
                     "-", metadata$list.vname[match(vars, metadata$ms.vname)])
vars <- vars[order(names(vars))]

shinyServer(function(input, output) {
  
  ### SINGLE VARIABLE PANEL ##################################################
  
  # Subset by family ..............
  output$taxoDat <- renderUI({

    choices <- as.list(c("all", sort(as.character(unique(
      dat$family[!is.na(dat[, input$variable[[1]]])])))))

    selectInput("taxo", "select families", choices = choices, selected = "all",
                selectize = TRUE, multiple = T)
  })
  
  # Plot output ##############################################################
  output$plot1 <-renderPlotly({

    
    

  
    if(is.null(input$taxo)){select <- rep(TRUE, dim(dat)[1])}else{  
  if(input$taxo == "all"){
    select <- rep(TRUE, dim(dat)[1])}else{
      select <- dat$family %in% input$taxo}}
     select <- select & !is.na(dat[, input$variable[[1]]])
   
   titl <- metadata$descr[metadata$ms.vname == input$variable[[1]]]
   
   
   
    
   if(metadata$plot.type[metadata$ms.vname == input$variable[[1]]] == "bar"){
     
     # BAR...........................................................
         
     x <- unlist(strsplit(metadata$levels[metadata$ms.vname == input$variable[[1]]], ","))
     tab <- table(numerise(dat[select, input$variable[[1]]]))
     
     if(any(x == "levels")){x <- names(tab)
     y <- as.vector(tab)}else{
       y <- rep(0, length(x))
       names(y) <- unlist(strsplit(metadata$scores[metadata$ms.vname == input$variable[[1]]], ","))
       y[match(names(tab), names(y))] <- tab}
     names(y) <- NULL
     
     p1 <- plot_ly(x = x,  y = y, 
             type = "bar",
             marker = list(color = toRGB("aquamarine3"),
                           line = list(color = toRGB("aquamarine3")))) %>%
     
     layout(autosize = T, xaxis = list(title = titl), 
            yaxis = list(title = "counts"), 
            title = paste("n =", sum(select)), margin = list(l = 120,
                                                             r = 80,
                                                             b = 120,
                                                             t = 60))
     p1}else{
     
              # HISTOGRAM.....................................................
              
              if(input$log == T){x <- log(as.numeric(dat[select, input$variable[[1]]]))
              titl <- paste("log", titl)}else{
                x <- as.numeric(dat[select, input$variable[[1]]])
              }
              
              if(all(x >= 0 & 1 >= x)){xaxis <- list(title = titl, range = c(0,1), 
                   autorange = F, autotick = T, tick0 = 0)}else{
                xaxis <- list(title = titl, autorange = T,
                              autotick = T, tick0 = 0) 
              }
              
              minx <- min(x) - 1
              maxx <- max(x) + 1
              size <- (maxx - minx) / input$bins
    

    
            p1 <- plot_ly(data = dat, x = x, 
                      type = "histogram", autobinx = F, 
                      xbins = list(start = minx, 
                                   end = maxx, 
                                   size = size),
                      marker = list(color = toRGB("aquamarine3"),
                                    line = list(color = toRGB("aquamarine3")))) %>%
            
              
              layout(xaxis = xaxis,
                     title = paste("n =", sum(select)))  # dtick = size))
            p1
   }
    
    })
  
  # Summary output ##############################################################
  output$summary <- renderDataTable({
  
    
    if(input$taxo == "all"){
      select <- rep(TRUE, dim(dat)[1])}else{
        select <- dat$family %in% input$taxo}
    select <- select & !is.na(dat[, input$variable[[1]]])
    
    x <- dat[select, input$variable[[1]]]
    
    if(metadata$plot.type[metadata$ms.vname == input$variable[[1]]] == "histogram"){
      x <- numerise(x)
      sum <- summary(x)
      extra <- as.data.frame(round(psych::describe(x),2))
      sumtab <- as.data.frame(c(sum, extra[,c("n","sd", "range", "skew", "kurtosis", "se")]))
      names(sumtab) <- gsub("X", "", names(sumtab))
      sumtab}else{
        sumtab <- round(prettyR::describe(as.character(x))[[2]]$x,2)
        sumtab <- cbind(descr = rownames(sumtab), sumtab)}
    
    
  }, options = list(searching = FALSE,
                    paging = FALSE))
  
  # Data output ##############################################################
  output$data <- renderDataTable({

    if(input$taxo == "all"){
      select <- rep(TRUE, dim(dat)[1])}else{
        select <- dat$family %in% input$taxo}
    select <- select & !is.na(dat[, input$variable[[1]]])
    
    rawdat <- dat[select, c("species", "family", input$variable[[1]])]
    rawdat <- as.data.frame(lapply(rawdat,FUN = numerise))
  })
  
  
  ### CROSS VARIABLE PANEL ##################################################
  output$relVar <- renderUI({
    if(input$rel){
    choices <- c(vars[vars == input$var1], vars[vars == input$var2])
    
    selectInput("relVar", "select variable", choices = choices, 
                selectize = F)}
  })
  
  output$plot2 <-renderPlotly({

    #source("app_output_functions.R")
    
    df <- dat[, c("species", input$var1, input$var2)]
    df <- df[complete.cases(df),]
    
    validate(
      need(nrow(df) >0, "no species with overlapping data. Select different variable pair")
    )
   
    xw <- 50
    yw <- 30
    ylw <- 12
    xlw <- 14
    
    ### Both variables NUMERIC ##########################################################
    
    if(all(metadata$plot.type[metadata$ms.vname == input$var1] == "histogram",
           metadata$plot.type[metadata$ms.vname == input$var2] == "histogram")){
      
      
      type <- "scatter"
      x <- df[, which(names(df) == input$var1)]
      x.titl <- metadata$descr[metadata$ms.vname == input$var1]
      if(input$log1 == T){x <- log(x)
      x.titl <- paste("log", x.titl)}
      y <- df[, which(names(df) == input$var2)]
      y.titl <- metadata$descr[metadata$ms.vname == input$var2]
      if(input$log2 == T){y <- log(y)
      y.titl <- paste("log", y.titl)}
      
      
      z <- NULL
      xaxis <- list(title = wordbreakHTML(x.titl, xw))
      yaxis <- list(title = wordbreakHTML(y.titl, yw))
      text <- df$species
      hoverinfo ="x+y+text"
      }else{
        if(all(metadata$plot.type[metadata$ms.vname == input$var1] == "bar",
               metadata$plot.type[metadata$ms.vname == input$var2] == "bar")){
          
          
          type <- "heatmap"
          x1 <- df[, which(names(df) == input$var1)]
          x2 <- df[, which(names(df) == input$var2)]
          
          r.nm <- data.frame(s = unlist(strsplit(metadata$scores[metadata$ms.vname == input$var1], ",")),
                             n = unlist(strsplit(metadata$levels[metadata$ms.vname == input$var1], ",")))
          c.nm <- data.frame(s = unlist(strsplit(metadata$scores[metadata$ms.vname == input$var2], ",")),
                             n = unlist(strsplit(metadata$levels[metadata$ms.vname == input$var2], ",")))
          
          tab <- table(x1, x2)
          ids <- data.frame(v = as.vector(tab), 
                            r = rep(match(rownames(tab), r.nm$s),length(colnames(tab))),
                            c = rep(match(colnames(tab), c.nm$s), each = length(rownames(tab))))
          
          z <- matrix(0, nrow = nrow(r.nm), ncol = nrow(c.nm))
          z[as.matrix(ids[,c("r","c")])] <- ids[,"v"]
          
          if(input$rel){
           z <- round(z / matrix(rep(apply(z, which(c(input$var1, input$var2) == input$relVar), 
                  FUN = sum), each = dim(z)[which(c(input$var1, input$var2) != input$relVar)]),
                  ncol = dim(z)[2], nrow = dim(z)[1], byrow = input$var1 == input$relVar)
                  ,2)
          }
          z <- t(z)


          
          xaxis <- list(title = wordbreakHTML(metadata$descr[metadata$ms.vname == input$var1], xw))
          yaxis <- list(title = wordbreakHTML(metadata$descr[metadata$ms.vname == input$var2], yw))
          text <- ""
          hoverinfo ="z+x+y+text"
          

          y <-sapply(as.character(c.nm[,"n"]), FUN = wordbreakHTML,  width = ylw,
          USE.NAMES = F)
          x <- sapply(as.character(r.nm[,"n"]), FUN = wordbreakHTML,  width = xlw,
          USE.NAMES = F)
          names <- sapply(unique(as.character(x)), FUN = wordbreakHTML,  width = ylw,
                          USE.NAMES = F)
 
        }else{
          
          df <- df[,c("species", input$var1, input$var2)]
          
          vid <- metadata$plot.type[match(c(input$var1, input$var2), metadata$ms.vname )] == "bar"
          x <- df[,c(input$var1, input$var2)[vid]]
          y <- df[,c(input$var1, input$var2)[!vid]]
          y.titl <- metadata$descr[metadata$ms.vname == c(input$var1, input$var2)[!vid]]
          
          if(input[[c("log1", "log2")[!vid]]] == T){y <- log(y)
          y.titl <- paste("log", y.titl)}
          
          x.nm <- data.frame(s = unlist(strsplit(metadata$scores[metadata$ms.vname == c(input$var1, input$var2)[vid]], ",")),
                             n = unlist(strsplit(metadata$levels[metadata$ms.vname == c(input$var1, input$var2)[vid]], ",")))
          
          x <- x.nm$n[match(x, x.nm$s)]
          x <- sort(factor(x, levels = x.nm$n))
          
          names <- sapply(unique(as.character(x)), FUN = wordbreakHTML,  width = xlw,
                          USE.NAMES = F)
          x <- names[match(x, unique(as.character(x)))]
          x <- factor(x, levels = names)
          
          x.titl <- metadata$descr[metadata$ms.vname == c(input$var1, input$var2)[vid]]
          type <- "box"
          


          xaxis <- list(title = wordbreakHTML(x.titl, xw))
          yaxis <- list(title = wordbreakHTML(y.titl, yw))
          z <- NULL
          text <- df$species
          hoverinfo ="x+y+text"
        }
        
      }
      
  
      
      p2 <- plot_ly(x = x, y = y, z = z, hoverinfo = hoverinfo, text = text, name = names,
              type = type, mode = "markers", colorscale = "Greens", reversescale = T,
              marker = list(color = toRGB("aquamarine2"), opacity = 0.5, size = 4.5,
                            line = list(color = toRGB("aquamarine4"), width = 0.5))) %>%
      
      layout(xaxis = xaxis, yaxis = yaxis,
             title = paste("n =", dim(df)[1]),
             margin = list(l = 150,
                           r = 80,
                           b = 120,
                           t = 60)) 
      
      p2
  })
  
  ### DOWNLOAD PANEL ##################################################
  
 
  
  
  # Subset by family - output panel
  output$taxoDat2 <- renderUI({
    
    if(is.null(c(input$varOut, input$varGroup))){choices <- list("NA")}else{
    if(any(input$varOut == "all")){choices <- as.list(c("all", sort(as.character(unique(
      dat$family)))))}else{
      vars.out <- unique(c(input$varOut, metadata$ms.vname[metadata$cat %in% input$varGroup &
                                                           metadata$ms.vname %in% vars]))
      choices <- as.list(c("all", sort(as.character(unique(
        dat$family[data.frame(which(!is.na(dat[,unlist(vars.out)]), arr.ind = T))[,1]])))))
      }
      }
    
    selectInput("taxoOut", "select families", choices = choices, selected = "all",
                selectize = T, multiple = T)
  })
  
  # Print selected families
  output$fam_out <- renderUI({
    
    if(is.null(input$taxoOut)){}else{if(any(input$taxoOut == "all")){"all"}else{
      
      HTML(paste(input$taxoOut, '<br/>'))
    }}
  })
  
  # Print selected variables
  output$var_out <-  renderDataTable({
    
    if(is.null(c(input$varOut, input$varGroup))){}else{
      
      if(any(input$varOut == "all")){vars.out <- unlist(vars, use.names = F)}else{
        vars.out <- unique(c(input$varOut, 
                             metadata$ms.vname[metadata$ms.vname %in% vars & 
                                                 metadata$cat %in% input$varGroup]))
        }
      
      # Print selected families
      if(is.null(input$taxoOut)){fam <- unique(dat$family)}else{if(any(input$taxoOut == "all")){
        fam <- unique(dat$family)}else{fam <- input$taxoOut}}
      
      # calculated unique species data points for each variable for selected families
      n <- apply(data.frame(dat[dat$family %in% fam,unlist(vars.out)]), 2, FUN = function(x){sum(!is.na(x))})
      n[n == 0] <- "-"
      
      
      
      dfprint <- data.frame(var = paste(metadata$cat[match(vars.out, metadata$ms.vname)],
                                        " : ",
                                        metadata$list.vname[match(vars.out, metadata$ms.vname)],
                                        sep = ""), 
                            n = n)
      dfprint <- dfprint[order(dfprint$var),]
    }
  },
  options = list(paging = FALSE,
                 searching = FALSE))
  

  observe({
    if (input$password == password) {
      Sys.sleep(0)
      # enable the download button
      shinyjs::enable("downloadData")}
  })
  
  output$downloadData <- downloadHandler(
    filename = paste("birdDat-",Sys.Date(),".zip", sep = ""),
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      #print(tempdir())
      
      if(any(input$varOut == "all")){vars.out <- unlist(vars, use.names = F)}else{
        vars.out <- unique(c(input$varOut, 
                             metadata$ms.vname[metadata$ms.vname %in% vars & 
                                                 metadata$cat %in% input$varGroup]))}
      
      if(any(input$taxoOut == "all")){
        spp <- unique(dat$species[dat$var %in% vars.out])}else{
          spp <- unique(dat$species[dat$family %in% input$taxoOut & dat$var %in% vars.out])}
      
      wide.out <- dat[dat$species %in% spp, vars.out]
      metadata.out <- metadata[metadata$ms.vname %in% vars.out,]
      
      fs <- c("data.csv", "metadata.csv")

      write.csv(wide.out, file = "data.csv")
      write.csv(metadata.out, file = "metadata.csv")
      
      zip(zipfile=fname, files=fs)
      if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip"), fname)}
    },
    contentType = "application/zip")
  
  # disable the downdload button on page load
  shinyjs::disable("downloadData")
  
  

  
})


