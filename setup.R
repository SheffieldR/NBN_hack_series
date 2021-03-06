
if(!library('rnbn', logical.return=TRUE)){ 
  install.packages("rnbn") 
}

# These are super-useful:
install.packages("ggplot2")
install.packages("ggmap")
install.packages("dplyr")

library(rnbn)

if(file.exists('NBN_Details.R')){
  source("NBN_Details.R")
}else{

  create <- readline("NBN_Details.R doesn't exist. Do you want to create it?\nCreate file (y/n)? ")
  retry <- tolower(create) == "y"
  if(!retry){
    writeLines("You'll need to call nbnLogin() yourself before you do anything else\n")
    writeLines("Please be careful to keep your account details out of your repository!\n")
  }
  
  while(retry){
    user <- readline("NBN username: ")
    password <- readline("NBN password: ")
    retry <- tryCatch(
      {
        rnbn::nbnLogin(user,password);
        sink("NBN_Details.R");
        writeLines(paste0("NBN_USER <- '", gsub("'", "\\'", user), "';"));
        writeLines(paste0("NBN_PASSWORD <- '", gsub("'", "\\'", password), "';"));
        sink();
        source("NBN_Details.R")
        FALSE
      },
      error = function(e){
        choice<- readline("Login failed. Try again (y/n)? ")
        tolower(choice) == 'y'
      },
      finally=function(){
        sink()
      }
    )
    
  }
}