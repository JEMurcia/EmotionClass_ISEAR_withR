library(pracma)
# PREPROCESS EMAIL
# return a list of two variable word/char vectors; the subject and the sender

process.email <- function(filename){
  #setwd( "C:/Users/Sebastian/Documents/APAU/enron1/enron1" )
  
  email.to.classify <<- filename
  lines <- readLines(filename, file.info(filename)$size)
  
  # LOWER CASE
  test <- tolower(lines)
  
  #Get the message subject
  subject <- unique( strsplit(test[1], "subject: | |:|/|)|\\(|<|>|@|,|\\.|!|=")[[1]] )
  test <- test[-1]
  
  # STRIP ALL MESSAGE
  test <- strsplit( test, " |:|/|)|\\(|<|>|@|,|\\.|!|=" )
  content <- unique(unlist(test))
  
  #CONSTRUCT THE B-CEL
  bc <- list()
  bc[[1]] <- subject
  bc[[2]] <- content
  if ( is.null(subject) ){ bc[[1]] <- "" }
  if ( is.null(content) ){ bc[[2]] <- "" }
  
  return(bc)
  
}

get.emails <- function(num_emails = -1, path){
  #SET THE CURRENT WORKING PATH
  #setwd( "C:/Users/Sebastian/Documents/APAU/enron1/enron1" )
  #dir_iemails <- "ham"
  
  #OBTAIN THE NUMBER OF EMAILS THAT ARE INTERESTING FOR US
  files <- list.files( path = path )
  if ( num_emails == -1 ){
    num_emails <- length(files)
  }
  
  #SET THE TRAINING SET
  te <- list()
  
  #GET ALL B-CELLS
  for (i in 1:num_emails){
    bc <- process.email( paste( path, files[i], sep="/" ) )
    te[[length(te)+1]] <- bc
  }
  return(te)
}

process.cell <- function(){
  
  file.copy(from = email.to.classify, to = paste(getwd(),paste('uninteresting_emails',basename(email.to.classify),sep = "/"),sep = "/"))
  
}
