complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  if(!exists("bindedCsv")){
    bindedCsv<-mycsvBinder(directory)    
  }
  completeDS<-bindedCsv[complete.cases(bindedCsv),"ID"]
  
  subcompleteDS<-completeDS[completeDS %in% id]
  tab<-table(subcompleteDS)
  id <- as.numeric(names(tab))
  nobs<-as.vector(tab)
  as.data.frame(cbind(id,nobs))
}