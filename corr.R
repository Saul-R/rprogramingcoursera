corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  if(!exists("bindedCsv")){
    bindedCsv<-mycsvBinder(directory)    
  }  
  ## Return a numeric vector of correlations
  comp<-complete(directory)
  indexes<-comp[comp$nobs>threshold,"id"]
  subbindedCsv<-bindedCsv[bindedCsv$ID %in% indexes,]
  cor(x= subbindedCsv$nitrate, y = subbindedCsv$sulfate)
}