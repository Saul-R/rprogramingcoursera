corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  mycorr <- function (monitorId){
    monitor<-getCsvByIndex(monitorId,directory)
    completeIndexesBoolean<-complete.cases(monitor)
    completeIndexes<-seq(from = 1, along.with = completeIndexesBoolean)[completeIndexesBoolean]
    ##lapply(completeIndexes, function (i) cor(monitor$nitrate[i],monitor$sulfate[i]))
    
  }
  completes<-complete(directory = directory)
  indexes<-completes$id[completes$nobs > threshold]
  listOfMonitors
  
}