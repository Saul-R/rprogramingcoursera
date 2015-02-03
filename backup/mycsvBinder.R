mycsvBinder <- function(directory) {
  savewd<-getwd()
  setwd(paste(getwd(),"/",directory,sep=""))
  filenames <- list.files(path = getwd())
  bindedCsv <- do.call(rbind,lapply(filenames, read.csv, header = TRUE)) #the heavy stuff
  setwd(savewd)
  bindedCsv #still trying to figure out how R functions return values, this works, but it might be cheap & dirty
}