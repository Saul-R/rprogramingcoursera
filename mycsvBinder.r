mycsvBinder <- function(directory) {
  savewd<-getwd()
  setwd(paste(getwd(),"/",directory,sep=""))
  filenames <- list.files(path = getwd())
  bindedCsv <- do.call(rbind,lapply(filenames, read.csv, header = TRUE))
  setwd(savewd)
  bindedCsv
}