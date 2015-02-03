getCsvByIndex <- function (index,directory){
  path<-sprintf("%s/%.3d.csv",directory,index)
  return <- read.csv(file = path)
  return
}