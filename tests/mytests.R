#Unit Tests for functions

test.pollutantmean <- function(){
  
  myMatrix<-matrix(nrow = 2,ncol=3, data = 1:6)
  valuesCsv<-data.frame(myMatrix)
  names(x = valuesCsv)<-c("id","nitrate","sulfate")

  # TEST CSV valuesCsv
  #
  #   id nitrate sulfate
  #   1  1       3       5
  #   2  2       4       6
  

  #Check the average
  checkEquals(5,algorythm_Pollutantmean(valuesCsv,pollutant = "sulfate",id = 1))

  #Check if pollutant is wrong
  checkException(algorythm_Pollutantmean(valuesCsv,pollutant = "A WRONG STRING",id = 1),)

  #Check average 2 values
  checkEquals(c(3,4),algorythm_Pollutantmean(valuesCsv,"nitrate",id = 1:2))
  
  #Check if has a wrong value
  checkException(algorythm_Pollutantmean(valuesCsv,"nitrate",id=3))
  
}

test.deactivation <- function()
{
  DEACTIVATED('Deactivating this test function')
}