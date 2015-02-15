best <- function(state, outcome) {
  
  # Aux function to upercase first letter of every word in a string
  
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  
  
  ## Read outcome data
  
  outcomeData<-read.csv(file = "outcome-of-care-measures.csv")
  
  ## Check that state and outcome are valid
  
  possibleOutcomes<-c("heart attack", "heart failure", "pneumonia")
  
  stringError<-paste("Error in best(\"",state,"\", \"",outcome,"\"): invalid",sep="")
  
  if(!state %in% levels(outcomeData$State)) {
    cat(stringError," state",sep = "")
    stop(stringError)
  }
  if(!outcome %in% possibleOutcomes) {
    cat(stringError," outcome")
    stop(stringError)
  }
  
  #Get the name of the column we want to extract in colName
  
  outcome<-simpleCap(outcome)
  outcome<-sub(" ",".",outcome)
  colName <- paste("Hospital.30.Day.Death..Mortality..Rates.from.",outcome,sep = "")
  
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  ratesFromState<-outcomeData[outcomeData$State == state,c("Hospital.Name",colName)]
  means<-tapply(X = ratesFromState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,INDEX = ratesFromState$Hospital.Name,FUN = mean,na.rm = TRUE)
  min(as.numeric(outcomeData[outcomeData$State == state,colName]),na.rm = TRUE)
  
}


