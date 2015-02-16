sortBy <- function(state, outcome) {
  
  # Aux function to upercase first letter of every word in a string
  notAvailable<-"Not Available"
  
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  
  ## Read outcome data
  
  outcomeData<-read.csv(file = "outcome-of-care-measures.csv")
  ## Check that state and outcome are valid
  possibleOutcomes<-c("heart attack", "heart failure", "pneumonia")
  errorState<-paste("Error in best(\"",state,"\", \"",outcome,"\"): invalid state",sep="")
  errorOutcome<-paste("Error in best(\"",state,"\", \"",outcome,"\"): invalid outcome",sep="")
  
  if(!state %in% levels(outcomeData$State)) {
    stop(errorState)
  }
  if(!outcome %in% possibleOutcomes) {
    stop(errorOutcome)
  }
  
  #Get the name of the column we want to extract in colName
  
  outcome<-simpleCap(outcome)
  outcome<-sub(" ",".",outcome)
  colName<-paste("Hospital.30.Day.Death..Mortality..Rates.from.",outcome,sep = "")
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  ratesFromState<-outcomeData[outcomeData$State == state,c("Hospital.Name",colName)]
  
  ##Clean the Not Available
  dataClean<-data.frame(ratesFromState$Hospital.Name,(as.numeric(sub(pattern = notAvailable,replacement = NA_real_,x = ratesFromState$Hospital.30))))
  names(dataClean)<-c("Hospital","Outcome")
  sorted<-dataClean[order(dataClean[,2],dataClean[,1]),]
  sorted
}

