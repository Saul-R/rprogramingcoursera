rankall <- function(outcome, num = "best") {

  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  # Aux function to upercase first letter of every word in a string
  notAvailable<-"Not Available"
  
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  
  selectHospital <- function (myState,num){
    #myState<-myState[[1]]
    myState<-na.exclude(myState[[1]])
    if (is.numeric(num)){
      if(num<=dim(myState)[1]){
        myIndex<-num
      }
      else{
        myIndex<-1
        myState<-cbind(NA,NA,NA)
      }
    }
    if (num == "best"){
      myIndex<-1
    }
    if(num=="worst"){
      myIndex<-dim(myState)[1]
    }
    myState<-myState[order(myState$state,myState$outcomes,myState$hospital),]
    myState[myIndex,]
  }
  
  ## Read outcome data
  if (is.numeric(num)){
    myIndex<-num
  }
  if (num == "best"){
    myIndex<-1
  }
  if(num=="worst"){
    myIndex<-"worst"
  }
  
  outcomeData<-read.csv(file = "outcome-of-care-measures.csv")
  ## Check that state and outcome are valid
  possibleOutcomes<-c("heart attack", "heart failure", "pneumonia")
  errorState<-paste("Error in best(\"",state,"\", \"",outcome,"\"): invalid state",sep="")
  errorOutcome<-paste("Error in best(\"",state,"\", \"",outcome,"\"): invalid outcome",sep="")
  
  #Get the name of the column we want to extract in colName
  
  outcomeCol<-simpleCap(outcome)
  outcomeCol<-sub(" ",".",outcomeCol)
  outcomeCol<-paste("Hospital.30.Day.Death..Mortality..Rates.from.",outcomeCol,sep = "")
  
  allHospitals<-data.frame(outcomeData$Hospital.Name,as.numeric(sub(pattern = notAvailable, outcomeData[,outcomeCol],replacement = NA_real_)),outcomeData$State)
  names(allHospitals)<-c("hospital","outcomes","state") 
  allHospitals<-na.exclude(allHospitals[order(allHospitals$state,allHospitals$outcomes,allHospitals$hospital),])
  ##
  ##
  ##split
  ##listHospitalsByStates<-split(allHospitals,allHospitals$state)
  indexes<-table(allHospitals$state)
  #indexes<-as.data.frame(indexes)
  #indexes<-data.frame(colnames(indexes),as.numeric(indexes[1,]))
  indexes<-cumsum(indexes)
  for(i in 2:(length(indexes))){
    if (myIndex=="worst"){
      indexNow<-indexes[i-1]
    }
    else{
      if((indexes[i-1]+myIndex)<indexes[i]){
        indexNow<-indexes[i-1]+myIndex
      }
      else{
        indexNow<-NA
      }
    indextostract[i]<-indexNow
    }
  }
  if(myIndex<indexes[1]){
    indextostract[1]<-myIndex
  }
  else {
    if(myIndex == "worst"){
      indextostract<-indexes
    }
  }
  
  hospitals<-allHospitals[indextostract,]$hospital
  res<-data.frame(hospitals,levels(allHospitals$state))  
  colnames(res)<-c("hospital","state")
  res
}
