#la funcion retorna el mejor hosp en outcome

best <- function(state, outcome){
  ##read csv 
  dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character",
                  na.strings = "Not Avaible")
  ##valid outcome and state 
  ValidOutcome <- c("heart attack", "heart failure", "pneumonia")
  
  if(!outcome %in% ValidOutcome){print("stop invalid outcome")}
  
  ValidState <- unique(dat[, 7])
  
  if(!state %in% ValidState){print("stop invalid state")}
  
  ##convert outcome name into colum name
  
  fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  collname <- fullColName[match(outcome, ValidOutcome)]
  ## Return hospital name in that the state with lowest thirty day death rate
  data.state <- dat[dat$State==state,]
  idx <- which.min(as.double(data.state[, collname]))
  data.state[idx, "Hospital.Name"]
}



    
