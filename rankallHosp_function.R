
#hace una evaluacion del mejor o peor
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character",
                  na.strings = "Not Avaible")
  ## Check that state and outcome are valid
  ValidOutcome <- c("heart attack", "heart failure", "pneumonia")
  
  if(!outcome%in%ValidOutcome){print("stop invalid outcome")}
  
  validstate <- unique(dat[, 7])
  if(!state%in%validstate){print("stop invalid state")}
  ## Return hospital name in that state with the given rank
  fullColname <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  collname <- fullColname[match(outcome, ValidOutcome)]
  ## 30-day death rate
  data.state <- dat[dat$State==state,]
  ##order data by outcome
  sorted.data.state <- data.state[order(as.numeric(data.state[[collname]]),
                                        data.state[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]
  ##handle num imput 
  if(num=="best") num = 1
  if(num=="worst") num = nrow(sorted.data.state)
  #will automatically return NA if num > nrow, as well as if it's some other text value
  # if someone passes num < 1, they'll get what's expected
  #if (is.numeric(num) & num > nrwo(sorted.data.state) return(NA)
  sorted.data.state[num, "Hospital.Name"]
}

rankhospital("MD", "heart attack", "worst")
