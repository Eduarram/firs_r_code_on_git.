#retorna el peor o mejor hosp depende 

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character",
                  na.strings = "Not Avaible")
  ## Check that state and outcome are valid
  ValidOutcome <- c("heart attack", "heart failure", "pneumonia")
  
  if(!outcome%in%ValidOutcome){print("stop invalid outcome")}
  
  validstate <- unique(dat[, 7])
  ##if(!state%in%validstate){print("stop invalid state")}
  
  fullColname <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  collname <- fullColname[match(outcome, ValidOutcome)] 
  ## For each state, find the hospital of the given rank
  hospital <- character(0)
  for (i in seq_along(validstate)) {
    data.state <- dat[dat$State==validstate[i],]
    ##order data by outcome
    sorted.data.state <- data.state[order(as.numeric(data.state[[collname]]),
                                          data.state[["Hospital.Name"]],
                                          decreasing=FALSE,na.last=NA), ]
    ##handle num input
    this.num = num
    if (this.num=="best") this.num = 1
    if (this.num=='worst') this.num = nrow(sorted.data.state)
    
    hospital[i] <- sorted.data.state[this.num,"Hospital.Name"]
  }
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  data.frame(hospital=hospital,state=validstate,row.names=validstate)
}

head(rankall("heart attack", 20), 10)