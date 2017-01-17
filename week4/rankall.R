#setting directory
setwd("/Users/Rafael/Documents/GitHub/Coursera-JH-RProgramming/week4")

#loading data
h_outcome <- read.csv("outcome-of-care-measures.csv",  colClasses = "character")
h_outcome[, 11] <- as.numeric(h_outcome[, 11])
h_outcome[, 17] <- as.numeric(h_outcome[, 17])
h_outcome[, 23] <- as.numeric(h_outcome[, 23])

rankall <- function(outcome, num = "best") {
    ## Read outcome data
    
    #test process
    #outcome <- "heart failure"
    #num <- 10
    
    ## Check that outcome is valid
    outcome <- casefold(outcome)
    if (outcome != "heart attack" & outcome != "heart failure" & outcome != "pneumonia") {
        stop("invalid outcome")
    }
    
    #adjusting outcome case and separetor
    capitalize <- function(x) {
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1, 1)), substring(s, 2),
              sep = "", collapse = " ")
    }
    
    outcome <- capitalize(outcome)
    outcome <- gsub(" ",".", outcome)
    
    #join the name of the column
    columnName <- paste("Hospital.30.Day.Death..Mortality..Rates.from",outcome, sep = ".")
    
    
    #Checking num
    if (is.character(num)) num <- casefold(num, upper = FALSE)
    if ( !is.numeric(num) & num != "best" & num != "worst") {
        stop("invalid rank")
    }
    ## Return hospital name in that state with the given rank
    
    #starting straction process
    remove <- !is.na(h_outcome[,columnName])
    processing <- h_outcome[remove,]
    processing <- processing[order(processing$Hospital.Name),]
    processing <- processing[order(processing[,columnName]),]
    processing <- processing[order(processing$State),]
    
    
    ## Return hospital name in that state with lowest 30-day death
    keepColumns <- c("Hospital.Name", "State")
    allHospital <- processing[,keepColumns]
    #allHospital$State <- as.factor(allHospital$State)
    #allHospital <- split(allHospital, allHospital$State)
    
    if (num == "best") num <- 1
    #if (num == "worst") num <- -1

    
    states <- unique(allHospital$State)
    
    #i <- 35
    
    for (i in 1:length(states)){
                              x <- states[i]
                              y <- allHospital[allHospital$State == x,]
                              #set worst
                              if (num == "worst") {
                                  y <- y[nrow(y),]
                              #check if num is to big
                              } else if (nrow(y) < num) {
                                  y <- c(NA, states[i])
                              } else{
                                  y <- y[num,]   
                              }
                              allHospital <- allHospital[allHospital$State != x,]
                              allHospital <- rbind(allHospital, y)
                              #i <- i+1
                          }
                          
    
    ## return
    allHospital
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
}

