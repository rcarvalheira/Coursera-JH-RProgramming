#setting directory
setwd("/Users/Rafael/Documents/GitHub/Coursera-JH-RProgramming/week4")

#loading data
h_outcome <- read.csv("outcome-of-care-measures.csv",  colClasses = "character")
h_outcome[, 11] <- as.numeric(h_outcome[, 11])
h_outcome[, 17] <- as.numeric(h_outcome[, 17])
h_outcome[, 23] <- as.numeric(h_outcome[, 23])


rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    
    #test process
    #outcome <- "heart failure"
    #state <- "TX"
    #num <- 4
    
    ## Check that outcome is valid
    outcome <- casefold(outcome)
    if (outcome != "heart attack" & outcome != "heart failure" & outcome != "pneumonia") {
        stop("invalid outcome")
    }
    
    ## Check that state is valid
    states <- unique(h_outcome$State)
    state <- casefold(state, upper = TRUE)
    if (length(states[states == state]) == 0) {
        stop("invalid state")
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
    processing <- processing[processing$State == state,]
    
    
    ## Return hospital name in that state with lowest 30-day death
    rankedHospital <- processing[,"Hospital.Name"]
    
    if (num == "best") num <- 1
    if (num == "worst") num <- nrow(processing)

    #check if num is to big
    if (nrow(processing) < num) return(NA)

    ## return
    rankedHospital[num]
    
    ## 30-day death rate
}