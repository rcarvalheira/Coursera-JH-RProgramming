#setting directory
setwd("/Users/Rafael/Documents/GitHub/Coursera-JH-RProgramming/week4")

#loading data
h_outcome <- read.csv("outcome-of-care-measures.csv",  colClasses = "character")
h_outcome[, 11] <- as.numeric(h_outcome[, 11])
h_outcome[, 17] <- as.numeric(h_outcome[, 17])
h_outcome[, 23] <- as.numeric(h_outcome[, 23])

#begin function BEST
best <- function(state, outcome) {
    ## Read outcome data    
    
    #test process
    #outcome <- "heart failure"
    #state <- "TX"
    
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
    
    #starting straction process
    remove <- !is.na(h_outcome[,columnName])
    processing <- h_outcome[remove,]
    processing <- processing[order(processing$Hospital.Name),]
    processing <- processing[order(processing[,columnName]),]
    ## Return hospital name in that state with lowest 30-day death
    best_h <- processing$Hospital.Name[processing$State == state]
    ## rate
    best_h[1]
}
