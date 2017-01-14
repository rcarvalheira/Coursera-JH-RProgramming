complete <- function(directory, id = 1:332){
    grouping <- data.frame(as.integer(),as.numeric())
    for (i in id){
        reading <- read.csv(paste(directory,
                                  if (i < 10){
                                      "/00"
                                  }else if (i <100){
                                      "/0"
                                  }else{
                                      "/"
                                  },
                                  i,
                                  ".csv",sep = ""),
                            colClasses = c("character",
                                           "numeric",
                                           "numeric", 
                                           "integer"),
                            dec=".")
        reading <- reading[complete.cases(reading),]
        nobs <- nrow(reading)
        grouping <- rbind(grouping, c(i,nobs))
    }
    colnames(grouping) <- c("id", "nobs")
    grouping
}

complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)
