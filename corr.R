corr <- function(directory, threshold = 0){
    files <- list.files(directory)
    nFiles <- length(files)
    grouping <- vector(mode="numeric", length=0)
    for (i in 1:nFiles){
        reading <- read.csv(paste(directory,"/",files[i],sep=""),
                            colClasses = c("character",
                                           "numeric",
                                           "numeric", 
                                           "integer"),
                            dec=".")
        reading <- reading[complete.cases(reading),]
        corr <- round(cor(reading$sulfate, reading$nitrate),5)
        if (nrow(reading) > threshold){    
            grouping <- rbind(grouping, corr)
        }
        rownames(grouping) <- NULL
    }
    grouping
}