pollutantmean <- function(directory, pollutant, id = 1:332){
    grouping <- vector(mode="numeric", length=0)
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
        cleaning <- colnames(reading) == pollutant
        reading <- reading[cleaning]
        reading <- reading[!is.na(reading)]
        grouping <- c(grouping,reading)
    }
    round(mean(grouping, na.rm = TRUE),3)
}
