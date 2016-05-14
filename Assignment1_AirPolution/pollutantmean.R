pollutantmean <- function(directory = "specdata", pollutant, id = 1:322) {
      paths <- paste(directory, "/",sprintf("%03d", id), ".csv", sep="")
      data <- data.frame()
      
      for(path in paths) {
            data <- rbind(data, read.csv(path))
      }
      
      mean(data[,pollutant],  na.rm = TRUE);
}

pollutantmean("specdata", "sulfate", 1:10)
## [1] 4.064

pollutantmean("specdata", "nitrate", 70:72)
## [1] 1.706

pollutantmean("specdata", "nitrate", 23)
## [1] 1.281