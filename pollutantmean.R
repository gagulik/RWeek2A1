pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
        
        fExt  <- "csv"     # Data File extension
        
        # Enumerate through all files
        for (i in seq_along(id)) {
                fName <- sprintf("%03d",id[i]) # add prepending zeroes to id to form file names
                
                # Construct file path in a platform-indpendent manner
                FullPath <- file.path(directory, paste(fName, fExt, sep="."), fsep = .Platform$file.sep)
                
                Data <- read.csv(FullPath)
                # Get vector from Data Frame column and calculate it's mean
#                MeanS[i] <- mean( Data[,pollutant], na.rm=TRUE )
                d2<-Data[!is.na(Data[pollutant]),]
                if (i>1) {
                        d3 <- rbind(d3,d2)
                } else {
                        d3 <- d2
                }
        }
        
        # Return value -  mean of means
        mean( d3[,pollutant], na.rm=TRUE )
}
