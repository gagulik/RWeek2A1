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
        fNames <- sprintf("%03d",id) # add prepending zeroes to id to form file names
        
        # Construct file path in a platform-indpendent manner
        FullPathes <- file.path(directory, paste(fNames, fExt, sep="."), fsep = .Platform$file.sep)
        
        # Create vector of mean values for each monitor
        MeanS <- numeric(length(FullPathes))

        # Enumerate through all files
        for (i in seq_along(FullPathes)) {
                DataFile <- FullPathes[i]
                #print(DataFile)
                Data <- read.csv(DataFile)
                
                # Filter-out NA values
                Poll <- Data[[pollutant]] # get pollutant as vector
                Poll2 <- Poll[!is.na(Poll)] # remove NA values
#                CleanData <- Data[!is.na(Data[pollutant]),]
                CleanData <- Data[!is.na(Data["sulfate"]),]
                CleanData <- CleanData[!is.na(CleanData["nitrate"]),]
                
                # Get vector from Data Frame column and calculate it's mean
                #MeanS[i] <- mean(Poll2)
                MeanS[i] <- mean( CleanData[[pollutant]] )
                #print(MeanS[i])
        }
        
        # Return value -  mean of means
        FinalMean <- mean(MeanS)
        FinalMean
}
