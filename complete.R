complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
        
        DataF <- data.frame(id=numeric(),
                         nobs=numeric(), 
                         stringsAsFactors=FALSE) 
        
        fExt  <- "csv"     # Data File extension

        nobs <- numeric(length(id))
        # Enumerate through all files
        for (i in seq_along(id)) {
                fName <- sprintf("%03d",id[i]) # add prepending zeroes to id to form file names
                
                # Construct file path in a platform-indpendent manner
                FullPath <- file.path(directory, paste(fName, fExt, sep="."), fsep = .Platform$file.sep)
                #print(FullPath)
                Data <- read.csv(FullPath)
                # Get vector from Data Frame column and calculate it's mean

                d2<-Data[!is.na(Data["sulfate"]),]
                d3<-d2[!is.na(d2["nitrate"]),]
                nobs[i] <- nrow(d3)
        }
        
        data.frame(id,nobs)
}
