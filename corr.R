corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
        
        source("complete.R")
        myNobs <- complete(directory)
        
        retVal <- myNobs[myNobs["nobs"]>threshold,]
        retVal <- retVal[,"id"]
        
        fExt  <- "csv"     # Data File extension
        
        myCorrs = numeric(length(retVal))
        # Enumerate through all files
        for (i in seq_along(retVal)) {
                fName <- sprintf("%03d",retVal[i]) # add prepending zeroes to id to form file names
                
                # Construct file path in a platform-indpendent manner
                FullPath <- file.path(directory, paste(fName, fExt, sep="."), fsep = .Platform$file.sep)
                #print(FullPath)
                Data <- read.csv(FullPath)

                d2<-Data[!is.na(Data["sulfate"]),]
                d22<-d2[!is.na(d2["nitrate"]),]
#                myCorrs[i] = cor(d22[,"nitrate"], d22[,"sulfate"])
                myCorrs[i] = cor(d22["sulfate"], d22["nitrate"])
        }
        
        # Return value -  correlation
#        cor(d3[,"nitrate"], d3[,"sulfate"])       
        myCorrs
}
