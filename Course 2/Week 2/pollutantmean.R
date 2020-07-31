pollutantmean<-function(directory,pollutant,id=1:332){
  setwd(file.path(getwd(), directory)) ## setting the directory
  total = 0                            ## the sum of all observed values of pollutant (either sulfate or nitrate)
  observations = 0                     ## the total number of observed values of pollutant (either sulfate or nitrate)
  
  #Looping thru the directory's files specified in the 'id' argument 
  for (i in id)
  {
    
    
    ## Due to the format of the filename, i.e 001, 010  instead of 1, 10. I became aware that the following method works but not efficient, 
    ## but at the time of the completion of this assignment, it was the only way I knew how to do it.           
    if (i <10) { 
      data <- read.csv(paste("0","0", as.character(i), ".csv", sep=""),  ## for example, if 'id' =7, we get 007.csv
                       header = T, 
                       na.strings=c("NA","NaN", " "))
    }
    
    else if (i>=10 & i<100) { 
      data <- read.csv(paste("0", as.character(i), ".csv", sep=""),  ## for example, if 'id' = 17, we get 017.csv
                       header = T, 
                       na.strings=c("NA","NaN", " ") 
      )
    }
    
    
    
    else       { 
      data <- read.csv(paste(as.character(i), ".csv", sep=""),     ## Normal
                       header = T, 
                       na.strings=c("NA","NaN", " ") 
      )
    }
    
    ## getting rid of all the "NA" values and, consequently, all the non-complete ovservations (the ones with at least one NA in row)
    data = na.omit(data)    
    ##  cumulative addition of the complete observations
    observations = observations + nrow(data)
    ## depending the poluttant ( sulfate or nitrate), we aggregate the observed values
    if (pollutant == "sulfate") {total = total + sum(data$sulfate)}
    else {total = total + sum(data$nitrate)}
    
  }
  
  ## reset directory path
  setwd("..")
  ## returning the mean of the pollutant values
  return (total/observations)
  
}

