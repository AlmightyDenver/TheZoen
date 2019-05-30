
library(plyr)
#make directory lists
dirs <- list.dirs(path = 'C:/Users/TJ/Desktop/weather/snow', full.names = TRUE, recursive = TRUE)
dirs
for (dir in dirs) {
    #make csv files list in dir
  setwd(dir)
  file.list <- list.files( pattern = "*.csv")
  file.list
    
    #Merge all csv files
  merge(A, B, by='key)
  #save merged dataframe as csv file 
  write.csv(df, file = "cbind.csv", row.names = FALSE)
  }

