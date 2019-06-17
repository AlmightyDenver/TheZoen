
library(plyr)
#make directory lists
dirs <- list.dirs(path = 'C:\\Users\\TJ\\Desktop\\da\\¼­¿ï', full.names = TRUE, recursive = TRUE)
dirs
for (dir in dirs) {
  #make csv files list in dir
  setwd(dir)
  file.list <- list.files(pattern = "*.csv")
  file.list
    
  #Merge all csv files
  dfs <- lapply(file.list, read.csv, header = TRUE)
  df <- do.call(rbind, dfs)
  #save merged dataframe as csv file 
  write.csv(df, file = "rbind.csv", row.names = FALSE)
}

