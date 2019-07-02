#install.packages("xlsx")
library(xlsx)

#set working dir
setwd("C:\\Users\\TJ\\Documents\\Denver\\GitHub\\TheZoen\\CombinedData")
xlsx_f <- "C:\\Users\\TJ\\Documents\\Denver\\GitHub\\TheZoen\\CombinedData\\gyeongbook_combine.xlsx"

#open xlsx file
data <- read.xlsx2(xlsx_f, sheetIndex = 1)

#save file as .csv
write.csv(data, file = "gyeongbook_combine.csv", row.names = FALSE)
