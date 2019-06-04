install.packages("plyr")
library(plyr)

getwd()
#make directory lists
dirs <- list.dirs(path = 'C:\\Users\\TJ\\Desktop\\da\\¼­¿ï', full.names = TRUE, recursive = TRUE)
dirs
for (dir in dirs) {
  #make csv files list in dir
  setwd(dir)
  file.list <- list.files(pattern = "*.csv")
  file.list
  
}
setwd("C:\\Users\\TJ\\Desktop\\weather\\weather\\seoul")
loc = "C:\\Users\\TJ\\Desktop\\weather\\snow\\seoul"
setwd(loc)
file.list <- list.files(pattern = "*.csv")
file.list
length(file.list)
#date <- read.csv("C:\\Users\\TJ\\Desktop\\weather\\weather\\gyeongnam\\mt.csv")
#files <- data.frame(date$date)
dir_file <- paste(loc, file.list[1], sep = '\\')
f2 <- read.csv("C:\\Users\\TJ\\Desktop\\weather\\snow\\seoul\\total2.csv")
f1 <- read.csv("C:\\Users\\TJ\\Desktop\\weather\\snow\\seoul\\total3.csv")

total <- merge.data.frame(x = f1, y = f2, z = f1, by = 'date', all = TRUE)
head(total)
write.csv(total, file = "totallala.csv", row.names = FALSE)





setwd("C:/Users/TJ/Desktop/weather/weather/gyeongbook")
getwd()
file.list <- list.files(pattern = "*.csv")
file.list
f1 <- read.csv('mt1.csv')
f2 <- read.csv('mts.csv')

total <- merge(f1, f2, by="date")
str(total)
summary(total)
head(total)
head(f1)
head(f2)
x <- data.table::(total$temp.x, total$temp.y)
total$temp <- mean(x)
head(total$temp)
mean(x = total$temp.x, )

total$wind <- ((total$wind.x+total$wind.y)/2, na.exclude(total$temp.x))
total$rain <- (total$rain.x+total$rain.y)/2
head(total$rain)
head(total$rain.x)
head(total$rain.y)
str(total)
summary(total)
na.action(na.exclude())

write.csv(total, file = "total.csv", row.names = FALSE)

