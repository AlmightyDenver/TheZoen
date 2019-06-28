library(xlsx)
setwd("C:\\Users\\TJ\\Documents\\Denver\\GitHub\\TheZoen\\Data\\CombinedData")
data <- read_excel("combine_seoul.xlsx")
head(data)
#View(data)
data <- data[, c(3,4,10:15)]

attach(data)
#fit <- lm(total.snow ~ total.temp, data=data)
#plot(fit)
#library(car)
#library(psych)
#pairs.panels(data)
#plot(total.snow ~ total.temp)
plot(total.snow ~Cause)
causes <- transform(data$Cause, cause = ifelse(Cause == "개인질환", "A",
                                              ifelse(Cause == "산악기타", "B",
                                              ifelse(Cause == "낙석낙빙", "C",
                                              ifelse(Cause == "실족추락", "D",
                                              ifelse(Cause == "암벽등반", "E",
                                              ifelse(Cause == "일반조난", "F",
                                              ifelse(Cause == "저체온증", "G",
                                              "H"
                                                ))))))))
data <- transform(data, snow = ifelse(total.snow == 0, 0, 99))
data <- transform(data, rain = ifelse(total.rain == 0, 0, 99))
View(data)
data$causes <- causes[,2]
head(data)                  
View(data)
plot(causes ~ total.snow, data = data)
colnames(data)

library(ggmosaic)
ggplot(data = data) + geom_mosaic(aes(weight=frequency(causes), x=product(snow), fill = causes))
ggplot(data = data) + geom_mosaic(aes(weight=frequency(causes), x=product(rain), fill = causes))
count(data %>% filter(total.rain != 0))
count(data %>% filter(total.rain == 0))
count(data %>% filter(total.snow != 0))
count(data %>% filter(total.snow == 0))
