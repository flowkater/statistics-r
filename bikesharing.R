

setwd("~/oldwork/statistics-R/")


bikeSharing <- read.csv("./data/BikeSharing.csv", header = T, stringsAsFactors = F)
View(bikeSharing)
head(bikeSharing)
str(bikeSharing)
summary(bikeSharing)

bikeSharing$datetime

as.Date(bikeSharing$datetime)
strptime(bikeSharing$datetime, "%Y-%m-%d %H:%M:%S")


datet <- strptime("2011-01-08 07:00:00", "%Y-%m-%d %H:%M:%S")

date$year + 1900
# 1900년을 0으로 시작하는 값이라서 + 1900 해주면 된다.

date$mon + 1
# 1월은 0으로 시작하는 값이라 + 1 해주면 된다.

date$mday

datet$hour
datet$min
date$sec

datet$wday
# 일요일을 0으로 시작하는 값


b <- as.Date("2018-2-25")

months(b)
weekdays(b)

hist(bikeSharing$temp)
hist(bikeSharing$atemp)
hist(bikeSharing$humidity)
hist(bikeSharing$windspeed)

hist(bikeSharing$casual)
hist(bikeSharing$registered)

View(bikeSharing)

library(MASS)

model <- lm(log(count)~season+holiday+workingday+temp+atemp+humidity+windspeed, data = bikeSharing)
summary(model)

modelAIC <- stepAIC(model, direction = "both")


results <- predict(modelAIC, newdata=test)


test <- read.csv("./bike/test.csv", header = T, stringsAsFactors = F)

results <- predict(model, newdata = subset(test, select = c('temp', 'atemp', 'humidity', 'windspeed')), type = 'response')
results


test$count <- 0
test$count <- exp(results)

test$count[test$count < 0] <- 0
test$count[test$count < 0]

submission <- test[, c('datetime', 'count')]
View(submission)

write.csv(submission, paste("./bike/submission", Sys.time(),  ".csv", sep = ""), row.names = F)

