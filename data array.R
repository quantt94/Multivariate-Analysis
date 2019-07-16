setwd("~/Documents/Study Materials/M.Fin/705 - Econometrics II");

library(readxl);
library(zoo);
library(lmtest);
library(car);
library(sandwich);
library(timeSeries);
library(fBasics);
library(fUnitRoots);

SPX<- read_excel("~/Documents/Study Materials/M.Fin/705 - Econometrics II/index price.xlsx", col_types = c("date", "numeric"), skip = 1);
names(SPX) = c("time", "log_return");
head(SPX);


library(timeDate);

# A ???timeDate??? Sequence
tS <- timeSequence(as.Date("2009/12/2"), as.Date("2018/12/1"))

# Subset weekdays
tW <- tS[isWeekday(tS)]; 

# Calculate log return
SPX_log_return <- vector();
for (i in 2:dim(SPX)[1]) {
  SPX_log_return[i-1] <- (log(SPX$log_return[i]) - log(SPX$log_return[i-1]))
}

# Plot Log Return
plot(tW, SPX_log_return, type='l', main="S&P 500 DAILY LOG RETURN", col='blue', xlab = "Dates", ylab = "Return");

# Descriptive Statistics
basicStats(SPX_log_return);

# Test Normality of log return distribution
jarqueberaTest(SPX_log_return);

# Test unit root of log return
unitrootTest(SPX_log_return);

acf(SPX_log_return);
pacf(SPX_log_return*SPX_log_return)

