library(dplyr)
library(fitdistrplus)
library(ggplot2)
library(fBasics)
library(fpp)
library(forecast)
data <- read.csv("Data/BOS_NYY_2009_2019.csv", stringsAsFactors = FALSE)
data$Game_Year <- paste(data$Rk, data$Year, sep = "_")
# Turn every number into 000
data$yearGame <- as.integer(paste(as.character(data$Year), ifelse(data$Rk < 10, paste("00", as.character(data$Rk), sep = ""),
       ifelse(data$Rk >= 10 & data$Rk < 100, paste("0", as.character(data$Rk), sep = ""),as.character(data$Rk))), sep = ""))

# Average 
avgData <- data %>% 
  group_by(yearGame, Year) %>% 
  summarise(mean_PerSK = mean(PerSK))
avgData2019 <- avgData %>% 
  dplyr::filter(Year == 2019)
avgIndex2019 = ts(avgData2019$mean_PerSK,start=c(2019,1), frequency = 162)
avgData2009_2018 <- avgData %>% 
  dplyr::filter(Year < 2019)
avgIndex2008_2018 = ts(avgData2009_2018$mean_PerSK,start=c(2009,1), frequency = 162)
# Split by Boston and New York 
# Boston Hold out 2019
Bos2019 <- data %>% 
  dplyr::filter(Team == "BOS" & Year == 2019)
BosIndex2019 = ts(Bos2019$PerSK,start=c(2019,1), frequency = 162)
# Boston 2009 to 2018
Bos2009_2018 <- data %>% 
  dplyr::filter(Team == "BOS" & Year < 2019)
BosIndex2009_2018 = ts(Bos2009_2018$PerSK,start=c(2009,1), frequency = 162)
# New York 2019
Nyy2019 <- data %>% 
  dplyr::filter(Team == "NYY" & Year == 2019)
NyyIndex2019 = ts(Nyy2019$PerSK,start=c(2019,1), frequency = 162)
# New York 2009 to 2018
Nyy2009_2018 <- data %>% 
  dplyr::filter(Team == "NYY" & Year < 2019)
NyyIndex2009_2018 = ts(Nyy2009_2018$PerSK,start=c(2009,1), frequency = 162)

# Check Distribution 

## Average 
descdist(as.numeric(avgIndex2009_2018), discrete = FALSE)
descdist(as.numeric(avgIndex2019), discrete = FALSE)
fBasics::normalTest(avgIndex2009_2018, method = 'jb')
basicStats(avgIndex2019)
basicStats(avgIndex2009_2018)
fBasics::normalTest(avgIndex2019, method = "jb")  

### Average Distributions 
plot(fitdist(as.numeric(avgIndex2009_2018), "norm"))
plot(fitdist(as.numeric(avgIndex2019), "norm"))

## Boston 
descdist(as.numeric(BosIndex2009_2018), discrete = FALSE)
descdist(as.numeric(BosIndex2019), discrete = FALSE)
fBasics::normalTest(BosIndex2009_2018, method = 'jb')
basicStats(BosIndex2019)
basicStats(BosIndex2009_2018)
fBasics::normalTest(BosIndex2019, method = "jb")  
basicStats(as.numeric(BosIndex2009_2018))
 
### Boston Distribution Plots
plot(fitdist(as.numeric(BosIndex2009_2018), "norm"))
plot(fitdist(as.numeric(BosIndex2019), "norm"))

## New York 
descdist(as.numeric(NyyIndex2009_2018), discrete = FALSE)
descdist(as.numeric(NyyIndex2019), discrete = FALSE)
fBasics::normalTest(NyyIndex2009_2018, method = 'jb')
basicStats(NyyIndex2019)
basicStats(NyyIndex2009_2018)
fBasics::normalTest(NyyIndex2019, method = "jb")  
basicStats(as.numeric(NyyIndex2009_2018))

### New York Distribution Plots
plot(fitdist(as.numeric(NyyIndex2009_2018), "norm"))
plot(fitdist(as.numeric(NyyIndex2019), "norm"))

# Time Plot 

## Average

## 2009-2018
autoplot(avgIndex2009_2018) + xlab("Year Game Index") +
  ylab("Percent of Pitches That Are Strike") + ggtitle("Boston Red Sox and New York Yankees Percenatge of Pitches That Are Strikes 2009-2018") +
  theme_classic()

## 2019
autoplot(avgIndex2019) + xlab("Year Game Index") +
  ylab("Percent of Pitches That Are Strike") + ggtitle("Boston Red Sox and New York Yankees Percenatge of Pitches That Are Strikes 2019") +
  theme_classic()

## Boston 
autoplot(BosIndex2009_2018) + xlab("Year Game Index") +
  ylab("Percent of Pitches That Are Strike") + ggtitle("Boston Red Sox Percenatge of Pitches That Are Strikes 2009-2018") +
  theme_classic()

autoplot(BosIndex2019) + xlab("Year Game Index") +
  ylab("Percent of Pitches That Are Strike") + ggtitle("Boston Red Sox Percenatge of Pitches That Are Strikes 2019") +
  theme_classic()

## New York
# NYY 2009-2018
autoplot(NyyIndex2009_2018) + xlab("Year Game Index") +
  ylab("Percent of Pitches That Are Strike") + ggtitle("New York Yankees Percenatge of Pitches That Are Strikes 2009-2018") +
  theme_classic()
# 2019
autoplot(NyyIndex2019) + xlab("Year Game Index") +
  ylab("Percent of Pitches That Are Strike") + ggtitle("New York Yankees Percenatge of Pitches That Are Strikes 2019") +
  theme_classic()

# Pull out 2014 to Look at Trends 
## Average 
avgData2014 <- avgData %>% 
  dplyr::filter(Year == 2014)
avgIndex2014 = ts(avgData2014$mean_PerSK,start=c(2014,1), frequency = 162)
autoplot(avgIndex2014) + xlab("Year Game Index") +
  ylab("Percent of Pitches That Are Strike") + ggtitle("Boston Red Sox and New York Yankees Percenatge of Pitches That Are Strikes 2014") +
  theme_classic()

## Boston 
Bos2014 <- data %>% 
  dplyr::filter(Team == "BOS" & Year == 2014)
BosIndex2014 = ts(Bos2019$PerSK,start=c(2014,1), frequency = 162)
autoplot(BosIndex2014) + xlab("Year Game Index") +
  ylab("Percent of Pitches That Are Strike") + ggtitle("Boston Red Sox Percenatge of Pitches That Are Strikes 2014") +
  theme_classic()
## NewYork
Nyy2014 <- data %>% 
  dplyr::filter(Team == "NYY" & Year == 2014)
NyyIndex2014 = ts(Nyy2014$PerSK,start=c(2014,1), frequency = 162)
autoplot(NyyIndex2014) + xlab("Year Game Index") +
  ylab("Percent of Pitches That Are Strike") + ggtitle("New York Yankees Percenatge of Pitches That Are Strikes 2014") +
  theme_classic()

# Naive
## Average 
averageModelNaive <- naive(avgIndex2009_2018, h = 162)
accuracy(averageModelNaive, avgIndex2019)

## Boston 
BosNaive <- naive(BosIndex2009_2018, h = 162)
accuracy(BosNaive, BosIndex2019)

## New York 
NyyNaive <- naive(NyyIndex2009_2018, h = 162)
accuracy(NyyNaive, NyyIndex2019)

#SES 
## Average 
averageModelSES <- ses(avgIndex2009_2018, h = 162)
accuracy(averageModelSES, avgIndex2019)

## Boston 
BosModelSES <- ses(BosIndex2009_2018, h = 162)
accuracy(BosModelSES, BosIndex2019)

## New York
NyyModelSES <- ses(NyyIndex2009_2018, h = 162)
accuracy(NyyModelSES, NyyIndex2019)

# ETS
## Average 
averageETS <- ets(avgData2009_2018$mean_PerSK)
summary(averageETS)
ETSForcast <- forecast(averageETS, h = 162)
accuracy(ETSForcast, avgData2019$mean_PerSK)

## Bos 
BosETS <- ets(Bos2009_2018$PerSK)
summary(BosETS)
BosETSForcast <- forecast(BosETS, h = 162)
accuracy(BosETSForcast, Bos2019$PerSK)

# Nyy
NyyETS <- ets(Nyy2009_2018$PerSK)
summary(NyyETS)
NyyETSForcast <- forecast(NyyETS, h = 162)
accuracy(NyyETSForcast, Nyy2019$PerSK)

# Arima

# ACF
## Average
adf.test(avgIndex2009_2018)
ggAcf(avgIndex2009_2018)
Pacf(avgIndex2009_2018)
tsdisplay(diff(avgIndex2009_2018))
ndiffs(avgIndex2009_2018)
nsdiffs(avgIndex2009_2018)

## Boston 
adf.test(BosIndex2009_2018)
ggAcf(BosIndex2009_2018)
Pacf(BosIndex2009_2018)
tsdisplay(diff(BosIndex2009_2018))
ndiffs(BosIndex2009_2018)
nsdiffs(BosIndex2009_2018)

## New York
adf.test(NyyIndex2009_2018)
ggAcf(NyyIndex2009_2018)
Pacf(NyyIndex2009_2018)
tsdisplay(diff(NyyIndex2009_2018))
ndiffs(NyyIndex2009_2018)
nsdiffs(NyyIndex2009_2018)

# Box Test 

## Average
Box.test(avgIndex2009_2018,lag=10,type='Ljung') 
Box.test(avgIndex2009_2018,lag=5,type='Ljung')
Box.test(avgIndex2009_2018,lag=20,type='Ljung')
Box.test(avgIndex2009_2018,lag=7,type='Ljung')
Box.test(avgIndex2009_2018,lag=162,type='Ljung')

## Boston 
Box.test(BosIndex2009_2018,lag=10,type='Ljung') 
Box.test(BosIndex2009_2018,lag=5,type='Ljung')
Box.test(BosIndex2009_2018,lag=20,type='Ljung')
Box.test(BosIndex2009_2018,lag=7,type='Ljung')
Box.test(BosIndex2009_2018,lag=162,type='Ljung')

## New York
Box.test(NyyIndex2009_2018,lag=10,type='Ljung') 
Box.test(NyyIndex2009_2018,lag=5,type='Ljung')
Box.test(NyyIndex2009_2018,lag=20,type='Ljung')
Box.test(NyyIndex2009_2018,lag=7,type='Ljung')
Box.test(NyyIndex2009_2018,lag=162,type='Ljung')

# t.test
## Average
t.test(avgIndex2009_2018)

## Boston 
t.test(BosIndex2009_2018)

## New York
t.test(NyyIndex2009_2018)

# Auto.arima

## Average
auto.arima(avgIndex2009_2018)

### Arima 0, 1, 1
avgArima0_1_1 <-  Arima(avgIndex2009_2018, order= c(0,1,1))
summary(avgArima0_1_1)

### Arima 0, 1, 2
avgArima0_1_2 <-  Arima(avgIndex2009_2018, order= c(0,1,2))
summary(avgArima0_1_2)

### Arima 0, 1, 3
avgArima0_1_3 <-  Arima(avgIndex2009_2018, order= c(0,1,3))
summary(avgArima0_1_3)

### Arima 0, 0, 2
avgArima0_0_2 <-  Arima(avgIndex2009_2018, order= c(0,0,2))
summary(avgArima0_0_2)

### Arima 0, 2, 3
avgArima0_2_3 <-  Arima(avgIndex2009_2018, order= c(0,2,3))
summary(avgArima0_2_3)

### Arima 1,1,1 
avgArima1_1_1 <-  Arima(avgIndex2009_2018, order= c(1,1,1))
summary(avgArima1_1_1)

### Arima 1,1,2
avgArima1_1_2 <-  Arima(avgIndex2009_2018, order= c(1,1,2))
summary(avgArima1_1_2)

### Arima 1,2,1
avgArima1_2_1 <-  Arima(avgIndex2009_2018, order= c(1,2,1))
summary(avgArima1_2_1)

## Boston 
auto.arima(BosIndex2009_2018)

### Arima 3, 1, 1
BosArima3_1_1 <-  Arima(BosIndex2009_2018, order= c(3,1,1))
summary(BosArima3_1_1)

### Arima 3, 0, 1
BosArima3_0_1 <-  Arima(BosIndex2009_2018, order= c(3,0,1))
summary(BosArima3_0_1)

### Arima 3, 1, 0
BosArima3_1_0 <-  Arima(BosIndex2009_2018, order= c(3,1, 0))
summary(BosArima3_1_0)

### Arima 3, 2, 2
BosArima3_2_2 <-  Arima(BosIndex2009_2018, order= c(3,2,2))
summary(BosArima3_2_2)

### Arima 3,2,1 
BosArima3_2_1 <-  Arima(BosIndex2009_2018, order= c(3,2,1))
summary(BosArima3_2_1)

### Arima 3,1,2 
BosArima3_1_2 <-  Arima(BosIndex2009_2018, order= c(3,1,2))
summary(BosArima3_1_2)

### Arima 3, 1, 1
BosArima3_1_1 <-  Arima(BosIndex2009_2018, order= c(3,1,1))
summary(BosArima3_1_1)

### Arima 3, 0, 1
BosArima3_0_1 <-  Arima(BosIndex2009_2018, order= c(3,0,1))
summary(BosArima3_0_1)

### Arima 3, 1, 0
BosArima3_1_0 <-  Arima(BosIndex2009_2018, order= c(3,1, 0))
summary(BosArima3_1_0)

### Arima 3, 2, 2
BosArima3_2_2 <-  Arima(BosIndex2009_2018, order= c(3,2,2))
summary(BosArima3_2_2)

### Arima 3,2,1 
BosArima3_2_1 <-  Arima(BosIndex2009_2018, order= c(3,2,1))
summary(BosArima3_2_1)

### Arima 3,1,2 
BosArima3_1_2 <-  Arima(BosIndex2009_2018, order= c(3,1,2))
summary(BosArima3_1_2)

### Arima 4, 1, 1
BosArima4_1_1 <-  Arima(BosIndex2009_2018, order= c(4,1,1))
summary(BosArima4_1_1)

### Arima 4, 0, 1
BosArima4_0_1 <-  Arima(BosIndex2009_2018, order= c(4,0,1))
summary(BosArima4_0_1)

### Arima 4, 1, 0
BosArima4_1_0 <-  Arima(BosIndex2009_2018, order= c(4,1, 0))
summary(BosArima4_1_0)

### Arima 4, 2, 2
BosArima4_2_2 <-  Arima(BosIndex2009_2018, order= c(4,2,2))
summary(BosArima4_2_2)

### Arima 4,2,1 
BosArima4_2_1 <-  Arima(BosIndex2009_2018, order= c(4,2,1))
summary(BosArima4_2_1)

### Arima 4,1,2 
BosArima4_1_2 <-  Arima(BosIndex2009_2018, order= c(4,1,2))
summary(BosArima4_1_2)

### Arima 5, 1, 1
BosArima5_1_1 <-  Arima(BosIndex2009_2018, order= c(5,1,1))
summary(BosArima5_1_1)

### Arima 5, 0, 1
BosArima5_0_1 <-  Arima(BosIndex2009_2018, order= c(5,0,1))
summary(BosArima5_0_1)

### Arima 5, 1, 0
BosArima5_1_0 <-  Arima(BosIndex2009_2018, order= c(5,1, 0))
summary(BosArima5_1_0)

### Arima 5, 2, 2
BosArima5_2_2 <-  Arima(BosIndex2009_2018, order= c(5,2,2))
summary(BosArima5_2_2)

### Arima 5,2,1 
BosArima5_2_1 <-  Arima(BosIndex2009_2018, order= c(5,2,1))
summary(BosArima5_2_1)

### Arima 5,1,2 
BosArima5_1_2 <-  Arima(BosIndex2009_2018, order= c(5,1,2))
summary(BosArima5_1_2)

## New York
auto.arima(NyyIndex2009_2018)

### Arima 0, 1, 1
NyyArima0_1_1 <-  Arima(NyyIndex2009_2018, order= c(0,1,1))
summary(NyyArima0_1_1)

### Arima 0, 1, 2
NyyArima0_1_2 <-  Arima(NyyIndex2009_2018, order= c(0,1,2))
summary(NyyArima0_1_2)

### Arima 0, 1, 3
NyyArima0_1_3 <-  Arima(NyyIndex2009_2018, order= c(0,1,3))
summary(NyyArima0_1_3)

### Arima 0, 0, 2
NyyArima0_0_2 <-  Arima(NyyIndex2009_2018, order= c(0,0,2))
summary(NyyArima0_0_2)

### Arima 0, 2, 3
NyyArima0_2_3 <-  Arima(NyyIndex2009_2018, order= c(0,2,3))
summary(NyyArima0_2_3)

### Arima 1,1,1 
NyyArima1_1_1 <-  Arima(NyyIndex2009_2018, order= c(1,1,1))
summary(NyyArima1_1_1)

### Arima 1,1,2
NyyArima1_1_2 <-  Arima(NyyIndex2009_2018, order= c(1,1,2))
summary(NyyArima1_1_2)

### Arima 1,2,1
NyyArima1_2_1 <-  Arima(NyyIndex2009_2018, order= c(1,2,1))
summary(NyyArima1_2_1)
# Auto.arima stepwise = FALSE
#auto.arima(avgIndex2008_2018, stepwise = FALSE, approximation =  FALSE)
