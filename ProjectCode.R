library(dplyr)
library(fitdistrplus)
library(ggplot2)
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
  filter(Year == 2019)
avgData2009_2018 <- avgData %>% 
  filter(Year < 2019)

# Split by Boston and New York 
# Boston Hold out 2019
Bos2019 <- data %>% 
  filter(Team == "BOS" & Year == 2019)
# Boston 2009 to 2018
Bos2009_2018 <- data %>% 
  filter(Team == "BOS" & Year < 2019)
# New York 2019
Nyy2019 <- data %>% 
  filter(Team == "NYY" & Year == 2019)
# New York 2009 to 2018
Nyy2009_2018 <- data %>% 
  filter(Team == "NYY" & Year < 2019)

# Check Distribution 

## Average 
descdist(avgData2009_2018$mean_PerSK, discrete = FALSE)
descdist(avgData2019$mean_PerSK, discrete = FALSE)

### Average Distributions 
plot(fitdist(avgData2009_2018$mean_PerSK, "norm"))
plot(fitdist(avgData2019$mean_PerSK, "norm"))

## Boston 
descdist(Bos2009_2018$PerSK, discrete = FALSE)
descdist(Bos2019, discrete = FALSE)

### Boston Distribution Plots
plot(fitdist(Bos2009_2018$PerSK, "norm"))
plot(fitdist(Bos2019$PerSK, "norm"))

## New York 
descdist(Nyy2009_2018$PerSK, discrete = FALSE)
descdist(Nyy2019$PerSK, discrete = FALSE)

### New York Yankees Distribution Plots
plot(fitdist(Nyy2009_2018$PerSK, "norm"))
plot(fitdist(Nyy2019$PerSK, "norm"))

# Time Plot 

## Average

## 2009-2018
ggplot(avgData2009_2018, aes(yearGame, mean_PerSK)) + xlab("Year Game Index") +
  ylab("Percent of Pitches That Are Strike") + ggtitle("Boston Red Sox and New York Yankees Percenatge of Pitches That Are Strikes 2009-2018") +
  geom_line() + theme_classic()

## 2019
ggplot(avgData2019, aes(yearGame, mean_PerSK)) + xlab("Year Game Index") +
  ylab("Percent of Pitches That Are Strike") + ggtitle("Boston Red Sox and New York Yankees Percenatge of Pitches That Are Strikes 2019") +
  geom_line() + theme_classic()

## Boston 
ggplot(Bos2009_2018, aes(yearGame, PerSK)) + xlab("Year Game Index") +
  ylab("Percent of Pitches That Are Strike") + ggtitle("Boston Red Sox Percenatge of Pitches That Are Strikes 2009-2018") +
  geom_line() + theme_classic()

ggplot(Bos2019, aes(yearGame, PerSK)) + xlab("Year Game Index") +
  ylab("Percent of Pitches That Are Strike") + ggtitle("Boston Red Sox Percenatge of Pitches That Are Strikes 2019") +
  geom_line() + theme_classic()

## New York
# NYY 2009-2018
ggplot(Nyy2009_2018, aes(yearGame, PerSK)) + xlab("Year Game Index") +
  ylab("Percent of Pitches That Are Strike") + ggtitle("New York Yankees Percenatge of Pitches That Are Strikes 2009-2018") +
  geom_line() + theme_classic()
# 2019
ggplot(Nyy2019, aes(yearGame, PerSK)) + xlab("Year Game Index") +
  ylab("Percent of Pitches That Are Strike") + ggtitle("New York Yankees Percenatge of Pitches That Are Strikes 2019") +
  geom_line() + theme_classic()







# ETS Plot

## Average 

## Boston 

## New York 

# Forecast for 2019
# Foreast by game
# compare to average from pervious year
# Forecast Overall percentage for whole season

# Arima

# ETS

# Maybe more advanced 

# Analysis of Risiduals

# Forecast Accuracy 

# Conclusion