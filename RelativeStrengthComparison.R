library(quantmod)
library(tidyverse)
library(tidyr)
library(dplyr)
library(PerformanceAnalytics)

Symbole <- c('AAPL', 'NDX')
DataStart <- Sys.Date() - 365*20
DataStop <- Sys.Date()

getSymbols(Symbols = Symbole, src = 'yahoo', from = DataStart, to = DataStop, periodicity = 'daily', verbose = TRUE)

AAPL_D <- AAPL
AAPL_W <- to.weekly(AAPL)
AAPL_M <- to.monthly(AAPL)
AAPL_Q <- to.quarterly(AAPL)

colnames(AAPL_D) <- c('Open', 'High', 'Low',  'Close', 'Volume', 'Adj')
colnames(AAPL_W) <- c('Open', 'High', 'Low',  'Close', 'Volume', 'Adj')
colnames(AAPL_M) <- c('Open', 'High', 'Low',  'Close', 'Volume', 'Adj')
colnames(AAPL_Q) <- c('Open', 'High', 'Low',  'Close', 'Volume', 'Adj')

NDX_D <- NDX
NDX_W <- to.weekly(NDX)
NDX_M <- to.monthly(NDX)
NDX_Q <- to.quarterly(NDX)


AAPL_D$RSC_D10 <- lag.xts(ROC(SMA(AAPL_D$Close/NDX_D$NDX.Close, n = 10, type = 'continuous'), n = 1)*100)
AAPL_D$RSC_D3 <- lag.xts(ROC(SMA(AAPL_D$Close/NDX_D$NDX.Close, n = 3, type = 'continuous'), n = 1)*100)
AAPL_D$RSC_D1 <- lag.xts(ROC(SMA(AAPL_D$Close/NDX_D$NDX.Close, n = 1, type = 'continuous'), n = 1)*100)
AAPL_W$RSC_W10 <- lag.xts(ROC(SMA(AAPL_W$Close/NDX_W$NDX.Close, n = 10, type = 'continuous'), n = 1)*100)
AAPL_W$RSC_W3 <- lag.xts(ROC(SMA(AAPL_W$Close/NDX_W$NDX.Close, n = 3, type = 'continuous'), n = 1)*100)
AAPL_W$RSC_W1 <- lag.xts(ROC(SMA(AAPL_W$Close/NDX_W$NDX.Close, n = 1, type = 'continuous'), n = 1)*100)
AAPL_M$RSC_M10 <- lag.xts(ROC(SMA(AAPL_M$Close/NDX_M$NDX.Close, n = 10, type = 'continuous'), n = 1)*100)
AAPL_M$RSC_M3 <- lag.xts(ROC(SMA(AAPL_M$Close/NDX_M$NDX.Close, n = 3, type = 'continuous'), n = 1)*100)
AAPL_M$RSC_M1 <- lag.xts(ROC(SMA(AAPL_M$Close/NDX_M$NDX.Close, n = 1, type = 'continuous'), n = 1)*100)
AAPL_Q$RSC_Q10 <- lag.xts(ROC(SMA(AAPL_Q$Close/NDX_Q$NDX.Close, n = 10, type = 'continuous'), n = 1)*100)
AAPL_Q$RSC_Q3 <- lag.xts(ROC(SMA(AAPL_Q$Close/NDX_Q$NDX.Close, n = 3, type = 'continuous'), n = 1)*100)
AAPL_Q$RSC_Q1 <- lag.xts(ROC(SMA(AAPL_Q$Close/NDX_Q$NDX.Close, n = 1, type = 'continuous'), n = 1)*100)

AAPL_Total <- merge.xts(as.xts(AAPL_D[, c(1,7,8,9)]), AAPL_W$RSC_W10, AAPL_W$RSC_W3,AAPL_W$RSC_W1, AAPL_M$RSC_M10, AAPL_M$RSC_M3,AAPL_M$RSC_M1, AAPL_Q$RSC_Q10, AAPL_Q$RSC_Q3, AAPL_Q$RSC_Q1)
AAPL_Total <- na.locf(AAPL_Total) #fromLast = FALSE


AAPL_Total$SygD <- ifelse(AAPL_Total$RSC_D10 > 0 & lag.xts(AAPL_Total$RSC_D10) > 0 & ((AAPL_Total$RSC_D10 > lag.xts(AAPL_Total$RSC_D10) &  AAPL_Total$RSC_D3 > 0 & AAPL_Total$RSC_D1 > 0)|(AAPL_Total$RSC_D3 > 0 & lag.xts(AAPL_Total$RSC_D3) > 0) & ( ( AAPL_Total$RSC_D3 > 0 & AAPL_Total$RSC_D1 > 0) | (AAPL_Total$RSC_D1 > 0& AAPL_Total$RSC_D1 > lag.xts(AAPL_Total$RSC_D1)))), 1, 0)
AAPL_Total$SygW <- ifelse(AAPL_Total$RSC_W10 > 0 & lag.xts(AAPL_Total$RSC_W10) > 0 & ((AAPL_Total$RSC_W10 > lag.xts(AAPL_Total$RSC_W10) &  AAPL_Total$RSC_W3 > 0 & AAPL_Total$RSC_W1 > 0)|(AAPL_Total$RSC_W3 > 0 & lag.xts(AAPL_Total$RSC_W3) > 0) & ( ( AAPL_Total$RSC_W3 > 0 & AAPL_Total$RSC_W1 > 0) | (AAPL_Total$RSC_W1 > 0& AAPL_Total$RSC_W1 > lag.xts(AAPL_Total$RSC_W1)))), 1, 0)
AAPL_Total$SygM <- ifelse(AAPL_Total$RSC_M10 > 0 & lag.xts(AAPL_Total$RSC_M10) > 0 & ((AAPL_Total$RSC_M10 > lag.xts(AAPL_Total$RSC_M10) &  AAPL_Total$RSC_M3 > 0 & AAPL_Total$RSC_M1 > 0)|(AAPL_Total$RSC_M3 > 0 & lag.xts(AAPL_Total$RSC_M3) > 0) & ( ( AAPL_Total$RSC_M3 > 0 & AAPL_Total$RSC_M1 > 0) | (AAPL_Total$RSC_M1 > 0& AAPL_Total$RSC_M1 > lag.xts(AAPL_Total$RSC_M1)))), 1, 0)
AAPL_Total$SygQ <- ifelse(AAPL_Total$RSC_Q10 > 0 & lag.xts(AAPL_Total$RSC_Q10) > 0 & ((AAPL_Total$RSC_Q10 > lag.xts(AAPL_Total$RSC_Q10) &  AAPL_Total$RSC_Q3 > 0 & AAPL_Total$RSC_Q1 > 0)|(AAPL_Total$RSC_Q3 > 0 & lag.xts(AAPL_Total$RSC_Q3) > 0) & ( ( AAPL_Total$RSC_Q3 > 0 & AAPL_Total$RSC_Q1 > 0) | (AAPL_Total$RSC_Q1 > 0& AAPL_Total$RSC_Q1 > lag.xts(AAPL_Total$RSC_Q1)))), 1, 0)

AAPL_Total$Sygnal <- ifelse(AAPL_Total$SygW == 1 & AAPL_Total$SygM == 1 & AAPL_Total$SygQ == 1, 1, 0)

AAPL_Total$Return <- weeklyReturn(AAPL_D)

AAPL_Total$Wynik <- ifelse(AAPL_Total$Sygnal == 1, AAPL_Total$Return, 0)

AAPL_Total = na.omit(AAPL_Total)

Wynik <- sum(AAPL_Total$Wynik, na.rm = TRUE)

WynikNarastaj¹co <- cumsum(AAPL_Total$Wynik)

WynikBuyHold <- cumsum(AAPL_Total$Return)

plot(WynikBuyHold)
plot(WynikNarastaj¹co)

AverageDrawdown(WynikNarastaj¹co)
AverageDrawdown(WynikBuyHold)
maxDrawdown(WynikNarastaj¹co)
maxDrawdown(WynikBuyHold)

