library(PerformanceAnalytics)
library(FinancialInstrument)
library(pracma)
library(tseries)
library(TTR)
library(roll)
library(xts)
library(devtools)
library(pkgbuild)
library(blotter)
library(quantstrat)
library(boot)
library(PortfolioAnalytics)
library(Quandl)
library(DEoptim)
library(tidyquant)
library(dplyr)
library(caret)
library(corrplot)
library(forecast)
library(kernlab)
library(quantmod)
library(rugarch)
library(fBasics)
library(fOptions)



start.pf <- '2007-01-02'        # start portfolio
start.date <- '2007-01-03'
end.date <- '2016-12-30'
Sys.setenv(TZ = 'UTC')          # Ustawienia strefy czasowej
init.eq <- 10000                # Initial equity

# 2. Pobieranie danych

getSymbols(c('SPY', 'AAPL', '^IRX', 'VEU.AX', 'AUBAX'), src = "yahoo", from = start.date, to = end.date, )

SPY <- na.omit(SPY)
IRX <- na.omit(IRX)
VEU.AX <- na.omit(VEU.AX)
AUBAX <- na.omit(AUBAX)

barChart(SPY,theme=chartTheme("white"))
addRSI(n=14)
addSMA(n=5,col="darkblue")
addSMA(n=21,col="darkred")
legend("right",col=c("green","darkblue"),lty=1,legend=c("SPY","SMA(5)"),cex=0.6)



# 1. Ustawienia podstawowe

currency (primary_id = c('USD'))
stock(primary_id = 'SPY', currency = 'USD', multiplier = 1)
tr1.st <- 'Tr1St'			
rm.strat(tr1.st)
strategy(name = tr1.st, store = T)
summary(getStrategy(tr1.st))

initPortf(name = tr1.st, symbols = 'SPY', initDate = start.pf)
initAcct(name = tr1.st, portfolios = tr1.st, initDate = start.pf, initEq = init.eq)
initOrders(portfolio = tr1.st, initDate = start.pf)

# Buy Rule: Buy when Fast SMA > Slow SMA, 
# Sell Rule: Sell when Fast SMA < Slow SMA

# 5.1. Add Fast SMA and Slow SMA Indicators
add.indicator(strategy=tr1.st,name='SMA',arguments=list(x=quote(Cl(SPY)),n=14),label='FastSMA')
add.indicator(strategy=tr1.st,name='SMA',arguments=list(x=quote(Cl(SPY)),n=21),label='SlowSMA')

# 5.2. Add Buying and Selling Signals
add.signal(strategy=tr1.st,name='sigCrossover',arguments=list(columns=c('FastSMA','SlowSMA'),relationship='gt'),label='BuySignal')
add.signal(strategy=tr1.st,name='sigCrossover',arguments=list(columns=c('FastSMA','SlowSMA'),relationship='lt'),label='SellSignal')


# 5.3. Add Enter and Exit Rules
addPosLimit(portfolio=tr1.st,symbol='SPY',timestamp=start.date,maxpos=10)
add.rule(strategy=tr1.st,name='ruleSignal',arguments=list(sigcol='BuySignal',sigval=T,orderqty=10,osFUN=osMaxPos,ordertype='market',orderside='long'),type='enter',label='EnterRule',enabled=T)
add.rule(strategy=tr1.st,name='ruleSignal',arguments=list(sigcol='SellSignal',sigval=T,orderqty='all',ordertype='market',orderside='long',TxnFees=-6),type='exit',label='ExitRule',enabled=T)



# Stop-Loss and Trailing-Stop-Loss (enabled=F)  # opcjonalnie mo¿emy wybraæ stopa;
# add.rule(strategy=tr1.st,name='ruleSignal',arguments=list(sigcol='BuySignal',sigval=T,orderqty='all',ordertype='stoplimit',threshold=quote(0.05),tmult=T,orderside='long'),type='chain',label='StopLoss',parent='EnterRule',enabled=T)
# add.rule(strategy=tr1.st,name='ruleSignal',arguments=list(sigcol='BuySignal',sigval=T,orderqty='all',ordertype='stoptrailing',threshold=quote(0.05),tmult=T,orderside='long'),type='chain',label='TrailingStop',parent='EnterRule',enabled=T)



# 6. Strategy Application
applyStrategy(strategy=tr1.st,portfolios=tr1.st)
updatePortf(Portfolio=tr1.st)
updateAcct(name=tr1.st)
updateEndEq(Account=tr1.st)

# # 7. Strategy Reporting

# 7.1. Trading Statistics
tr1.dstats <- dailyStats(Portfolios=tr1.st)
tr1.tstats <- tradeStats(Portfolios=tr1.st)
tr1.ptstats <- perTradeStats(Portfolio=tr1.st)
View(t(tr1.dstats))
View(t(tr1.tstats))
View(tr1.ptstats)

# 7.2. Strategy Position and Equity Curve

# Position Chart
chart.theme <- chart_theme()
chart.theme$col$dn.col <- 'white'
chart.theme$col$dn.border <- 'lightgray'
chart.theme$col$up.border <- 'lightgray'
chart.Posn(Portfolio=tr1.st,Symbol='SPY',theme=chart.theme)

# Equity Curve Chart
tr1.at <- getAccount(Account=tr1.st)
tr1.eq <- tr1.at$summary$End.Eq
plot(tr1.eq ,main="Trend1 Strategy Equity Curve")

# 7.3. Performance Metrics Chart and Table
tr1.ret <- PortfReturns(Account=tr1.st)
colnames(tr1.ret) <- 'tr1.ret'
tr1.comp <- tr1.ret
charts.PerformanceSummary(tr1.comp)
table.AnnualizedReturns(tr1.comp)

# 8. Strategy Risk Management

# 8.1. Maximum Adverse Excursion and Maximum Favorable Excursion Charts
chart.ME(Portfolio=tr1.st,Symbol='SPY',type='MAE',scale='percent')
chart.ME(Portfolio=tr1.st,Symbol='SPY',type='MFE',scale='percent')




#### OPTYMALIZACJA STRATEGII

opt.tr1.st <- "OptTr1St"
rm.strat(opt.tr1.st)
strategy(name=opt.tr1.st,store=T)
summary(getStrategy(opt.tr1.st))

# 4.2. Portfolio, Account and Order Book Initialization
initPortf(name=opt.tr1.st,symbols='SPY',initDate=start.pf)
initAcct(name=opt.tr1.st,portfolios=opt.tr1.st,initDate=start.pf,initEq=init.eq)
initOrders(portfolio=opt.tr1.st,initDate=start.pf)

# 5. Strategy Definition

# 5.1. Add Fast SMA and Slow SMA Indicators
add.indicator(strategy=opt.tr1.st,name='SMA',arguments=list(x=quote(Cl(mktdata))),label='FastSMA')
add.indicator(strategy=opt.tr1.st,name='SMA',arguments=list(x=quote(Cl(mktdata))),label='SlowSMA')

# 5.2. Add Buying and Selling Signals
add.signal(strategy=opt.tr1.st,name='sigCrossover',arguments=list(columns=c('FastSMA','SlowSMA'),relationship='gt'),label='BuySignal')
add.signal(strategy=opt.tr1.st,name='sigCrossover',arguments=list(columns=c('FastSMA','SlowSMA'),relationship='lt'),label='SellSignal')

# 5.3. Add Enter and Exit Rules
addPosLimit(portfolio=opt.tr1.st,symbol='SPY',timestamp=start.date,maxpos=10)
add.rule(strategy=opt.tr1.st,name='ruleSignal',arguments=list(sigcol='BuySignal',sigval=T,orderqty=10,osFUN=osMaxPos,ordertype='market',orderside='long'),type='enter',label='EnterRule',enabled=T)
add.rule(strategy=opt.tr1.st,name='ruleSignal',arguments=list(sigcol='SellSignal',sigval=T,orderqty='all',ordertype='market',orderside='long',TxnFees=-6),type='exit',label='ExitRule',enabled=T)

# Stop-Loss and Trailing-Stop-Loss (enabled=F)
# add.rule(strategy=opt.tr1.st,name='ruleSignal',arguments=list(sigcol='BuySignal',sigval=T,orderqty='all',ordertype='stoplimit',threshold=quote(0.05),tmult=T,orderside='long'),type='chain',label='StopLoss',parent='EnterRule',enabled=F)
# add.rule(strategy=opt.tr1.st,name='ruleSignal',arguments=list(sigcol='BuySignal',sigval=T,orderqty='all',ordertype='stoptrailing',threshold=quote(0.05),tmult=T,orderside='long'),type='chain',label='TrailingStop',parent='EnterRule',enabled=F)

# 5.4. Add SMA Parameters Combinations
add.distribution(strategy=opt.tr1.st,paramset.label='OptTr1Par',component.type='indicator',component.label='FastSMA',variable=list(n=c(5,10,15)),label='nFastSMA')
add.distribution(strategy=opt.tr1.st,paramset.label='OptTr1Par',component.type='indicator',component.label='SlowSMA',variable=list(n=c(20,25,30)),label='nSlowSMA')
summary(getStrategy(opt.tr1.st))

# 6. Strategy Optimization
opt.tr1.res <- apply.paramset(strategy.st=opt.tr1.st,paramset.label='OptTr1Par',portfolio.st=opt.tr1.st,account.st=opt.tr1.st,nsamples=0,verbose=T)

# 7. Strategy Reporting

# 7.1. Trading Statistics
opt.tr1.tstats <- opt.tr1.res$tradeStats
View(t(opt.tr1.tstats))

# 7.2. Net Trading PL, Maximum Drawdown, Profit to Maximum Drawdown and Cumulative PL Charts
barplot(opt.tr1.tstats$Net.Trading.PL,names.arg=opt.tr1.tstats$Portfolio,main='Trend1 Optimization Net Trading PL',xlab='Portfolio',ylab='Net.Trading.PL')
barplot(opt.tr1.tstats$Max.Drawdown,names.arg=opt.tr1.tstats$Portfolio,main='Trend1 Optimization Maximum Drawdown',xlab='Portfolio',ylab='Max.Drawdown')
barplot(opt.tr1.tstats$Profit.To.Max.Draw,names.arg=opt.tr1.tstats$Portfolio,main='Trend1 Optimization Profit to Maximum Drawdown',xlab='Portfolio',ylab='Profit.To.Max.Draw')
plot(opt.tr1.res$cumPL,main='Trend1 Optimization Cumulative PL')


# 7.1. Optimization Trials Daily Returns

ovf.tr1.ret1 <- dailyReturn(init.eq+opt.tr1.res$OptTr1St.train.1$cumPL)
ovf.tr1.ret2 <- dailyReturn(init.eq+opt.tr1.res$OptTr1St.train.2$cumPL)
ovf.tr1.ret3 <- dailyReturn(init.eq+opt.tr1.res$OptTr1St.train.3$cumPL)
ovf.tr1.ret4 <- dailyReturn(init.eq+opt.tr1.res$OptTr1St.train.4$cumPL)


# 7.2. Optimization Trials Performance Summary

ovf.tr1.comp <- na.exclude(cbind(ovf.tr1.ret1,ovf.tr1.ret2,ovf.tr1.ret3,ovf.tr1.ret4))
colnames(ovf.tr1.comp) <- c('tr1.opt1','tr1.opt2','tr1.opt3','tr1.opt4')
table.AnnualizedReturns(ovf.tr1.comp)
charts.PerformanceSummary(ovf.tr1.comp)


# 8.1. Multiple Hypothesis Testing

# 8.1.1. Multiple Hypothesis Testing p-values
ovf.tr1.mret <- ovf.tr1.comp[,1:4]
ovf.tr1.mtstat <- apply(ovf.tr1.mret,2,mean)/(apply(ovf.tr1.mret,2,sd)/sqrt(nrow(ovf.tr1.comp))) 
ovf.tr1.mpval <- 2*pt(-abs(ovf.tr1.mtstat),df=nrow(ovf.tr1.comp)-1)



# 8.2. Time Series Bootstrap 

# 8.2.1. Population Mean Probability Distribution Simulation 
# Random Fixed Block Re-Sampling with Replacement
bmean.fun <- function(x,i) {mean(x[i])}
ovf.tr1.tsb <- tsboot(ovf.tr1.ret4,statistic=bmean.fun,R=100000,sim='fixed',l=100)
ovf.tr1.tsb.ci <- boot.ci(ovf.tr1.tsb,conf=0.95,type='perc')
hist(ovf.tr1.tsb$t,freq=F,col='gray')
abline(v=mean(ovf.tr1.ret4),col='blue')
abline(v=mean(ovf.tr1.tsb$t),col='red')
abline(v=0,col='orange')
abline(v=ovf.tr1.tsb.ci$percent[4],col='green')
abline(v=ovf.tr1.tsb.ci$percent[5],col='green')
legend('topright',col=c('blue','red','green'),lty=1,legend=c('mean(ovf.tr1.ret4)','mean(ovf.tr1.tsb$t)','ovf.tr1.tsb.ci'),cex=0.6)

# 8.2.2. Time Series Bootstrap p-value 
ovf.tr1.tsb.pval <- 2*min(mean(ovf.tr1.tsb$t<=0),mean(ovf.tr1.tsb$t>0))

# 8.2.3. Time Series Bootstrap p-value Multiple Testing Adjustment
ovf.tr1.tsb.pval.adj <- 1-(1-ovf.tr1.tsb.pval)^4



#### CROSS-VALIDATION

wfa.tr1.st <- 'WFATr1St'
rm.strat(wfa.tr1.st)
strategy(name=wfa.tr1.st,store=T)
summary(getStrategy(wfa.tr1.st))

# 4.2. Portfolio, Account and Order Book Initialization
initPortf(name=wfa.tr1.st,symbols='SPY',initDate=start.pf)
initAcct(name=wfa.tr1.st,portfolios=wfa.tr1.st,initDate=start.pf,initEq=init.eq)
initOrders(portfolio=wfa.tr1.st,initDate=start.pf)

# 5. Strategy Definition

# 5.1. Add Fast SMA and Slow SMA Indicators
add.indicator(strategy=wfa.tr1.st,name='SMA',arguments=list(x=quote(Cl(mktdata))),label='FastSMA')
add.indicator(strategy=wfa.tr1.st,name='SMA',arguments=list(x=quote(Cl(mktdata))),label='SlowSMA')

# 5.2. Add Buying and Selling Signals
add.signal(strategy=wfa.tr1.st,name='sigCrossover',arguments=list(columns=c('FastSMA','SlowSMA'),relationship='gt'),label='BuySignal')
add.signal(strategy=wfa.tr1.st,name='sigCrossover',arguments=list(columns=c('FastSMA','SlowSMA'),relationship='lt'),label='SellSignal')

# 5.3. Add Enter and Exit Rules
addPosLimit(portfolio=wfa.tr1.st,symbol='SPY',timestamp=start.date,maxpos=10)
add.rule(strategy=wfa.tr1.st,name='ruleSignal',arguments=list(sigcol='BuySignal',sigval=T,orderqty=10,osFUN=osMaxPos,ordertype='market',orderside='long'),type='enter',label='EnterRule',enabled=T)
add.rule(strategy=wfa.tr1.st,name='ruleSignal',arguments=list(sigcol='SellSignal',sigval=T,orderqty='all',ordertype='market',orderside='long',TxnFees=-6),type='exit',label="ExitRule",enabled=T)

# Stop-Loss and Trailing-Stop-Loss (enabled=F)
# add.rule(strategy=wfa.tr1.st,name='ruleSignal',arguments=list(sigcol='BuySignal',sigval=T,orderqty='all',ordertype='stoplimit',threshold=quote(0.05),tmult=T,orderside='long'),type='chain',label='StopLoss',parent='EnterRule',enabled=F)
# add.rule(strategy=wfa.tr1.st,name='ruleSignal',arguments=list(sigcol='BuySignal',sigval=T,orderqty='all',ordertype='stoptrailing',threshold=quote(0.05),tmult=T,orderside='long'),type='chain',label='TrailingStop',parent='EnterRule',enabled=F)

# 5.4. Add SMA Parameters Combinations
add.distribution(strategy=wfa.tr1.st,paramset.label='WFATr1Par',component.type='indicator',component.label='FastSMA',variable=list(n=c(5,15)),label='nFastSMA')
add.distribution(strategy=wfa.tr1.st,paramset.label='WFATr1Par',component.type='indicator',component.label='SlowSMA',variable=list(n=c(20,30)),label='nSlowSMA')
summary(getStrategy(wfa.tr1.st))

# 6. Strategy Walk Forward Analysis
wfa.tr1.start <- Sys.time()
wfa.tr1.res <- walk.forward(strategy.st=wfa.tr1.st,paramset.label='WFATr1Par',portfolio.st=wfa.tr1.st,account.st=wfa.tr1.st,period='months',k.training=48,k.testing=12,nsamples=0,anchored=F,audit.prefix='wfa.tr1',verbose=T)
wfa.tr1.end <- Sys.time()
wfa.tr1.end-wfa.tr1.start
updatePortf(Portfolio=wfa.tr1.st)
updateAcct(name=wfa.tr1.st)
updateEndEq(Account=wfa.tr1.st)

# 7. Strategy Reporting

# 7.1. Walk Forward Testing Parameters and Trading Statistics

# Walk Forward Testing Parameters
wfa.tr1.res$testing.parameters[c(3,1,2)]

# Trading Statistics
wfa.tr1.dstats <- dailyStats(Portfolios=wfa.tr1.st)
wfa.tr1.ptstats <- perTradeStats(Portfolio=wfa.tr1.st)
View(cbind(t(wfa.tr1.dstats),t(bh.dstats)))
View(wfa.tr1.ptstats)

# 7.2. Walk Forward, Position and Equity Curve Charts

# Walk Forward Chart
chart.forward(wfa.tr1.res)

# Position Chart
chart.theme <- chart_theme()
chart.theme$col$dn.col <- 'white'
chart.theme$col$dn.border <- 'lightgray'
chart.theme$col$up.border <- 'lightgray'
chart.Posn(Portfolio=wfa.tr1.st,Symbol='SPY',theme=chart.theme)

# Equity Curve Chart
wfa.tr1.at <- getAccount(Account=wfa.tr1.st)
wfa.tr1.eq <- wfa.tr1.at$summary$End.Eq
plot(wfa.tr1.eq,main="Trend1 Strategy WFA Equity Curve")

# 7.3. Performance Metrics Chart and Table
wfa.tr1.ret <- PortfReturns(Account=wfa.tr1.st)
colnames(wfa.tr1.ret) <- 'wfa.tr1.ret'
wfa.tr1.comp <- wfa.tr1.ret
charts.PerformanceSummary(wfa.tr1.comp)
table.AnnualizedReturns(wfa.tr1.comp)

# 8. Strategy Risk Management

# 8.1. Maximum Adverse Excursion and Maximum Favorable Excursion Charts
chart.ME(Portfolio=wfa.tr1.st,Symbol='SPY',type='MAE',scale='percent')
chart.ME(Portfolio=wfa.tr1.st,Symbol='SPY',type='MFE',scale='percent')




# 
# 
# 
# # Moving Average Trading Strategy 
# 
# SMA5 <- SMA(Cl(SPY), n = 5)
# SMA21 <- SMA(Cl(SPY), n = 21)
# 
# SMA5TR <- Lag(ifelse(Lag(Cl(SPY)) < Lag(SMA5) & Cl(SPY) > SMA5, 1, ifelse(Lag(Cl(SPY)) > Lag(SMA5) & Cl(SPY) < SMA5, -1, 0)))
# SMATR <- Lag(ifelse(Lag(SMA5) < Lag(SMA21) & SMA5 > SMA21, 1, ifelse(Lag(SMA5) > Lag(SMA21) & SMA5 < SMA21, -1, 0)))
# 
# SMA5POS <- ifelse(SMA5TR == 0, 0, 0)
# for(i in 1:length(Cl(SPY))){SMA5POS[i] <- ifelse(SMA5TR[i]==1,1,ifelse(SMA5TR[i]==-1,0,SMA5POS[i-1]))}
# SMA5POS[is.na(SMA5POS)] <- 0
# sma5poscomp <- cbind(Cl(SPY),SMA5,SMA5TR,SMA5POS)
# colnames(sma5poscomp) <-c("Close",'sma5',"sma5tr","sma5pos")
# # View(sma5poscomp)
# 
# SMAPOS <- ifelse(SMATR == 0, 0, 0)
# for(i in 1:length(Cl(SPY))) {SMAPOS[i] <- ifelse(SMATR[i] == 1, 1, ifelse(SMATR[i] == -1, 0, SMAPOS[i-1]))}
# SMAPOS[is.na(SMAPOS)] <- 0
# SMAPOSCOMP <- cbind(Cl(SPY), SMA5, SMA21, SMATR, SMAPOS)
# colnames(SMAPOSCOMP) <- c('Close', 'SMA5', 'SMA21', 'SMATR', 'SMAPOS')
# # View(SMAPOSCOMP)
# 
# ret <- dailyReturn(Cl(SPY),type="arithmetic")
# ret[1] <- 0
# BHStrat <- ret
# SMA5Strat <- ret*SMA5POS
# SMA5Stratc <- ifelse((SMA5TR == 1|SMA5TR == -1) & SMA5POS != Lag(SMA5POS), (ret - 0.005)*SMA5POS, ret*SMA5POS)
# SMA5COMP <- cbind(SMA5Strat, SMA5Stratc, BHStrat)
# colnames(SMA5COMP) <- c('SMA(5) ', 'SMA(5) TC', 'BH')
# # View(SMA5COMP)
# table.AnnualizedReturns(SMA5COMP)
# charts.PerformanceSummary(SMA5COMP)
# 
# SMAStrat <- ret * SMAPOS
# SMAStratc <- ifelse((SMATR == 1|SMATR == -1) & SMAPOS != Lag(SMAPOS), (ret - 0.01)*SMAPOS, ret*SMAPOS)
# SMACOMP <- cbind(SMAStrat, SMAStratc, BHStrat)
# colnames(SMACOMP) <- c('SMA(5,21) ', 'SMA(5,21) TC', 'BH')
# # View(SMACOMP)
# table.AnnualizedReturns(SMACOMP)
# charts.PerformanceSummary(SMACOMP)
# 
# # Bollinger Strategy
# 
# BB <- BBands(HLC(SPY), n = 20, sd = 2)
# BBTR <- Lag(ifelse(Lag(Cl(SPY)) < Lag(BB[ , 1]) & Cl(SPY) > BB[ , 1], 1, ifelse(Lag(Cl(SPY)) > Lag(BB[ , 3]) & Cl(SPY) < BB[ , 3], -1, 0)))
# BBTR[is.na(BBTR)] <- 0
# BBPOS <- ifelse(BBTR == 0, 0, 0)
# for(i in 1:length(Cl(SPY))) {BBPOS[i] <- ifelse(BBTR[i] == 1, 1, ifelse(BBTR[i] == -1, 0, BBPOS[i-1]))}
# BBPOS[is.na(BBPOS)] <- 0
# BBPOSCOMP <- cbind(Cl(SPY), BB[ , 1], BB[ , 3], BBTR, BBPOS)
# colnames(BBPOSCOMP) <- c('Close', 'Lower', 'Upper', 'BBTR', 'BBPOS')
# # View(BBPOSCOMP)
# 
# ret <- dailyReturn(Cl(SPY),type="arithmetic")
# ret[1] <- 0
# BBStrat <- ret * BBPOS
# BBStratc <- ifelse((BBTR == 1|BBTR == -1) & BBPOS != Lag(BBPOS), (ret - 0.01) * BBPOS, ret * BBPOS)
# BBCOMP <- cbind(BBStrat, BBStratc, BHStrat)
# colnames(BBCOMP) <- c('BB', 'BB TC', 'BH')
# # View(BBCOMP)
# table.AnnualizedReturns(BBCOMP)
# charts.PerformanceSummary(BBCOMP)
# 
# 
# # RSI Strategy
# 
# RSI <- RSI(Cl(SPY), n = 14)
# RSITR <- Lag(ifelse(Lag(RSI) < 30 & RSI > 30, 1, ifelse(Lag(RSI) > 70 & RSI < 70, -1, 0)))
# RSITR[is.na(RSITR)] <- 0
# RSIPOS <- ifelse(RSITR == 0, 0, 0)
# for(i in 1:length(Cl(SPY))) {RSIPOS[i] <- ifelse(RSITR[i] == 1, 1, ifelse(RSITR[i] == -1, 0, RSIPOS[i-1]))}
# RSIPOS[is.na(RSIPOS)] <- 0
# RSIPOSCOMP <- cbind(Cl(SPY), RSI, RSITR, RSIPOS)
# colnames(RSIPOSCOMP) <- c('Close', 'RSI', 'RSITR', 'RSIPOS')
# # View(RSIPOSCOMP)
# 
# ret <- dailyReturn(Cl(SPY), type = "arithmetic")
# ret[1] <- 0
# BHStrat <- ret
# RSIStrat <- ret * RSIPOS
# RSIStratc <- ifelse((RSITR == 1|RSITR == -1) & RSIPOS != Lag(RSIPOS), (ret - 0.01) * RSIPOS, ret * RSIPOS)
# RSICOMP <- cbind(RSIStrat, RSIStratc, BHStrat)
# colnames(RSICOMP) <- c('RSI(14)', 'RSI(14) TC', 'BH')
# View(RSICOMP)
# table.AnnualizedReturns(RSICOMP)
# charts.PerformanceSummary(RSICOMP)
# 
# 
# 
# # RSI & SMA Strategy
# 
# RSI <- RSI(Cl(SPY), n = 14)
# SMA5 <- SMA(Cl(SPY), n = 5)
# RSISMATR <- Lag(ifelse(Lag(Cl(SPY)) < Lag(SMA5) & Cl(SPY) > SMA5 & RSI < 30, 1, ifelse(Lag(Cl(SPY)) > Lag(SMA5) & Cl(SPY) < SMA5 & RSI > 70, -1, 0)))
# RSISMATR[is.na(RSISMATR)] <- 0
# RSISMAPOS <- ifelse(RSISMATR == 0, 0, 0)
# for(i in 1:length(Cl(SPY))) {RSISMAPOS[i] <- ifelse(RSISMATR[i] == 1, 1, ifelse(RSISMATR[i] == -1, 0, RSISMAPOS[i-1]))}
# RSISMAPOS[is.na(RSISMAPOS)] <- 0
# RSISMAPOSCOMP <- cbind(Cl(SPY), RSI, SMA5, RSISMATR, RSISMAPOS)
# colnames(RSISMAPOSCOMP) <- c('Close', 'RSI', 'SMA5', 'RSISMATR', 'RSISMAPOS')
# View(RSISMAPOSCOMP)
# 
# ret <- dailyReturn(Cl(SPY), type = "arithmetic")
# ret[1] <- 0 						
# BHStrat <- ret
# RSISMAStrat <- ret * RSISMAPOS
# RSISMAStratc <- ifelse((RSISMATR == 1|RSISMATR == -1) & RSISMAPOS != Lag(RSISMAPOS), (ret - 0.01) * RSISMAPOS, ret * RSISMAPOS)
# RSISMACOMP <- cbind(RSISMAStrat, RSISMAStratc, BHStrat)
# colnames(RSISMACOMP) <- c('RSI(14) + SMA(5)', 'RSI(14) + SMA(5) TC', 'BH')
# #View(RSISMACOMP)
# table.AnnualizedReturns(RSISMACOMP)
# charts.PerformanceSummary(RSISMACOMP)
# 
# plot(RSISMATR)



############## Analiza pojedynczych instrumentów

AAPLm <- monthlyReturn(AAPL, na.rm = TRUE)
table.AnnualizedReturns(AAPLm)
charts.PerformanceSummary(AAPLm)

SPYm <- monthlyReturn(SPY, na.rm = TRUE)
table.AnnualizedReturns(SPYm)
charts.PerformanceSummary(SPYm)

any(is.na(VEU.AX))

IRXm <- mean((IRX$IRX.Close/100)/12, na.rm = TRUE)

AUBAXm <- monthlyReturn(AUBAX, na.rm = TRUE)
table.AnnualizedReturns(AUBAXm)
charts.PerformanceSummary(AUBAXm)


VEU.AXm <- monthlyReturn(VEU.AX, na.rm = TRUE)
table.AnnualizedReturns(VEU.AXm)
charts.PerformanceSummary(VEU.AXm)



assetscomp <- cbind(SPYm,AAPLm)
table.AnnualizedReturns(assetscomp)
charts.PerformanceSummary(assetscomp)

mmean <- mean(AAPLm)

mskew <- skewness(AAPLm)
mkurt <- kurtosis(AAPLm)
mjb <- jarque.bera.test(AAPLm)
mvar <- VaR(AAPLm,p=0.99,method = 'historical')
cvar <- CVaR(AAPLm,p=0.99,method = 'historical')
cov <- cov(data.frame(AAPLm, SPYm))
corr <- cor(data.frame(AAPLm, SPYm))

SharpeRatio <- SharpeRatio(AAPLm, Rf = IRXm)
TreynorRatio <- TreynorRatio(Ra = AAPLm, Rb = SPYm, Rf = IRXm)
SortinoRatio <- SortinoRatio(R = AAPLm, MAR = IRXm)

# 3.	Optymalizacja portfela 

# Portfel naiwny (równe wagi)

mport <- cbind(SPYm, IRXm, AUBAXm, VEU.AXm)
mnaivew <- as.numeric(t(c(0.25, 0.25, 0.25, 0.25)))
names(mnaivew) <- c('S&P500', 'T-Bill ', 'World Bond', 'World Stock')
mnaive <- Return.portfolio(R = mport, weights = mnaivew, geometric = F, rebalance_on = 'months')
colnames(mnaive) <- 'mnaive'



mport1c <- portfolio.spec(assets = colnames(mport))

mport1c <- add.constraint(mport1c,type="weight_sum",min_sum=0.99,max_sum=1.01)
mport1c <- add.constraint(mport1c,type="long_only")


mport1c <- add.objective(mport1c,type="return",name="mean")


mportopt1 <- optimize.portfolio(R=mport["::2014-12-31"],portfolio=mport1c,optimize_method='DEoptim',search_size=20000,trace=T)
chart.Weights(mportopt1)

# Portfolio Backtesting (Monthly Rebalancing)
mport1 <- Return.portfolio(R=mport["2015-01-31::"],weights=extractWeights(mportopt1),geometric=F,rebalance_on="months")
colnames(mport1) <- "mport1"
mportcomp1 <- cbind(mnaive["2015-01-31::"],mport1)
table.AnnualizedReturns(mportcomp1)
charts.PerformanceSummary(mportcomp1)

mport2c <- portfolio.spec(assets = colnames(mport))
mport2c <- add.constraint(mport2c, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
mport2c <- add.constraint(mport2c, type = "long_only")
mport2c <- add.objective(mport2c, type = "risk", name = "StdDev")
mportopt2 <- optimize.portfolio(R = mport["::2014-12-31"], portfolio = mport2c, optimize_method = "DEoptim", search_size = 20000, trace = T)
chart.Weights(mportopt2)
mport2 <- Return.portfolio(R = mport["2015-01-31::"], weights = extractWeights(mportopt2), geometric = F, rebalance_on = "months")
colnames(mport2) <- "mport2"
mportcomp2 <- cbind(mnaive["2015-01-31::"], mport2)
table.AnnualizedReturns(mportcomp2)
charts.PerformanceSummary(mportcomp2)


mport3c <- portfolio.spec(assets = colnames(mport))
mport3c <- add.constraint(mport3c, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
mport3c <- add.constraint(mport3c, type = "long_only")
mport3c <- add.objective(mport3c, type = "return", name = "mean")
mport3c <- add.objective(mport3c, type = "risk", name = "StdDev")
mportopt3 <- optimize.portfolio(R = mport["::2014-12-31"], portfolio = mport3c, optimize_method = "DEoptim", search_size = 20000, trace = T)
chart.Weights(mportopt3)
chart.EfficientFrontier(mportopt3, match.col = "StdDev")
mport3 <- Return.portfolio(R = mport["2015-01-31::"], weights = extractWeights(mportopt3), geometric = F, rebalance_on = "months")
colnames(mport3) <- "mport3"
mportcomp3 <- cbind(mnaive["2015-01-31::"], mport3)
table.AnnualizedReturns(mportcomp3)
charts.PerformanceSummary(mportcomp3)


mport4c <- portfolio.spec(assets = colnames(mport))
mport4c <- add.constraint(mport3c, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
mport4c <- add.constraint(mport3c, type = "long_only")
mport4c <- add.objective(mport3c, type = "return", name = "mean")
mport4c <- add.objective(mport3c, type = "risk", name = "VaR", arguments = list(p = 0.99, method = "modified"))
mportopt4 <- optimize.portfolio(R = mport["::2014-12-31"], portfolio = mport4c, optimize_method = "DEoptim", search_size = 20000, trace = T)
chart.Weights(mportopt4)
chart.EfficientFrontier(mportopt3, match.col = "StdDev")
mport4 <- Return.portfolio(R = mport["2015-01-31::"], weights = extractWeights(mportopt4), geometric = F, rebalance_on = "months")
colnames(mport4) <- "mport4"
mportcomp4 <- cbind(mnaive["2015-01-31::"], mport4)
table.AnnualizedReturns(mportcomp4)
charts.PerformanceSummary(mportcomp4)


mportcomp = cbind(mnaive["2015-01-31::"], mport1, mport2, mport3, mport4)
table.AnnualizedReturns(mportcomp)
charts.PerformanceSummary(mportcomp)

