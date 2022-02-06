# 1. ZAŁADOWANIE ŚRODOWISKA --------------------------------------------------
rm(list=ls())
# load("//wawa-fs01a/data/Risk/DKR/Value at Risk/VaR_Environment_NOWE_20170531.RData")
library(RODBC)
library(quantmod)
#library(stats)
library(ggplot2)
library(scales)
library(beepr)
library(lubridate)
options(scipen=0)
load("S:\\Risk\\DKR\\Value at Risk\\VaR_Funkcje_NOWE_20210628.Rdata")
# 2. ZMIENNE STARTOWE --------------------------------------------------------

#dateVaR<-'2016-11-21'
dateVaR <- as.character(Sys.Date())
ConfidenceLevel=0.99   
zapis=1
# hour=as.character('00')
if (hour(Sys.time())<10){
  hour<-paste("0",hour(Sys.time()),sep="")
} else {
  hour<-as.character(hour(Sys.time()))
}

# dateVaR = "2017-04-19"
hour0 = 6
hour1 = 7

hourI = hour0
for (hourI in hour0:hour1){
    if (hourI<10){
      hour<-paste("0",hourI,sep="")
    } else {
      hour<-as.character(hourI)
    }
  
  # 3. POBRANIE PORTFELA HANDLOWEGO -----------------------------------------
  
  query<-paste("set nocount on exec RiskDB.risk.VaR_portfolio '",dateVaR,"',",1,",",0,",'",hour,"'",sep="")
  polaczenie<-odbcConnect("RiskDB")
  portfolio<-sqlQuery(polaczenie, query)
  odbcClose(polaczenie)
  portfolio$Data<-as.POSIXct(portfolio$Data)
  portfolio$symbol<-as.character(portfolio$symbol)
  portfolio[,6]<-portfolio[,6]/portfolio[,9]
  inst_del<-c()
  
  #  xlsx::write.xlsx(priceMatrix, file = paste("C:/Users/atarasinska/Desktop/priceMatrix_dzis_7.xls",sep=''))
  # library(xlsx)
  
  #   3.1. DODANIE WARTOŚCI NOMINALNEJ I ODPOWIEDNIE PRZELICZENIE WALUTOWE ---------
  
  FXCross<-portfolio$CurrencyValue/portfolio$EUR
  FaceValue<-portfolio$Pozycja*portfolio$Value*portfolio$Multiplier*FXCross
  PortfolioValue<-sum(FaceValue)
  Contribution<-(FaceValue/PortfolioValue)
  
  portfolio<-data.frame(portfolio,FXCross,FaceValue,Contribution)
  rm(FaceValue,Contribution,FXCross,query,polaczenie)
  
  # 4. VaR DN, VaR SH, CVaR ----------------------------------------------------
  VaR_df<-data.frame(DateStamp=as.Date(character()),Type=integer(),Value=double())
  
  value_at_risk1(portfolio,1,dateVaR,0,hour)
  value_at_risk1(portfolio,2,dateVaR,0,hour)
  
  value_at_risk1(portfolio,1,dateVaR,2,hour)
  value_at_risk1(portfolio,1,dateVaR,3,hour)
  value_at_risk1(portfolio,1,dateVaR,4,hour)
  value_at_risk1(portfolio,1,dateVaR,5,hour)
  
  value_at_risk1(portfolio,2,dateVaR,2,hour)
  value_at_risk1(portfolio,2,dateVaR,3,hour)
  value_at_risk1(portfolio,2,dateVaR,4,hour)
  value_at_risk1(portfolio,2,dateVaR,5,hour)
  
  VaR_df<-VaR_df[complete.cases(VaR_df),]
  
  # 5. Incremental VaR ------------------------------------------------------
  
  var<-as.numeric(VaR_df[1,3])
  
  val<-numeric(nrow(portfolio))
  
  returnMatrix<-apply(priceMatrix,2,Delt)
  returnMatrix<-returnMatrix[-1,]
  
  correlationMatrix<-cor(returnMatrix,method = "pearson")
  alpha<-qnorm(ConfidenceLevel)
  stdev<-apply(returnMatrix,2,sd)
  
  
  for (kp in 1:nrow(portfolio)){
    
    portfolio1<-portfolio[,c(-13,-14)]
    portfolio1[kp,5]<-portfolio1[kp,5]+1
    FaceValue<-portfolio1$Pozycja*portfolio1$Value*portfolio1$Multiplier*portfolio1$FXCross
    
    portfolio1<-data.frame(portfolio1,FaceValue)
    rm(FaceValue)
    
    VaR_DN_Instruments<-alpha*stdev*portfolio1$FaceValue
    VaR_DN<-as.numeric(sqrt(t(VaR_DN_Instruments)%*%correlationMatrix%*%VaR_DN_Instruments))
    
    val[kp]<-VaR_DN-var
    rm(portfolio1,VaR_DN,VaR_DN_Instruments)  
    
  }
  
  rm(stdev,correlationMatrix,returnMatrix,kp)
  #żeby ggplot nie sortował alfabetycznie
  inst<-portfolio$symbol
  inst<- factor(inst,levels=inst)
  VaR_Incremental<-data.frame(rep(dateVaR,nrow(portfolio)),inst,ticker=portfolio$TickerDict_ID,val)
  
  # 6. Marginal VaR ---------------------------------------------------------
  
  # var<-as.numeric(VaR_df[1,3])
  
  val<-c()
  
  for (kl in 1:nrow(portfolio)){
    
    portfolio1<-portfolio[,-14]
    portfolio1<-portfolio1[-kl,]
    priceMatrix1<-priceMatrix[,-(kl:kl)]
    PortfolioValue<-sum(portfolio1$FaceValue)
    
    ##### MACIERZ STÓP ZWROTU, KOWARIANCJI I KORELACJI
    returnMatrix<-apply(priceMatrix1,2,Delt)
    returnMatrix<-returnMatrix[-1,]
    correlationMatrix<-cor(returnMatrix,method = "pearson")
    
    stdev<-apply(returnMatrix,2,sd)
    VaR_DN_Instruments<-alpha*stdev*portfolio1$FaceValue
    VaR_DN<-as.numeric(sqrt(t(VaR_DN_Instruments)%*%correlationMatrix%*%VaR_DN_Instruments))
    
    val[kl]<-VaR_DN-var
    
    rm(VaR_DN,VaR_DN_Instruments,stdev,correlationMatrix,returnMatrix,portfolio1,priceMatrix1)
  }
  rm(alpha,priceMatrix,kl)
  
  #żeby ggplot nie sortował alfabetycznie
  VaR_Marginal<-data.frame(inst,val)
  rm(inst,val,var)
  
  # 7. Wgrywanie danych do HD -----------------------------------------------
  if (zapis==1){
    zap<-c()
    for (i in 1:nrow(VaR_df)){
      zap[i]<-paste("(convert(varchar(8),cast('",VaR_df[i,1],"' as date),112)+' ",hour,":00:00',",VaR_df[i,2],",",VaR_df[i,3],")",collapse="",sep="")
    }
    
    sq<-paste0("INSERT INTO RiskDB.VaR.xRisk(DateTime,Type,Value)
             VALUES ",paste(zap,collapse=","))
    rm(zap)
    polaczenie<-odbcConnect("RiskDB")
    sqlQuery(polaczenie,sq)
    odbcClose(polaczenie)
    rm(sq,polaczenie,i)
    
    zap<-c()
    for (i in 1:nrow(portfolio)){
      zap[i]<-paste("(convert(varchar(8),cast('",VaR_Incremental[i,1],"' as date),112)+' ",hour,":00:00',",VaR_Incremental[i,3],",",
                    VaR_Incremental[i,4],",",VaR_Marginal[i,2],")",collapse="",sep="")
    }
    
    sq<-paste0("INSERT INTO RiskDB.VaR.xRisk_instruments(DateTime,TickerDict_ID,Incremental,Marginal)
             VALUES ",paste(zap,collapse=","))
    polaczenie<-odbcConnect("RiskDB")
    sqlQuery(polaczenie,sq)
    odbcClose(polaczenie)
    rm(sq,polaczenie,i)
  }
  print(paste("hour: ", hour))
}
  
