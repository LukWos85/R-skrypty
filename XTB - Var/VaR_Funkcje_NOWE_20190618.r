rm(list=ls())
library(RODBC)
library(quantmod)
library(stats)
library(data.table)
library(reshape2)
library(caret)

options("scipen"=100, "digits"=9) 
maxabs = function(x){max(abs(x))}

# INCREMENTAL

incremental<-function(kp){
  portfolio[kp,5]<-portfolio[kp,5]+1
  FaceValue<-portfolio$Pozycja*portfolio$Value*portfolio$Multiplier*portfolio$FXCross
  
  VaR_DN_Instruments<-alpha*stdev*FaceValue
  VaR_DN<-as.numeric(sqrt(t(VaR_DN_Instruments)%*%correlationMatrix%*%VaR_DN_Instruments))
  
  val<-VaR_DN-var
  return(val)
}

# MARGINAL

marginal<-function(kl){ 
  
  returnMatrix<-apply(priceMatrix[,-kl],2,Delt)
  returnMatrix<-returnMatrix[-1,]
  correlationMatrix<-cor(returnMatrix,method = "pearson")
  
  stdev<-apply(returnMatrix,2,sd)
  VaR_DN_Instruments<-alpha*stdev*portfolio[-kl,]$FaceValue
  VaR_DN<-as.numeric(sqrt(t(VaR_DN_Instruments)%*%correlationMatrix%*%VaR_DN_Instruments))
  
  val<-VaR_DN-var
}

# FUNKCJA DO MONTE CARLO
var_monte_carlo<-function(portfel,priceMatrix,PortfolioValue,cholesky,mean,stdev){
  losowe<-runif(nrow(portfel))
  normalne<-qnorm(losowe)
  zmienne<-t(as.matrix(normalne))%*%cholesky
  
  ceny<-sapply(1:nrow(portfel),function(x) priceMatrix[250,x]*exp(  (mean[x]-((stdev[x]^2)/2)) + stdev[x]*zmienne[x]  ))
  
  portfolio1<-portfel
  FaceValue1<-portfolio1$Pozycja*portfolio1$Multiplier*ceny
  PortfolioValue1<-sum(FaceValue1)
  roznica<-PortfolioValue1-PortfolioValue
  return(roznica)
}


# FUNKCJA WYLICZAJÄ„CA VaR ------------------------------------------------
value_at_risk<-function(portfel,priceMatrix,method,date,class,duration=1,significance=1,n_sim=10000){
  
  ConfidenceLevel=1-(significance*0.01)
  portfel<-as.data.table(portfel)
  
  klasa<-list("akcje",c("gold","towar"),c("indeks","indeks_uznany"),"obligacje","waluta")
  
  if (method==1){
    
    if(class==0){
      
      returnMatrix<-apply(priceMatrix,2,Delt)
      returnMatrix<-returnMatrix[-1,]
      
      correlationMatrix<-cor(returnMatrix,method = "pearson")
      alpha<-qnorm(ConfidenceLevel)
      stdev<-apply(returnMatrix,2,sd)
      
      VaR_DN_Instruments<-alpha*stdev*portfel$FaceValue
      VaR<-as.numeric(sqrt(t(VaR_DN_Instruments)%*%correlationMatrix%*%VaR_DN_Instruments)*sqrt(duration))
      rm(ConfidenceLevel)
      VaR_df[1,]<<-c(dateVaR,1,VaR)

    } else {
      portfolio<-portfel[RiskClassName %in% klasa[[class]]]
      PriceMatrix<-priceMatrix[,which(portfel$RiskClassName %in% klasa[[class]])]
            
      # macierz stÃ³p zwrotu, kowariancji
      returnMatrix<-apply(PriceMatrix,2,Delt)
      returnMatrix<-returnMatrix[-1,]
      
      correlationMatrix<-cor(returnMatrix,method = "pearson")
      alpha<-qnorm(ConfidenceLevel)
      stdev<-apply(returnMatrix,2,sd)
      
      VaR_DN_Instruments<-alpha*stdev*portfolio$FaceValue
      VaR<-as.numeric(sqrt(t(VaR_DN_Instruments)%*%correlationMatrix%*%VaR_DN_Instruments)*sqrt(duration))
      rm(ConfidenceLevel)
      VaR_df[class+1,]<<-c(dateVaR,class+1,VaR)
    } 
    
  } else if(method==2){          
    
    if(class==0){  
      prz<-portfel$Pozycja*portfel$Multiplier
      val_list<-apply(priceMatrix,1,function(x) sum(x*prz))
      
      roznice<-diff(val_list)
      
      VaR<-as.numeric(quantile(roznice,significance*0.01,na.rm=T)*sqrt(duration))
      CVaR<-mean(abs(roznice[roznice<VaR]))
      
      VaR_df[7,]<<-c(dateVaR,7,abs(VaR))
      VaR_df[13,]<<-c(dateVaR,13,CVaR)

    } else {
      
      portfolio<-portfel[RiskClassName %in% klasa[[class]]]
      PriceMatrix<-priceMatrix[,which(portfel$RiskClassName %in% klasa[[class]])]
          
      prz<-portfolio$Pozycja*portfolio$Multiplier
      val_list<-apply(PriceMatrix,1,function(x) sum(x*prz))
      
      roznice<-diff(val_list)
      
      VaR<-as.numeric(quantile(roznice,significance*0.01,na.rm=T)*sqrt(duration))
      CVaR<-mean(abs(roznice[roznice<VaR]))
      
      VaR_df[class+7,]<<-c(dateVaR,class+7,abs(VaR))
      VaR_df[class+13,]<<-c(dateVaR,class+13,CVaR)
      
    } 
  } else if (method==3){
    if(class==0){
      portfolio_value<-sum(portfel$FaceValue)    
      returnMatrix<-apply(priceMatrix,2,Delt)
      returnMatrix<-returnMatrix[-1,]
      
      correlationMatrix<-cor(returnMatrix,method = "pearson")
      
      ## zmiana 20190402 KB - skondensowanie skorelowanych instrumentów do 1
        skorelowane <<- findCorrelation(correlationMatrix, cutoff = 0.999999, verbose=FALSE, names=TRUE)  # które kolumny skorelowane
        NieZeroweZmiany = which(apply(returnMatrix[,which((colnames(returnMatrix) %in% skorelowane))],2, maxabs)>0.01)
        if (length(NieZeroweZmiany)>0){
          skorelowane = skorelowane[-NieZeroweZmiany]
        }
        
        if (length(skorelowane)>1){
          skorelowane <<- skorelowane[2:length(skorelowane)]
          print(paste("Usunieto ",length(skorelowane)," instrumentów skorelowanych w 99.9%"))
          returnMatrix = returnMatrix[,which(!(colnames(returnMatrix) %in% skorelowane))] # usuniecie skorelowanych kolumn - var_monte_carlo zwraca ró¿nice, wiêc nie ma wp³ywu na wynik
      
          correlationMatrix<-cor(returnMatrix,method = "pearson")  # przeliczenie korelacji po usunieciu skorelowanych kolumn
  
          portfel = portfel[which(!(portfel$symbol %in% skorelowane)),]  # usuwamy rekordy, ale nie zmieniamy portfolio_value
          priceMatrix = priceMatrix[,which(!(colnames(priceMatrix) %in% skorelowane))]   
          
        }
      ## koniec zmiany 20190402 KB
      
      # Dekompozycja Choleskiego
      cholesky<-chol(correlationMatrix, pivot=TRUE)
      
      mean<-apply(returnMatrix,2,mean)
      stdev<-apply(returnMatrix,2,sd)
      
      roznice<-c()
      roznice<-replicate(n_sim,var_monte_carlo(portfel,priceMatrix,portfolio_value,cholesky,mean,stdev))
      
      VaR<-as.numeric(quantile(roznice,significance*0.01,na.rm=T)*sqrt(duration))
      CVaR<-mean(abs(roznice[roznice<VaR]))
      
      VaR_df[19,]<<-c(dateVaR,19,abs(VaR))
      VaR_df[25,]<<-c(dateVaR,25,CVaR)
      
    } else {
      
      portfolio<-portfel[RiskClassName %in% klasa[[class]]]
      PriceMatrix<-priceMatrix[,which(portfel$RiskClassName %in% klasa[[class]])]
      portfolio_value<-sum(portfolio$FaceValue)
      
      returnMatrix<-apply(PriceMatrix,2,Delt)
      returnMatrix<-returnMatrix[-1,]
      
      correlationMatrix<-cor(returnMatrix,method = "pearson")
      
      ## zmiana 20190402 KB - skondensowanie skorelowanych instrumentów do 1
      skorelowane <<- findCorrelation(correlationMatrix, cutoff = 0.999999, verbose=FALSE, names=TRUE)  # które kolumny skorelowane
      NieZeroweZmiany = which(apply(returnMatrix[,which((colnames(returnMatrix) %in% skorelowane))],2, maxabs)>0.01)
      if (length(NieZeroweZmiany)>0){
        skorelowane = skorelowane[-NieZeroweZmiany]
      }
      if (length(skorelowane)>1){
        skorelowane <<- skorelowane[2:length(skorelowane)]  
        print(paste("Usunieto ",length(skorelowane)," instrumentów skorelowanych w 99.9%"))
        returnMatrix = returnMatrix[,which(!(colnames(returnMatrix) %in% skorelowane))] # usuniecie skorelowanych kolumn - var_monte_carlo zwraca ró¿nice, wiêc nie ma wp³ywu na wynik
        
        correlationMatrix<-cor(returnMatrix,method = "pearson")  # przeliczenie korelacji po usunieciu skorelowanych kolumn
        
        portfel = portfel[which(!(portfel$symbol %in% skorelowane)),]  # usuwamy rekordy, ale nie zmieniamy portfolio_value
        PriceMatrix = PriceMatrix[,which(!(colnames(PriceMatrix) %in% skorelowane))]   
      }
      ## koniec zmiany 20190402 KB
      
      # Dekompozycja Choleskiego
      cholesky<-chol(correlationMatrix, pivot=TRUE)    
      
      mean<-apply(returnMatrix,2,mean)
      stdev<-apply(returnMatrix,2,sd)
      
      roznice<-c()
      roznice<-replicate(n_sim,var_monte_carlo(portfolio,PriceMatrix,portfolio_value,cholesky,mean,stdev)) 
      
      VaR<-as.numeric(quantile(roznice,significance*0.01,na.rm=T)*sqrt(duration))
      CVaR<-mean(abs(roznice[roznice<VaR]))
      
      VaR_df[class+19,]<<-c(dateVaR,class+19,abs(VaR))
      VaR_df[class+25,]<<-c(dateVaR,class+25,CVaR)
           
    } 
  }
}
# FUNKCJA WYZNACZAJÄ„CA ODPOWIEDNIÄ„ DATÄ˜ STARTOWÄ„ --------------------------
StartDay<-function(dateVaR,days){
  date1<-as.Date(dateVaR)
  date2<-date1-1
  while(sum(!weekdays(seq(date2, date1, "days")) %in% c("sobota", "niedziela"))<days){
    date2<-date2-1
  }
  start_dateVaR<<-date2
  return(start_dateVaR)
}

# FUNKCJA SPRAWDZAJÄ„CA NIETYPOWE ZACHOWANIA CEN ---------------------------
PriceDifference<-function(threshold){
  df1<-apply(priceMatrix[,2:ncol(priceMatrix)],2,Delt)
  diff1<-df1[nrow(df1),]
  diff1<-diff1[complete.cases(diff1)]
  diff1<-diff1[abs(diff1)>(threshold*0.01)]
  (diff1*100)
}

# FUNKCJA SPRAWDZAJÄ„CA ZMIANE PORTFELA HANDLOWEGO -------------------------
CompositionDifference<-function(dateVaR,consolidated=0){
  dateVaR<-as.Date(dateVaR)
  dateVaR1<-dateVaR-1
  query<-paste("set nocount on exec RiskDB.risk.VaR_portfolio @Date='",dateVaR1,"', @consolidated = ",consolidated,sep="")
  
  polaczenie<-odbcConnect("RiskDB")
  portfolio1<-sqlQuery(polaczenie, query)
  odbcClose(polaczenie)
  
  if(nrow(portfolio) < nrow(portfolio1)){
    symb_roznica<<-setdiff(portfolio1$symbol,portfolio$symbol)
    
    roznica<-nrow(portfolio1)-nrow(portfolio)
    paste("Nast?pi?o zmniejszenie sk?adu portfela handlowego o ",roznica
          ,ifelse(length(symb_roznica)>1," instrumenty ("," instrument ("),paste(symb_roznica, collapse = ' '),")",sep="")
  } else if (nrow(portfolio)==nrow(portfolio1)){
    paste("Sk?ad portfela handlowego nie uleg? zmianie")
  } else if (nrow(portfolio) > nrow(portfolio1)){
    symb_roznica<-setdiff(portfolio$symbol,portfolio1$symbol)
    
    roznica<-nrow(portfolio)-nrow(portfolio1)
    paste("Nast?pi?o zwi?kszenie sk?adu portfela handlowego o ",roznica
          ,ifelse(length(symb_roznica)>1," instrumenty ("," instrument ("),paste(symb_roznica, collapse = ' '),")",sep="")
  }
  
}  

# FUNKCJA USUWAJÄ„CA DANE INSTRUMENTY -------------------------------------------
RemoveInstruments<-function(instruments){
  v<-which(portfolio$symbol==c(instruments))
  portfolio<<-portfolio[-v,]
  z<-c()
  for(i in seq_along(instruments)){
    z[i]<-which(colnames(priceMatrix)==instruments[i])
  }
  priceMatrix<<-priceMatrix[,-z]
}

# FUNKCJA LICZÄ„CA P&L -----------------------------------------------------
ProfitLoss<-function(date,consolidated=0){
  query<-paste("set nocount on exec RiskDB.risk.VaR_portfolio @Date='",date,"' , @method=0, @consolidated = ",consolidated,sep="")
  query1<-paste("set nocount on exec RiskDB.risk.VaR_PortfolioBacktest @Date='",date,"', @consolidated = ",consolidated,sep="")
  polaczenie<-odbcConnect("RiskDB")
  portfolio<-sqlQuery(polaczenie, query)
  portfolio1<-sqlQuery(polaczenie, query1)
  
  
  query<-paste("set nocount on exec RiskDB.risk.VaR_PriceErrors @Date='",date,"',@consolidated = ",consolidated,sep="")
  query1<-paste("set nocount on exec RiskDB.risk.VaR_PriceLack @Date='",date,"',@consolidated = ",consolidated,sep="")
  PriceErrors<-sqlQuery(polaczenie,query)
  PriceLack<-sqlQuery(polaczenie,query1)
  odbcClose(polaczenie)
  
  inst_del<-which(portfolio$symbol %in% as.character(PriceLack[,1]))
  if (length(inst_del)>0){
    portfolio<-portfolio[-inst_del,]
  }
  inst_del<-which(portfolio1$symbol %in% as.character(PriceLack[,1]))
  if (length(inst_del)>0){ 
    portfolio1<-portfolio1[-inst_del,]
  }
  inst_del<-which(portfolio$symbol %in% as.character(PriceErrors[,1]))
  if (length(inst_del)>0){
    portfolio<-portfolio[-inst_del,]
  }
  inst_del<-which(portfolio1$symbol %in% as.character(PriceErrors[,1]))
  if (length(inst_del)>0){
    portfolio1<-portfolio1[-inst_del,]
  }
  
  # ###======================================================
  # ### Dodatnie kryptowalut do portfolio
  # ###======================================================
  # 
  # query_port_cr <- paste("select
  #                        groupsymbol as 'Symbol'
  #                        , TickerDict = 0
  #                        , DateStamp as 'Data'
  #                        , RiskClassName
  #                        ,sum((case when [Buy/Sell]='Sell' then -XTB_Lots else XTB_Lots end )) as Pozycja
  #                        , ClosePrice
  #                        , CurrencyQuote
  #                        from
  #                        riskdb.wymogi.PortfelHandlowy_final
  #                        where
  #                        groupsymbol in ('DASH', 'ETHEREUM', 'LITECOIN', 'RIPPLE','BITCOIN')
  #                        and	DateStamp = '",date,"'
  #                        group by
  #                        groupsymbol, DateStamp, RiskClassName, ClosePrice, CurrencyQuote")
  # 
  # port_CR <- sqlQuery(ch, query_port_cr)
  # CV_cr <- portfolio$CurrencyValue[portfolio$symbol == 'EURUSD']
  # cr_mul <- 1
  # cr_div <- 1
  # EUR_cr <- unique(portfolio$EUR)
  # 
  # port_CR <- cbind(port_CR, CV_cr, cr_mul, cr_div, EUR_cr)
  # port_CR <- port_CR[,c(1,2,3,4,5,6,9,7,10,8,11)]
  # 
  # colnames(port_CR) <- c("symbol","TickerDict_ID","Data","RiskClassName","Pozycja","Value","Multiplier","CurrencyQuote","Div_","CurrencyValue","EUR")
  # portfolio <- rbind(portfolio, port_CR)
  # 
  # ###======================================================
  # ### Dodatnie kryptowalut do portfolio1
  # ###======================================================
  # 
  # date1 <- as.Date(date) + 1
  # 
  # query_ceny <- paste("select distinct
  #                     groupsymbol,
  #                     ClosePrice
  #                     from
  #                     riskdb.wymogi.PortfelHandlowy_final
  #                     where
  #                     groupsymbol in ('DASH', 'ETHEREUM', 'LITECOIN', 'RIPPLE','BITCOIN')
  #                     and	DateStamp = '",date1,"'
  #                     order by
  #                     groupsymbol")
  # 
  # ceny <- sqlQuery(ch,query_ceny)
  # port_CR$Value <- ceny$ClosePrice
  # port_CR$Value <- as.numeric(port_CR$Value)
  # colnames(port_CR) <- c("symbol","TickerDict_ID","Data","RiskClassName","Pozycja","Value","Multiplier","CurrencyQuote","Div_","Value.1","EUR")
  # portfolio1 <- rbind(portfolio1, port_CR)
  # 
  # ###====================================
  # ### Koniec dodawania kryptowalut
  # ###====================================
  
  portfolio1[,6]<-portfolio1[,6]/portfolio1[,9]
  
  FXCross<-portfolio[,10]/portfolio[,11]
  FXCross1<-portfolio1[,10]/portfolio1[,11]
  FaceValue<-portfolio[,5]*portfolio[,6]*portfolio[,7]*FXCross
  FaceValue1<-portfolio1[,5]*portfolio1[,6]*portfolio1[,7]*FXCross1
  PortfolioValue<-sum(FaceValue)
  PortfolioValue1<-sum(FaceValue1)
  
  rm(FXCross,FXCross1,FaceValue,FaceValue1,portfolio,portfolio1,inst_del,PriceLack,PriceErrors,query,query1,polaczenie)
  
  P_L<-PortfolioValue1-PortfolioValue
  P_L<<-PortfolioValue1-PortfolioValue
  
  if (P_L==0){
    warning(paste('Zerowy P&L do backtestu. Sprawd??? portfel i funkcj??? ProfitLoss'))
  }
  if (abs(P_L)>50000000 ){
    warning(paste('Du???a zmiana P&L do backtestu. Sprawd??? portfel i funkcj??? ProfitLoss'))
  }
  
  sq<-paste("INSERT INTO RiskDB.VaR.ProfitLoss_portfolio(Datestamp,ProfitLoss,consolidated) VALUES ('",date,"',",P_L,",",consolidated,")")
  
  polaczenie<-odbcConnect("RiskDB")
  sqlQuery(polaczenie,sq)
  odbcClose(polaczenie)
  rm(sq,P_L,polaczenie)
}

# FUNKCJA WYLICZAJÄ„CA VaR - xRisk -----------------------------------------

value_at_risk1<-function(portfel,method,date,class,hour,minute='00',duration=1,significance=1,consolidated=0){
  dateVaR = date
  if(method==1){ 
    
    if(class==0){ 
      
      ConfidenceLevel=1-(significance*0.01)                                      
      
      portfolio<-portfel
      
      ##### MACIERZ CEN DLA INSTRUMENT?W Z PORTFELA HANDLOWEGO
      polaczenie<-odbcConnect("RiskDB")
      query<-paste("set nocount on exec RiskDB.risk.VaR_PortfolioPrices '",date,"',",1,",",class,",'",hour,"','",minute,"',@consolidated=",consolidated, sep="")
      PriceMatrix<-sqlQuery(polaczenie, query)
      odbcClose(polaczenie)
      
      
      priceMatrix<-matrix(PriceMatrix$Price,nrow=250)
      zlicz = setDT(PriceMatrix)[, .N, by = .(Datestamp,symbol)]  
      if (max(zlicz$N)>1){stop(simpleError("Dubel w PriceMatrix"))}
      pM = dcast(PriceMatrix[,c("DateStamp","symbol","Price")], DateStamp~symbol, value = "Price" )
      pM$DateStamp = as.Date(pM$DateStamp)    
      rownames(pM)=pM$DateStamp
      pM=pM[,-1]
      priceMatrix<-pM
      priceMatrix<<-priceMatrix
      
      rm(PriceMatrix)
      ##### MACIERZ STÃ“P ZWROTU, KOWARIANCJI I KORELACJI
      returnMatrix<-apply(priceMatrix,2,Delt)
      returnMatrix<-returnMatrix[-1,]
      
      correlationMatrix<-cor(returnMatrix,method = "pearson")
      alpha<-qnorm(ConfidenceLevel)
      stdev<-apply(returnMatrix,2,sd)
      
      VaR_DN_Instruments<-alpha*stdev*portfolio$FaceValue
      VaR<-as.numeric(sqrt(t(VaR_DN_Instruments)%*%correlationMatrix%*%VaR_DN_Instruments)*sqrt(duration))
      rm(ConfidenceLevel)
      VaR_df[1,]<<-c(dateVaR,1,VaR)
      
    } else {
      
      ConfidenceLevel=1-(significance*0.01)                                      
      dateVaR<-date
      ##### PORTFEL HANDLOWY NA ZADANY DZIEÅƒ
      query<-paste("set nocount on exec RiskDB.risk.VaR_portfolio '",dateVaR,"',",1,",",class,",'",hour,"','",minute,"',@consolidated=",consolidated,sep="")
      
      polaczenie<-odbcConnect("RiskDB")
      portfolio<-sqlQuery(polaczenie, query)
      portfolio$Data<-as.POSIXct(portfolio$Data)
      portfolio$symbol<-as.character(portfolio$symbol)
      odbcClose(polaczenie)
      
      FXCross<-portfolio$CurrencyValue/portfolio$EUR
      FaceValue<-portfolio$Pozycja*portfolio$Value*portfolio$Multiplier*FXCross
      PortfolioValue<-sum(FaceValue)
      Contribution<-(FaceValue/PortfolioValue)
      
      portfolio<-data.frame(portfolio,FXCross,FaceValue,Contribution)
      rm(FaceValue,Contribution,FXCross,query,polaczenie)
      ##### MACIERZ CEN DLA INSTRUMENTÃ“W Z PORTFELA HANDLOWEGO
      
      query<-paste("set nocount on exec RiskDB.risk.VaR_PortfolioPrices '",dateVaR,"',",1,",",class,",'",hour,"','",minute,"',@consolidated=",consolidated,sep="")
      polaczenie<-odbcConnect("RiskDB")
      PriceMatrix<-sqlQuery(polaczenie, query)
      odbcClose(polaczenie)
      
      priceMatrix<-matrix(PriceMatrix$Price,nrow=250) 
      zlicz = setDT(PriceMatrix)[, .N, by = .(DateStamp,symbol)]  
      if (max(zlicz$N)>1){stop(simpleError("Dubel w PriceMatrix"))}
      pM = dcast(PriceMatrix[,c("DateStamp","symbol","Price")], DateStamp~symbol, value = "Price")
      pM$DateStamp = as.Date(pM$DateStamp)    
      rownames(pM)=pM$DateStamp
      pM=pM[,-1]
      priceMatrix<-pM
      rm(PriceMatrix)
      
      ##### MACIERZ STÃ“P ZWROTU, KOWARIANCJI I KORELACJI
      returnMatrix<-apply(priceMatrix,2,Delt)
      returnMatrix<-returnMatrix[-1,]
      
      correlationMatrix<-cor(returnMatrix,method = "pearson")
      alpha<-qnorm(ConfidenceLevel)
      stdev<-apply(returnMatrix,2,sd)
      
      VaR_DN_Instruments<-alpha*stdev*portfolio$FaceValue
      VaR<-as.numeric(sqrt(t(VaR_DN_Instruments)%*%correlationMatrix%*%VaR_DN_Instruments)*sqrt(duration))
      rm(ConfidenceLevel)
      
      VaR_df[class+1,]<<-c(dateVaR,class+1,VaR)  
      
    } 
  } else if(method==2){
    
    if(class==0){  
      
      query<-paste0("set nocount on exec RiskDB.risk.VaR_PortfolioPrices '",date,"',",1,",",class,",'",hour,"','",minute,"',@consolidated =",consolidated)
      polaczenie<-odbcConnect("RiskDB")
      PriceMatrix<-sqlQuery(polaczenie, query)
      odbcClose(polaczenie)
      
      priceMatrix<-matrix(PriceMatrix$Price,nrow=250)
      zlicz = setDT(PriceMatrix)[, .N, by = .(DateStamp,symbol)]  
      if (max(zlicz$N)>1){stop(simpleError("Dubel w PriceMatrix"))}
      pM = dcast(PriceMatrix[,c("DateStamp","symbol","Price")], DateStamp~symbol, value = "Price")
      pM$DateStamp = as.Date(pM$DateStamp)    
      rownames(pM)=pM$DateStamp
      pM=pM[,-1]
      priceMatrix<-pM
      
      prz<-portfolio$Pozycja*portfolio$Multiplier
      val_list<-apply(priceMatrix,1,function(x) sum(x*prz))
      
      roznice<-diff(val_list)
      
      VaR<-as.numeric(quantile(roznice,significance*0.01,na.rm=T)*sqrt(duration))
      CVaR<-mean(abs(roznice[roznice<VaR]))
      
      VaR_df[7,]<<-c(dateVaR,7,abs(VaR))
      VaR_df[13,]<<-c(dateVaR,13,CVaR)
      
    } else {
      
      query<-paste("set nocount on exec RiskDB.risk.VaR_PortfolioPrices '",date,"',",1,",",class,",'",hour,"','",minute,"',@consolidated=",consolidated,sep="")
      query1<-paste("set nocount on exec RiskDB.risk.VaR_portfolio '",date,"',",1,",",class,",'",hour,"','",minute,"',@consolidated=",consolidated,sep="")
      polaczenie<-odbcConnect("RiskDB")
      portfolio<-sqlQuery(polaczenie, query1)
      PriceMatrix<-sqlQuery(polaczenie, query)
      odbcClose(polaczenie)
      
      priceMatrix<-matrix(PriceMatrix$Price,nrow=250)
      zlicz = setDT(PriceMatrix)[, .N, by = .(DateStamp,symbol)]  
      if (max(zlicz$N)>1){stop(simpleError("Dubel w PriceMatrix"))}
      pM = dcast(PriceMatrix[,c("DateStamp","symbol","Price")], DateStamp~symbol, value = "Price")
      pM$DateStamp = as.Date(pM$DateStamp)    
      rownames(pM)=pM$DateStamp
      pM=pM[,-1]
      priceMatrix<-pM
      
      prz<-portfolio$Pozycja*portfolio$Multiplier
      val_list<-apply(priceMatrix,1,function(x) sum(x*prz))
      
      roznice<-diff(val_list)
      
      VaR<-as.numeric(quantile(roznice,significance*0.01,na.rm=T)*sqrt(duration))
      CVaR<-mean(abs(roznice[roznice<VaR]))
      
      VaR_df[class+7,]<<-c(dateVaR,class+7,abs(VaR))
      VaR_df[class+13,]<<-c(dateVaR,class+13,CVaR)  
      
    }
  }
}

value_at_risk_err<-function(portfel,priceMatrix,method,date,class,duration=1,significance=1,n_sim=10000){
  
  ConfidenceLevel=1-(significance*0.01)
  portfel<-as.data.table(portfel)
  
  klasa<-list("akcje",c("gold","towar"),c("indeks","indeks_uznany"),"obligacje","waluta")
  
  if (method==1){
    
    if(class==0){
      
      returnMatrix<-apply(priceMatrix,2,Delt)
      returnMatrix<-returnMatrix[-1,]
      
      correlationMatrix<-cor(returnMatrix,method = "pearson")
      alpha<-qnorm(ConfidenceLevel)
      stdev<-apply(returnMatrix,2,sd)
      
      VaR_DN_Instruments<-alpha*stdev*portfel$FaceValue
      VaR<-as.numeric(sqrt(t(VaR_DN_Instruments)%*%correlationMatrix%*%VaR_DN_Instruments)*sqrt(duration))
      rm(ConfidenceLevel)
      VaR_df[1,]<<-c(dateVaR,1,VaR)
      
    } else {
      portfolio<-portfel[RiskClassName %in% klasa[[class]]]
      PriceMatrix<-priceMatrix[,which(portfel$RiskClassName %in% klasa[[class]])]
      
      # macierz stÃ³p zwrotu, kowariancji
      returnMatrix<-apply(PriceMatrix,2,Delt)
      returnMatrix<-returnMatrix[-1,]
      
      correlationMatrix<-cor(returnMatrix,method = "pearson")
      alpha<-qnorm(ConfidenceLevel)
      stdev<-apply(returnMatrix,2,sd)
      
      VaR_DN_Instruments<-alpha*stdev*portfolio$FaceValue
      VaR<-as.numeric(sqrt(t(VaR_DN_Instruments)%*%correlationMatrix%*%VaR_DN_Instruments)*sqrt(duration))
      rm(ConfidenceLevel)
      VaR_df[class+1,]<<-c(dateVaR,class+1,VaR)
    } 
    
  } else if(method==2){          
    
    if(class==0){  
      prz<-portfel$Pozycja*portfel$Multiplier
      val_list<-apply(priceMatrix,1,function(x) sum(x*prz))
      
      roznice<-diff(val_list)
      
      VaR<-as.numeric(quantile(roznice,significance*0.01,na.rm=T)*sqrt(duration))
      CVaR<-mean(abs(roznice[roznice<VaR]))
      
      VaR_df[7,]<<-c(dateVaR,7,abs(VaR))
      VaR_df[13,]<<-c(dateVaR,13,CVaR)
      
    } else {
      
      portfolio<-portfel[RiskClassName %in% klasa[[class]]]
      PriceMatrix<-priceMatrix[,which(portfel$RiskClassName %in% klasa[[class]])]
      
      prz<-portfolio$Pozycja*portfolio$Multiplier
      val_list<-apply(PriceMatrix,1,function(x) sum(x*prz))
      
      roznice<-diff(val_list)
      
      VaR<-as.numeric(quantile(roznice,significance*0.01,na.rm=T)*sqrt(duration))
      CVaR<-mean(abs(roznice[roznice<VaR]))
      
      VaR_df[class+7,]<<-c(dateVaR,class+7,abs(VaR))
      VaR_df[class+13,]<<-c(dateVaR,class+13,CVaR)
      
    } 
  } else if (method==3){
    if(class==0){
      portfolio_value<-sum(portfel$FaceValue)    
      returnMatrix<-apply(priceMatrix,2,Delt)
      returnMatrix<-returnMatrix[-1,]
      
      correlationMatrix<-cor(returnMatrix,method = "pearson")
      
      ## zmiana 20190402 KB - skondensowanie skorelowanych instrumentów do 1
        skorelowane <<- findCorrelation(correlationMatrix, cutoff = 0.999999, verbose=FALSE, names=TRUE)  # które kolumny skorelowane
        NieZeroweZmiany = which(apply(returnMatrix[,which((colnames(returnMatrix) %in% skorelowane))],2, maxabs)>0.01)
        if (length(NieZeroweZmiany)>0){
          skorelowane = skorelowane[-NieZeroweZmiany]
        }
        if (length(skorelowane)>1){
          skorelowane <<- skorelowane[2:length(skorelowane)]
          print(paste("Usunieto ",length(skorelowane)," instrumentów skorelowanych w 99.9%"))
          returnMatrix = returnMatrix[,which(!(colnames(returnMatrix) %in% skorelowane))] # usuniecie skorelowanych kolumn - var_monte_carlo zwraca ró¿nice, wiêc nie ma wp³ywu na wynik
          
          correlationMatrix<-cor(returnMatrix,method = "pearson")  # przeliczenie korelacji po usunieciu skorelowanych kolumn
          
          portfel = portfel[which(!(portfel$symbol %in% skorelowane)),]  # usuwamy rekordy, ale nie zmieniamy portfolio_value
          priceMatrix = priceMatrix[,which(!(colnames(priceMatrix) %in% skorelowane))]   
        }
      ## koniec zmiany 20190402 KB
      
      cholStatus <- try(u <- chol(correlationMatrix, pivot = TRUE), silent = FALSE)
      cholError <- ifelse(class(cholStatus) == "try-error", TRUE, FALSE)
      
      # fix the correl matrix
      newMat <- correlationMatrix
      
      iter <- 0
      while (cholError) 
      {
        
        iter <- iter + 1
        cat("iteration ", iter, "\n")
        
        # replace -ve eigen values with small +ve number
        newEig <- eigen(newMat)
        newEig2 <- ifelse(newEig$values < 0, 0, newEig$values)
        
        # create modified matrix
        newMat <- newEig$vectors %*% diag(newEig2) %*% t(newEig$vectors)
        
        # normalize modified matrix
        newMat <- newMat/sqrt(diag(newMat) %*% t(diag(newMat)))
        
        # try chol again
        cholStatus <- try(u <- chol(newMat, pivot = TRUE), silent = TRUE)
        cholError <- ifelse(class(cholStatus) == "try-error", TRUE, FALSE)
        if (iter>200){ break}
      }
      
      
      # Dekompozycja Choleskiego
      cholesky<-chol(newMat, pivot = TRUE)    
      
      mean<-apply(returnMatrix,2,mean)
      stdev<-apply(returnMatrix,2,sd)
      
      roznice<-c()
      roznice<-replicate(n_sim,var_monte_carlo(portfel,priceMatrix,portfolio_value,cholesky,mean,stdev))
      
      VaR<-as.numeric(quantile(roznice,significance*0.01,na.rm=T)*sqrt(duration))
      CVaR<-mean(abs(roznice[roznice<VaR]))
      
      VaR_df[19,]<<-c(dateVaR,19,abs(VaR))
      VaR_df[25,]<<-c(dateVaR,25,CVaR)
      
    } else {
      
      portfolio<-portfel[RiskClassName %in% klasa[[class]]]
      PriceMatrix<-priceMatrix[,which(portfel$RiskClassName %in% klasa[[class]])]
      portfolio_value<-sum(portfolio$FaceValue)
      
      returnMatrix<-apply(PriceMatrix,2,Delt)
      returnMatrix<-returnMatrix[-1,]
      
      correlationMatrix<-cor(returnMatrix,method = "pearson")
      ## zmiana 20190402 KB - skondensowanie skorelowanych instrumentów do 1
      skorelowane <<- findCorrelation(correlationMatrix, cutoff = 0.999999, verbose=FALSE, names=TRUE)  # które kolumny skorelowane
      NieZeroweZmiany = which(apply(returnMatrix[,which((colnames(returnMatrix) %in% skorelowane))],2, maxabs)>0.01)
      if (length(NieZeroweZmiany)>0){
        skorelowane = skorelowane[-NieZeroweZmiany]
      }
      if (length(skorelowane)>1){
        skorelowane <<- skorelowane[2:length(skorelowane)]
        print(paste("Usunieto ",length(skorelowane)," instrumentów skorelowanych w 99.9%"))
        returnMatrix = returnMatrix[,which(!(colnames(returnMatrix) %in% skorelowane))] # usuniecie skorelowanych kolumn - var_monte_carlo zwraca ró¿nice, wiêc nie ma wp³ywu na wynik
        
        correlationMatrix<-cor(returnMatrix,method = "pearson")  # przeliczenie korelacji po usunieciu skorelowanych kolumn
        
        portfolio = portfolio[which(!(portfolio$symbol %in% skorelowane)),]  # usuwamy rekordy, ale nie zmieniamy portfolio_value
        PriceMatrix = PriceMatrix[,which(!(colnames(PriceMatrix) %in% skorelowane))]  
      }
      ## koniec zmiany 20190402 KB
      
      
      cholStatus <- try(u <- chol(correlationMatrix, pivot = TRUE), silent = FALSE)
      cholError <- ifelse(class(cholStatus) == "try-error", TRUE, FALSE)
      
      # fix the correl matrix
      newMat <- correlationMatrix
      
      iter <- 0
      while (cholError) 
      {
        
        iter <- iter + 1
        cat("iteration ", iter, "\n")
        
        # replace -ve eigen values with small +ve number
        newEig <- eigen(newMat)
        newEig2 <- ifelse(newEig$values < 0, 0, newEig$values)
        
        # create modified matrix
        newMat <- newEig$vectors %*% diag(newEig2) %*% t(newEig$vectors)
        
        # normalize modified matrix
        newMat <- newMat/sqrt(diag(newMat) %*% t(diag(newMat)))
        
        # try chol again
        cholStatus <- try(u <- chol(newMat, pivot = TRUE), silent = TRUE)
        cholError <- ifelse(class(cholStatus) == "try-error", TRUE, FALSE)
        if (iter>200){ break}
      }
      
      
      # Dekompozycja Choleskiego
      cholesky<-chol(newMat, pivot = TRUE)    
      
      mean<-apply(returnMatrix,2,mean)
      stdev<-apply(returnMatrix,2,sd)
      
      roznice<-c()
      roznice<-replicate(n_sim,var_monte_carlo(portfolio,PriceMatrix,portfolio_value,cholesky,mean,stdev))
      
      VaR<-as.numeric(quantile(roznice,significance*0.01,na.rm=T)*sqrt(duration))
      CVaR<-mean(abs(roznice[roznice<VaR]))
      
      VaR_df[class+19,]<<-c(dateVaR,class+19,abs(VaR))
      VaR_df[class+25,]<<-c(dateVaR,class+25,CVaR)
      
    } 
  }
}

