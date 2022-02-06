
rm(list=ls())
library(RODBC)
library(jrvFinance)

dds<-paste0("2021-06-07")

sq<-paste0("
     select
  o.id,
  o.Nazwa,
  (case when o.StalyKupon=0 then ods.OkresDo
  else o.DataWykupu end) as matu,
  (case when o.Czestotliwosc=0 then 1 else o.Czestotliwosc end) as fre,
  100*p.Cena/Nominal as pri,
  ods.Kupon/100 as cou,
  100 as rva
  from RiskDB.bonds.Obligacje as o
  inner join 
  RiskDB.bonds.Pozycja as p
  on(p.idObligacje=o.id)
  inner join
  RiskDB.bonds.Odsetki as ods
  on(ods.idObligacje=o.id and ods.OkresOd<='",dds,"' and ods.OkresDo>'",dds,"')
  left join RiskDB.bonds.Duration as d
  on(p.idObligacje=d.idObligacje and p.Data=d.DateDur)
  where d.idObligacje is null
  and p.Data='",dds,"'
  order by Nazwa
")

RiskDB<-odbcConnect("RiskDB")
dane<-sqlQuery(RiskDB,sq)
odbcClose(RiskDB)


l_ins<-nrow(dane)

for (ip in 1:l_ins) {
  print(ip)
  print(dane$Nazwa[ip])

  sett<-dds
  matu<-dane$matu[ip]
  cou<-dane$cou[ip]
  fre<-dane$fre[ip]
  pri<-dane$pri[ip]
  rva<-dane$rva[ip]


  
  tc<-bond.TCF(settle=sett, mature=matu, coupon=cou, freq = fre, convention = "ACT/ACT", redemption_value = rva)
  
  pri<-pri-tc$accrued
  
  yi<-bond.yield(settle=sett, mature=matu, coupon=cou, freq = fre, price=pri,
                 convention = "ACT/ACT",
                 comp.freq = fre, redemption_value = rva)
  
  print(yi)
  
  dur<-bond.duration(settle=sett, mature=matu, coupon=cou, freq = fre, yield=yi,
                     convention = "ACT/ACT",
                     modified = FALSE, comp.freq = fre, redemption_value = rva)
  dur<-round(dur, digits=3)
  print(dur)
  
  mdur<-bond.duration(settle=sett, mature=matu, coupon=cou, freq = fre, yield=yi,
                      convention = "ACT/ACT",
                      modified = TRUE, comp.freq = fre, redemption_value = rva)
  mdur<-round(mdur, digits=3)
  print(mdur)
  
  
  sq<-paste0("
    INSERT INTO RiskDB.bonds.Duration
    (idObligacje, YTM, Duration, ModDur, DateDur)
    VALUES (",
             dane$id[ip],
             ",",
             yi,
             ",",
             dur,
             ",",
             mdur,
             ",'",
             dds,
             "')")
  polaczenie<-odbcConnect("RiskDB")
  sqlQuery(polaczenie,sq)
  odbcClose(polaczenie)
  
  
  
  
}




