wyniki=read.csv2("wyniki.csv", header=T) 
n=ncol(wyniki)
minimum=matrix(0,1,n)
maksimum=matrix(1,1,n)
zwrot=0.00131809600298421
wynikits=as.timeSeries(wyniki)
Spec = portfolioSpec()
setTargetReturn(Spec) = zwrot 
Constraints = c("maxW[1:12]=0.2","maxsumW[1:2]=0.3","maxsumW[1:4]=0.4", "minsumW[1:4]=0.4")
Data=wynikits
eff=efficientPortfolio(Data, Spec, Constraints) 
minr=minriskPortfolio(Data, Spec, Constraints )
frontier = portfolioFrontier(Data, Spec, Constraints)
x = frontierPoints(frontier, risk = "Sigma", auto = FALSE)
write.csv2(x,"frontier")
wagi=getWeights(eff)
write.csv2(wagi,"wagi")