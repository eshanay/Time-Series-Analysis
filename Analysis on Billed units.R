library(astsa)
library(forecast)

data=read.csv("C:\\Users\\eshan\\Downloads\\CESC billed units.csv",header=T,sep=",")
View(data)
units=ts(data$Billed.Units,frequency=12,start=c(2019,4))
mon=c("Apr","May","Jun","July","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar")
yr=c(rep(19,9),rep(20:23,each=12),rep(24,3))
xlab=paste(rep(month.abb[c(4:12,1:3)],times=5),yr,sep="'")

#plotting time series data on billed units
par(mar=c(6,1,1,1))
tsplot(units,xaxt="n",xlab="",type="o",main="Billed electricity units",col="blue",gg=T)
axis(side=1,at=time(units),labels=xlab,cex=0.5,las=2)
acf(units)#checking for stationarity

#visualizing seasonal effect
boxplot(units~cycle(units),xlab="months",ylab="units (kWh)",xaxt="n")
axis(side=1,at=1:12,labels=month.abb)

#applying log-transformation and then plotting
tsplot(x<-log(units),xaxt="n",xlab="",type="o",main="log(units)",col="blue")
axis(side=1,at=time(units),labels=xlab,cex=0.5,las=2)
abline(lm(log(units)~time(units)),col="red")

#linear regression summary on transformed data
summary(lm(log(units)~time(units)))
lag1.plot(x,12)
acf2(log(units),max.lag=24)
#still non-stationary, so we need to deseasonalize it and then check for stationarity

#removing seasonal effect using forward difference of lag 12
y=diff(log(units),lag=12)
tsplot(y,main=expression(y==nabla[12]~log(units)),col="blue")
acf2(y,max.lag=30)

#comparing the goodness of fits of some candidate models
sarima(log(units),p=1,d=0,q=1,P=1,D=1,Q=0,S=12,details=F)$ICs["AICc"] #ARIMA(1,0,1)x(1,1,0)12
sarima(log(units),p=1,d=0,q=1,P=0,D=1,Q=0,S=12,details=F)$ICs["AICc"] #ARIMA(1,0,1)x(0,1,0)12
sarima(log(units),p=1,d=0,q=0,P=0,D=1,Q=0,S=12,details=F)$ICs["AICc"] #ARIMA(1,0,0)x(0,1,0)12
sarima(log(units),p=1,d=0,q=0,P=1,D=1,Q=0,S=12,details=F)$ICs["AICc"] #ARIMA(1,0,0)x(1,1,0)12
sarima(x,p=0,d=0,q=1,P=1,D=1,Q=0,S=12,no.constant=T,details=F)
sarima(x,p=0,d=0,q=1,P=0,D=1,Q=0,S=12,no.constant=T,details=F)
ARIMA(1,0,0)x(1,1,0)12 works the best

#Finally I want to forecast the units consumption for the next few days
sarima(log(units),p=1,d=0,q=0,P=1,D=1,Q=0,S=12) #ARIMA(1,0,0)x(1,1,0)12

mdl<-sarima.for(log(units),n.ahead=12,1,0,0,1,1,0,12)
#back-transforming the predicted data and the 1 s.e. width confidence bounds to forecast the billed electricity units of my house for the next 12 months.
units.pred<-exp(mdl$pred)
units.low<-exp(mdl$pred-mdl$se)
units.high<-exp(mdl$pred+mdl$se)
combined<-ts(c(units,units.pred),start=start(units),frequency=frequency(units))
tsplot(combined,ylim=range(c(units.low,units.high,units)),main="Billed units",col="blue")
polygon(c(time(units.pred),rev(time(units.pred))),c(units.low,rev(units.high)),col="lightgrey",density=70,border="n")
lines(units.pred,col="red")
lines(units.low,col="lightgreen")
lines(units.high,col="darkgreen")
