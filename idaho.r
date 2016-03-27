# Regression Modelling of Idaho WINE Sales
library(TSA)
library(astsa)
library(fGarch)
data<-read.csv("idaho.csv")
colnames(data)
data$POPL <- data$POPN/data$POPSUM
data$WineWVperH<-10*data$WINE/data$POPN

data$LogWineWVperH<-log(10*data$WINE/data$POPN)

# NOTE: no Other States Control Series for WINE!

# Some plots:
#pdf("WestVirgWINESalesWithInterventions.pdf")
par(mfrow=c(3,1))
ts.plot(ts(data$WineWVperH,start=1968,frequency=12),
        ylab="Litres of Ethanol/Head",
        main="WINE Consumption in Idaho per 1000,000 people \n 1968 to 1998")

ts.plot(ts(data$LogWineWVperH,start=1968,frequency=12),
        ylab="Log(Litres of Ethanol/Head)",
        main="Log WINE Consumption in Idaho per 1000,000 people\n 1968 to 1998")

ts.plot(ts(data$MDAVAL,start = c(1968, 1), frequency = 12),col="red",ylim=c(0,1),
        ylab="Indicator")
lines(ts(data$WINEOFF,start = c(1968, 1), frequency = 12),col="green")
#lines(ts(data$TrendMDAVALdown,start = c(1968, 1), frequency = 12),col="blue")

legend("bottomright",c("MDAVAL","WINEOFF"),
       lty=c(2,2),
       col=c("red","green"))
title("Idaho: Interventions")
data$WineWVperH<-log(10*data$WINE/data$POPN)
#use log
#dev.off()
# Decide to model WINE Consumption per Head (not log scale)

# Defining regression variables associated with interventions trend changes

# Time trend (annual change)
N<-length(data$YEAR)
data$Trend<-(1:N)/12

# New Trend from Time of Wine Privatisation
data$TrendWineOff<-c(rep(0,41),(1:(N-41)/12))

# New Trend from MDAVAL.
#231-54
data$TrendMDAVALdown<-c(rep(0,54),((1:177)/12),rep(0,N-231))
data$TrendMDAVALup <-c(rep(0,231),(1:(N-231)/12))
data$PriP<-rep(0,N)
data$PriP[42] <- 1

par(mfrow=c(1,1))
ts.plot(ts(data$Trend,start=c(1968,1),frequency = 12),lwd=2)
lines(ts(data$TrendWineOff,start=c(1968,1),frequency = 12),col="green",lwd=2)
lines(ts(data$TrendMDAVALdown,start=c(1968,1),frequency = 12),col="blue",lwd=2)
lines(ts(data$TrendMDAVALup,start=c(1968,1),frequency = 12),col="red",lwd=2)
legend("topleft",c("MDAVALdown","WINEOFF","MDAVALup"),
       lty=c(1,1,1),lwd=rep(2,3),
       col=c("blue","green","red"))
title("Idaho: Trend Interventions")


# Seasonal Dummies (we set July as the intercept month)

data$Jan<-rep(0,N); data$Jan[data$MONTH==1]<-1
data$Feb<-rep(0,N); data$Feb[data$MONTH==2]<-1
data$Mar<-rep(0,N); data$Mar[data$MONTH==3]<-1
data$Apr<-rep(0,N); data$Apr[data$MONTH==4]<-1
data$May<-rep(0,N); data$May[data$MONTH==5]<-1
data$Jun<-rep(0,N); data$Jun[data$MONTH==6]<-1
data$Jul<-rep(0,N); data$Jul[data$MONTH==7]<-1
data$Aug<-rep(0,N); data$Aug[data$MONTH==8]<-1
data$Sep<-rep(0,N); data$Sep[data$MONTH==9]<-1
data$Oct<-rep(0,N); data$Oct[data$MONTH==10]<-1
data$Nov<-rep(0,N); data$Nov[data$MONTH==11]<-1
data$Dec<-rep(0,N); data$Dec[data$MONTH==12]<-1

# Ordinary Least Square Regression (using the dataframe data)

Model.OLS<-lm(WineWVperH~ Trend+
                WINEOFF+TrendWineOff+MDAVAL+
                TrendMDAVALup+TrendMDAVALdown+PriP+
                Jan+Feb+Mar+Apr+May+Jun+Aug+Sep+Oct+Nov+Dec,
              data=data)

summary(Model.OLS)

#first X
X<-as.matrix(data[,c("Trend","WINEOFF","MDAVAL","TrendMDAVALup","TrendMDAVALdown","PriP")])
coeffs<-Model.OLS$coefficients[c("Trend","WINEOFF","MDAVAL","TrendMDAVALup","TrendMDAVALdown","PriP")]
names(Model.OLS$coefficients)[-1]
trendterm<-Model.OLS$coefficients[1]+X%*%coeffs	
names(Model.OLS$coefficients)
data[,names(Model.OLS$coefficients[-1])]
X<-as.matrix(data[,names(Model.OLS$coefficients)[-1]])

monthnames<-c("Jan","Feb","Mar","Apr","May","Jun","Aug","Sep","Oct","Nov","Dec")
trendterm<-Model.OLS$fitted-X[,monthnames]%*%Model.OLS$coefficients[monthnames]
####---------------------------------------------------------------------not used
#pdf("WestVaWINEOLSfit_diagnostics.pdf")
par(mfrow=c(2,2))		
acf(Model.OLS$residuals, lag.max=36)
pacf(Model.OLS$residuals,lag.max=36)
qqnorm(Model.OLS$residuals)
hist(Model.OLS$residuals)
#dev.off()

shapiro.test(Model.OLS$residuals)
# identify where negative outliers are
#outliertimes<-1968+((1:N)[Model.OLS$residuals >=10]-1)/12

#pdf("WestVaWINEOLSfit_FitsResiduls.pdf")
par(mfrow=c(1,1))

ts.plot(ts(data$WineWVperH,start=1968,frequency=12),
        ylab="Litres of Ethanol/Head",
        main="WINE Consumption in Idaho per 1000,000 people \n 1968 to 1998")
abline(v=1968+(c(42,54,231))/12,lty=2)

lines(ts(Model.OLS$fitted,start=1968,frequency=12),col=3,lwd=2)
lines(ts(trendterm,start=1968,frequency=12),col=4,lwd=2)
#points(outliertimes,data$WineWVperH[Model.OLS$residuals >=10],col=2)

ts.plot(ts(Model.OLS$residuals,start=1968,frequency=12), type="h")
#points(outliertimes,Model.OLS$residuals[Model.OLS$residuals <= -1],col=2)
abline(v=1968+(c(42,54,231))/12,lty=2)
#dev.off()
####---------------------------------notused
# Function to find outliers 
find.outliers<-function(resids){ # function to find outliers
  N<-length(resids)
  xbar<-mean(resids,na.rm=T)
  Stdev<-var(resids,na.rm=T)^0.5
  outlier.time<-NULL
  for (i in 1:N)
    if(abs(resids[i])>xbar+3*Stdev) outlier.time<-c(outlier.time,i) # 3 sigma rule for outliers
  return(outlier.time)
}

OLSOutliers<-find.outliers(Model.OLS$residuals)

# identify where outliers are on time axis:

outliertimes<-1968+((1:N)[OLSOutliers]-1)/12

# Removing the impact of outliers by dummy variables
outlier1<-rep(0,N)
outlier1[43] <-1
#outlier1[(1:N)[OLSOutliers[1]]]<-1
outlier2<-rep(0,N)
outlier2[211] <-1
#outlier2[(1:N)[OLSOutliers[2]]]<-1
outlier3<-rep(0,N)
outlier3[240] <-1
#outlier3[(1:N)[OLSOutliers[3]]]<-1
outlier4<-rep(0,N)
outlier4[(1:N)[OLSOutliers[4]]]<-1
#-----------------------------------------

Model.OLS.oo<-lm(WineWVperH~ Trend+
                   WINEOFF+MDAVAL+
                   TrendMDAVALup+TrendMDAVALdown+PriP+
                   Jan+Feb+Mar+Apr+May+Jun+Aug+Sep+Oct+Nov+Dec,
                 #outlier1+outlier2+outlier3,
                 data=data, x=T)    # NOTE: keep the design matrix for this!

summary(Model.OLS.oo)


par(mfrow=c(2,2))  	
acf(Model.OLS.oo$residuals, lag.max=36)
acf(Model.OLS.oo$residuals,lag.max=36, type="partial")
qqnorm(Model.OLS.oo$residuals)
hist(Model.OLS.oo$residuals)
shapiro.test(Model.OLS.oo$residuals)

# Removing outliers does not impact findings above

# Calculate Trends, fitted values etc.

# X<-as.matrix(data[,c("Trend","WINEOFF","TrendMDAVALdown", "TrendMDAVALup")])
# coeffs<-Model.OLS.oo$coefficients[c("Trend","WINEOFF","TrendMDAVALdown", "TrendMDAVALup")]
# 
# trendterm<-Model.OLS.oo$coefficients[1]+X%*%coeffs  
# X<-as.matrix(data[,names(Model.OLS.oo$coefficients)[-1]])

Xoo<-Model.OLS$x  # Use Design matrix from outlier removed analysis.
colnames(Xoo)
monthnames<-c("Jan","Feb","Mar","Apr","May","Jun","Aug","Sep","Oct","Nov","Dec")
trendterm<-Model.OLS.oo$fitted-Xoo[,monthnames]%*%Model.OLS.oo$coefficients[monthnames]
trendterm<-Model.OLS$fitted-X[,monthnames]%*%Model.OLS$coefficients[monthnames]
par(mfrow=c(2,1))
trendterm
ts.plot(ts(data$WineWVperH,start=1968,frequency=12),
        ylab="Litres of Ethanol/Head",
        main="WINE Consumption in Idaho per 100,000 people \n 1968 to 1998")
abline(v=1968+(c(42,54,231))/12,lty=2)
#abline(v=1968+(c(162,T,266)-1)/12,lty=2)

lines(ts(Model.OLS$fitted,start=1968,frequency=12),col=3,lwd=2)
lines(ts(trendterm,start=1968,frequency=12),col=4,lwd=2)
#points(outliertimes,data$WineWVperH[Model.OLS.oo$residuals <= -1],col=2)

ts.plot(ts(Model.OLS$residuals,start=1968,frequency=12), type="h")
points(Model.OLS.oo$residuals[OLSOutliers],col=2)
abline(v=1968+(c(42,54,231))/12,lty=2)
#abline(v=1968+(c(162,T,266)-1)/12,lty=2,,lwd=2,col=c("green","red","blue"))

# Summarising the Trend plus intervention effect:
trendterm.oo<-trendterm-Xoo[,c("outlier1","outlier2")]%*%Model.OLS.oo$coefficients[c("outlier1","outlier2")]
par(mfrow=c(1,1))
ts.plot(ts(trendterm.oo,start=1968,frequency=12))
#abline(v=1968+(c(162,T,266)-1)/12,lty=2,,lwd=2,col=c("green","red","blue"))
abline(v=1968+(c(42,54,231))/12,lty=2,col=c("green","red","blue"))
title("Summary of Trend plus intervention effects")
legend("bottomright",c("WINEOFF","TrendMDAVALup","TrendMDAVALdown"),
       lty=c(2,2,2),
       col=c("red","green","blue"))


# Seasonals? Can they be simplified.

plot((1:12)[-7],Model.OLS.oo$coefficients[monthnames],xlab="Month (Jan = 1)",ylab="Seasonal")
points(7,0)  # add in July (baseline month)
# 3 monthes 
title("Seasonal Effects using Seasonal Dummies \n (Outliers Removed OLS fit)")
# Conclude Could replace seasonal dummies by smooth Fourier type terms
# cos(2pi/12),sin(2pi/12),cos(4pi/12),sin(4pi/12)


# lets try:
freq<-2*pi*(1:N)/12
HarmonicSeasonals<-cbind(cos(freq),sin(freq),cos(2*freq),sin(2*freq))
colnames(HarmonicSeasonals)<-c("cos1","sin1","cos2","sin2")
ts.plot(HarmonicSeasonals[1:12,],col=1:4)
X.oo.HS<-cbind(as.matrix(data[,c("Trend","WINEOFF"+"MDAVAL")]),outlier1,outlier2,HarmonicSeasonals[,1:2])
colnames(X.oo.HS)
Y<-data$WineWVperH
Model.OLS.oo.HS<-lm(Y~ X.oo.HS,
                    data=data, x=T)    # NOTE: keep the design matrix for this!

summary(Model.OLS.oo.HS)

ts.plot(ts(data$WineWVperH,start=1968,frequency=12),
        ylab="Litres of Ethanol/Head",
        main="WINE Consumption in Idaho per 1000,000 people \n 1968 to 1998")
abline(v=1968+(c(42,54,231))/12,lty=2)

lines(ts(Model.OLS.oo.HS$fitted,start=1968,frequency=12),col="red",lwd=2)
lines(ts(Model.OLS.oo$fitted,start=1968,frequency=12),col="blue",lwd=2)

# Compare with Seasonal

summary(Model.OLS.oo)

#############################################################
# arima modelling

# In order to set things up for use of `arima'
#  select a response Y and a matrix of regressors X.
data$TrendWineOff

X = cbind(data$Trend,data$WINEOFF,data$TrendWineOff,data$MDAVAL,data$TrendMDAVALup,data$TrendMDAVALdown,data$PriP,
          data$Jan,data$Feb,data$Mar,data$Apr,data$May,data$Jun,data$Aug,data$Sep,data$Oct,data$Nov,data$Dec)
#,outlier1,outlier2,outlier3)
colnames(X) <- c("Trend","WINEOFF","TrendWineOff","MDAVAL","TrendMDAVALup","TrendMDAVALdown",
                 "Jan","Feb","Mar","Apr","May","Jun,","Aug","Sep","Oct","Nov","Dec","Prip","o1","o2","o3")

X = cbind(data$Trend,data$WINEOFF,data$TrendWineOff,data$MDAVAL,data$TrendMDAVALup,data$TrendMDAVALdown,data$PriP,outlier1,outlier2,outlier3)
colnames(X) <- c("Trend","WINEOFF","MDAVAL","TrendMDAVALup","TrendMDAVALdown","Prip","o1","o2","o3")
regModel<- sarima(data$WineWVperH,0,1,1,1,0,1,12,xreg = X)
regModel 

shapiro.test(regModel$fit$residuals)


residuals<-regModel$fit$residuals
par(mfrow=c(2,1))
acf(diff(diff(residuals,12)))
pacf(diff(diff(residuals,12)))
# ARIMA Modelling 


#m1
#xterm<- as.matrix(cbind(data$Trend,data$WINEOFF,data$MDAVAL,data$TrendMDAVALup,data$TrendMDAVALdown,data$Prip))
#Mo<-arimax(data$WineWVperH,xreg=xterm,order=c(1,1,0),seasonal = list(order = c(0, 1,1), period = 12))
#Model.Sarima.1
#Model.Sarima.1

# Non-significant AR(1) and Seasonal AR(1) Results 
# confirm that autocorrelation is NOT present in the OLS residuals.
###########################-------------------#Garch
rdt <-regModel$fit$residuals
par(mfrow=c(2,2))
acf(rdt^2)
pacf(rdt^2)
acf((rdt-mean(rdt))^2)
pacf((rdt-mean(rdt))^2)


g11<-garchFit(data=rdt,formula=~garch(1,1))

summary(g10)
g10<-garchFit(data=rdt,formula=~garch(1,0))

g10<-garchFit(data=rdt,formula=~arma(0,1)+garch(1,0))
summary(g10)


g10.std<-garchFit(data=rdt,formula=~garch(1,0),cond.dist = "std")
summary(g10.std)
g10.QMLE<-garchFit(data=rdt,formula=~garch(1,1),cond.dist="QMLE")
summary(g10.QMLE)
summary(g11)
g20<-garchFit(data=rdt,formula=~garch(2,0))
summary(g20)
restd.g10<-g10@residuals/g10@sigma.t 
g10ar2<-garchFit(data=rdt,formula=~garch(1,0))

# GARCH(1,0) model using QMLE and Robust Covariance Estimate.


par(mfrow=c(2,2))
acf(stdresid.garch10)
pacf(stdresid.garch10)
acf(stdresid.garch10^2)
qqnorm(stdresid.garch10)
plot(g10)




redid<-g10@residuals/g10@sigma.t 


par(mfrow=c(2,2))
plot(g10, which=c(1,2,3,4))  
par(mfrow=c(3,1))
ts.plot(g10@residuals, main="GARCH(1,0) residuals")
ts.plot(g10@sigma.t,main="GARCH(1,0) conditional SD")
ts.plot(restd.g10,main="xGARCH(1,0) Standardised Residuals")
# see Wurtz JSS article, page 31 for plot method and options for graphs.


# Compare Conditional SD profiles
pdf("DJIndIndReturnsARCHGARCHcondSD.pdf") # Figure 9.10
par(mfrow=c(2,1))
ts.plot(DJII.arch2@sigma.t,main="ARCH(2): Conditional SD Process")
ts.plot(DJII.garch10@sigma.t, main="GARCH(1,1): Conditional SD Process")
dev.off()

# GARCH(1,1) t-residuals

stdresid.g10.std<-g10.std@residuals/g10.std@sigma.t 
tdf<-coef(g10.std)["shape"]

# Figure 9.12
par(mfrow=c(2,2))
acf(stdresid.garch10.std)
pacf(stdresid.garch10.std)
acf(stdresid.garch10.std^2)
# qqplot for T distribution
N<-length(DJII)
stquantile<-qt(((1:N)-0.5)/N,tdf)
qqplot(stquantile,stdresid.garch10.std)

# Demonstrating how standardisation works
# Figure 9.11
par(mfrow=c(3,1))
ts.plot(g10.std@residuals, main="GARCH(1,0) residuals")
ts.plot(g10.std@sigma.t,main="GARCH(1,0) conditional SD")
ts.plot(g10.std,main="GARCH(1,0) Standardised Residuals")

#------------------5. Prediction 
L <-12
#361-41
#rm(newTrend)
#newTrend <-ts(c(data$Trend,ts((361:372)/12)))
newTrend<-((N+1):(N+L))/12 
# New Trend from Time of Wine Privatisation
newTrendWineOff<-((N-40):(N+L-41))/12
# New Trend from MDAVAL.
newMDAVAL <-rep(1.00,12)
#372-231
#360-231+12
newTrendMDAVALdown<-rep(0,12)
newTrendMDAVALup <-((N-230):(N+L-231))/12
newPriP<-rep(0,12)
newPriP[42] <- 1
#X = cbind(data$Trend,data$WINEOFF,data$MDAVAL,data$TrendMDAVALup,data$TrendMDAVALdown,data$PriP)
#colnames(X) <- c("Trend","WINEOFF","MDAVAL","TrendMDAVALup","TrendMDAVALdown","Prip")

X = as.data.frame(cbind(data$Trend,data$WINEOFF,data$MDAVAL,data$TrendMDAVALup,data$TrendMDAVALdown,data$PriP))
#data$Jan,data$Feb,data$Mar,data$Apr,data$May,data$Jun,data$Aug,data$Sep,data$Oct,data$Nov,data$Dec)
newReg = as.data.frame(cbind(newTrend,newTrendWineOff,newMDAVAL,newTrendMDAVALup,newTrendMDAVALdown,newPriP))
#,Jan,Feb,Mar,Apr,May,Jun,Aug,Sep,Oct,Nov,Dec)
colnames(newReg) <- c("Trend","WINEOFF","MDAVAL","TrendMDAVALup","TrendMDAVALdown","Prip")
colnames(X) <- c("Trend","WINEOFF","MDAVAL","TrendMDAVALup","TrendMDAVALdown","Prip")
regg<-arima(data$WineWVperH,order=c(0,1,1),xreg=X,
            seasonal=list(order=c(1,0,1),period=12),method="ML")
rrr <- arima(Model.OLS$residuals,order=c(0,1,1),seasonal=list(order=c(1,0,1),period=12),method="ML")

acf(regg$residuals)
pacf(regg$residuals)
regModel
library(forecast)
pdtG<- predict(regg,n.ahead=12,newxreg=newReg)
pdtR<- predict(rrr,n.ahead=12)
#ts.plot(data$WineWVperH,predictdata$pred)
ts.plot(data$WineWVperH)
lines(pdtG$pred,col=4,lwd=2)
ts.plot(data$WineWVperH,pdt,col=c(1,3),ylim=c(17,18.2),lwd=c(1,2))
lines(pdtG$pred+1.96*pdtG$se,lty="dashed",col=3,lwd=2)
lines(pdtG$pred-1.96*pdtG$se,lty="dashed",col=3,lwd=2)










