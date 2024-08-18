#PRAKTIKUM SARIMA 

#Library yang digunakan
library(tseries)
library(forecast)
library(TTR)
library(TSA)
library(graphics)
library(astsa)
library(car)
library(portes)


#Input data
USpass<-read_excel("D:/KULIAH/KULIAH SEMESTER 5/ANALISIS DERET WAKTU/DATA/AirPassengers.xlsx")
class(USpass)
head(USpass)


#Merubah class
USpass.ts <- ts(USpass, frequency = 12, start = c(1949,1), end = c(1960,12))
class(USpass.ts)

#Plot Time Series
plot.ts(USpass.ts[,2], main = "Monthly U.S. Air Passenger Data", xlab = "Year",ylab = "Air Passenger Miles",lwd=2)

#Plot per Musim
seasonplot(USpass.ts[,2],main="US Air Passenger", ylab="Air Passenger (miles)",year.labels = TRUE, col=rainbow(18))

#Deskriptif USpass
monthplot(USpass.ts[,2],ylab="Air Passenger (miles)")

#Boxplot
boxplot(USpass.ts[,2] ~ cycle(USpass.ts[,2]), xlab = "Month", ylab = "APM", main = "Monthly U.S. Air Passenger Data - Boxplot")

#------Identifikasi Model--------#
#Transformasi Logaritma
lnAPM=log(USpass.ts[,2])

#Plot Hasil Transformasi Logaritma
ts.plot(lnAPM, main = "Monthly logged U.S. Air Passenger Data", xlab = "Year", ylab = "ln(APM)")

#Deskriptif ln US Pass
monthplot(lnAPM,ylab="Air Passenger(miles)")
boxplot(lnAPM ~ cycle(lnAPM), xlab = "Month", ylab = "ln(APM)", main = "Monthly U.S. Air Passenger Data - Boxplot")

#Tes Kehomogenan Ragam
fligner.test(USpass.ts[,1]~Passengers, data=USpass)
fligner.test(log(USpass.ts[,1])~Passengers, data=USpass)

#Pembagian data lnAPM menjadi training-testing
lpass<- subset(lnAPM,start=1,end=132)
test <- subset(lnAPM,start=133,end=144)



#Identifikasi kestasioneran data ln US Pass
acf0<-acf(lpass,main="ACF",lag.max=48,xaxt="n")
axis(1, at=0:48/12, labels=0:48)

acf0$lag=acf0$lag*12
acf0.1 <- as.data.frame(cbind(acf0$acf,acf0$lag))
acf0.2 <- acf0.1[which(acf0.1$V2%%12==0),]
barplot(height = acf0.2$V1, names.arg=acf0.2$V2, ylab="ACF", xlab="Lag")


#Melakukan proses pembedaan pertama pada series non musiman data lnAPM
diff1.lpass=diff(lpass)
plot(diff1.lpass, main = "Time series plot of lpas d=1")

#Identifikasi kestasioneran data ln US Pass
acf1 <- acf(diff1.lpass,lag.max=48,xaxt="n", main="ACF d1")
axis(1, at=0:48/12, labels=0:48)

pacf1 <- pacf(diff1.lpass,lag.max=48,xaxt="n", main="PACF d1")
axis(1, at=0:48/12, labels=0:48)

acf1$lag <- acf1$lag * 12
acf1.1 <- as.data.frame(cbind(acf1$acf,acf1$lag))
acf1.2 <- acf1.1[which(acf1.1$V2%%12==0),]
barplot(height=acf1.2$V1,names.arg=acf1.2$V2,ylab="ACF",xlab="lag")


#Melakukan proses pembedaan pada series musiman
diff12.lpass <- diff(lpass,12)
plot(diff12.lpass, main = "Time series plot of ln(APM) D=12")
axis(1, at=0:48/12, labels=0:48)

acf2<- acf(diff12.lpass,lag.max=48,xaxt="n", main="ACF d1D1")
axis(1, at=0:48/12, labels=0:48)

pacf2<- pacf(diff12.lpass,lag.max=48,xaxt="n", main="PACF d1D1")
axis(1, at=0:48/12, labels=0:48)

acf2$lag <- acf2$lag * 12
acf2.1 <- as.data.frame(cbind(acf2$acf,acf2$lag))
acf2.2 <- acf2.1[which(acf2.1$V2%%12==0),]
barplot(height=acf2.2$V1,names.arg=acf2.2$V2,ylab="ACF", xlab="Lag")

#Melakukan pembedaan pertama pada data yang telah dilakukan pembedaan pada series musimannya
diff12.1lpass <- diff(diff12.lpass,1)
plot(diff12.lpass, main = "Time series plot of ln(APM) d=1, D=12")

#Identifikasi Model
acf2(diff12.1lpass,48)

eacf(diff12.lpass)

auto.arima(lpass)

#Pendugaan Parameter
model1 <- Arima(lpass,order=c(0,1,1),seasonal=c(0,1,1))
summary(model1)

model2<-Arima(lpass,order=c(0,1,1),seasonal=c(1,1,0))
summary(model2)

model3<-Arima(lpass,order=c(0,1,1),seasonal=c(2,1,0))
summary(model3)

#Seleksi Model
#Pengujian Parameter Model
printstatarima <- function (x, digits = 4,se=TRUE){
  if (length(x$coef) > 0) {
    cat("\nCoefficients:\n")
    coef <- round(x$coef, digits = digits)
    if (se && nrow(x$var.coef)) {
      ses <- rep(0, length(coef))
      ses[x$mask] <- round(sqrt(diag(x$var.coef)), digits = digits)
      coef <- matrix(coef, 1, dimnames = list(NULL, names(coef)))
      coef <- rbind(coef, s.e. = ses)
      statt <- coef[1,]/ses
      pval  <- 2*pt(abs(statt), df=length(x$residuals)-1, lower.tail = FALSE)
      coef <- rbind(coef, t=round(statt,digits=digits),sign.=round(pval,digits=digits))
      coef <- t(coef)
    }
    print.default(coef, print.gap = 2)
  }
}

printstatarima(model1)
printstatarima(model2)
printstatarima(model3)

#Diagnostik Model
#checkresiduals(model1,lag=c(10,30))
tsdisplay(residuals(model1), lag.max=45, main='ARIMA(0,1,1)(0,1,1)12 Model Residuals')

#korelasi
ljbtest <- LjungBox(residuals(model1),lags=seq(5,30,5),order=0,season=1,squared.residuals=FALSE)
ljbtest

#Kenormalan
jarque.bera.test(residuals(model1))

#Melakukan Peramalan menggunakan Model Terbaik
#Validasi Model
ramalan_arima = forecast(model1, 12)
accuracy(ramalan_arima,test)

plot(ramalan_arima)
