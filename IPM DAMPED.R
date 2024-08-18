#DOUBLE EXPONENTIAL SMOOTHING HOLT

#Memanggil Data
IPM <- read_excel("DATA PKL/Indeks Pembangunan Manusia.xlsx", 
                  sheet = "Sheet2")
View(IPM)


#Membuat Data Menjadi Time-series
IPM <- ts(IPM$Ipm, start = 2010,  frequency = 1)
ts.plot(IPM.ts, main = "IPM Kabupaten Lampung Timur Tahun 2010-2022", xlab = "Tahun")

holtb.IPM = HoltWinters(IPM.ts, alpha = NULL, beta = NULL, gamma = FALSE)
holtb.IPM
holtb.IPM$fitted

#Mengukur Akurasi Eror
sse = holtb.IPM$SSE
sse
mse = holtb.IPM$SSE/NROW(holtb.IPM$fitted)
mse
rmse = sqrt(mse)
rmse
mape = mean (abs((IPM.ts-holtb.IPM$fitted[,1])/IPM.ts), na.rm =TRUE)*100
mape

#Prediksi
pred.holtb = predict(holtb.IPM,5)
pred.holtb

#Plot Data 
plot(IPM, main = "IPM Kabupaten Lampung Timur", lwd = 2, col = "blue", 
     xlim = c(2010,2023),type = "o", pch = 20)
limitDate = end(IPM.ts)[1] + (end(IPM.ts)[2]-1)/frequency(IPM.ts)
abline(v=limitDate, lty = 2)
lines(holtb.IPM$fitted[,1],lwd = 2, col = "red", type = "o", pch = 15)

lines(pred.holtb, col = "red", type = "o", pch = 18)
legend("bottomright", legend = c("Data Aktual", "Fitted Value"),col=
         c("blue","red"), lty=1, cex = 0.6, inset = 0.02)

#DOUBLE EXPONENTIAL SMOOTHING DENGAN PARAMETER DAMPED

library(forecast)
holt.IPM2 = holt(IPM, h= 5, damped = TRUE, alpha = NULL, beta = NULL, phi = NULL)
holt.IPM2
holt.IPM2$model


holt.IPM2$fitted

#Mengukur Eror
mse = mean(holt.IPM2$residual^2)
mse
rmse = sqrt(mse)
rmse
mape = mean(abs(holt.IPM2$residual)/IPM.ts, na.rm = TRUE)*100
mape

#Plot Data
plot(holt.IPM2, main ='IPM Kabupaten Lampung Timur',col="purple", lwd =2)
lines(holt.IPM2$fitted, col = "red",lwd=2)
legend("bottomright", legend= c(" Data Aktual","Fitted Value", "Peramalan"), 
       col = c("purple", "red","green"), lty = 1, cex = 0.8 , inset = 0.02)

