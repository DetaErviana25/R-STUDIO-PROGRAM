setwd("D:\\SKRIPSI DETA\\DATA SKRIPSI")

#Package Uji Asumsi
library(lmtest)
library(MASS)
library(car) #Uji Multikol
library(plm) #Analisis Data Panel
library(skedastic) #Uji Multikol
library(lawstat) #uji autokorelasi
library(tseries) #uji normalitas
library(olsrr) #uji multikol
#Package Analisis Spasial
library(GWmodel) #geographically weighted model#
library(sp)
library(spdep) #untuk pembobotan#
library(spgwr) #geographically weighted regression (GWR)#
#Package Pemetaan
library(plotly)
library(sf)
library(ggplot2)
library(ggpubr)
library(rgdal)
library(corrplot)
#Package Penggabungan Data
library(tidyr)
library(dplyr)
library(readxl)

#IMPORT DATA
Data.mapping=read_excel("Data_Panel_IPMIDN.xlsx")
Data.Panel=read_excel("Data_Panel_IPM_Indonesia.xlsx")
within.trans=read_excel("Data_GWR_PANEL_IPM.xlsx")

#---------------------------------------------
#             EKSPLORASI DATA 
#---------------------------------------------
summary(Data.Panel)

#Keragaman Antar Waktu
ggplot(data = Data.Panel, aes(x = Tahun, y = IPM)) +
  geom_line() +
  labs(x = "Tahun", y = "Jumlah IPM") +
  theme(legend.position = "none") +
  theme_bw()

#Keragaman Antar Individu
gplots::plotmeans(IPM ~ `PROVINSI`, main="Keragaman Antar Jumlah IPM", Data.Panel, n.label = F, xlab = "a")

#Plot Korelasilibrary(ggplot2)
corrplot(cor(Data.Panel[-c(1:6)]), method="color", type = "upper", tl.pos = 'tp')
corrplot(cor(Data.Panel[-c(1:6)]), method="number", diag=F, add=T, type = "lower",
         tl.pos = 'n', cl.pos = 'n')


#--------------Eksplorasi Data Aktual dengan Pemetaan-----------------#
#----------------------------------------------------------------------#
##IMPORT PETA SHP
shp.IDN=read_sf("BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL.shp")

#Menggabungkan Data ke file SHP
gabung.IDN=left_join(shp.IDN,Data.mapping,by="OBJECTID")
View(gabung.IDN)

#Pemetaan IPM dalam 6 TAHUN
plot.IDN1 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = IPM_2017),color="white")+ 
  labs(title="IPM INDO Tahun 2017")

plot.IDN2 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = IPM_2018),color="white") + 
  labs(title="IPM INDO Tahun 2018")

plot.IDN3 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = IPM_2019),color="white") + 
  labs(title="IPM INDO Tahun 2019")

plot.IDN4 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = IPM_2020),color="white") + 
  labs(title="IPM INDO Tahun 2020")

plot.IDN5 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = IPM_2021),color="white") + 
  labs(title="IPM INDO Tahun 2021")

plot.IDN6 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = IPM_2022),color="white") + 
  labs(title="IPM INDO Tahun 2022")

#Pemetaan Variabel Y
varY <- gabung.IDN %>% select("IPM_2017","IPM_2018","IPM_2019","IPM_2020","IPM_2021","IPM_2022",geometry) %>% 
  gather(VAR, IPM, -geometry)%>%
  mutate(IPM.INDO = cut_number(IPM, n = 4,dig.lab=3 ))

ggplot(data = varY, aes(fill = IPM.INDO)) + 
  geom_sf() + 
  facet_wrap(~VAR, ncol = 2) +
  #geom_sf_text(aes(label = V1), colour = "black",size=2)+
  scale_fill_brewer(type = "seq", palette = "YlGnBu")

#Pemetaan TPT dalam 6 TAHUN
plot.IDN1 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = TPT_2017),color="white")+ 
  labs(title="TPT INDO Tahun 2017")

plot.IDN2 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = TPT_2018),color="white") + 
  labs(title="TPT INDO Tahun 2018")

plot.IDN3 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = TPT_2019),color="white") + 
  labs(title="TPT INDO Tahun 2019")

plot.IDN4 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = TPT_2020),color="white") + 
  labs(title="TPT INDO Tahun 2020")

plot.IDN5 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = TPT_2021),color="white") + 
  labs(title="TPT INDO Tahun 2021")

plot.IDN6 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = TPT_2022),color="white") + 
  labs(title="TPT INDO Tahun 2022")

#Pemetaan Variabel Y
varY <- gabung.IDN %>% select("TPT_2017","TPT_2018","TPT_2019","TPT_2020","TPT_2021","TPT_2022",geometry) %>% 
  gather(VAR, TPT, -geometry)%>%
  mutate(TPT.INDO = cut_number(TPT, n = 4,dig.lab=3 ))

ggplot(data = varY, aes(fill = TPT.INDO)) + 
  geom_sf() + 
  facet_wrap(~VAR, ncol = 2) +
  #geom_sf_text(aes(label = V1), colour = "black",size=2)+
  scale_fill_brewer(type = "seq", palette = "YlGnBu")
  

#Pemetaan Pengeluaran Perkapita dalam 6 TAHUN
plot.IDN1 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = PP_2017),color="white")+ 
  labs(title="PP INDO Tahun 2017")

plot.IDN2 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = PP_2018),color="white") + 
  labs(title="PP INDO Tahun 2018")

plot.IDN3 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = PP_2019),color="white") + 
  labs(title="PP INDO Tahun 2019")

plot.IDN4 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = PP_2020),color="white") + 
  labs(title="PP INDO Tahun 2020")

plot.IDN5 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = PP_2021),color="white") + 
  labs(title="PP INDO Tahun 2021")

plot.IDN6 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = PP_2022),color="white") + 
  labs(title="PP INDO Tahun 2022")

#Pemetaan Variabel Y
varY <- gabung.IDN %>% select("PP_2017","PP_2018","PP_2019","PP_2020","PP_2021","PP_2022",geometry) %>% 
  gather(VAR, PP, -geometry)%>%
  mutate(PP.INDO = cut_number(PP, n = 4,dig.lab=3 ))

ggplot(data = varY, aes(fill = PP.INDO)) + 
  geom_sf() + 
  facet_wrap(~VAR, ncol = 2) +
  #geom_sf_text(aes(label = V1), colour = "black",size=2)+
  scale_fill_brewer(type = "seq", palette = "YlGnBu")

#Pemetaan JUMLAH PENDUDUK dalam 6 TAHUN
plot.IDN1 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = JPM_2017),color="white")+ 
  labs(title="JPM INDO Tahun 2017")

plot.IDN2 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = JPM_2018),color="white") + 
  labs(title="JPM INDO Tahun 2018")

plot.IDN3 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = JPM_2019),color="white") + 
  labs(title="JPM INDO Tahun 2019")

plot.IDN4 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = JPM_2020),color="white") + 
  labs(title="JPM INDO Tahun 2020")

plot.IDN5 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = JPM_2021),color="white") + 
  labs(title="JPM INDO Tahun 2021")

plot.IDN6 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = JPM_2022),color="white") + 
  labs(title="JPM INDO Tahun 2022")

#Pemetaan Variabel Y
varY <- gabung.IDN %>% select("JPM_2017","JPM_2018","JPM_2019","JPM_2020","JPM_2021","JPM_2022",geometry) %>% 
  gather(VAR, JPM, -geometry)%>%
  mutate(JPM.INDO = cut_number(JPM, n = 4,dig.lab=3 ))

ggplot(data = varY, aes(fill = JPM.INDO)) + 
  geom_sf() + 
  facet_wrap(~VAR, ncol = 2) +
  #geom_sf_text(aes(label = V1), colour = "black",size=2)+
  scale_fill_brewer(type = "seq", palette = "YlGnBu")

#Pemetaan INDEKS KEMAHALAN KONSTRUKSI dalam 6 TAHUN
plot.IDN1 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = IKK_2017),color="white")+ 
  labs(title="IKK INDO Tahun 2017")

plot.IDN2 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = IKK_2018),color="white") + 
  labs(title="IKK INDO Tahun 2018")

plot.IDN3 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = IKK_2019),color="white") + 
  labs(title="IKK INDO Tahun 2019")

plot.IDN4 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = IKK_2020),color="white") + 
  labs(title="IKK INDO Tahun 2020")

plot.IDN5 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = IKK_2021),color="white") + 
  labs(title="IKK INDO Tahun 2021")

plot.IDN6 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = IKK_2022),color="white") + 
  labs(title="IKK INDO Tahun 2022")

#Pemetaan Variabel Y
varY <- gabung.IDN %>% select("IKK_2017","IKK_2018","IKK_2019","IKK_2020","IKK_2021","IKK_2022",geometry) %>% 
  gather(VAR, IKK, -geometry)%>%
  mutate(IKK.INDO = cut_number(IKK, n = 4,dig.lab=3 ))

ggplot(data = varY, aes(fill = IKK.INDO)) + 
  geom_sf() + 
  facet_wrap(~VAR, ncol = 2) +
  #geom_sf_text(aes(label = V1), colour = "black",size=2)+
  scale_fill_brewer(type = "seq", palette = "Purples")

#Pemetaan ANGKA PARTISIPASI SEKOLAH dalam 6 TAHUN
plot.IDN1 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = APS_2017),color="white")+ 
  labs(title="APS INDO Tahun 2017")

plot.IDN2 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = APS_2018),color="white") + 
  labs(title="APS INDO Tahun 2018")

plot.IDN3 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = APS_2019),color="white") + 
  labs(title="APS INDO Tahun 2019")

plot.IDN4 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = APS_2020),color="white") + 
  labs(title="APS INDO Tahun 2020")

plot.IDN5 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = APS_2021),color="white") + 
  labs(title="APS INDO Tahun 2021")

plot.IDN6 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = APS_2022),color="white") + 
  labs(title="APS INDO Tahun 2022")

#Pemetaan Variabel Y
varY <- gabung.IDN %>% select("APS_2017","APS_2018","APS_2019","APS_2020","APS_2021","APS_2022",geometry) %>% 
  gather(VAR, APS, -geometry)%>%
  mutate(APS.INDO = cut_number(APS, n = 4,dig.lab=3 ))

ggplot(data = varY, aes(fill = APS.INDO)) + 
  geom_sf() + 
  facet_wrap(~VAR, ncol = 2) +
  #geom_sf_text(aes(label = V1), colour = "black",size=2)+
  scale_fill_brewer(type = "seq", palette = "Blues")

#Pemetaan RATA-RATA LAMA SEKOLAH dalam 6 TAHUN
plot.IDN1 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = RRLS_2017),color="white")+ 
  labs(title="RRLS INDO Tahun 2017")

plot.IDN2 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = RRLS_2018),color="white") + 
  labs(title="RRLS INDO Tahun 2018")

plot.IDN3 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = RRLS_2019),color="white") + 
  labs(title="RRLS INDO Tahun 2019")

plot.IDN4 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = RRLS_2020),color="white") + 
  labs(title="IPM di Indonesia Tahun 2020")

plot.IDN5 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = RRLS_2021),color="white") + 
  labs(title="RRLS INDO Tahun 2021")

plot.IDN6 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = RRLS_2022),color="white") + 
  labs(title="RRLS INDO Tahun 2022")

#Pemetaan Variabel Y
varY <- gabung.IDN %>% select("RRLS_2017","RRLS_2018","RRLS_2019","RRLS_2020","RRLS_2021","RRLS_2022",geometry) %>% 
  gather(VAR, RRLS, -geometry)%>%
  mutate(RRLS.INDO = cut_number(RRLS, n = 4,dig.lab=3 ))

ggplot(data = varY, aes(fill = RRLS.INDO)) + 
  geom_sf() + 
  facet_wrap(~VAR, ncol = 2) +
  #geom_sf_text(aes(label = V1), colour = "black",size=2)+
  scale_fill_brewer(type = "seq", palette = "Greens")

#Pemetaan PERSEDIAAN SUMBER AIR MINUM dalam 6 TAHUN
plot.IDN1 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = PSAM_2017),color="white")+ 
  labs(title="PSAM INDO Tahun 2017")

plot.IDN2 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = PSAM_2018),color="white") + 
  labs(title="PSAM INDO Tahun 2018")

plot.IDN3 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = PSAM_2019),color="white") + 
  labs(title="PSAM INDO Tahun 2019")

plot.IDN4 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = PSAM_2020),color="white") + 
  labs(title="PSAM INDO Tahun 2020")

plot.IDN5 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = PSAM_2021),color="white") + 
  labs(title="PSAM INDOa Tahun 2021")

plot.IDN6 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = PSAM_2022),color="white") + 
  labs(title="PSAM INDO Tahun 2022")

#Pemetaan Variabel Y
varY <- gabung.IDN %>% select("PSAM_2017","PSAM_2018","PSAM_2019","PSAM_2020","PSAM_2021","PSAM_2022",geometry) %>% 
  gather(VAR, PSAM, -geometry)%>%
  mutate(PSAM.INDO = cut_number(PSAM, n = 4,dig.lab=3 ))

ggplot(data = varY, aes(fill = PSAM.INDO)) + 
  geom_sf() + 
  facet_wrap(~VAR, ncol = 2) +
  #geom_sf_text(aes(label = V1), colour = "black",size=2)+
  scale_fill_brewer(type = "seq", palette = "Greens")

#Pemetaan GINI RATIO dalam 6 TAHUN
plot.IDN1 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = GR_2017),color="white")+ 
  labs(title="GR INDO Tahun 2017")

plot.IDN2 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = GR_2018),color="white") + 
  labs(title="GR INDO Tahun 2018")

plot.IDN3 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = GR_2019),color="white") + 
  labs(title="GR INDO Tahun 2019")

plot.IDN4 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = GR_2020),color="white") + 
  labs(title="GR INDO Tahun 2020")

plot.IDN5 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = GR_2021),color="white") + 
  labs(title="GR INDO Tahun 2021")

plot.IDN6 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = GR_2022),color="white") + 
  labs(title="GR INDO Tahun 2022")

#Pemetaan Variabel Y
varY <- gabung.IDN %>% select("GR_2017","GR_2018","GR_2019","GR_2020","GR_2021","GR_2022",geometry) %>% 
  gather(VAR, GR, -geometry)%>%
  mutate(GR.INDO = cut_number(GR, n = 4,dig.lab=3 ))

ggplot(data = varY, aes(fill = GR.INDO)) + 
  geom_sf() + 
  facet_wrap(~VAR, ncol = 2) +
  #geom_sf_text(aes(label = V1), colour = "black",size=2)+
  scale_fill_brewer(type = "seq", palette = "Oranges")

#---------------------------------------------------
#         Pengujian Multikolinearitas
#---------------------------------------------------
#multikolinearitas
model<-lm(IPM ~ TPT + PP + JPM + IKK + APS + RRLS + PSAM + GR, data=Data.Panel)
ols_vif_tol(model)

#----------------------------------------------------------------------#
#                        UJI EFEK SPASIAL
#----------------------------------------------------------------------#
#Uji pengaruh waktu
plmtest(IPM ~ TPT + PP + JPM + IKK + APS + RRLS + PSAM + GR,
        data=Data.Panel,type="bp",effect = "time",index = c("No","Tahun"))
#Uji pengaruh Lokasi
plmtest(IPM ~ TPT + PP + JPM + IKK + APS + RRLS + PSAM + GR,data=Data.Panel,type="bp",effect = "individual",index = c("No","Tahun"))
#Uji pengaruh gabungan
plmtest(IPM ~ TPT + PP + JPM + IKK + APS + RRLS + PSAM + GR,data=Data.Panel,type="bp",effect = "twoways",index = c("No","Tahun"))

#----------------------------------------------------------------------------
### MODEL PLS, FIXED, dan RANDOM
#----------------------------------------------------------------------------
#Model PLS
modelpanel1<-plm(IPM ~ TPT + PP + JPM + IKK + APS + RRLS + PSAM + GR,data=Data.Panel,model="pooling",
                 index = c("No","Tahun"))
summary(modelpanel1)

##Model Fixed (FEM)
modelpanel2<-plm(IPM ~ TPT + PP + JPM + IKK + APS + RRLS + PSAM + GR, data=Data.Panel,model="within", effect= "individual",
                 index = c("No","Tahun"))
summary(modelpanel2)

#Model Random (REM)
modelpanel3<-plm(IPM ~ TPT + PP + JPM + IKK + APS + RRLS + PSAM + GR,data=Data.Panel,model="random", 
                 index = c("No","Tahun"))
summary(modelpanel3)

#----------------------------------------------------------
#                      UJI PEMILIHAN MODEL
#----------------------------------------------------------
### uji chow ### (membandingkan OLS dengan FEM)
pFtest(modelpanel2,modelpanel1)

### uji hausman ### (membandingkan FEM dengan REM)
phtest(modelpanel2,modelpanel3)

#-----------------------------------------
#             Uji Asumsi Klasik                 
#-----------------------------------------
#Uji Normalitas
res.fem <- residuals(modelpanel2)
(normal <- jarque.bera.test(res.fem))

#Uji Autokorelasi
lawstat::runs.test(resid(modelpanel2))

##Dengan anggapan bahwa model adalah FEM sehingga uji BP_Test langsung modelpanel2
#Uji Keragaman Spasial
bptest(modelpanel2,studentize = FALSE)

#------------------------------------------------------------------------#
#                              GWR PANEL
#------------------------------------------------------------------------#
#PEMODELAN GWR PANEL
#Merubah data ke Spasial Titik Data Frame
data.sp.GWPR=within.trans
coordinates(data.sp.GWPR)=4:5 #kolom 4 dan 5 menyatakan letak Long-Lat
class(data.sp.GWPR)  
head(data.sp.GWPR)

#Menentuian Bandwidth yang digunakan
# Adaptive Gaussian
bwd.GWPRGAUS<-bw.gwr(IPM ~ TPT + PP + JPM + IKK + APS + RRLS + PSAM + GR,data = data.sp.GWPR, approach = "CV",kernel = "gaussian",adaptive = T)
hasil.GWPRGAUS<-gwr.basic(IPM ~ TPT + PP + JPM + IKK + APS + RRLS + PSAM + GR,data = data.sp.GWPR,bw=bwd.GWPRGAUS, kernel = "gaussian",adaptive = T)
summary(hasil.GWPRGAUS)

# Adaptive Bisquare
bwd.GWPRBISQUR<-bw.gwr(IPM ~ TPT + PP + JPM + IKK + APS + RRLS + PSAM + GR,data = data.sp.GWPR, approach = "CV",kernel = "bisquare",adaptive = T)
hasil.GWPRBISQUR<-gwr.basic(IPM ~ TPT + PP + JPM + IKK + APS + RRLS + PSAM + GR,data = data.sp.GWPR,bw=bwd.GWPRBISQUR, kernel = "bisquare",adaptive = T)
summary(hasil.GWPRBISQUR)

# Adaptive Exponential
bwd.GWPREXPONENTIAL<-bw.gwr(IPM ~ TPT + PP + JPM + IKK + APS + RRLS + PSAM + GR,data = data.sp.GWPR, approach = "CV",kernel = "exponential",adaptive = T)
hasil.GWPREXPONENTIAL<-gwr.basic(IPM ~ TPT + PP + JPM + IKK + APS + RRLS + PSAM + GR,data = data.sp.GWPR,bw=bwd.GWPREXPONENTIAL, kernel = "exponential",adaptive = T)
summary(hasil.GWPREXPONENTIAL)

#Perbandingan Bandwith
Diagnostic <- cbind(hasil.GWPRGAUS$GW.diagnostic,
                    hasil.GWPRBISQUR$GW.diagnostic,
                    hasil.GWPREXPONENTIAL$GW.diagnostic)
colnames(Diagnostic) <- c("Gaussian","Bisquare","Exponential")
Diagnostic

#----------------------------------------------------------------------------
##VERSI 1
# penduga parameter
#Parameter.GWPR<-cbind(hasil.GWPR$SDF$Intercept, hasil.GWPR$SDF$X1, hasil.GWPR$SDF$X2, 
#                      hasil.GWPR$SDF$X3, hasil.GWPR$SDF$X4, hasil.GWPR$SDF$X5)
Parameter.GWPR <- hasil.GWPRBISQUR$SDF[,1:15]@data
View(Parameter.GWPR)
write.table(Parameter.GWPR,file="D:\\SKRIPSI DETA\\SKRIPSI\\OUTPUT\\Hasil_Parameter_GWPR.csv",sep=";")

#P-Value
p.value.GWPR=gwr.t.adjust(hasil.GWPRBISQUR)$results$p
View(p.value.GWPR)
write.table(p.value.GWPR,file="D:\\SKRIPSI DETA\\SKRIPSI\\OUTPUT\\Hasil_P-Value_GWPR.csv",sep=";")

#Lokal R Square
local.R2.GWPR<-hasil.GWPRBISQUR$SDF$Local_R2
View(local.R2.GWPR)
write.table(local.R2.GWPR,file="D:\\SKRIPSI DETA\\SKRIPSI\\OUTPUT\\Hasil_R2 Lokal_GWPR.csv",sep=";")

###VERSI 2
# penduga parameter
#Estimasi Model GWPR Lokal (Provinsi)
longlat <- cbind(Data.Panel$LONG[1:34], Data.Panel$LAT[1:34])
Lokasi_Bandwidth <- gw.adapt(dp = longlat, fp = longlat,
                             quant = hasil.GWPRBISQUR$GW.arguments$bw/204) #204 jumlah data

#Nilai Estimasi Parameter, Std. Error dan T-Value Tiap Provinsi
Parameter_GWPR <- as.data.frame(hasil.GWPRBISQUR$SDF)
Parameter_GWPR[1:34,]
write.table(Parameter_GWPR,file="D:\\SKRIPSI DETA\\SKRIPSI\\OUTPUT\\Hasil_Parameter_GWPR.csv",sep=";")

#Nilai P-Value Masing-Masing Parameter Tiap Provinsi
PValue_GWPR <- as.data.frame(gwr.t.adjust(hasil.GWPRBISQUR)$results$p)
PValue_GWPR[1:34,]
write.table(PValue_GWPR,file="D:\\SKRIPSI DETA\\SKRIPSI\\OUTPUT\\Hasil_PValue_GWPR.csv",sep=",")

#Nilai R-Squared Dari Model Tiap Provinsi
RLocal_GWPR <- as.data.frame(hasil.GWPRBISQUR$SDF$Local_R2)
RLocal_GWPR[1:34,]
write.table(RLocal_GWPR,file="D:\\SKRIPSI DETA\\SKRIPSI\\OUTPUT\\Hasil_Rlocal_GWPR.csv",sep=",")

#Nilai Bandwidth setiap lokasi
longlat=cbind(Data.Panel$LONG[1:34],Data.Panel$LAT[1:34])
bwd.lokasi=gw.adapt(dp=longlat, fp=longlat,quant=hasil.GWPRBISQUR$GW.arguments$bw/34)
bwd.lokasi=as.data.frame((bwd.lokasi))
write.table(bwd.lokasi,file="D:\\SKRIPSI DETA\\SKRIPSI\\OUTPUT\\Bandwidth_Lokasi.csv")

#Pembuatan jarak euclidean
n <- 34 #jumlah wilayah
U <- Data.Panel$LONG #data Longitude
V <- Data.Panel$LAT #data Latitude
d <- matrix(0,n,n)
for (i in 1:n) {
  for (j in 1:n) {
    d[i,j] <- sqrt(((U[i]-U[j])^2)+((V[i]-V[j])^2))
  }
}
d
write.table(d,file="D:\\SKRIPSI DETA\\SKRIPSI\\OUTPUT\\Hasil_Jarak_Euclidean.csv",sep=";") 


#Menentukan Bobot Penimbang
i<-nrow(bwd.lokasi)
pembobotB<-matrix(nrow=34,ncol=34)
for(i in 1:34)
  for(j in 1:34)
  {pembobotB[i,j]=(1-(d[i,j]/bwd.lokasi[i,])**2)**2
  pembobotB[i,j]<-
    ifelse(d[i,j]<bwd.lokasi[i,],pembobotB[i,j],0)}

pembobotB

write.table(pembobotB,file="D:\\SKRIPSI DETA\\SKRIPSI\\OUTPUT\\Hasil_Pembobot.csv",sep=";") 


#Uji Kecocokan Model
Adaptbisquare_GWPR=gwr.sel(IPM ~ TPT + PP + JPM + IKK + APS + RRLS + PSAM + GR, data = within.trans,coords=cbind(within.trans$LONG,within.trans$LAT),
                    adapt=T,gweight=gwr.bisquare) 
GWPR=gwr(IPM ~ TPT + PP + JPM + IKK + APS + RRLS + PSAM + GR, 
         data = within.trans,adapt = Adaptbisquare_GWPR,coords=cbind(within.trans$LONG,within.trans$LAT),hatmatrix=TRUE,gweight=gwr.bisquare)
BFC02.gwr.test(GWPR) 
LMZ.F1GWR.test(GWPR)
LMZ.F2GWR.test(GWPR)
LMZ.F3GWR.test(GWPR)

#F tabel
ftabelgwr=qf(.95, df1=195.00, df2=156.96)
ftabelgwr

#T-HITUNG
#Menampilkan t-hitung
t_TPT=GWPR$SDF$TPT/GWPR$SDF$TPT_se 
t_PP=GWPR$SDF$PP/GWPR$SDF$PP_se 
t_JPM=GWPR$SDF$JPM/GWPR$SDF$JPM_se 
t_IKK=GWPR$SDF$IKK/GWPR$SDF$IKK_se 
t_APS=GWPR$SDF$APS/GWPR$SDF$APS_se 
t_RRLS=GWPR$SDF$RRLS/GWPR$SDF$RRLS_se 
t_PSAM=GWPR$SDF$JPM/GWPR$SDF$PSAM_se 
t_GR=GWPR$SDF$JPM/GWPR$SDF$GR_se 

#T-tabel
alpha=0.05
n=nrow(Data.Panel)
ttabel=qt(1-(alpha/2),df=n-1)
ttest=cbind(t_TPT, t_PP,t_JPM,t_IKK,t_APS,t_RRLS,t_PSAM,t_GR)

write.table(ttest,file="D:\\SKRIPSI DETA\\SKRIPSI\\OUTPUT\\hasil thitung.csv",sep=";") 


#----------------------------------------------------------------------------
#Export Hasil analisis GWPR ke Excel
#----------------------------------------------------------------------------
#Export Hasil analisis GWPR ke Excel
OBJECTID=within.trans$OBJECTID
output.GWPR=as.data.frame(cbind(OBJECTID,Parameter_GWPR,PValue_GWPR,RLocal_GWPR))
write.table(output.GWPR,"hasil GWR Panel.csv", sep=";")

#---------------------------------------------------------------#
#    Signifikansi variabel (misal variabel TPT)
#---------------------------------------------------------------#
output.GWPR$signfikansi_TPT <- NA

# Signifikan
output.GWPR[(output.GWPR$TPT_p <= 0.05), "signfikansi_TPT"] <- "Signifikan"
# Tidak Signifikan
output.GWPR[(output.GWPR$TPT_p >= 0.05), "signfikansi_TPT"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
#IMPORT PETA SHP
shp.IDN2=read_sf("BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL.shp")

#Menggabungkan Data ke file SHP
gabung.IDN2=left_join(shp.IDN2,output.GWPR,by="OBJECTID")
View(gabung.IDN2)

plot.IDN1 = ggplot(data=gabung.IDN2) +
  geom_sf(mapping=aes(fill =signfikansi_TPT)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="Signifikansi")

plot.IDN1

#---------------------------------------------------------------#
#    Signifikansi variabel (misal variabel PP)
#---------------------------------------------------------------#
output.GWPR$signfikansi_PP <- NA

# Signifikan
output.GWPR[(output.GWPR$PP_p <= 0.05), "signfikansi_PP"] <- "Signifikan"
# Tidak Signifikan
output.GWPR[(output.GWPR$PP_p >= 0.05), "signfikansi_PP"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
#IMPORT PETA SHP
shp.IDN2=read_sf("BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL.shp")

#Menggabungkan Data ke file SHP
gabung.IDN2=left_join(shp.IDN2,output.GWPR,by="OBJECTID")
View(gabung.IDN2)

plot.IDN2 = ggplot(data=gabung.IDN2) +
  geom_sf(mapping=aes(fill =signfikansi_PP)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="Signifikansi")

plot.IDN2

#---------------------------------------------------------------#
#    Signifikansi variabel (misal variabel JPM)
#---------------------------------------------------------------#
output.GWPR$signfikansi_JPM <- NA

# Signifikan
output.GWPR[(output.GWPR$JPM_p <= 0.05), "signfikansi_JPM"] <- "Signifikan"
# Tidak Signifikan
output.GWPR[(output.GWPR$JPM_p >= 0.05), "signfikansi_JPM"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
#IMPORT PETA SHP
shp.IDN2=read_sf("BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL.shp")

#Menggabungkan Data ke file SHP
gabung.IDN2=left_join(shp.IDN2,output.GWPR,by="OBJECTID")
View(gabung.IDN2)

plot.IDN3 = ggplot(data=gabung.IDN2) +
  geom_sf(mapping=aes(fill =signfikansi_JPM)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="Signifikansi")

plot.IDN3

#---------------------------------------------------------------#
#    Signifikansi variabel (misal variabel IKK)
#---------------------------------------------------------------#
output.GWPR$signfikansi_IKK <- NA

# Signifikan
output.GWPR[(output.GWPR$IKK_p <= 0.05), "signfikansi_IKK"] <- "Signifikan"
# Tidak Signifikan
output.GWPR[(output.GWPR$IKK_p >= 0.05), "signfikansi_IKK"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
#IMPORT PETA SHP
shp.IDN2=read_sf("BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL.shp")

#Menggabungkan Data ke file SHP
gabung.IDN2=left_join(shp.IDN2,output.GWPR,by="OBJECTID")
View(gabung.IDN2)

plot.IDN4 = ggplot(data=gabung.IDN2) +
  geom_sf(mapping=aes(fill =signfikansi_IKK)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="Signifikansi")

plot.IDN4

#---------------------------------------------------------------#
#    Signifikansi variabel (misal variabel APS)
#---------------------------------------------------------------#
output.GWPR$signfikansi_APS <- NA

# Signifikan
output.GWPR[(output.GWPR$APS_p <= 0.05), "signfikansi_APS"] <- "Signifikan"
# Tidak Signifikan
output.GWPR[(output.GWPR$APS_p >= 0.05), "signfikansi_APS"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
#IMPORT PETA SHP
shp.IDN2=read_sf("BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL.shp")

#Menggabungkan Data ke file SHP
gabung.IDN2=left_join(shp.IDN2,output.GWPR,by="OBJECTID")
View(gabung.IDN2)

plot.IDN5 = ggplot(data=gabung.IDN2) +
  geom_sf(mapping=aes(fill =signfikansi_APS)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="Signifikansi")

plot.IDN5

#---------------------------------------------------------------#
#    Signifikansi variabel (misal variabel RRLS)
#---------------------------------------------------------------#
output.GWPR$signfikansi_RRLS <- NA

# Signifikan
output.GWPR[(output.GWPR$RRLS_p <= 0.05), "signfikansi_RRLS"] <- "Signifikan"
# Tidak Signifikan
output.GWPR[(output.GWPR$RRLS_p >= 0.05), "signfikansi_RRLS"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
#IMPORT PETA SHP
shp.IDN2=read_sf("BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL.shp")

#Menggabungkan Data ke file SHP
gabung.IDN2=left_join(shp.IDN2,output.GWPR,by="OBJECTID")
View(gabung.IDN2)

plot.IDN6 = ggplot(data=gabung.IDN2) +
  geom_sf(mapping=aes(fill =signfikansi_RRLS)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="Signifikansi")

plot.IDN6

#---------------------------------------------------------------#
#    Signifikansi variabel (misal variabel PSAM)
#---------------------------------------------------------------#
output.GWPR$signfikansi_PSAM <- NA

# Signifikan
output.GWPR[(output.GWPR$PSAM_p <= 0.05), "signfikansi_PSAM"] <- "Signifikan"
# Tidak Signifikan
output.GWPR[(output.GWPR$PSAM_p >= 0.05), "signfikansi_PSAM"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
#IMPORT PETA SHP
shp.IDN2=read_sf("BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL.shp")

#Menggabungkan Data ke file SHP
gabung.IDN2=left_join(shp.IDN2,output.GWPR,by="OBJECTID")
View(gabung.IDN2)

plot.IDN7 = ggplot(data=gabung.IDN2) +
  geom_sf(mapping=aes(fill =signfikansi_PSAM)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="Signifikansi")

plot.IDN7

#---------------------------------------------------------------#
#    Signifikansi variabel (misal variabel GR)
#---------------------------------------------------------------#
output.GWPR$signfikansi_GR <- NA

# Signifikan
output.GWPR[(output.GWPR$GR_p <= 0.05), "signfikansi_GR"] <- "Signifikan"
# Tidak Signifikan
output.GWPR[(output.GWPR$GR_p >= 0.05), "signfikansi_GR"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
#IMPORT PETA SHP
shp.IDN2=read_sf("BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL.shp")

#Menggabungkan Data ke file SHP
gabung.IDN2=left_join(shp.IDN2,output.GWPR,by="OBJECTID")
View(gabung.IDN2)

plot.IDN8 = ggplot(data=gabung.IDN2) +
  geom_sf(mapping=aes(fill =signfikansi_GR)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="Signifikansi")

plot.IDN8

#---------------------------------------------------------------#
#    Plotting variabel signifikan
#---------------------------------------------------------------#
Signif=read.csv("Hasil_PValue_GWPR_Gabung.csv",sep = ";")
head(Signif)

#Gabung data GWR dengan SHP
#IMPORT PETA SHP
shp.indo=read_sf("BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL.shp")

#Menggabungkan Data ke file SHP
gabung.indo=left_join(shp.indo,Signif,by="OBJECTID")

#---------------------------------------------------------------#
#Pemetaan Gabungan dengan Label Provinsi
plot.indo= ggplot(data=gabung.indo) +
  geom_sf(mapping=aes(fill =gabung)) +
  scale_fill_discrete()+
  geom_sf_text(aes(label = PROVINSI.x),size=2.5, colour = "black")+
  labs(fill="Variabel yang signifikan")

plot.indo

#---------------------------------------------------------------#
#Pemetaan Gabungan Tanpa Label
plot.indo= ggplot(data=gabung.indo) +
  geom_sf(mapping=aes(fill =gabung)) +
  scale_fill_discrete()+
  labs(fill="Variabel yang signifikan")

plot.indo

