#Project Ekonometrika
#Regresi Panel

#Package yang digunakan
library(readxl)
library(plm)
library(corrplot)
library(knitr)
library(kableExtra)
library(htmltools)
library(tidyverse)
library(scales)
library(ExPanDaR)
library(plotly)
library(hrbrthemes)
library(car)
library(lmtest)
library(performance)

## Memanggil data ##
data <- read_excel("D:/KULIAH/Kuliah Semester 6/EKONOMETRIK/DATA/DATA PANEL.xlsx")
data %>% kbl(format = "html", caption= "Data Kemiskinan Indonesia Tahun 2016-2020",align = 'c',longtable = 'T',) %>% kable_material(full_width=F,c("striped", "hover", "condensed", "responsive")) %>% scroll_box(width="100%", height="400px") 

str(data)

### EKSPLORASI DATA ###

#Plot Korelasi
data2 <- data[,-c(1,2)]
data2 %>% kbl(format = "html", caption= "Data Kemiskinan Di Indonesia Tahun 2016-2020",
              align = 'c',longtable = 'T',) %>% kable_material(
                full_width=F,c("striped", "hover", "condensed", "responsive"
                               )) %>% scroll_box(width="100%", height="400px") 

corr_matrix <- round(cor(data2), 2)

# membuat plot korelasi
corrplot(corr_matrix, 
         type="lower",
         method = "color", 
         tl.cex = 0.5, 
         tl.col = "black",
         addCoef.col = "#2F2F2F",
         addCoefasPercent = FALSE,
         number.cex = 0.5,
         diag = FALSE)

###Persentase penduduk miskin per tahun
# Plot
data %>%
  tail(5) %>%
  ggplot( aes(x=Tahun, y=Kemiskinan)) +
  geom_line( color="Blue", size=1) +
  geom_point(shape=21, color="black", fill="#69b3a2", size=4) +
  scale_x_continuous(limits = c(2016, 2020), breaks = seq(2016, 2020,1))+
  theme_bw() +
  theme(plot.title=element_text(size=15))+
  labs(title="Persentase Penduduk Miskin di Indonesia", subtitle =" Tahun 2016-2020")+
  theme(plot.title = element_text(size = 14L, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11L, face = "plain", hjust = 0.5))


##Nilai Pembangunan Ekonomi per tahun
# Plot
data %>%
  tail(5) %>%
  ggplot( aes(x=Tahun, y=PE)) +
  geom_line( color="cyan4", size=1) +
  geom_point(shape=21, color="darkred", fill="darkorange", size=4) +
  scale_x_continuous(limits = c(2016, 2020), breaks = seq(2016, 2020, 1))+
  theme_bw() +
  theme(plot.title=element_text(size=10))+
  labs(title="Persentase Pertumbuhan Ekonomi di Indonesia", subtitle =" Tahun 2008-2020")+
  theme(plot.title = element_text(size = 14L, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11L, face = "plain", hjust = 0.5))

###Persentase IPM per tahun
# Plot
data %>%
  tail(5) %>%
  ggplot( aes(x=Tahun, y=IPM)) +
  geom_line( color="bisque4", size=1) +
  geom_point(shape=21, color="darkred", fill="bisque", size=4) +
  scale_x_continuous(limits = c(2016, 2020), breaks = seq(2016, 2020, 1))+
  theme_bw() +
  theme(plot.title=element_text(size=15))+
  labs(title="Persentase Indeks Pembangunan Manusia di Indonesia", subtitle =" Tahun 2016-2020")+
  theme(plot.title = element_text(size = 14L, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11L, face = "plain", hjust = 0.5))

##Persentase Inflasi per tahun
# Plot
data %>%
  tail(5) %>%
  ggplot( aes(x=Tahun, y=INF)) +
  geom_line( color="darkturquoise", size=1) +
  geom_point(shape=21, color="darkslategray", fill="darkslategray4", size=4) +
  scale_x_continuous(limits = c(2016, 2020), breaks = seq(2016, 2020, 1))+
  theme_bw() +
  theme(plot.title=element_text(size=15))+
  labs(title="Persentase Inflasi di Indonesia", subtitle =" Tahun 2016-2020")+
  theme(plot.title = element_text(size = 14L, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11L, face = "plain", hjust = 0.5))

## pengangguran per tahun
# Plot
data %>%
  tail(5) %>%
  ggplot( aes(x=Tahun, y=PSGR)) +
  geom_line( color="azure4", size=1) +
  geom_point(shape=21, color="antiquewhite3", fill="coral3", size=4) +
  scale_x_continuous(limits = c(2016, 2020), breaks = seq(2016, 2020, 1))+
  theme_bw() +
  theme(plot.title=element_text(size=15))+
  labs(title="Persentase Pengangguran di Indonesia", subtitle =" Tahun 2016-2020")+
  theme(plot.title = element_text(size = 14L, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11L, face = "plain", hjust = 0.5))

##PENGECEKAN MULTIKOLINEARITAS##
model.ols <- lm(Kemiskinan ~ ., data=data2)
vif(model.ols)

##Model CEM (Common Effect Model)##
cem <- plm(Kemiskinan ~ PE+IPM+INF+PSGR, data=data, model="pooling")
summary(cem)

##Asumsi Normalitas
res <- residuals(cem)
ks.test(res, "pnorm")

#PLot
hist(res, 
     xlab = "sisaan",
     col = "steelblue", 
     breaks=30,  
     prob = TRUE) 
lines(density(res), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")

#Asumsi Autokorelasi
bgtest(cem)

#Asumsi Homoskedastisitas
bptest(cem)

##Model FEM (Fixed Effect Model)##
fem <- plm(Kemiskinan ~ PE+IPM+INF+PSGR, data, model = "within",index = c("Provinsi","Tahun"))
summary(fem)


#Asumsi normalitas
res.fem <- residuals(fem)
ks.test(res.fem, "pnorm")

#Plot
hist(res.fem, 
     xlab = "sisaan",
     col = "steelblue", 
     breaks=30,  
     prob = TRUE) 
lines(density(res), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")

#Asumsi Autokorelasi
bgtest(fem)

#Asumsi Homoskedastisitas
bptest(fem)

##Model REM (Random Effect Model)##
rem <- plm(Kemiskinan~PE+IPM+INF+PSGR, data, model = "random", index = c("Provinsi","Tahun"))
summary(rem)

#Asumsi Normalitas
res.rem <- residuals(rem)
ks.test(res.rem, "pnorm")

#plot
hist(res.rem, 
     xlab = "sisaan",
     col = "steelblue", 
     breaks=30,  
     prob = TRUE) 
lines(density(res), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")

#Asumsi Autokorelasi
bgtest(rem)

#Asumsi Homoskedastisitas
bptest(rem)

##UJI CHOW##
pooltest(cem,fem)


##UJI HAUSMAN##
phtest(fem,rem)


##FEM (Fixed Effect Model)##
m=plm(Kemiskinan~PE+IPM+INF+PSGR, data, model = "within", index = c("Provinsi","Tahun"))

#uji efek individu maupun waktu
plmtest(m, effect = "twoways", type = "bp")

# uji efek individu 
plmtest(m,effect = "individual")

#uji efek waktu
plmtest(m,effect = "time",type = "bp")


##FEM LSDV##
fem.lsdv <- lm((Kemiskinan) ~ PE+IPM+INF+PSGR + factor(Provinsi) + factor(Tahun), data)
summary(fem.lsdv)

#Asumsi normalitas
res.lsdv <- residuals(fem.lsdv)
ks.test(res.lsdv, "pnorm")

#plot
hist(res.lsdv, 
     xlab = "sisaan",
     col = "steelblue", 
     breaks=30,  
     prob = TRUE) 
lines(density(res), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")

#Autokorelasi
bgtest(fem.lsdv)

#Homoskedastisitas
bptest(fem.lsdv)

##PEMILIHAN MODEL TERBAIK##
model_performance(cem,metrics = "all")

model_performance(fem,metrics = "all")

model_performance(rem,metrics = "all")

model_performance(fem.lsdv,metrics = "all")

# Membuat data frame
perbandingan <- data.frame(Model = c("CEM", "FEM Within", "REM ", "FEM LSDV"), 
                           NILAI.AIC = c(977.208,124.898,172.145, 184.959), 
                           ADJUSTED.R.SQUARE = c(0.450,0.458,0.530,0.996),
                           RMSE = c(4.137, 0.339,0.388,0.324))
perbandingan <- kable(perbandingan, caption = "Perbandingan Performa Model") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
perbandingan

summary(fem.lsdv)



