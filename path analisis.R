#TUGAS PROJECT
#PATH ANALISIS

#LIBRARY YANG DIGUNAKAN
library(lavaan)
library(semPlot)
library(OpenMx)
library(tidyverse)
library(knitr)
library(kableExtra)
library(GGally)
library(corrplot)

#INPUT DATA
data_path <- read_excel("D:/KULIAH/KULIAH SEMESTER 4/ANREG/data path.xlsx")
data.frame(data_path)
data=data.frame(data_path)

#PLOT KORELASI
cor1 = cor(data_path)
corrplot(cor1, method = 'square')

ggcorr(data_path[c(2,3,4)], nbreaks = 6, label = T, low = "red3", high = "green3", 
       label_round = 2, name = "Correlation Scale", label_alpha = T, hjust = 0.75) +
  ggtitle(label = "Correlation Plot") +
  theme(plot.title = element_text(hjust = 0.6))

#MODEL PATH
model <-'
Loyalitas ~ Pelayanan +Kepuasan
Pelayanan ~ Kepuasan
'
fit <- cfa(model, data = data_path)
summary(fit, fit.measures = TRUE, standardized=T,rsquare=T)

#Struktur
semPaths(fit, 'std', layout = 'circle')
semPaths(fit,"std",layout = 'tree', edge.label.cex=.9, curvePivot = TRUE)


semPaths(fit, 'std', 'est', curveAdjacent = TRUE, style = "lisrel")