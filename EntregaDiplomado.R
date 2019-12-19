library(dplyr)
library(ggplot2)
#==================================================================================
#Se realiza analisis exploratorio de datos para identifiar el contenido de los datos
#===================================================================================

blood <- read.csv("F:/examen diplomado/bloodPressure.csv")

#=========================
# Estructura de df de blood
#=========================
#    'data.frame':	10167 obs. of  6 variables:
#    $ Patient        : int  1369 1410 1156 663 1198 740 574 787 623 1116 ...
#  $ Systolic       : int  113 91 91 114 100 96 111 132 141 107 ...
#  $ Diastolic      : int  93 87 58 73 60 59 87 56 82 75 ...
#  $ AvBloodPressure: int  93 110 92 121 99 92 109 133 83 91 ...
#  $ HeartRate      : int  109 99 93 62 61 106 53 101 78 93 ...
#  $ Date           : Factor w/ 9289 levels "1/1/2013 10:41",..: 28 29 30 31 32 33 34 35 36 37 ...
#=========================
# Estructura de df de glucose
#=========================  
  glucose <- read.csv("F:/examen diplomado/Glucose.csv")  
  
  str(glucose)
  
#  'data.frame':	10074 obs. of  3 variables:
#    $ Patient: int  1369 1410 1156 663 1198 740 574 787 623 1116 ...
#  $ Glucose: int  61 82 89 68 135 105 130 81 138 137 ...
#  $ Date   : Factor w/ 9163 levels "1/1/2013 10:41",..: 28 29 30 31 32 33 34 35 36 37 ...
#=========================
# Estructura de df de oximetry
#=========================  
  
oximetry <- read.csv("F:/examen diplomado/Oximetry.csv")

str(oximetry)

#'data.frame':	9872 obs. of  4 variables:
#  $ Patient  : int  1369 1410 1156 663 1198 740 574 787 623 1116 ...
#$ SpO2     : int  96 73 85 66 88 86 67 63 78 70 ...
#$ HeartRate: int  73 61 57 87 75 62 73 103 106 71 ...
#$ Date     : Factor w/ 8984 levels "1/1/2013 10:41",..: 28 29 30 31 32 33 34 35 36 37 ...


#=========================
# Estructura de df de wei_hei
#=========================  
Wei_hei <- read.csv("F:/examen diplomado/Weight_Height.csv")

str(Wei_hei)

#'data.frame':	10170 obs. of  5 variables:
#  $ Patient: int  1369 1410 1156 663 1198 740 574 787 623 1116 ...
#$ Weight : num  94.9 64.6 119 61.6 50.4 ...
#$ Height : num  1.59 1.67 1.49 1.86 1.57 1.86 1.84 1.62 1.84 1.46 ...
#$ IMC    : num  37.5 23.1 53.6 17.8 20.5 ...
#$ Date   : Factor w/ 9267 levels "1/1/2013 10:41",..: 28 29 30 31 32 33 34 35 36 37 ...


#==================================
# Exploratory data analysis
# Resume of data blood
#==================================

#Patient          Systolic       Diastolic      AvBloodPressure   HeartRate               Date      
#Min.   :   1.0   Min.   :  0.0   Min.   :  0.00   Min.   :  0.0   Min.   :  0   2/1/2015 20:55:    5  
#1st Qu.: 420.0   1st Qu.: 99.0   1st Qu.: 63.00   1st Qu.: 92.0   1st Qu.: 67   2/1/2015 9:55 :    5  
#Median : 730.0   Median :113.0   Median : 74.00   Median :108.0   Median : 80   2/5/2015 14:58:    5  
#Mean   : 720.2   Mean   :112.8   Mean   : 73.38   Mean   :110.5   Mean   : 80   2/9/2015 10:49:    5  
#3rd Qu.:1073.0   3rd Qu.:127.0   3rd Qu.: 84.00   3rd Qu.:129.0   3rd Qu.: 93   2/1/2015 12:45:    4  
#Max.   :1453.0   Max.   :187.0   Max.   :118.00   Max.   :149.0   Max.   :183   2/1/2015 19:25:    4  

head(blood)
summary(blood)

#==================================
# Analisis de presión sanguinea
#==================================
ggplot(blood,aes(x="Presionblood",y=AvBloodPressure))+
  geom_boxplot()

ggplot(blood,aes(x=AvBloodPressure))+
  geom_bar()

blood %>% select(Patient,AvBloodPressure) %>%
  filter(AvBloodPressure <= 0)

#==================================
# Analisis de latidos del corazón
#==================================
ggplot(blood,aes(x="heartrate",y=HeartRate))+
  geom_boxplot()

ggplot(blood,aes(x=HeartRate))+
  geom_bar()

blood %>% select(Patient,HeartRate) %>%
  filter(HeartRate >= 120)


#==================================
# Analisis de systolic
#==================================
ggplot(blood,aes(x="systolic",y=Systolic))+
  geom_boxplot()

ggplot(blood,aes(x=Systolic))+
  geom_bar()

blood %>% select(Patient,Systolic) %>%
  filter(Systolic >= 149)

#==================================
# Analisis de Dialostolic
#==================================
ggplot(blood,aes(x="Dialostic",y=Diastolic))+
  geom_boxplot()

ggplot(blood,aes(x=Diastolic))+
  geom_bar()

blood %>% select(Patient,Diastolic) %>%
  filter(Diastolic >= 115)
#==================================
# Analisis conjunto de datos glucosa
#==================================

str(glucose)

summary(glucose)


#summary(glucose)
#Patient          Glucose                  Date      
#Min.   :   1.0   Min.   : 51.0   2/1/2015 20:55:    5  
#st Qu.: 426.0   1st Qu.: 82.0   2/1/2015 9:55 :    5  
#edian : 736.0   Median :102.0   2/5/2015 14:58:    5  
#Mean   : 726.2   Mean   :105.6   2/9/2015 10:49:    5  
#3rd Qu.:1076.0   3rd Qu.:127.0   2/1/2015 12:45:    4  
#Max.   :1453.0   Max.   :207.0   2/1/2015 19:25:    4  
#(Other)       :10046  

ggplot(glucose,aes(x="Glucose",y=Glucose))+
  geom_boxplot()

ggplot(glucose,aes(x=Glucose))+
  geom_bar()


# Outlier Glucosa + 1.5 * IRQ glucosa = 127 + 1.5
#127+1.5*IQR(glucose$Glucose)
# 194.5
glucose %>% select(Patient,Glucose) %>%
  filter(Glucose <= 194.5)

## Elimino outliers crando un nuevo set de datos

glucose2 <- glucose %>% select(Patient,Glucose,Date) %>%
  filter(Glucose <= 194.5)

## obtengo resumen de los datos
str(glucose2)
summary(glucose2)


#summary(glucose2)
#Patient          Glucose                  Date     
#Min.   :   1.0   Min.   : 51.0   2/1/2015 20:55:   5  
#1st Qu.: 436.0   1st Qu.: 82.0   2/1/2015 9:55 :   5  
#Median : 746.0   Median :102.0   2/5/2015 14:58:   5  
#Mean   : 736.6   Mean   :104.1   2/9/2015 10:49:   5  
#3rd Qu.:1081.0   3rd Qu.:126.0   2/1/2015 12:45:   4  
#Max.   :1453.0   Max.   :188.0   2/1/2015 19:25:   4  
#(Other)       :9886 

ggplot(glucose2,aes(x="Glucose",y=Glucose))+
  geom_boxplot()

ggplot(glucose,aes(x=Glucose))+
  geom_bar()

#==================================
# Analisis conjunto de datos oximetria
#==================================

summary(oximetry)

#Patient            SpO2          HeartRate                  Date     
#Min.   :   1.0   Min.   : 44.00   Min.   : 44.00   2/1/2015 20:55:   5  
#1st Qu.: 438.0   1st Qu.: 71.00   1st Qu.: 66.00   2/1/2015 9:55 :   5  
#Median : 749.0   Median : 83.00   Median : 80.00   2/5/2015 14:58:   5  
#Mean   : 739.8   Mean   : 81.94   Mean   : 80.36   2/9/2015 10:49:   5  
#3rd Qu.:1082.0   3rd Qu.: 94.00   3rd Qu.: 94.00   2/9/2015 13:25:   5  
#Max.   :1453.0   Max.   :100.00   Max.   :217.00   2/1/2015 12:45:   4  
#(Other)       :9843

## Revisión SpO2
ggplot(oximetry,aes(x="SpO2",y=SpO2))+
  geom_boxplot()

ggplot(oximetry,aes(x=SpO2))+
  geom_bar()

## Revision heart rate
ggplot(oximetry,aes(x="HeartRate",y=HeartRate))+
  geom_boxplot()

ggplot(oximetry,aes(x=HeartRate))+
  geom_bar()

#limite superior 94+1.5*IQR
#128.5

 oximetry2 <- oximetry %>% select(Patient,HeartRate,SpO2,Date) %>%
   filter(HeartRate <= 128.5)

  summary(oximetry2)
 
 
 ## Revision heart rate
 ggplot(oximetry2,aes(x="HeartRate",y=HeartRate))+
   geom_boxplot()
 
 #==================================
 # Analisis conjunto de datos peso y altura.
 #==================================
 
 summary(Wei_hei)
 
 #Patient           Weight           Height           IMC                    Date      
 #Min.   :   1.0   Min.   : -9.70   Min.   :1.450   Min.   :-4.20   2/1/2015 20:55:    5  
 #1st Qu.: 418.2   1st Qu.: 65.37   1st Qu.:1.600   1st Qu.:21.61   2/1/2015 9:55 :    5  
 #Median : 730.0   Median : 80.00   Median :1.720   Median :26.91   2/5/2015 14:58:    5  
 #Mean   : 719.6   Mean   : 81.17   Mean   :1.726   Mean   :27.83   2/9/2015 10:49:    5  
 #3rd Qu.:1072.0   3rd Qu.: 98.85   3rd Qu.:1.850   3rd Qu.:33.41   2/1/2015 12:45:    4  
 #Max.   :1453.0   Max.   :172.00   Max.   :6.520   Max.   :56.56   2/1/2015 19:25:    4  
 #(Other)       :10142
 
 ## Revisión Weigth Heigth
# IQR 33.48 limte inferior 15,13 limite superior 149,07
 ggplot(Wei_hei,aes(x="weight",y=Weight))+
   geom_boxplot()
 
 ggplot(Wei_hei,aes(x=Weight))+
   geom_bar()
# Validación de campo altura
# se identifica error outlier ya que el limite superior es 1.975
 ggplot(Wei_hei,aes(x="height",y=Height))+
   geom_boxplot()
 
 ggplot(Wei_hei,aes(x=Height))+
   geom_bar()
# se limpia base sacando outlier de altura
 Wei_hei %>% select(Patient,Weight,Height,IMC,Date) %>%
   filter(Weight >= 15.13,Weight <= 149.07,Height <1.975,IMC >= 4.49, IMC <= 51.44)
 
# se realiza validación de IMC
   ggplot(Wei_hei,aes(x="IMC",y=IMC))+
   geom_boxplot()
 
 ggplot(Wei_hei,aes(x=IMC))+
   geom_bar()
 
 
wei_hei2 <- Wei_hei %>% select(Patient,Weight,Height,IMC,Date) %>%
  filter(Weight >= 15.13,Weight <= 149.07,Height <1.975,IMC >= 4.49, IMC <= 51.44)

ggplot(wei_hei2,aes(x="IMC",y=IMC))+
  geom_boxplot()

ggplot(wei_hei2,aes(x=IMC))+
  geom_bar()

summary(wei_hei2)
#====================================
# integración de datos
# ===================================
str(blood)

str(glucose2)

 
blood_glucose <- inner_join(blood,glucose2, by = c("Patient","Date"))
str(blood_glucose)
blod_glu_oxi <- inner_join(blood_glucose,oximetry2,by = c("Patient","Date"))
str(blod_glu_oxi)
patient <-  inner_join(blod_glu_oxi,wei_hei2,by = c("Patient","Date"))

str(patient)

summary(patient)

#==================================
#Filtrado de datos
#==================================
library(corrplot)
# Analisis de correlación entre variables

corm <- cor(patient[,-c(1,6)])
corrplot(corm)


# Analisis de componentes principales
# se omiten las columnas de nuestro dataset que no deben ser utilizadas en la medición
# patient, DAte, Weight, Height
patient.acp <- prcomp(patient[,-c(1,6,10,11)], scale=TRUE, center=TRUE)

str(patient)
# Graficando componentes principales y variables
biplot(patient.acp)

# Graficando la varianza por cada componente
acp.var <- patient.acp$sdev*patient.acp$sdev
acpve <- acp.var/sum(acp.var)

plot(acpve, xlab = "Componente Principal", 
     ylab = "Proporcion de varianza explicada", 
     ylim = c(0, 1), type = "b")

plot(cumsum(acpve), xlab = "Componente Principal", 
     ylab = "Proporcial acumulada de varianza", 
     ylim = c(0, 1), type = "b")

#==================================
#Modelado de los datos
#==================================

# Armo df de datos

patient.df <- patient[,-c(1,6,10,11)]

#===================================
# Modelo No Supervisado cluster jerarquico,
# Se realiza con el fin de identificar aquellos grupos que aún no conocemos
#===================================

# Escalamos datos

patient.sca <- scale(patient.df)

# Calculamos la distancia
data.dist <- dist(patient.sca)

# Aplicando hclust para la clasificacion
wisc.hclust <- hclust(data.dist, method = "complete")

# Graficando el dendograma
plot(wisc.hclust)

# Limitando la cantidad de grupos a 3
wisc.hclust.clusters <- cutree(wisc.hclust, k=3)

# Implementación de kmeans

wss <- 0

# Desde 1 a 15 numeros de klusters
for (i in 1:15) {
  km.out <- kmeans(patient.sca, centers = i, nstart=20)
  # Save total sin suma de cuadrados para variable wss 
  wss[i] <- km.out$tot.withinss
}

# Graficando y ubicando el codo (elbow)
plot(1:15, wss, type = "b", 
     xlab = "Número de clusters", 
     ylab = "Sin grupo suma de cuadrados")

# de acuerdo con el grafico anterior, tomo como k 4 clusters ya que tendre una mejor agrupación
k <- 3

# Memoria 100
set.seed(100)

# Aplicamos K Means
wisc.km <- kmeans(patient.sca, centers = k, nstart = 20, iter.max = 100)
summary(wisc.km)


# summary(wisc.km)
#Length Class  Mode   
#cluster      7851   -none- numeric
#centers        24   -none- numeric
#totss           1   -none- numeric
#withinss        3   -none- numeric
#tot.withinss    1   -none- numeric
#betweenss       1   -none- numeric
#size            3   -none- numeric
#iter            1   -none- numeric
#ifault          1   -none- numeric

# Graficamos y comparamos vs nuestro ACP, comparamos dos de la


# Grafica X = Glucose Y = IMC agrupado por sus clusters
  plot(patient.df[,c("Glucose","IMC")],
       col = wisc.km$cluster,
       main = paste("k-means de Pacientes con", k, "clusters"),
       xlab = "Glucose", ylab = "IMC")

# Grafica X = PC1 Y = PC2 (componentes principales del PCA) agrupado por sus clusters hclust
plot(patient.acp$x[,1],patient.acp$x[,2],
     col = wisc.hclust.clusters,
     main = paste("k-means de Pacientes con", k, "clusters"),
     xlab = "PC1", ylab = "PC2")

# Grafica X = PC1 Y = PC2 (componentes principales del PCA) agrupado por sus clusters kmeans
plot(patient.acp$x[,1],patient.acp$x[,2],
     col = wisc.km$cluster,
     main = paste("k-means de Pacientes con", k, "clusters"),
     xlab = "PC1", ylab = "PC2")
head(wisc.km)

