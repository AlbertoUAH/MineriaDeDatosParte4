library(readxl)
library(forecast)
library(ggplot2)
library(tidyquant)
library(gridExtra)
library(lmtest)
library(reshape2)
setwd("UCM/Mineria de Datos y Modelizacion Predictiva IV/Practica 4/")
# URL dataset: https://fred.stlouisfed.org/series/MRTSSM4481USN
# Apartado 1.
ventas.ropa <- read_excel("retail_sales.xls")
ventas.ropa$observation_date <- format(as.Date(ventas.ropa$observation_date), "%Y-%m")

# Columnas del conjunto de datos
min(ventas.ropa$observation_date) # Fecha min: Enero 2006
max(ventas.ropa$observation_date) # Fecha max: Diciembre 2019

# Analizamos las 6 primeras filas
head(ventas.ropa)

# Estadisticas ingresos
summary(ventas.ropa$sales)

# Apartado 2.
ventas.ropa.ts <- ts(ventas.ropa[,-1], start=c(2006,1), frequency=12)
ventas.ropa.test <- window(ventas.ropa.ts,start=c(2019,1), end=c(2019,12))
autoplot(ventas.ropa.ts) +  ggtitle("Ingresos por ventas en ropa de caballero") +
  xlab("Mes-Año") +  ylab("Millones de dólares")

ventas.ropa.comp<- decompose(ventas.ropa.ts,type=c("multiplicative"))
autoplot(ventas.ropa.comp)

# Componente estacional
# Mes Diciembre: los ingresos por ventas de ropa son un 57.8 % superior a la media anual
# Meses Enero y Febrero: los ingresos por ventas de ropa son, respectivamente, un 25.5 y 14.9 % inferior con respecto a la media anual, aprox.
comp.est <- data.frame(t(ventas.ropa.comp$seasonal[c(1:12)]))
colnames(comp.est) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
print(round(comp.est,3), row.names = F)

# Representamos la serie con la tendencia y la serie ajustada estacionalmente 
autoplot(ventas.ropa.ts, series="Datos") +
autolayer(trendcycle(ventas.ropa.comp), series="Tendencia") + autolayer(seasadj(ventas.ropa.comp), series="Estacionalmente ajustada") + 
  xlab("Mes-Año") + ylab("Millones de dólares") +
  ggtitle("Ingresos por ventas en ropa de caballero") + scale_colour_manual(values=c("gray","blue","red"),
                                                        breaks=c("Datos","Estacionalmente ajustada","Tendencia"))

my_colors <- RColorBrewer::brewer.pal(4, "Blues")[1:13]
ggseasonplot(ventas.ropa.ts, year.labels=TRUE, year.labels.left=TRUE) + ylab("Número") +
  ggtitle("Seasonal plot: ingresos por ventas en ropa de caballero") + theme(text = element_text(size = 9)) +
  scale_fill_manual(values = my_colors,
                    labels = c(2006:2019))

# Representamos los coeficientes de estacionalidad e irregularidad
ventas.ropa.season<- cbind(ventas.ropa.comp$seasonal,ventas.ropa.comp$random)
autoplot(ventas.ropa.season,facets=TRUE)

# Apartado 3.
ventas.ropa.ts.transformado <- window(ventas.ropa.ts,start=c(2006,1), end=c(2018,12))

# Apartado 4.
# Alisado simple
ventas.ropa.ss <- ses(ventas.ropa.ts.transformado, h=12)
summary.ss <- summary(ventas.ropa.ss)$`Point Forecast`[c(1:12)]
alisado.simple <- autoplot(ventas.ropa.ss) + 
  autolayer(fitted(ventas.ropa.ss), series="Fitted") + 
  ylab("Millones de millas") + xlab("Año")
alisado.simple
round(accuracy(ventas.ropa.ss),3)

# Alisado doble (Holt)
ventas.ropa.holt <- holt(ventas.ropa.ts.transformado, h=12)
summary.holt <- summary(ventas.ropa.holt)$`Point Forecast`[c(1:12)]
alisado.holt <- autoplot(ventas.ropa.holt) + 
  autolayer(fitted(ventas.ropa.holt), series="Fitted") + 
  ylab("Millones de millas") + xlab("Año")
alisado.holt
round(accuracy(ventas.ropa.holt),3)

# Alisado de Holt-Winters
ventas.ropa.hw <- hw(ventas.ropa.ts.transformado, h=12, seasonal="multiplicative")
summary.hw <- summary(ventas.ropa.hw)$`Point Forecast`[c(1:12)]
alisado.hw <- autoplot(ventas.ropa.hw) + 
  autolayer(fitted(ventas.ropa.hw), series="Fitted") + 
  ylab("Millones de millas") + xlab("Año")
alisado.hw
round(accuracy(ventas.ropa.hw),3)

estadisticas.suavizado <- rbind(round(accuracy(ventas.ropa.ss),3), round(accuracy(ventas.ropa.holt),3),
                          round(accuracy(ventas.ropa.hw),3))
rownames(estadisticas.suavizado) <- c("Alisado simple", "Alisado doble", "Alisado Holt-Winters")

data.frame(cbind(round(accuracy(prediccion, ventas.ropa.test), 3), "AIC" = AIC(fitARIMA.2), "SBC" = BIC(fitARIMA.2)))

# Elegimos el modelo de Holt-Winter
summary(ventas.ropa.hw)
