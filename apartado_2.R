# Apartado 5.
var.max <- apply(matrix(ventas.ropa.ts.transformado, ncol = 12, byrow = TRUE), 1, FUN=max)
var.min <- apply(matrix(ventas.ropa.ts.transformado, ncol = 12, byrow = TRUE), 1, FUN=min)
serie.temp <- autoplot(ventas.ropa.ts.transformado)
serie.temp +  ggtitle("Ingresos por ventas en ropa de caballero") +
  xlab("Mes-Año") +  ylab("Millones de dólares") + geom_line(data = data.frame(x = serie.temp$data[which(serie.temp$data$y %in% var.max), 2], y = var.max), color = "red", linetype = "dashed") +
  geom_line(data = data.frame(x = serie.temp$data[which(serie.temp$data$y %in% var.min), 2], y = var.min), color = "red", linetype = "dashed")

# Sin embargo, empleando el logaritmo (lambda = 0) parece que estabilizamos la varianza
transf.box.cox <-  log(ventas.ropa.ts.transformado)
var.max <- apply(matrix(transf.box.cox, ncol = 12, byrow = TRUE), 1, FUN=max)
var.min <- apply(matrix(transf.box.cox, ncol = 12, byrow = TRUE), 1, FUN=min)
serie.temp <- autoplot(transf.box.cox)
serie.temp +  ggtitle("Ingresos por ventas en ropa de caballero (log)") +
  xlab("Mes-Año") +  ylab("Millones de dólares (log)")  + geom_line(data = data.frame(x = serie.temp$data[which(serie.temp$data$y %in% var.max), 2], y = var.max), color = "red", linetype = "dashed") +
  geom_line(data = data.frame(x = serie.temp$data[which(serie.temp$data$y %in% var.min), 2], y = var.min), color = "red", linetype = "dashed")

# Por otro lado, segun la funcion BoxCox.lambda, nos recomienda transformar la serie con lambda = 0.36
BoxCox.lambda(ventas.ropa.ts.transformado, method = "guerrero")
BoxCox.lambda(ventas.ropa.ts.transformado, method = "loglik")
# Sin embargo, con lambda = 0.36 no parece estabilizar aun mas la varianza
transf.box.cox.2 <-  BoxCox(ventas.ropa.ts.transformado, lambda = BoxCox.lambda(ventas.ropa.ts.transformado, method = "guerrero"))
var.max <- apply(matrix(transf.box.cox.2, ncol = 12, byrow = TRUE), 1, FUN=max)
var.min <- apply(matrix(transf.box.cox.2, ncol = 12, byrow = TRUE), 1, FUN=min)
serie.temp <- autoplot(transf.box.cox.2)
serie.temp +  ggtitle("Ingresos por ventas en ropa de caballero (BoxCox - \u03bb = 0.36)") +
  xlab("Mes-Año") +  ylab("Millones de dólares (transformada)")  + geom_line(data = data.frame(x = serie.temp$data[which(serie.temp$data$y %in% var.max), 2], y = var.max), color = "red", linetype = "dashed") +
  geom_line(data = data.frame(x = serie.temp$data[which(serie.temp$data$y %in% var.min), 2], y = var.min), color = "red", linetype = "dashed")

# Funcion de autocorrelacion
# Se observa un comportamiento repetitivo de las autocorreaciones a lo largo de 12 meses. No obstante,
# la autocorrelacion mas fuerte se da en los retardos multiplos de 12.
ggAcf(transf.box.cox, lag = 48)
# Funcion de autocorrelacion parcial
ggPacf(transf.box.cox, lag = 48)

# Necesitare diferenciacion estacional de orden 12
# Funcion de autocorrelacion
ggAcf(diff(transf.box.cox, 12), lag = 48)
# Funcion de autocorrelacion parcial
ggPacf(diff(transf.box.cox, 12), lag = 48)

# La media sigue sin ser constante
autoplot(diff(transf.box.cox, 12)) +  ggtitle("Ingresos por ventas en ropa de caballero (log)") +
  xlab("Mes-Año") +  ylab("Millones de dólares (log)")

# De hecho, si realizamos el test kpss
# Rechazamos la hipotesis nula de que la serie es estacionaria en cuanto a tendencia se refiere
tseries::kpss.test(diff(transf.box.cox, 12), null = "Trend")

# Sin embargo, al aplicar diferenciacion sobre la componente regular
autoplot(diff(diff(transf.box.cox, 12))) +  ggtitle("Ingresos por ventas en ropa de caballero (log)") +
  xlab("Mes-Año") +  ylab("Millones de dólares (log)")
# p-valor > 0.05, NO podemos rechazar la hipotesis nula de que la serie no presenta tendencia...
# ¿Por que no aparece el p-valor real? https://stackoverflow.com/questions/54405313/what-is-the-meaning-of-warning-message-in-adf-testdfrn-p-value-greater-tha
tseries::kpss.test(diff(diff(transf.box.cox, 12)), null = "Trend")
# ... Como estacionalidad
summary(seastests::wo(diff(diff(transf.box.cox, 12))))

# Funcion de autocorrelacion
ggAcf(diff(diff(transf.box.cox, 12)), lag = 48)
# Funcion de autocorrelacion parcial
ggPacf(diff(diff(transf.box.cox, 12)), lag = 48)

comparar.autocorrelaciones <- function(datos.1, datos.2, tipo, titulo) {
  if(tipo == "acf") {
    acf1 <- acf(datos.1, plot = F, lag.max = 48)
    acf2 <- acf(datos.2, plot = F, lag.max = 48)
  } else {
    acf1 <- pacf(datos.1, plot = F, lag.max = 48)
    acf2 <- pacf(datos.2, plot = F, lag.max = 48)
  }
  df<- data.frame(lag = acf1$lag,acf1=acf1$acf,acf2=acf2$acf)
  colnames(df)<-c("lag","1 Dif. Regular","2 Dif. Regulares")
  data<-melt(df,id="lag")
  
  ggplot() + geom_bar(data=data[data$variable=="1 Dif. Regular",], aes(x = lag, y = value, 
                                                                         fill = variable), stat = "identity") +
    geom_bar(data=data[data$variable=="2 Dif. Regulares",],
             aes(x = lag, y = value, fill=variable), position = "dodge", stat = "identity") + 
    ggtitle(titulo) +
    scale_fill_manual("legend", values = c("1 Dif. Regular" = "royalblue1", "2 Dif. Regulares" = "tomato1"))
}
comparar.autocorrelaciones(diff(diff(transf.box.cox, 12)), diff(diff(diff(transf.box.cox, 12))), tipo = "acf", "Comparación entre ACFs")
comparar.autocorrelaciones(diff(diff(transf.box.cox, 12)), diff(diff(diff(transf.box.cox, 12))), tipo = "pacf", "Comparación entre PACFs")

autoplot(diff(diff(diff(transf.box.cox, 12))), series = "2 Dif. Regulares", size = 0.75)  +
  xlab("Mes-Año") +  ylab("Millones de dólares (log)") + autolayer(diff(diff(transf.box.cox, 12)), series = "1 Dif. Regular", size = 0.75) +
  ggtitle("Ingresos por ventas en ropa de caballero (comparacion entre diferenciaciones)") +
  scale_fill_manual("legend")

# Funcion de autocorrelacion
ggAcf(diff(diff(transf.box.cox, 12)), lag = 48)
# Funcion de autocorrelacion parcial
ggPacf(diff(diff(transf.box.cox, 12)), lag = 48)

fitARIMA.1<-arima(transf.box.cox,order = c(2,1,0), seasonal=c(0,1,2))
coeftest(fitARIMA.1)
checkresiduals(fitARIMA.1, plot = FALSE)
ggtsdisplay(residuals(fitARIMA.1))
data.frame(cbind(round(accuracy(fitARIMA.1), 3), "AIC" = AIC(fitARIMA.1), "SBC" = BIC(fitARIMA.1)))

# ¿Y la componente estacional?
# Podemos tener en cuenta dos componentes autoregresivas
# Explicar porqué no podemos dejar unicamente dos coeficientes autoregresivos (p-valor se reduce a 0.28, aproximadamente)
fitARIMA.1.1 <- arima(transf.box.cox,order = c(2,1,0), seasonal=c(3,1,0))
coeftest(fitARIMA.1.1)
checkresiduals(fitARIMA.1.1, plot = FALSE)
ggtsdisplay(residuals(fitARIMA.1.1))
data.frame(cbind(round(accuracy(fitARIMA.1.1), 3), "AIC" = AIC(fitARIMA.1.1), "SBC" = BIC(fitARIMA.1.1)))

# ¿Podriamos mejorar la parte estacional? Sobretodo con la estacionalidad 36
fitARIMA.1.2 <- arima(transf.box.cox,order = c(2,1,0), seasonal=c(3,1,2))
coeftest(fitARIMA.1.2)
checkresiduals(fitARIMA.1.2, plot = FALSE)
ggtsdisplay(residuals(fitARIMA.1.2))
data.frame(cbind(round(accuracy(fitARIMA.1.2), 3), "AIC" = AIC(fitARIMA.1.2), "SBC" = BIC(fitARIMA.1.2)))

# Funcion para comparar graficamente modelos ARIMA
comparar.resultados <- function(series, leyenda) {
  i <- 1
  p <- autoplot(ventas.ropa.test, PI = FALSE)
  lista.accuracy.train <- data.frame(); lista.accuracy.test <- c()
  df.predicciones <- ventas.ropa.test
  for(serie in series) {
    prediccion <- forecast(serie, h = 12)
    prediccion$x <- exp(1) ** prediccion$x
    prediccion$mean <- exp(1) ** prediccion$mean
    prediccion$lower <- exp(1) ** prediccion$lower
    prediccion$upper <- exp(1) ** prediccion$upper
    lista.accuracy.train <- rbind(lista.accuracy.train, accuracy(prediccion, ventas.ropa.test)[1,])
    lista.accuracy.test <- rbind(lista.accuracy.test, accuracy(prediccion, ventas.ropa.test)[2,])
    df.predicciones <- cbind(df.predicciones, prediccion$mean - ventas.ropa.test)
    p <- p + autolayer(prediccion, series = leyenda[i], alpha = 5, PI = FALSE)
    i <- i + 1
  }
  p <- p + autolayer(ventas.ropa.test, series = "Datos test") +
    guides(colour = guide_legend("Modelo"), fill=guide_legend(title="Modelos de prediccion")) +
    ggtitle("Comparativa modelos ARIMA") + scale_color_manual(values=c("black", "red", "blue", "purple4"))
  colnames(lista.accuracy.train) <- c("ME", "RMSE", "MAE", "MPE", "MAPE", "MASE", "ACF1", "Theil's U")
  colnames(lista.accuracy.test) <- c("ME", "RMSE", "MAE", "MPE", "MAPE", "MASE", "ACF1", "Theil's U")
  colnames(df.predicciones) <- c("Test", leyenda)
  rownames(lista.accuracy.train) <- leyenda; rownames(lista.accuracy.test) <- leyenda 
  return(list(p, lista.accuracy.train, lista.accuracy.test, df.predicciones))
}

# (2,1,3) ; (0,1,1)
fitARIMA.2<-arima(transf.box.cox,order = c(2,1,3), seasonal=c(0,1,1))
coeftest(fitARIMA.2)
print(checkresiduals(fitARIMA.2, plot = FALSE))
ggtsdisplay(residuals(fitARIMA.2))
data.frame(cbind(round(accuracy(fitARIMA.2), 3), "AIC" = AIC(fitARIMA.2), "SBC" = BIC(fitARIMA.2)))

fitARIMA.2.1<-arima(transf.box.cox,order = c(2,1,3), seasonal=c(2,1,0))
coeftest(fitARIMA.2.1)
print(checkresiduals(fitARIMA.2.1, plot = FALSE))
ggtsdisplay(residuals(fitARIMA.2.1))
data.frame(cbind(round(accuracy(fitARIMA.2.1), 3), "AIC" = AIC(fitARIMA.2.1), "SBC" = BIC(fitARIMA.2.1)))

# ¿Que nos recomienda el modelo auto.arima?
fitARIMA.auto <- auto.arima(transf.box.cox, seasonal = TRUE)
coeftest(fitARIMA.auto)
print(checkresiduals(fitARIMA.auto, plot = FALSE))
ggtsdisplay(residuals(fitARIMA.auto))
data.frame(cbind(round(accuracy(fitARIMA.auto), 3), "AIC" = AIC(fitARIMA.auto), "SBC" = BIC(fitARIMA.auto)))

comparar.resultados(list(fitARIMA.1, fitARIMA.2, fitARIMA.auto), c("fitARIMA.1", "fitARIMA.2", "Modelo Auto-ARIMA"))

# Recuperamos los coeficientes del modelo ganador
coef(fitARIMA.2)

# Calculamos las predicciones y los intervalos de confianza para el año 2019
prediccion <- forecast(fitARIMA.2, h = 12)
prediccion$x <- exp(1) ** prediccion$x
prediccion$mean <- exp(1) ** prediccion$mean
prediccion$lower <- exp(1) ** prediccion$lower
prediccion$upper <- exp(1) ** prediccion$upper

cbind("Ingresos por ventas (log)" = transf.box.cox, 
      "Valores ajustados" =fitted(fitARIMA.2)) %>% autoplot() + xlab("Mes-Año") + ylab("") + ggtitle("Ingresos observados y ajustados")

autoplot(ventas.ropa.ts) + autolayer(prediccion, series = "Modelo ARIMA Final", alpha = 0.5) + 
  guides(colour = guide_legend("Modelo")) + ggtitle("Prediccion modelo ARIMA Final")

#prediccion$x <- exp(log(0.3614745 * prediccion$x + 1) / 0.3614745)
#prediccion$mean <- exp(log(0.3614745 * prediccion$mean + 1) / 0.3614745)
#prediccion$lower <- exp(log(0.3614745 * prediccion$lower + 1) / 0.3614745)
#prediccion$upper <- exp(log(0.3614745 * prediccion$upper + 1) / 0.3614745)

# Por ultimo, comparamos el modelo ARIMA Final con la prediccion obtenida con el modelo de alisado de Holt-Winters
autoplot(ventas.ropa.test, PI = FALSE) + 
  autolayer(prediccion, series = "Modelo ARIMA Final", alpha = 5, PI = FALSE) +
  autolayer(ventas.ropa.hw, series = "Modelo Holt-Winters", alpha = 5, PI = FALSE) +
  autolayer(ventas.ropa.test, series = "Datos Test", alpha = 5, PI = FALSE) +
  guides(colour = guide_legend("Modelo"), fill=guide_legend(title="Modelos de prediccion")) +
  ggtitle("Comparativa modelo ARIMA + Holt-Winters") + scale_color_manual(values=c("black", "blue", "red"))

# Por otro lado, comparamos la anchura de los intervalos de confianza de ambos modelos
cbind("Modelo ARIMA Final (80 %)" = prediccion$upper[,1] - prediccion$lower[,1], "Modelo ARIMA Final (95 %)" = prediccion$upper[,2] - prediccion$lower[,2])
cbind("Modelo Holt-Winters (80 %)" = ventas.ropa.hw$upper[,1] - ventas.ropa.hw$lower[,1], "Modelo Holt-Winters (95 %)" = ventas.ropa.hw$upper[,2] - ventas.ropa.hw$lower[,2])

autoplot(ventas.ropa.test) + 
  autolayer(ventas.ropa.hw, series = "Modelo Holt-Winters", alpha = 5) +
  autolayer(prediccion, series = "Modelo ARIMA Final", alpha = 5) +
  autolayer(ventas.ropa.test, series = "Datos Test", alpha = 5) +
  guides(colour = guide_legend("Modelo"), fill=guide_legend(title="Modelos de prediccion")) +
  ggtitle("Comparativa modelo ARIMA + Holt-Winters (Intervalos de confianza)") + scale_color_manual(values=c("black", "blue", "red"))

cbind(ventas.ropa.test, "Modelo ARIMA Final" = prediccion$mean, "Modelo Holt-Winters" = ventas.ropa.hw$mean)
cbind(ventas.ropa.test, "Modelo ARIMA Final (dif)" = prediccion$mean - ventas.ropa.test, "Modelo Holt-Winters (dif)" =  ventas.ropa.hw$mean - ventas.ropa.test)

comparar.resultados(list(fitARIMA.2, ventas.ropa.hw), c("Modelo ARIMA Final", "Alisado Holt-Winters"))

# Comparamos tanto las estadisticas de cada modelo...
data.frame(cbind(round(accuracy(prediccion, ventas.ropa.test), 3), "AIC" = AIC(fitARIMA.2), "SBC" = BIC(fitARIMA.2)))
data.frame(cbind(round(accuracy(ventas.ropa.hw, ventas.ropa.test), 3), "AIC" = ventas.ropa.hw$model$aic, "SBC" = ventas.ropa.hw$model$bic))

# ...Además de los residuos obtenidos
cat("Modelo ARIMA Final: \n")
checkresiduals(fitARIMA.2, plot = FALSE)
cat("Modelo Holt-Winters: \n")
checkresiduals(ventas.ropa.hw, plot = FALSE)




