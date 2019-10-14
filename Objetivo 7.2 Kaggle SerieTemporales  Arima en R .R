
## SCRIPT:OBJETIVO 7.COMPETICIONESDE KAGGLE

# Comenzamos con bloque de definición de librerias

library(forecast)
library(gplots)
library(ROCR)
library(caTools)
library(tseries)

# Bloque de carga del DataSet y definición de directorio de trabajo en un solo paso

competiciones <-read.csv("F:/Master Data Analytics/TFM/TFM KAGGLE/Objetivo 7. Kaggle Series Temporales/Entrega Definitiva/serieteams.csv")
names(competiciones)<- c("Mes","Equipos")
head(competiciones)
tail(competiciones)

# Bloque de revisión del DataSet y estadisticas básicas

summary(competiciones)


# Bloque de conversión de los datos en serie de datos

ts_competiciones<-ts(competiciones[,2])
ts_competiciones

# Bloque de  de los datos en serie de datos

ts_competiciones = ts(ts_competiciones, start = c(2010,5), frequency = 12)
print(ts_competiciones)

# Bloque de ploteado de la serie

plot(ts_competiciones,main="Participación histórica", xlab="Mes", ylab="Equipos")

# DESCOMPOSICION ESTRUCTURAL DE LA SERIE

plot(decompose(ts_competiciones))

# NUMMERO DE DIFENCIACIONES NECESARIAS PARA CONSEGUIR ESTACIONARIEDAD CON FUNCIONES

ndiffs(ts_competiciones)
nsdiffs(ts_competiciones)

# FIRST DIFERENCIA ( para corregir la estacionalidad)

ts_competiciones_d1=diff(ts_competiciones, lag=1,differences=1)
plot(ts_competiciones_d1)
plot(decompose(ts_competiciones_d1))

# SEGUNDA DIFERENCIA (no sería necesaria a la vista del resultado de nsdiffs)

ts_competiciones_d2=diff(ts_competiciones, lag=12,differences=1)
plot(ts_competiciones_d2)
plot(decompose(ts_competiciones_d2))

# AUTOCORRELACION


acf(ts_competiciones_d1,lag.max=36)
pacf(ts_competiciones_d1,lag.max=36)

# MODELO1 . Modelo ARIMA

## Mdelo Estimacion de los mejores parametros del modelo y precccion usando autoarima.

auto.arima(ts_competiciones)
forecast(auto.arima(ts_competiciones),h=24)
plot(forecast(auto.arima(ts_competiciones),h=24))


# MODELO 2 . Modelo holtwinters

model_hw=HoltWinters(ts_competiciones) 
plot(model_hw)
summary(model_hw)
plot(forecast(model_hw, fan=TRUE,h=24))
forecast <- predict(model_hw, n.ahead = 24, prediction.interval = T, level = 0.95)
forecast

