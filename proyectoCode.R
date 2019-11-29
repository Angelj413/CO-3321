'
Universidad Simon Bolivar
Estadistica para Ingenieros
Sep-Dic 2019
Nombre: Angel Morante
Proyecto
'


#Lectura de la base de datos (NOTA: configurar working space)
file <- 'datosproy.txt'
datos <- read.table(file, header = TRUE, sep = "\t")
str(datos)

#variables generales

ventas <- c(datos$ventas)
facebook <- c(datos$facebook)
periodico <- c(datos$periodico)
instagram <- c(datos$instagram)
tv <- c(datos$tv)
ebay <- c(datos$ebay)
regiones <- c(datos$Region)

#Analisis descriptivo

#VENTAS
par(mfrow = c(1, 2) ) # para dibujar dos graficos en una misma página:
hist(ventas, main = "Ventas en $k", ylab = "Frecuencias", xlab = "Ingresos en Ventas", ylim=c(0,80))
boxplot(ventas, col = "green", horizontal = F, xlab = "Ventas", main="Boxplot de Ventas")
summary(ventas)
var(ventas)
sd(ventas)

#FACEBOOK
par(mfrow = c(1, 2) ) # para dibujar dos graficos en una misma página:
hist(facebook, main = "Ventas en Facebook", ylab = "Frecuencias", xlab = "Ingresos en facebook", ylim=c(0,50), xlim=c(0,300))
boxplot(facebook, col = "blue", horizontal = F, xlab = "Ventas", main="Boxplot de Ingresos FB")
summary(facebook)
var(facebook)
sd(facebook)

#PERIODICO
par(mfrow = c(1, 2) ) # para dibujar dos graficos en una misma página:
hist(periodico, main = "Ventas en Periodico", ylab = "Frecuencias", xlab = "Ingresos en Periodicos", ylim=c(0,50), xlim=c(0,120))
boxplot(periodico, col = "gray", horizontal = F, xlab = "Ventas", main="Boxplot de Ingresos Periodico.")
summary(periodico)
var(periodico)
sd(periodico)

#INSTAGRAM
par(mfrow = c(1, 2) ) # para dibujar dos graficos en una misma página:
hist(instagram, main = "Ventas en Instagram", ylab = "Frecuencias", xlab = "Ingresos en Instagram", ylim=c(0,40), xlim=c(0,50))
boxplot(instagram, col = "pink", horizontal = F, xlab = "Ventas", main="Boxplot de Ingresos Instagram")
summary(instagram)
var(instagram)
sd(instagram)

#TV
par(mfrow = c(1, 2) ) # para dibujar dos graficos en una misma página:
hist(tv, main = "Ventas en TV", ylab = "Frecuencias", xlab = "Ingresos en TV", ylim=c(0,40), xlim=c(0,1000))
boxplot(tv, col = "yellow", horizontal = F, xlab = "Ventas", main="Boxplot de Ingresos TV")
summary(tv)
var(tv)
sd(tv)

#EBAY
par(mfrow = c(1, 2) ) # para dibujar dos graficos en una misma página:
hist(ebay, main = "Ventas en EBAY", ylab = "Frecuencias", xlab = "Ingresos en EBAY", ylim=c(0,25), xlim=c(0,2500))
boxplot(ebay, col = "red", horizontal = F, xlab = "Ventas", main="Boxplot de Ingresos EBAY")
summary(ebay)
var(ebay)
sd(ebay)


#REGIONES
par(mfrow = c(1, 2) ) # para dibujar dos graficos en una misma página:
hist(regiones, main = "Ventas por Regiones", ylab = "Frecuencias", xlab = "VentasxRegion", ylim=c(0,60), xlim=c(0,4))
boxplot(split(ventas,regiones), col = "orange", horizontal = F, xlab = "Ventas", main="Boxplot de VentasxRegion")
summary(regiones)
var(regiones)
sd(regiones)

#Graficas de dispersion y matriz de correlacion
pairs(datos, main = "Graficas de dispersión")
cor(datos)

###################################################
# Calcular el intervalo de confianza del 95% para las medias de ventas por región

intervalo <- function(x, alfa) {
  n <- length(x)
  nu <- n - 1 #grados de libertad
  cuantil <- qt( 1 - alfa/2, df = nu )
  LS <- mean( x ) + cuantil*sqrt( var( x ) / n )
  LI <- mean( x ) - cuantil*sqrt( var( x ) / n )
  return( c( LI, mean( x ), LS ) )
}

ventas_region1 <- datos$ventas[datos$Region == 1]
ventas_region2 <- datos$ventas[datos$Region == 2]
ventas_region3 <- datos$ventas[datos$Region == 3]
ventas_region4 <- datos$ventas[datos$Region == 4]

par( mfrow = c( 1, 1 ) )
qqnorm(ventas_region1)
qqline(ventas_region1)

intervalo_ventas_r1 = intervalo(ventas_region1, 0.5)
print(intervalo_ventas_r1)

qqnorm(ventas_region2)
qqline(ventas_region2)

intervalo_ventas_r2 = intervalo(ventas_region2, 0.5)
print(intervalo_ventas_r2)

qqnorm(ventas_region3)
qqline(ventas_region3)

intervalo_ventas_r3 = intervalo(ventas_region3, 0.5)
print(intervalo_ventas_r3)

qqnorm(ventas_region4)
qqline(ventas_region4)

intervalo_ventas_r4 = intervalo(ventas_region4, 0.5)
print(intervalo_ventas_r4)

###################################################
#Encuentre el modelo de regresión simple que mejor se ajuste a los datos

par( mfrow = c( 2, 2 ) )
modeloFacebook = lm(ventas~facebook)
summary(modeloFacebook)
plot(modeloFacebook, main = "Ventas ~ Facebook")

modeloPeriodico = lm(ventas~periodico)
summary(modeloPeriodico)
plot(modeloPeriodico, main = "Ventas ~ Periodico")

modeloInstagram = lm(ventas~instagram)
summary(modeloInstagram)
plot(modeloInstagram, main = "Ventas ~ Instagram")

modeloTv = lm(ventas~tv)
summary(modeloTv)
plot(modeloTv, main = "Ventas ~ TV")

modeloEbay = lm(ventas~ebay)
summary(modeloEbay)
plot(modeloEbay, main = "Ventas ~ Ebay")


###################################################
#Consiga el modelo múltiple más apropiado

multiMod1 = lm(ventas~facebook + periodico + instagram + tv + ebay)
summary(multiMod1)

#Menos significativo Ebay

multiMod2 = lm(ventas~facebook + periodico + instagram + tv)
summary(multiMod2)

#Menos significativo tv

multiMod3 = lm(ventas~facebook + periodico + instagram)
summary(multiMod3)

#Menos significativo periodico

multiMod4 = lm(ventas~facebook + instagram)
summary(multiMod4)
plot(multiMod4, main = "Modelo Multiple Más Apropiado")

###################################################
###################################################
#H0: med = 150
#Ha: med > 150
# alfa : 0.01
#find de stadistic Z
#ventas_region1
Ho <- 150/length(ventas_region1)
str(mean(ventas_region1))
z <- mean(ventas_region1) -Ho / (sd(ventas_region1)/ sqrt(length(ventas_region1)))
print(z)
#Zalfa : 2.33
# RR: {Zalfa > 2.33}
p_valor <- pnorm(z, lower.tail=FALSE)
p_valor

###################################################
###################################################
#Predicciones

#Facebook
newFacebook=data.frame(facebook=c(300,320,338,350,400))# valores con los que se van a
#predecir la concentración de Ozono
predictionFacebook =predict(modeloFacebook,newFacebook,interval="prediction")# Intervalo de prediccion
confidenceFacebook =predict(modeloFacebook,newFacebook,interval='confidence')# Intervalo de confianza del 95%
matplot(newFacebook$facebook,cbind(predictionFacebook, confidenceFacebook[,-1]), lty=c(1,2,2,3,3),
        type="l", ylab="Predicción de ventas en millones para Facebook", xlab="Presupuesto de Inversion")

#Facebook
newFacebook=data.frame(facebook=c(300,320,338,350,400))# valores con los que se van a
#predecir la concentración de Ozono
predictionFacebook =predict(modeloFacebook,newFacebook,interval="prediction")# Intervalo de prediccion
confidenceFacebook =predict(modeloFacebook,newFacebook,interval='confidence')# Intervalo de confianza del 95%
matplot(newFacebook$facebook,cbind(predictionFacebook, confidenceFacebook[,-1]), lty=c(1,2,2,3,3),
        type="l", ylab="Predicción de ventas en millones para Facebook", xlab="Presupuesto de Inversion")

#Periodico
newPeriodico=data.frame(periodico=c(116, 130, 180, 195, 210))# valores con los que se van a
#predecir la concentración de Ozono
predictionPeriodico =predict(modeloPeriodico,newPeriodico,interval="prediction")# Intervalo de prediccion
confidencePeriodico =predict(modeloPeriodico,newPeriodico,interval='confidence')# Intervalo de confianza del 95%
matplot(newPeriodico$periodico,cbind(predictionPeriodico, confidencePeriodico[,-1]), lty=c(1,2,2,3,3),
        type="l", ylab="Predicción de ventas en millones para Periodico", xlab="Presupuesto de Inversion")

#Instagram
newInstagram=data.frame(instagram=c(52, 58, 63, 79, 81))# valores con los que se van a
#predecir la concentración de Ozono
predictionInstagram =predict(modeloInstagram,newInstagram,interval="prediction")# Intervalo de prediccion
confidenceInstagram =predict(modeloInstagram,newInstagram,interval='confidence')# Intervalo de confianza del 95%
matplot(newInstagram$instagram,cbind(predictionInstagram, confidenceInstagram[,-1]), lty=c(1,2,2,3,3),
        type="l", ylab="Predicción de ventas en millones para Instagram", xlab="Presupuesto de Inversion")

#Tv
newTv=data.frame(tv=c(920, 970, 988, 52, 1020, 1100))# valores con los que se van a
#predecir la concentración de Ozono
predictionTv =predict(modeloTv,newTv,interval="prediction")# Intervalo de prediccion
confidenceTv =predict(modeloTv,newTv,interval='confidence')# Intervalo de confianza del 95%
matplot(newTv$tv,cbind(predictionTv, confidenceTv[,-1]), lty=c(1,2,2,3,3),
        type="l", ylab="Predicción de ventas en millones para Tv", xlab="Presupuesto de Inversion")

#Ebay
newEbay=data.frame(ebay=c(2300, 2400, 2850, 3000, 3200))# valores con los que se van a
#predecir la concentración de Ozono
predictionEbay =predict(modeloEbay,newEbay,interval="prediction")# Intervalo de prediccion
confidenceEbay =predict(modeloEbay,newEbay,interval='confidence')# Intervalo de confianza del 95%
matplot(newEbay$ebay,cbind(predictionEbay, confidenceEbay[,-1]), lty=c(1,2,2,3,3),
        type="l", ylab="Predicción de ventas en millones para Ebay", xlab="Presupuesto de Inversion")


###################################################
###################################################
#ANOVA
#alfa: 0.03
par(mfrow = c(1, 1) ) # para dibujar dos graficos en una misma página:
boxplot(split(ventas,regiones), col = "orange", horizontal = F, xlab = "Ventas", main="Boxplot de VentasxRegion")
mod_lm = lm(ventas~regiones)
anova(mod_lm)
