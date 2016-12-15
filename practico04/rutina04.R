# PRÁCTICO 4. Violación de la independencia y estructuras de correlación.

# Caso 1.

dat <- read.table("tempcorr.txt", header = TRUE)

plot(abund ~ year, data = dat)
plot(abund ~ rain, data = dat)

library(nlme)

# Modelo Lineal común (violación de supuestos)
m0 <- gls(abund ~ rain + year, na.action = na.omit, data = dat)
summary(m0)
AIC(m0)
plot(m0)
acf(m0$residuals)

# Modelo con autocorrelación de simetría compuesta
m1 <- gls(abund ~ rain + year, na.action = na.omit, data = dat,
          correlation = corCompSymm(form = ~ year))
summary(m1)
AIC(m1)

# Modelo con autocorrelación AR-1
m2 <- gls(abund ~ rain + year, na.action = na.omit, data = dat,
          correlation = corAR1(form = ~ year))
summary(m2)
AIC(m2)

## Caso 2.

dat <- read.table("Boreality.txt", header = TRUE)
head(dat)

## modelo lineal (incorrecto)
fit1 <- lm (Bor ~ Wet, data = dat)
summary(fit1)

## visualización de los residuos en el espacio
## 1) Bubble plots
library(gstat)
library(sp)
E <- rstandard(fit1)
spatial <- data.frame(E, dat$x, dat$y)
coordinates(spatial) <- c("dat.x", "dat.y")
bubble(spatial, "E")

## 2) Variograma
## el eje y indica cuan diferentes son las obs.
Vario1 = variogram(E ~ 1, spatial)
plot(Vario1)

## Agregar correlaciones al modelo
library(nlme)

m1 <- gls(Bor ~ Wet, data = dat) # sin correlación

m2 <- gls(Bor ~ Wet, data = dat, correlation = 
            corSpher(form =~ x+y, nugget = TRUE))

m3 <- gls(Bor ~ Wet, data = dat, correlation = 
            corLin(form =~ x+y, nugget = TRUE))

m4 <- gls(Bor ~ Wet, data = dat, correlation = 
            corRatio(form =~ x+y, nugget = TRUE))

m5 <- gls(Bor ~ Wet, data = dat, correlation = 
            corGaus(form =~ x+y, nugget = TRUE))

m6 <- gls(Bor ~ Wet, data = dat, correlation = 
            corExp(form =~ x+y, nugget = TRUE))

AIC(m1, m2, m3, m4, m5, m6)

## Examinando nuevamente los variogramas
var1 <- Variogram(m1, form =~x+y, robust = TRUE, 
                  resType = "normalized")
plot(var1)

var1 <- Variogram(m1, form =~x+y, robust = TRUE, 
                  resType = "normalized", maxDist = 2000)
plot(var1)


var6 <- Variogram(m6, form =~x+y, robust = TRUE, 
                  resType = "normalized", maxDist = 5000)
plot(var6)

## modelo final
summary(m6)
plot(m6)

### END ###