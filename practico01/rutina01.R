## PRACTICO1. Datos de conteo. Regresion de Poisson.

### Caso1

ants <- read.table("hormigas.txt", header = TRUE)
# ants <- read.table(file = file.choose(), header = TRUE)

# modelo
gfit1 <- glm(Srich ~ Habitat + Latitude + Elevation, data = ants, family = poisson)

# Tabla "tipo regresión" valores de los parámetros y prueba de Wald
summary(gfit1)

# Tabla de análisis de la devianza
anova(gfit1, test = "Chisq")

# significancia del modelo completo (cociente de verosimilitud)
gfit0 <- glm(Srich ~ 1, data = ants, family = poisson)
anova(gfit0, gfit1, test = "Chisq")

# DIAGNÓSTICOS COMUNES
library(car)
vif(gfit1)

layout(matrix(1:4, 2, 2))
plot(gfit1)
layout(1)

# DIAGNÓSTICOS PARA MLG

# 1¿Es adecuada la relación media-varianza? (¿es el parámetro de
# dispersión = a 1?)
gfit2 <- glm(Srich ~ Habitat + Latitude + Elevation, data = ants,
family = quasipoisson(link=log))
summary(gfit2)
anova(gfit2, test = "F")

# 2 ¿Es adecuado el enlace?
PL <- gfit1$linear.predictors^2
gfit3 <- glm(Srich ~ Habitat + Latitude + Elevation + PL, data=ants,
family = poisson(link=log))
summary(gfit3)

# INTERPRETACIÓN DE LOS PARÁMETROS
be <- gfit1$coefficients
exp(be)

IC <- confint(gfit1)
exp(IC)

### END ###
