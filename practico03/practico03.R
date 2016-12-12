# PRÁCTICO 3. Violación de la Independencia. Modelo mixtos.

## Caso 1.
rikz <- read.table("RIKZ.txt", header = TRUE)
# rikz <- read.table(file = file.choose(), header = TRUE)

# Codificar factores
rikz$Beach <- as.factor(rikz$Beach)
rikz$Exposure <- as.factor(rikz$Exposure)

library(nlme)

# 1) Elección de la mejor parte random.
# La parte fija debe ser lo más compleja posible.
# No se estima random para la "pendiente" de Exposure
# porque esta variable no tiene variabilidad dentro de 
# cada playa.

r1 <- lme(Richness ~ NAP*Exposure, random = ~ 1 | Beach, 
          data = rikz, method = "REML")
r2 <- lme(Richness ~ NAP*Exposure, random = ~ 1 + NAP | Beach, 
          data = rikz, method = "REML")
AIC(r1)
AIC(r2)

# 2) Eleccion de la parte fija.
# Conservamos la estrutura random elegida en el paso anterior y 
# Probamos diferentes estructuras fijas.
# Reajustamos el modelo por ML.

f1 <- lme(Richness ~ NAP*Exposure, random = ~ 1 | Beach, 
          data = rikz, method = "ML")
f2 <- lme(Richness ~ NAP+Exposure, random = ~ 1 | Beach, 
          data = rikz, method = "ML")
anova(f1, f2) #se puede usar AIC, BIC o L.ratios test

# 3) Presentamos el modelo (ajustado por REML)
fit <- lme(Richness ~ NAP+Exposure, random = ~ 1 | Beach, 
           data = rikz, method = "REML")
summary(fit)

# 4) Diagnósticos gráficos.
res <- resid(fit, type = "normalized")
pre <- fitted(fit)

qqnorm(res)

plot(pre, res)

# Puede examinarse la relación entre los residuos y las variables x

plot(res ~ rikz$Exposure)
plot(res ~ rikz$NAP)


## Caso 2.

rikz <- read.table("RIKZ.txt", header = TRUE)
# rikz <- read.table(file = file.choose(), header = TRUE)

# Codificar factores
rikz$Beach <- as.factor(rikz$Beach)
rikz$Exposure <- as.factor(rikz$Exposure)

library(lme4)

# 1) Elección de la mejor parte random.
# La parte fija debe ser lo más compleja posible.
# REML es dificil de definir en GLMM.

r1 <- glmer(Richness ~ NAP*Exposure + (1|Beach), 
            data = rikz, family = poisson)
r2 <- glmer(Richness ~ NAP*Exposure + (NAP|Beach), 
            data = rikz, family = poisson)
AIC(r1)
AIC(r2)

# 2) Eleccion de la parte fija.
# Conservamos la estrutura random elegida en el paso anterior y 
# Probamos diferentes estructuras fijas.

f1 <- glmer(Richness ~ NAP*Exposure + (NAP|Beach) + (0+NAP|Beach), 
          data = rikz, family=poisson)
f2 <- glmer(Richness ~ NAP+Exposure + (NAP|Beach) + (0+NAP|Beach), 
          data = rikz, family=poisson)
anova(f1, f2) #se puede usar AIC, BIC o L.ratios test

# 3) Presentamos el modelo.
fit <- glmer(Richness ~ NAP+Exposure + (NAP|Beach) + (0+NAP|Beach), 
          data = rikz, family=poisson)
summary(fit)

# 4) Diagnósticos gráficos.
res <- resid(fit, type = "pearson")
pre <- fitted(fit)

qqnorm(res)
plot(pre, res)

# Puede examinarse la relación entre los residuos y las variables x
plot(res ~ rikz$Exposure)
plot(res ~ rikz$NAP)

# 5) Cómo puedo probar (o ajustar) un modelo sobredisperso?
# (ejemplo)
library(MASS)
qfit <- glmmPQL(Richness ~ NAP+Exposure, random = ~NAP|Beach, 
                data = rikz, family=poisson)
summary(qfit)

# Observar: ¿cambian las significancias de los test t entre los 
# modelos ajustados con glmer y glmmPQL?

### END ###

