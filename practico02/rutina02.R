# PRÁCTICO 2. Regresión para datos binomiales agregados y no agregados.

## Caso 1.
ven <- read.table("veneno.txt", header = TRUE)
# ven <- read.table(file = file.choose(), header = TRUE)

# construcción de la variable respuesta
rta <- cbind(ven$muertos, ven$tot-ven$muertos)

# modelo
vfit <- glm(rta ~ veneno + dosis, data = ven, family = binomial(logit))

# significancia según el estadístico de Wald
summary(vfit)

# análisis de la devianza
anova(vfit, test = "Chisq")

# examen gráfico de los residuos
layout(matrix(1:4, 2, 2))
plot(vfit)
layout(1)

# examen sobre la presencia de sobredispersión
vfit2 <- glm(rta ~ veneno + dosis, data = ven, family = quasibinomial(logit))
summary(vfit2)

# examen sobre la pertinencia del enlace
LP <- vfit$linear.predictors^2
vfit3 <- glm(rta ~ veneno + dosis + LP, data = ven, family = binomial(logit))
summary(vfit3)

#INTERPRETACIÓN DE PARÁMETROS
exp(vfit$coeff)

# parámetros faltantes:
ven$veneno2 <- factor(ven$veneno, levels = c('R', 'M', 'D'))
reor.vfit <- glm(rta ~ veneno2 + dosis, family = binomial(logit), data = ven)
summary(reor.vfit)
exp(reor.vfit$coeff)
1/exp(reor.vfit$coeff)


## Caso 2.

datos <- read.table("uta.txt", header = TRUE)
# datos <- read.table(file = file.choose(), header = TRUE)

# modelo
fit <- glm(Uta ~ PA.ratio, data = datos, family = binomial(logit))

# parámetros y su significancia según el estadístico de Wald
summary(fit)

# interpretación de parámetros.
exp(fit$coefficients)

# notar cómo se vuelve más interpretable el intercepto al
# centrar la variable.
datos$PA.ratio.2 <- datos$PA.ratio - mean(datos$PA.ratio, na.rm = T)
fit2 <- glm(Uta ~ PA.ratio.2, data = datos, family = binomial(logit))
summary(fit2)
exp(fit2$coefficients)

# análisis de la devianza
anova(fit2, test = "Chisq")

# examen gráfico de los residuos
layout(matrix(1:4, 2, 2))
plot(fit2)
layout(1)

# examen sobre la pertinencia del enlace
LP <- fit2$linear.predictors^2
fit3 <- glm(Uta ~ PA.ratio.2 + LP, data = datos, family = binomial(logit))
summary(fit3)

#gráfico con predict
X <- seq(0, 64, 0.5)
Y <- predict(fit2, data.frame(PA.ratio.2 = X), type = "response")
plot(Uta ~ PA.ratio.2, data = datos, xlab = "perímetro/área",
ylab = "presencia de Uta")
points(X$PA.ratio.2, Y, type = "l")

### END ###
