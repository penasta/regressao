
library(pacman)
p_load(readxl)

dados <- read_excel("rdocs/dados/Dados_ex_6_5.xlsx")

fit <- lm(Y ~ X1 + X2,dados)
summary(aov(fit))
summary(fit)

# ---------------------------------------------------------------------------- #

XHB <- t(c(1,500,25))
BH <- t(t(c(26.340273,0.0336254,0.8945244)))
YHh <- XHB %*% BH # = YHh

t <- qt(1-0.10/2,15-3) # quantil da t para 1-alpha=.9; bilateral; 15-3=12 g.l.

A <- matrix(c(.5751552,-.000162,-.021959,
              -.000162,3.0994e-6,-.000058,
              -.021959,-.000058,.0023396),3,3)

MM <- XHB %*% A %*% t(XHB)

MSE <- 20.38

IC <- t*sqrt(MSE*(1/4 + MM)) # Intervalo p/ YHh ~ 65,51
YHh + IC
YHh - IC

# ---------------------------------------------------------------------------- #

# X1: espessura da dobra do triceps
# X2: circunferencia da coxa
# X3: circunferencia do braço

# Y: quantidade de gordura no corpo

# ---------------------------------------------------------------------------- #

library(pacman)
p_load(readxl)
gordura <- read_excel("dados/gordura.xlsx")

model1 <- lm(Y ~ X1, data=gordura)
model1

model2 <- lm(Y ~ X1+X2, data=gordura)
model2

model3 <- lm(Y ~ X1+X2+X3, data=gordura)
model3

anova(model3)

aov(model3)
