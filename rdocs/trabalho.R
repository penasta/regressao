if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,readxl) # pacotes necessários ----
dados <- read_excel("rdocs/trabalho/DADOS_TRABALHO_2023_1.xlsx", # dados ----
                                    sheet = "Dados", col_types = c("numeric", 
                                                                   "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric"))

# Modelo contendo todas as variáveis ---- 
fit <- lm(X1 ~ X2+X3+X4+X5+X6+X7+X8+X9+X10+X11,data=dados)

# Coeficientes do modelo ---- 
summary(fit)

# ANOVA do modelo ---- 
anova <- aov(fit)
summary(anova)

# Análise dos valores outliers nos resíduos (residuals standard e residuals studentized) ----
par(mfrow = c(1, 2))

plot(rstudent(fit))
plot(rstandard(fit))

# com o ggplot2 ----
dados$rstudent <- rstudent(fit)

ggplot(dados, aes(x = 1:length(rstudent), y = rstudent)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 2, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = -2, linetype = "dashed", color = "blue") +
  geom_point() +
#  geom_text(aes(label = round(rstudent, 2)), vjust = -1.5) +
  labs(x = "Observações", y = "rstudent") +
  ggtitle("Gráfico de rstudent") +
  theme_minimal()

dados$rstandard <- rstandard(fit)

ggplot(dados, aes(x = 1:length(rstandard), y = rstandard)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 2, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = -2, linetype = "dashed", color = "blue") +
  geom_point() +
  #  geom_text(aes(label = round(rstandard, 2)), vjust = -1.5) +
  labs(x = "Observações", y = "rstandard") +
  ggtitle("Gráfico de rstandard") +
  theme_minimal()

# INTERPRETAÇÃO: Aqueles valores maiores que |2| são possíveis outliers.

# Análise de valores influentes ----

dados$dffits <- dffits(fit)

p <- 11 # número de parâmetros do modelo
n <- nrow(dados) # tamanho da amostra

ggplot(dados, aes(x = 1:length(dffits), y = dffits)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 2*(p/n)^(1/2), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = -(2*(p/n)^(1/2)), linetype = "dashed", color = "blue") +
  geom_point() +
  #  geom_text(aes(label = round(dffits, 2)), vjust = -1.5) +
  labs(x = "Observações", y = "dffits") +
  ggtitle("Gráfico de dffits") +
  theme_minimal()

# INTERPRETAÇÃO: Aqueles valores maiores que |2*(p/n)^(1/2)| são possíveis valores influentes.

# Alguns gráficos do modelo ----
plot(fit)

# Avaliação do modelo por métodos automáticos ----
stepwise <- step(fit,direction = 'both') # stepwise
backward <- step(fit,direction = 'backward') # backward
forward <- step(fit,direction = 'forward') # forward

summary(stepwise)
summary(backward)
summary(forward)
