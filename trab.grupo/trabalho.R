if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,readxl,polycor,vegan,caTools,car,quantmod,MASS,corrplot) # pacotes necessários ----
dados <- read_excel("trab.grupo/trabalho/DADOS_TRABALHO_2023_1.xlsx", # dados ----
                                    sheet = "Dados")
dados$ID <- factor(dados$ID)
dados$X1 <- as.numeric(dados$X1)
dados$X2 <- as.numeric(dados$X2)
dados$X3 <- as.numeric(dados$X3)
dados$X4 <- as.numeric(dados$X4)
dados$X5 <- factor(dados$X5)
dados$X6 <- as.numeric(dados$X6)
dados$X7 <- factor(dados$X7)
dados$X8 <- as.numeric(dados$X8)
dados$X9 <- factor(dados$X9)
dados$X10 <- as.numeric(dados$X10)
dados$X11 <- factor(dados$X11)

#transformando variável ano em idade da casa
dados$X8 <- dados$X8-1885
# repare que, da forma que fiz a transformação; quanto maior o novo valor, mais NOVA é a casa. A casa mais antiga irá apresentar valor de X8 = 0; enquanto, neste caso, a mais VELHA irá apresentar valor X8 = 113

#mean(dados$X8)

# transformando x1 em log para tentar normalidade dos residuos
boxplot(dados$X1)
dados$lny <- log(dados$X1)
boxplot(dados$lny) # com a transformação, a variável apresenta menos outliers. talvez assim fique melhor de trabalhar.

# realizando o mesmo procedimento com as variáveis x2 e x10
boxplot(dados$X2)
dados$lnX2 <- log(dados$X2)
boxplot(dados$lnX2)

boxplot(dados$X10)
dados$lnX10 <- log(dados$X10)
boxplot(dados$lnX10) # essa segue com bastante outliers, apesar da transformação. Mas ao menos a distância da mediana aparenta ser menor.

# Salvando os dados brutos num 'cofre'
cofre <- dados

# Setando a seed do sample para garantir reprodutibilidade (favor não mexer nisso)
set.seed(150167636)

# Selecionando as 300 obs pro modelo de treino
dados <- sample_n(cofre,300)

# Separando o conjunto de dados de teste.
teste <- anti_join(cofre,dados)


# Modelo contendo todas as variáveis ---- 
fit <- lm(lny ~ lnX2+X3+X4+X5+X6+X7+X8+X9+lnX10+X11,data=dados)
alias(fit) # Existem variáveis linearmente dependentes, ou seja, devem ser eliminadas para o modelo funcionar.
car::vif(fit)
# Coeficientes do modelo ---- 
summary(fit)

# Analisando os coeficientes, notamos um valor r2 muito bom = 0.8463.
# Além disso, notamos que as variáveis X3 e X5 aparentam realmente não pertencer ao modelo, sob qualquer nível de significância.

# ANOVA do modelo ---- 
anova <- aov(fit)
summary(anova) # Pela ANOVA, reforçamos que X3 definitivamente deve ser removida do modelo.

fit2 <- lm(lny ~ lnX2+X4+X5+X6+X7+X8+X9+lnX10+X11,data=dados)
summary(fit2)

anova2 <- aov(fit2)
summary(anova2)

# o r^2 continua bom, e X7 segue aparentando não ajustar bem ao modelo. Vamos removê-la também.

fit3 <- lm(lny ~ lnX2+X4+X5+X6+X8+X9+lnX10+X11,data=dados)
summary(fit3)

anova3 <- aov(fit3)
summary(anova3)

# X5 aparenta ter se ajustado ao modelo, mas X11 aparenta ainda estar bem fora do modelo sob o nível de significância adotado

fit4 <- lm(lny ~ lnX2+X4+X5+X6+X8+X9+lnX10,data=dados)
summary(fit4)

anova4 <- aov(fit4)
summary(anova4)


# Avaliação do modelo por métodos automáticos ----
stepwise <- step(fit4,direction = 'both') # stepwise
backward <- step(fit4,direction = 'backward') # backward
forward <- step(fit4,direction = 'forward') # forward

summary(stepwise)
summary(backward)
summary(forward)

# Removendo X5
fit5 <- lm(lny ~ lnX2+X4+X6+X8+X9+lnX10,data=dados)
summary(fit5)

anova5 <- aov(fit5)
summary(anova5)
# r quadrado continua excelente

stepwise <- step(fit5,direction = 'both') # stepwise
summary(stepwise)

# Testando tirar X6
fit6 <- lm(lny ~ lnX2+X4+X8+X9+lnX10,data=dados)
summary(fit6)

anova6 <- aov(fit6)
summary(anova6)
# r quadrado continua excelente

stepwise <- step(fit6,direction = 'both') # stepwise
summary(stepwise)

# Testando tirar X4
fit7 <- lm(lny ~ lnX2+X8+X9+lnX10,data=dados)
summary(fit7)

anova7 <- aov(fit7)
summary(anova7)
# r quadrado continua excelente

stepwise <- step(fit7,direction = 'both') # stepwise
summary(stepwise)


shapiro.test(fit7$residuals) # normalidade 'ok' a 5%
qqnorm(fit7$residuals)
qqline(fit7$residuals)

# Análise dos valores outliers nos resíduos (residuals standard e residuals studentized) ----
par(mfrow = c(1, 2))

plot(rstudent(fit7))
plot(rstandard(fit7))

# com o ggplot2 ----
dados$rstudent <- rstudent(fit7)

ggplot(dados, aes(x = 1:length(rstudent), y = rstudent)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 2, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = -2, linetype = "dashed", color = "blue") +
  geom_point() +
  #  geom_text(aes(label = round(rstudent, 2)), vjust = -1.5) +
  labs(x = "Observações", y = "rstudent") +
  ggtitle("Gráfico de rstudent") +
  theme_minimal()

dados$rstandard <- rstandard(fit7)

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

dados$dffits <- dffits(fit7)

p <- 4 # número de parâmetros do modelo | um pouco de dúvida aqui se realmente são 4...
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
plot(fit7) # tem que dar <enter> no Console pra ir mostrando os gráficos


# Testando interação nas variáveis selecionadas


fit7_2 <- lm(lny ~ lnX2*X8*X9*lnX10,data=dados)
summary(fit7_2)
anova7_2 <- aov(fit7_2)
summary(anova7_2)

stepwise2 <- step(fit7_2,direction = 'both',steps=1000000000) 
summary(stepwise2)
anova_s <- aov(stepwise2)
summary(anova_s)
# Não gostei da seleção automática, vou fazer cherry-picking das interações que deram significativas na ANOVA

fit_f <- lm(lny~ lnX2+X8+X9+lnX10+lnX2:X9+lnX2:lnX10+lnX2:X8:X9:lnX10,data = dados)
summary(fit_f)
anova_f <- aov(fit_f)
summary(anova_f)
# Aparentemente a interação quádrupla deixa de ser significativa aqui

fit_f2 <- lm(lny~ lnX2+X8+X9+lnX10+lnX2:X9+lnX2:lnX10,data = dados)
summary(fit_f2) # r^2 = 0.8648. Muito bom!!
anova_f2 <- aov(fit_f2)
summary(anova_f2) # A tripla não é tãão significante, mas à 0,01 é; então creio que pode ser deixada no modelo.
# Aparentemente, este é o modelo final?

shapiro.test(fit_f2$residuals) # Normalidade bem ok (bem mais convincente que no modelo sem interações)
qqnorm(fit_f2$residuals)
qqline(fit_f2$residuals)

# Análise dos valores outliers nos resíduos (residuals standard e residuals studentized) ----
par(mfrow = c(1, 2))

plot(rstudent(fit_f2))
plot(rstandard(fit_f2))

# com o ggplot2 ----
dados$rstudent <- rstudent(fit_f2)

ggplot(dados, aes(x = 1:length(rstudent), y = rstudent)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 2, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = -2, linetype = "dashed", color = "blue") +
  geom_point() +
  #  geom_text(aes(label = round(rstudent, 2)), vjust = -1.5) +
  labs(x = "Observações", y = "rstudent") +
  ggtitle("Gráfico de rstudent") +
  theme_minimal()

dados$rstandard <- rstandard(fit_f2)

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

dados$dffits <- dffits(fit_f2)

p <- 9 # número de parâmetros do modelo (fiquei meio em dúvida aqui, se são 4; 5; 8 ou 9...)
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
plot(fit_f2) # tem que dar <enter> no console para os gráficos rodarem

confint(fit_f2) # intervalo de confiança 95% pros parâmetros do modelo
vif(fit_f2)

# Testando retornar as variáveis originais

fit <- lm(X1~X2+X8+X9+X10+X2:X9+X2:X10,data=dados)
summary(fit) # r2 bom, x2 e x10 perdem a significância.
anova <- aov(fit)
summary(anova) # aqui, x2 e x10 são significativos; porém x2:x10 perde a significância à 5% (talvez seja melhor remover essa interação?)

confint(fit)
vif(fit)

# checando problemas de multicolinearidade
# variáveis transformadas
t1 <- data.frame(dados$lnX2,dados$X8,dados$lnX10) # Como X9 não é numérica, não é possível testar correlação.
cor(t1) # ln X2 tem correlação moderada com X8; e ln X2 tem correlação fraca com ln X10. Correlação de X8 com ln X10 é quase nula.

# Verificando nas variáveis originais
t2 <- data.frame(dados$X2,dados$X8,dados$X10) # Como X9 não é numérica, não é possível testar correlação.
cor(t2) # Um padrão muito parecido aqui, apenas a correlação de X8 com ln X10 aumenta um pouco, mas continua fraca.

# Aparentemente, não teremos problemas de multicolinearidade com este modelo.


# Testando o ajuste do modelo no conjunto de teste
teste$lnypred <- predict(fit_f2, newdata = teste)
teste$X1_pred <- exp(teste$lnypred)

ln_m0_MSE  <- mean(teste$lnypred - teste$lny)^2
m0_MSE  <- mean(teste$X1_pred - teste$X1)^2
# Em ln, o modelo está com um MSE baixíssimo dos valores preditos pros valores reais.
# desfazendo a transformação, sobe para um valor aparentemente alto, mas que deve ser analisado com calma, pela escala da variável.
anova(fit_f2)
# --------------------------------------------------------------------------- #
p_load(tidyverse,knitr,cowplot,nlme,Rchoice,AICcmodavg,mdscore,questionr,olsrr)

model <- fit_f2
model2 <- lm(lny ~ lnX2+X8+X9+lnX10,data=dados)
model3 <- lm(lny ~ lnX2+X4+X5+X6+X8+X9+lnX10,data=dados)
model4 <- lm(lny ~ lnX2+X4+X8+X9+lnX10,data=dados)
fullmodel <- lm(lny ~ lnX2*X3*X4*X5*X6*X7*X8*X9*lnX10*X11,data=dados)

ols_mallows_cp(model, fullmodel)
ols_mallows_cp(model2, fullmodel)
ols_mallows_cp(model3, fullmodel)
ols_mallows_cp(model4, fullmodel)


model5 <- lm(lny ~ lnX2*X3*X4*X5*X6*X7*X8*X9*lnX10*X11,data=dados)
summary(model5)
anova_model5 <- aov(model5)
summary(anova_model5)

stepwise <- step(model5,direction = 'both',steps=1000000000) # demora muito
summary(stepwise) # resultado ruim
anova_s <- aov(stepwise)
summary(anova_s) # cheio de singularidade, multicolinearidade, overfitting. Horrível. Melhor o que eu tinha selecionado antes mesmo 



# --------------------------------------------------------------------------- #

# Testes que estão meio bugados:
p_load(olsrr)
test <- ols_step_all_possible(fit7_2) # demora bastante pra rodar; num pc bom. resultado confuso. não recomendo rodar.
plot(test)

test2 <- ols_step_both_p(fit4_2, pent = .05, prem = .3, details = TRUE)
