if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,readxl,polycor,vegan,caTools,car,quantmod,MASS,corrplot,
       knitr,cowplot,nlme,Rchoice,AICcmodavg,mdscore,questionr) # pacotes necessários ----

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

p <- 6 # número de parâmetros do modelo | um pouco de dúvida aqui se realmente são 4...
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

# INTERPRETAÇÃO: Aqueles valores maiores que |2*(p/n)^(1/2)| 
# são possíveis valores influentes.

# Alguns gráficos do modelo ----
plot(fit7) # tem que dar <enter> no Console pra ir mostrando os gráficos


# Testando interação nas variáveis selecionadas_________________________________


fit7_2 <- lm(lny ~ lnX2*X8*X9*lnX10,data=dados)
summary(fit7_2)
anova7_2 <- aov(fit7_2)
summary(anova7_2)

stepwise2 <- step(fit7_2,direction = 'both',steps=10000000) 
summary(stepwise2)
anova_s <- aov(stepwise2)
summary(anova_s)
# Não gostei da seleção automática, vou fazer cherry-picking das 
# interações que deram significativas na ANOVA

# Novo modelo:__________________________________________________________________

fit_f <- lm(lny~ lnX2+X8+X9+lnX10+lnX2:X9+lnX2:lnX10+lnX2:X8:X9:lnX10,data = dados)
summary(fit_f)
anova_f <- aov(fit_f)
summary(anova_f)

shapiro.test(fit_f$residuals) 
qqnorm(fit_f$residuals)
qqline(fit_f$residuals)
# Aparentemente a interação quádrupla deixa de ser significativa aqui

# TESTE PARA POSSÍVEL MELHORA___________________________________________________

fit_f2 <- lm(lny~ lnX2+X8+X9+lnX10+lnX2:X9+lnX2:lnX10,data = dados)
summary(fit_f2) # r^2 = 0.8648. Muito bom!!
anova_f2 <- aov(fit_f2)
summary(anova_f2) # A tripla não é tãão significante, mas à 0,01 é; então creio que pode ser deixada no modelo.
# Aparentemente, este é o modelo final?

shapiro.test(fit_f2$residuals) # Normalidade bem ok (bem mais convincente que no modelo sem interações)
qqnorm(fit_f2$residuals)
qqline(fit_f2$residuals)


# INTERPRETAÇÃO:
# Ainda que na ANOVA tenhamos valores que corroborem para melhor desempelnho do
# modelo f2, os residuos sofrem um leve desgaste, dessa forma, compreendemos que
# o modelo f é o melhor.


#_______________________________ ANALISE GRÁFICA________________________________
# Análise dos valores outliers nos resíduos 
# (residuals standard e residuals studentized) ----
par(mfrow = c(1, 2))

plot(rstudent(fit_f))
plot(rstandard(fit_f))

# Analisando com o ggplot2______________________________________________________
# Ala do Brunao ________________________________________________________________
dados$rstudent <- rstudent(fit_f)

ggplot(dados, aes(x = 1:length(rstudent), y = rstudent)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 2, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = -2, linetype = "dashed", color = "blue") +
  geom_point() +
  #  geom_text(aes(label = round(rstudent, 2)), vjust = -1.5) +
  labs(x = "Observações", y = "rstudent") +
  ggtitle("Gráfico de rstudent") +
  theme_minimal()

dados$rstandard <- rstandard(fit_f)

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

dados$dffits <- dffits(fit_f)

p <- 12 # número de parâmetros do modelo (fiquei meio em dúvida aqui, se são 4; 5; 8 ou 9...)
n <- nrow(dados) # tamanho da amostra

# Criação da função que contabiliza:
obs = rep("",nrow(dados))
verificar_maior_modulo <- function(valor) {
  if (abs(valor) > 2*(p/n)^(1/2)) {
    return(i)
  } else {
    return("")
  }
}
# Criação da sequencia de variaveis acima do valor indicado
sequencia = c(rep("",nrow(dados)))
for ( i in 1:nrow(dados)){
sequencia[i] = verificar_maior_modulo(dados$dffits[i])  

}


ggplot(dados, aes(x = 1:length(dffits), y = dffits)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 2*(p/n)^(1/2), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = -(2*(p/n)^(1/2)), linetype = "dashed", color = "blue") +
  geom_point() +
   geom_text(aes(label = sequencia), vjust = 1, hjust = 1) +
  labs(x = "Observações", y = "dffits") +
  ggtitle("Gráfico de dffits") +
  theme_minimal()

# INTERPRETAÇÃO: 
# Aqueles valores maiores que |2*(p/n)^(1/2)| são possíveis valores influentes.

# Alguns gráficos do modelo_____________________________________________________
plot(fit_f) # tem que dar <enter> no console para os gráficos rodarem

confint(fit_f) # intervalo de confiança 95% pros parâmetros do modelo
vif(fit_f)


# Implementação do Lucas________________________________________________________


dados$Residuo = fit_f$residuals
dados$Preditos <- predict(fit_f)


dados$Leverage <- influence.measures(fit_f)[[1]][,6]
dados$CooksD <- influence.measures(fit_f)[[1]][,5]



g1 <- ggplot(dados,aes(Preditos,Residuo)) + 
  geom_point(na.rm = T) + geom_hline(yintercept=0)+ 
  theme_classic()+labs(x = "Valores Ajustados", y="Residuos") 


verificar_maior_modulo.rs <- function(valor) {
  if (abs(valor) > 2) {
    return(i)
  } else {
    return("")
  }
}
# Criação da sequencia de variaveis acima do valor indicado
sequenciars = c(rep("",nrow(dados)))
for ( i in 1:nrow(dados)){
  sequenciars[i] = verificar_maior_modulo.rs(dados$rstudent[i])  
  
}

g2 <- ggplot(dados,aes(Preditos,rstudent)) + geom_point(na.rm = T) +
  geom_hline(yintercept=c(-2,2))+ theme_classic()+
  labs(x = "Valores Ajustados", y="RStudent") + 
  scale_y_continuous(limits=c(-3.5,4)) +
  geom_text(aes(label = sequenciars), vjust = 1, hjust = 1)


g3 <- ggplot(dados,aes(Leverage,rstudent)) + geom_point(na.rm = T) +
  geom_hline(yintercept=c(-2,2))+ theme_classic()+
  labs(x = "Leverage", y="RStudent") + 
  scale_y_continuous(limits=c(-4,4)) +
  geom_text(aes(label = sequenciars), vjust = 0, hjust = 1)

g4 <- ggplot(dados,aes(Preditos,X1)) + geom_point(na.rm = T) + 
  theme_classic()+labs(x = "Valores Ajustados", y="Resposta") + 
  scale_x_continuous(limits = c(.1,0.5)) + 
  scale_y_continuous(limits = c(.1,.5)) + 
  geom_abline(slope = 1, intercept = 0)


verificar_maior_modulo.cD <- function(valor) {
  if (abs(valor) > .15) {
    return(i)
  } else {
    return("")
  }
}
# Criação da sequencia de variaveis acima do valor indicado
sequenciacD = c(rep("",nrow(dados)))
for ( i in 1:nrow(dados)){
  sequenciacD[i] = verificar_maior_modulo.cD(dados$CooksD[i])  
  
}


g5 <- ggplot(dados,aes((1:nrow(dados)),CooksD)) + geom_point(na.rm = T) +
  geom_line(na.rm = T)+ theme_classic()+ 
  labs(x = "Observacao", y="Distancia de Cook") + 
  scale_x_discrete(limits = c(1,2,3,4,5,6,7)) + 
  scale_y_continuous(limits=c(-.4,.4)) +
  geom_text(aes(label = sequenciacD), vjust = 1, hjust = 1)


ggQQ = function(lm) {
  d <- data.frame(std.resid = residuals(lm))
  # calculate 1Q/4Q line
  y <- quantile(d$std.resid[!is.na(d$std.resid)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  p <- ggplot(data=d, aes(sample=std.resid)) +
    stat_qq(shape=1, size=2) +        # open circles
    labs(x="Quantis",                 # x-axis label
         y="Residuo") +               # y-axis label
    geom_abline(slope = slope, intercept = int, linetype="dashed")+
    theme_classic() 
  return(p)
}

g6 <-ggQQ(fit_f)  
plot_grid(g1,g2,g3,g4,g5,g6, ncol=3)# compilado de graficos da análise



# Testando retornar as variáveis originais______________________________________

fit <- lm(X1~X2+X8+X9+X10+X2:X9+X2:X10+X2:X8:X9:X1,data=dados)
summary(fit) # r2 bom, x2 e x10 perdem a significância.
anova <- aov(fit)
summary(anova) # aqui, x2 e x10 são significativos; 
# porém x2:x10 perde a significância à 5% 
# (talvez seja melhor remover essa interação?)

confint(fit)
# temos 11 variaveis como sendo as descritas para o modelo 
# (incluindo interações)
vif(fit)

# Checando Problemas de Multicolinearidade======================================

# variáveis transformadas_______________________________________________________
t1 <- data.frame(dados$lnX2,dados$X8,dados$lnX10)
# Como X9 não é numérica, não é possível testar correlação.
cor(t1) 
# ln X2 tem correlação moderada com X8; e ln X2 tem correlação fraca 
# com ln X10. Correlação de X8 com ln X10 é quase nula.

# Verificando nas variáveis originais___________________________________________
t2 <- data.frame(dados$X2,dados$X8,dados$X10) 
# Como X9 não é numérica, não é possível testar correlação.
cor(t2) 
# Um padrão muito parecido aqui, apenas a correlação de X8 com
# ln X10 aumenta um pouco, mas continua fraca.

# Aparentemente, não teremos problemas de multicolinearidade com este modelo.


# Testando o ajuste do modelo no conjunto de teste______________________________
teste$lnypred <- predict(fit_f, newdata = teste)
teste$X1_pred <- exp(teste$lnypred)

ln_m0_MSE  <- mean(teste$lnypred - teste$lny)^2;ln_m0_MSE
m0_MSE  <- mean(teste$X1_pred - teste$X1)^2;m0_MSE

# Em ln, o modelo está com um MSE baixíssimo dos valores preditos pros valores reais.
# desfazendo a transformação, sobe para um valor aparentemente alto, mas que deve ser analisado com calma, pela escala da variável.

#------------------------------------------------------------------------------#
# Validação do modelo pelos coeficiente de Mallow.


p_load(tidyverse,knitr,cowplot,nlme,Rchoice,AICcmodavg,mdscore,questionr,olsrr)
fit_f$coefficients
# 11 variaveis 
model <- fit_f
model2 <- lm(lny ~ lnX2+X8+X9+lnX10,data=dados)
model3 <- lm(lny ~ lnX2+X4+X5+X6+X8+X9+lnX10,data=dados)
model4 <- lm(lny ~ lnX2+X4+X8+X9+lnX10,data=dados)
fullmodel <- lm(lny ~ lnX2*X3*X4*X5*X6*X7*X8*X9*lnX10*X11,data=dados)

ols_mallows_cp(model, fullmodel)# converge quase certamenta para o numero de parâmetros 14 para 12(= p)
ols_mallows_cp(model2, fullmodel)
ols_mallows_cp(model3, fullmodel)
ols_mallows_cp(model4, fullmodel)


model5 <- lm(lny ~ lnX2*X3*X4*X5*X6*X7*X8*X9*lnX10*X11,data=dados)
summary(model5)
anova_model5 <- aov(model5)
summary(anova_model5)
ols_mallows_cp(model5, fullmodel)

# MELHOR MODELO POSSÍVEL é O MODELO F.
ols_mallows_cp(model, fullmodel)# Melhor critério até o momento.



#______________________________________________________________________________
# Análise de acrescimo de informação============================================

fit_f <- lm(lny~ lnX2+X8+X9+lnX10+lnX2:X9+lnX2:lnX10+lnX2:X8:X9:lnX10,data = dados)
summary(fit_f)
anova_f <- aov(fit_f)
summary(anova_f)

shapiro.test(fit_f$residuals) 
qqnorm(fit_f$residuals)
qqline(fit_f$residuals)

# Modelo principal com:
summary(model) # R2 = 0.8673 de R2 ponderado

# SOMA DE QUADRADOS TOTAL
SQTO = sum((dados$lny - mean(dados$lny))^2)

# Modelo mais simples: lnX2_____________________________________________________

summary(lm(data=dados, lny ~ lnX2))# R2 = 0.7231
Simp1 = (summary(aov(lm(data=dados, lny ~ lnX2))))[[1]]$`Sum Sq`[c(1:2)]
Simp1 = round(Simp1[1]/SQTO,5)*100

shapiro.test((lm(data=dados, lny ~ lnX2))$residuals)

p.v_simp1 = (shapiro.test((lm(data=dados, lny ~ lnX2))$residuals))$p.value


# Modelo mais simples: lnX2 + X8________________________________________________

summary(lm(data=dados, lny ~ lnX2 + X8))# R2 = 0.7787
Simp2 = (summary(aov(lm(data=dados, lny ~ lnX2 + X8))))[[1]]$`Sum Sq`[c(1:3)]
Simp2 = round(Simp2[2]/SQTO,5)*100

shapiro.test((lm(data=dados, lny ~ lnX2 + X8))$residuals)

p.v_simp2 = (shapiro.test((lm(data=dados, lny ~ lnX2 + X8))$residuals))$p.value


# Modelo mais simples: lnX2 + X8 + X9___________________________________________


summary(lm(data=dados, lny ~ lnX2 + X8 + X9))# R2 = 0.8298
Simp3 = (summary(aov(lm(data=dados, lny ~ lnX2 + X8 + X9))))[[1]]$`Sum Sq`[c(1:4)]
Simp3 = round(Simp3[3]/SQTO,5)*100

shapiro.test((lm(data=dados, lny ~ lnX2 + X8 + X9))$residuals)

p.v_simp3 = (shapiro.test((lm(data=dados, lny ~ lnX2 + X8 + X9))$residuals))$p.value

# Modelo mais simples: lnX2 + X8 + X9 + lnX10___________________________________
summary(lm(data=dados, lny ~ lnX2 + X8 + X9 +lnX10))# R2 = 0.8466
Simp4 = (summary(aov(lm(data=dados, lny ~ lnX2 + X8 + X9 +lnX10))))[[1]]$`Sum Sq`[c(1:5)]
Simp4 = round(Simp4[4]/SQTO,5)*100

shapiro.test((lm(data=dados, lny ~ lnX2 + X8 + X9 + lnX10 ))$residuals)

p.v_simp4 = (shapiro.test((lm(data=dados, lny ~ lnX2 + X8 + X9 + lnX10 ))$residuals))$p.value
  
  
# Modelo com interações: lnX2 + X8 + X9 + lnX10 + lnX2:X9_______________________
summary(lm(data=dados, lny ~ lnX2 + X8 + X9 + lnX10 + lnX2:X9))# R2 = 0.8622 
comp1 = (summary(aov(lm(data=dados, lny ~ lnX2 + X8 + X9 + lnX10 + lnX2:X9))))[[1]]$`Sum Sq`[c(1:6)]
comp1 = round(comp1[5]/SQTO,5)*100

shapiro.test((lm(data=dados, lny ~ lnX2 + X8 + X9 + lnX10 + lnX2:X9))$residuals)
p.v_comp1 = (shapiro.test((lm(data=dados, lny ~ lnX2 + X8 + X9 + lnX10 + lnX2:X9))$residuals))$p.value
  

# Modelo com interações:  lnX2 + X8 + X9 + lnX10 + lnX2:X9 + lnX2:lnX10_________
summary(lm(data=dados, lny ~ lnX2 + X8 + X9 + lnX10 + lnX2:X9 + lnX2:lnX10))# R2 = 0.8648 
comp2 = (summary(aov(lm(data=dados, lny ~ lnX2 + X8 + X9 + lnX10 + lnX2:X9 + lnX2:lnX10))))[[1]]$`Sum Sq`[c(1:7)]
comp2 = round(comp2[6]/SQTO,5)*100

shapiro.test((lm(data=dados, lny ~ lnX2 + X8 + X9 + lnX10 + lnX2:X9 + lnX2:lnX10))$residuals)

p.v_comp2 = (shapiro.test((lm(data=dados, lny ~ lnX2 + X8 + X9 + lnX10 + lnX2:X9 + lnX2:lnX10))$residuals))$p.value
  

# Modelo com interações: lnX2 + X8 + X9 + lnX10 + lnX2:X9 + lnX2:lnX10 + lnX2:X8:X9:lnX10
summary(lm(data=dados, lny ~ lnX2 + X8 + X9 + lnX10 + lnX2:X9 + lnX2:lnX10 + lnX2:X8:X9:lnX10 ))# R2 = 0.8673 
comp3 = (summary(aov(lm(data=dados, lny ~ lnX2 + X8 + X9 + lnX10 + lnX2:X9 + lnX2:lnX10 + lnX2:X8:X9:lnX10 ))))[[1]]$`Sum Sq`[c(1:8)]
comp3 = round(comp3[7]/SQTO,5)*100

shapiro.test((lm(data=dados, lny ~ lnX2 + X8 + X9 + lnX10 + lnX2:X9 + lnX2:lnX10 + lnX2:X8:X9:lnX10))$residuals)


p.v_comp3 = (shapiro.test((lm(data=dados, lny ~ lnX2 + X8 + X9 + lnX10 + lnX2:X9 + lnX2:lnX10 + lnX2:X8:X9:lnX10))$residuals))$p.value
  

# GRáfico de contribuição de cada componente do modelo:_________________________

cascata = data.frame(
  
  Variaveis = as.factor(c("lnX2","X8","X9","lnX10","lnX2:X9","lnX2:lnX10","lnX2:X8:X9:lnX10")),
  Valores = c(Simp1,Simp2,Simp3,Simp4,comp1,comp2,comp3)

)

cascata$Variaveis <- factor(cascata$Variaveis, levels = c("lnX2","X8","X9","lnX10","lnX2:X9","lnX2:lnX10","lnX2:X8:X9:lnX10"))


# "Porcetagem de explicação por variável"
library(waterfalls)
waterfall(values = cascata$Valores,
          labels = cascata$Variaveis)


# Convergência do p-valor para a normalidade conforme as variáveis são acrescentadas:


pvalores = data.frame(
  
  Variaveis = as.factor(c("lnX2","X8","X9","lnX10","lnX2:X9","lnX2:lnX10","lnX2:X8:X9:lnX10")),
  P.Valores = as.numeric(c(p.v_simp1,p.v_simp2,p.v_simp3,p.v_simp4,p.v_comp1,p.v_comp2,p.v_comp3))
  
)

pvalores$Variaveis <- factor(pvalores$Variaveis, levels = c("lnX2","X8","X9","lnX10","lnX2:X9","lnX2:lnX10","lnX2:X8:X9:lnX10"))


ggplot(data = pvalores, aes(x = Variaveis, y = P.Valores)) +
  geom_point() +
  labs(title = "Gráfico de Linhas da convergência do p-valor",
       x = "Eixo X",
       y = "Eixo Y") +
  theme_minimal()

