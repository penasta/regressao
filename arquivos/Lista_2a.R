# Diretorio:
setwd("C:/Users/Acer/Documents/UnB/Analise de Regressao Linear/modulo 2")
# Pacotes:
pacman::p_load(tidyverse, readxl, ggcorrplot,GGally)

# 1)============================================================================

# A)____________________________________________________________________________

# X2 = 3

# Definir a função do polinômio
polinomio <- function(x1,x2) {
  return(25 + 3*x1 + 4*x2 + 1.5*x1*x2)
}

# Criar uma sequência de valores para x1
x1_values <- seq(-10, 10, length.out = 100)

# Calcular os valores do polinômio para x1 com x2 fixo em 3
x2_fixed <- 3
y_values <- 25 + 3 * x1_values + 4 * x2_fixed + 1.5 * x1_values * x2_fixed

# Criar o dataframe com os valores de x1 e y
data <- data.frame(x1 = x1_values, y = y_values)

# Plotar o gráfico
ggplot(data, aes(x = x1, y = y)) +
  geom_line() +
  labs(x = "x1", y = "Polinômio", title = "Gráfico do Polinômio com x2 = 3")


#X2 = 6

# Criar uma sequência de valores para x1
x1_values <- seq(-10, 10, length.out = 100)

# Calcular os valores do polinômio para x1 com x2 fixo em 3
x2_fixed <- 6
y_values <- 25 + 3 * x1_values + 4 * x2_fixed + 1.5 * x1_values * x2_fixed

# Criar o dataframe com os valores de x1 e y
data <- data.frame(x1 = x1_values, y = y_values)

# Plotar o gráfico
ggplot(data, aes(x = x1, y = y)) +
  geom_line() +
  labs(x = "x1", y = "Polinômio", title = "Gráfico do Polinômio com x2 = 3")

# B)____________________________________________________________________________




# O modelo é aditivo podemos perceber que a escala ela dobra quando dobramos
# o X indicando que o valores sao maiores.


# 2)============================================================================
# # Achar as matrizes X Beta e Y:

# Feito na mão:

# A)____________________________________________________________________________
# Yi = Beta0 + Beta1*Xi1 + Beta2*Xi1*Xi2 + erro


# B)____________________________________________________________________________
# raiz(Yi) = Beta0 + Beta1*Xi1 + Beta2*Xi2 + erro






# 3)============================================================================

# Uma variavel pode estar tanto correlacionada positivamente ou negativamente com as 
# demais preditivas, assim temos uma importancia apenas para quanto aumenta ou diminui
# nos demais paramentros beta do modelo.


# 4) - 6.5 =====================================================================
library(readxl)
setwd("C:/Users/Acer/Documents/UnB/Analise de Regressao Linear/modulo 2")
dados <- read_excel("Dados_ex_6_5.xlsx", 
                    col_types = c("numeric", "numeric", "numeric"))


# A)____________________________________________________________________________

# Calcular a matriz de correlação
matriz_cor <- cor(dados[,-3]);matriz_cor

# Converter a matriz de correlação em um data frame
df_cor <- as.data.frame(matriz_cor)

# Criar o gráfico de correlação (corr plot)
ggcorrplot(df_cor,lab = TRUE)

#matriz grafico de dispersao

ggpairs(dados[,-3]) #podemos perceber que a variavel X2 é discreta (2,4)



# B)____________________________________________________________________________

modelo = lm(Y ~ X1 + X2, data = dados)
summary(modelo)


# C)____________________________________________________________________________


residuo = modelo$residuals

boxplot(residuo)
plot(residuo)
summary(residuo)
sd(residuo)


# D)____________________________________________________________________________


results = data.frame(modelo$model,modelo$fitted.values,modelo$residuals)

# scatter plot para Y_chapeu, X1 e X2:

plot(residuo,predict(modelo))
plot(residuo,dados$X1)
plot(residuo,dados$X2)

# scatter plot para X1*X2:

plot(residuo,dados$X1*dados$X2)


# graficos de residuos: (verificando a normalidade)

qqnorm(residuo)
qqline(residuo)
shapiro.test(residuo)

# E)____________________________________________________________________________

# O Teste de Breusch-Pagan é um teste estatístico que verifica a presença de
# heteroscedasticidade em um modelo de regressão. Heteroscedasticidade significa que a
# variância dos erros ou resíduos de um modelo de regressão não é constante. Em um modelo
# ideal, gostaríamos que os resíduos tivessem variância constante, uma condição conhecida
# como homoscedasticidade. Se a variância dos resíduos muda em função das variáveis
# independentes, isso pode causar problemas, como estimativas de coeficientes não confiáveis e
# testes de hipóteses inválidos.
# A hipótese nula do teste de Breusch-Pagan é que os erros (ou resíduos) do modelo de
# regressão têm variância constante, independentemente do valor das variáveis independentes.
# Isso é conhecido como homoscedasticidade

library(lmtest)

print(bptest(modelo))


# F)____________________________________________________________________________

# duplas diferentes dos dados = c
# valores dos graus de liberade (c) e o tamanho da amostra:
n = nrow(dados)
c = n-8

# banco de dados criado anteriormente:
results = results %>%
  mutate(y_barra = ave(Y, X1, FUN = mean))


# Calculo pela formula (estão nos slides):
SSPE = sum((results$Y - results$y_barra)^2)

SSLF = sum((results$y_barra - results$modelo.fitted.values)^2)

# divide-se pelos graus de liberade, assim criamos duas qui-quadrado:
MSPE = SSPE/(n-c)

MSLF = SSLF/(c-2)

# Dividimos as duas qui-quadrado e assim temos a estatistica F:

f_obs = MSLF/MSPE;f_obs

f_crit = pf(f_obs,n-2,n-c);f_crit


# H0) A média de Y pode ser descrita em função dos coeficientes
# H1) A média de Y não pode ser descrita em função dos coeficientes

# Conclusao do teste:

ifelse(f_obs>f_crit,
       "Não rejeita H0",
       "Rejeita H0")




# 4) - 6.6 =====================================================================
# A)____________________________________________________________________________
# Testar se existe regressao - 
## h0: B1 = B2 (ausencia de regressao)
## h1? Existe um Bj != 0 (existe reg)
## est F
## o que implica sobre b1 e b2, se nao rejeitar, variaveis na ajudam a explicar o comp de y
## se  rejeitar, alguma ajuda a explicar. ai testa individualmente
## h0) Bj = 0
## h1) Bj != 0 
## est T 

summary(modelo)

# F-statistic: 129.1 on 2 and 13 DF, 

# rejeitamos a hipotese nula de ausencia de regressão e confirmamos que os valores obtidos
# para B1 e B2 são relevantes para o modelo.


# B)____________________________________________________________________________

#  p-value: 2.658e-09


# C)____________________________________________________________________________

# Get the p-values of the coefficients
p_values <- summary(modelo)$coefficients[-1, 4];p_values

# Bonferroni corrected significance level
alpha_bonferroni <- 0.005

# T de bonferroni:

t_bonferroni = 2.9467


# P1 + P2 + Atividade + Trabalho

(0.3*6)+(0.3*3.5)+(0.15*4)+(0.25*6)


# Joint test (Bonferroni corrected)
beta1_significant <- p_values[1] < alpha_bonferroni
beta2_significant <- p_values[2] < alpha_bonferroni

# Results
print(paste("Beta1 significant:", beta1_significant))
print(paste("Beta2 significant:", beta2_significant))

# Estimação dos betas:


y = matrix(dados$Y, nrow = 16, ncol = 1, byrow = F)

betas = (solve(XtX) %*% t(X) %*% y);betas


# Calculo do MSE:

MSE = as.vector(sd(modelo$residuals))
# Matriz X:

X = matrix(c(rep(1,16),dados$X1,dados$X2), nrow = 16, ncol = 3, byrow = F); X

# XtX:

XtX =  t(X) %*% X 

solve(XtX)

# Estimação da variancia de Beta:

# var(Beta) = MSE*(XtX)^-1

var_beta = diag(MSE*solve(XtX))

# Intervalo de confiança para B1 e B2:

(betas + t_bonferroni*var_beta)[-1]
(betas - t_bonferroni*var_beta)[-1]



# 4) - 6.7 =====================================================================

# A)____________________________________________________________________________

summary(modelo)

# Multiple R^2 = 0.9521 

# Ele indica a qualidade do ajuste do modelo, ou seja, o quão bem o modelo 
# se ajusta aos dados observados. 


# Por exemplo, se o R-squared múltiplo for 0,80, isso significa que 80% da 
# variância na variável dependente é explicada pelas variáveis independentes 
# incluídas no modelo, enquanto os outros 20% são não explicados e podem ser 
# atribuídos à variabilidade aleatória ou a outros fatores não capturados pelo modelo.




# B)____________________________________________________________________________

# R^2 ajustado: formula =  1 - [(1 - R²) * (n - 1) / (n - p - 1)]

# p = numero de variaveis explicativas:

1 - ((1 - 0.9521) * (16 - 1) / (16 - 2 - 1))


# O R-squared ajustado é uma versão modificada do R-squared múltiplo que leva em 
# consideração o número de variáveis preditoras no modelo e ajusta o R-squared 
# para o número de graus de liberdade. Ele penaliza a adição de variáveis preditoras 
# irrelevantes ao modelo e fornece uma indicação mais confiável da qualidade do 
# ajuste do modelo quando há múltiplas variáveis preditoras.



# 4) - 6.8 =====================================================================

# A)____________________________________________________________________________

# Yo = valor_t * sqrt( MSE * (1 + X0*(XtX)^-1 * X0) )

# T de bonferroni:

t = 2.6025

# Leitura do X0:

X0 = matrix(c(1,5,4), nrow = 3, ncol =1 , byrow = T);X0

betas[1] + betas[2]*5 + betas[3]*4




# B)____________________________________________________________________________

Y0_chapeu = t(X0)%*%betas; Y0_chapeu

# Variabilidade do Y0

var_X0 = MSE * t(X0) %*% solve(XtX) %*% X0; var_X0

# Calculo do intervalo de confiaça

Y0_chapeu + sqrt(var_X0)*t
Y0_chapeu - sqrt(var_X0)*t

