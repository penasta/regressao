pacman::p_load(tidyverse, readxl, ggcorrplot,GGally)

#1)
#X2 = 3

# Definir a função do polinômio
polinomio <- function(x1,x2) {
  return(25 + 3*x1 + 4*x2 + 1.5*x1*x2)
}

# Plotar o gráfico do polinômio
curve(polinomio(x1 =x ,x2 = 3), from = -10, to = 10, xlab = "x", ylab = "y", main = "Gráfico do Polinômio")


# Fazendo pelo ggplot

# Criar um data frame com os valores de x
x <- seq(0, 20, length.out = 100)
df <- data.frame(x = x)

# Calcular os valores de y usando a função do polinômio
df$y <- polinomio(x1 = df$x, x2 = 3)

# Plotar o gráfico usando ggplot2
ggplot(df, aes(x = x, y = y)) +
  geom_line() +
  labs(x = "x", y = "y", title = "Gráfico do Polinômio X=3")




#X2 = 6

# Definir a função do polinômio
polinomio <- function(x1,x2) {
  return(25 + 3*x1 + 4*x2 + 1.5*x1*x2)
}

# Plotar o gráfico do polinômio
curve(polinomio(x1 =x ,x2 = 6), from = -10, to = 10, xlab = "x", ylab = "y", main = "Gráfico do Polinômio")

# Criar um data frame com os valores de x
x <- seq(0, 20, length.out = 100)
df <- data.frame(x = x)

# Calcular os valores de y usando a função do polinômio
df$y <- polinomio(x1 = df$x, x2 = 6)

# Plotar o gráfico usando ggplot2
ggplot(df, aes(x = x, y = y)) +
  geom_line() +
  labs(x = "x", y = "y", title = "Gráfico do Polinômio X=6")


#O modelo é aditivo podemos perceber que a escala ela dobra quando dobramos o X indicando que o valores sao maiores


#2)









#3)

















#4)
dados <- read_excel("dados/Dados_ex_6_5.xlsx", 
                    col_types = c("numeric", "numeric", "numeric"))


#a)

# Calcular a matriz de correlação
matriz_cor <- cor(dados)

# Converter a matriz de correlação em um data frame
df_cor <- as.data.frame(matriz_cor)

# Criar o gráfico de correlação (corr plot)
ggcorrplot(df_cor,lab = TRUE)

#matriz grafico de dispersao

ggpairs(dados) #podemos perceber que a variavel X2 é categorica (2,4)



#b)

modelo = lm(Y ~ X1 + X2, data = dados)
summary(modelo)


#c)


residuo = modelo$residuals

boxplot(residuo)


#fazendo pelo ggplot

# Calcular medidas resumo
medidas_resumo <- as.data.frame(residuos) %>%
  summarize(
    Min = min(residuos),
    Q1 = quantile(residuos, 0.25),
    Mediana = median(residuos),
    Q3 = quantile(residuos, 0.75),
    Max = max(residuos),
    Media = mean(residuos)
  )

# Criar o boxplot com medidas resumo
ggplot(dados, aes(y = residuos)) +
  geom_boxplot() +
  geom_point(data = medidas_resumo, aes(x=0,y = Media), color = "red", size = 3) +
  labs(x = "Residuos", y = "Valor", title = "Boxplot com Medidas Resumo")



#graficos de residuos

qqnorm(residuos)
qqline(residuos)
shapiro.test(residuos)


#d)

results = data.frame(modelo$model,modelo$fitted.values,modelo$residuals)

plot(results$modelo.fitted.values,results$Y)

plot(results$X1,residuos)

plot(results$X2,residuos)


#e)

SSR = anova(modelo)[[1,2]] #soma de quadrados da regressao
SSE = anova(modelo)[[2,2]] #soma de quadrados do residuo

n = length(dados$Y)

alfa = 0.05
est_obs = (SSR/2)/(SSE/n)^2

pchisq(est_obs,alfa,1)


#f)

glres = n-2

c = glres

results = results %>%
  mutate(y_barra = ave(Y, X1, FUN = mean))

SSPE = sum((results$Y - results$y_barra)^2)

SSLF = sum((results$y_barra - results$modelo.fitted.values)^2)


MSPE = SSPE/(n-c)

MSLF = SSLF/(c-2)


f_obs = MSLF/MSPE

pf(f_obs,n-2,n-c)

#3)

#a) testar se existe regressao - 
## h0: B1 = B2 (ausencia de regressao)
## h1? Existe um Bj != 0 (existe reg)
## est F
## o que implica sobre b1 e b2, se nao rejeitar, variaveis na ajudam a explicar o comp de y
## se  rejeitar, alguma ajuda a explicar. ai testa individualmente
## h0) Bj = 0
## h1) Bj != 0 
## est T 


# c) Ic de B1 e B2

#4) 
##a)
##b)

#5) 
##a)
##b)


