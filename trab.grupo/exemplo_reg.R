#### (3) CHALLENGER SPACE SHUTTLE O-RING FAILURES ####
### 
### Codigo original:
###       Original de Ana Duarte, Jady Goes e Jo?o Pedro 
###       Dados Categorizados - Turma 1/2018


require(ggplot2); 
require(knitr); require(cowplot)
require(nlme); require(Rchoice); require(AICcmodavg);
require(mdscore) # funcao lr.test 
require(questionr); library(tidyverse)

falhas <- c(2,1,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,2,0,0,0,0,0, rep(NA,12))	
temp <- c(53,57,58,63,66,67,67,67,68,69,70,70,70,70,72,73,75,
          75,76,76,78,79,80,31,30,35,40,45,50,55,60,65,70,75,80)
orings <- rep(6,length(falhas))
dados <- as.data.frame(cbind(falhas,temp,orings))
ggplot(dados,aes(temp,falhas)) + geom_point(na.rm = T) + 
  theme_classic() +labs(x = "Tempo", y="Numero de Falhas") + 
  scale_x_continuous(limits = c(50,80))


#  Analise INCORRETA 1 - Regressao  Linear sem Zeros 

dados1 <- dados[!dados$falhas==0,]

ggplot(dados1,aes(temp,falhas)) + geom_point(na.rm = T) + 
  theme_classic() +labs(x = "Tempo", y="N?mero de Falhas") + 
  scale_x_continuous(limits = c(50,80))

resp1 <- dados1$falhas/dados1$orings
dados1[,4] <- resp1
names(dados1)[4] <- 'resp'

mod1 <- lm(resp~temp, data = dados1)
anova(mod1)
summary(mod1)

distc <- influence.measures(mod1)[[1]][,5]
lev <- influence.measures(mod1)[[1]][,6]


ajuste1 <- as.data.frame(cbind(mod1$residuals,mod1$fitted.values,
                               dados1$resp[c(1:7)],dados1$temp[c(1:7)],
                               distc,lev, rstudent(mod1), c(1:7)))
ajuste1 <- rename(ajuste1,Residuo=V1,Preditos=V2,Resp=V3,
                  Temperatura=V4,RStudent=V7,Obs=V8,CooksD=distc,
                  Leverage=lev)


g1 <- ggplot(ajuste1,aes(Preditos,Residuo)) + 
  geom_point(na.rm = T) + geom_hline(yintercept=0)+ 
  theme_classic()+labs(x = "Valores Ajustados", y="Residuos") 

g2 <- ggplot(ajuste1,aes(Preditos,RStudent)) + geom_point(na.rm = T) +
  geom_hline(yintercept=c(-2,2))+ theme_classic()+
  labs(x = "Valores Ajustados", y="RStudent") + 
  scale_y_continuous(limits=c(-2,3))

g3 <- ggplot(ajuste1,aes(Leverage,RStudent)) + geom_point(na.rm = T) +
  geom_hline(yintercept=c(-2,2))+ theme_classic()+
  labs(x = "Leverage", y="RStudent") + 
  scale_y_continuous(limits=c(-2,3))

g4 <- ggplot(ajuste1,aes(Preditos,Resp)) + geom_point(na.rm = T) + 
  theme_classic()+labs(x = "Valores Ajustados", y="Resposta") + 
  scale_x_continuous(limits = c(.1,0.5)) + 
  scale_y_continuous(limits = c(.1,.5)) + 
  geom_abline(slope = 1, intercept = 0)

g5 <- ggplot(ajuste1,aes(Obs,CooksD)) + geom_point(na.rm = T) +
  geom_line(na.rm = T)+ theme_classic()+ 
  labs(x = "Observacao", y="Distancia de Cook") + 
  scale_x_discrete(limits = c(1,2,3,4,5,6,7)) + 
  scale_y_continuous(limits=c(-1,2))


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

g6 <-ggQQ(mod1)  
plot_grid(g1,g2,g3,g4,g5,g6, ncol=3)


ggplot(ajuste1,aes(Temperatura,Residuo)) + geom_point(na.rm = T) + 
  theme_classic()+labs(x = "Temperatura", y="Residuo") + 
  geom_hline(yintercept = 0)


# Inserindo intervalo de confianca

fit <- predict(mod1)
VarCov <- vcov(mod1)
X <- model.matrix(~ Temperatura,data=ajuste1)
se.fit <- sqrt(diag(X %*% VarCov %*% t(X)))

ajuste1$lwr <- fit - qnorm(0.975)*se.fit
ajuste1$upr <- fit + qnorm(0.975)*se.fit

ggplot(ajuste1,aes(Temperatura,Preditos)) + geom_line() + 
  scale_y_continuous(limits = c(0,.5)) + theme_classic() +
  geom_point(aes(Temperatura,Resp)) + 
  geom_ribbon(data=ajuste1,aes(ymin=as.numeric(lwr),
                               ymax=as.numeric(upr)),
              fill= "slateblue",alpha=0.3) + 
  labs(x = "Temperatura", y="Valores Ajustados")



#  Analise INCORRETA 2 - Regress?o  Linear Com Zeros 

resp <- dados$falhas/dados$orings
dados[,4] <- resp
names(dados)[4] <- 'resp'

mod2 <- lm(resp~temp, data = dados)
anova(mod2)
summary(mod2)

distc2 <- influence.measures(mod2)[[1]][,5]
lev2 <- influence.measures(mod2)[[1]][,6]

ajuste2 <- as.data.frame(cbind(mod2$residuals,mod2$fitted.values,
                               dados$resp[c(1:23)],dados$temp[c(1:23)],
                               distc2,lev2, rstudent(mod2)),c(1:23))
ajuste2 <- rename(ajuste2,Residuo=V1,Preditos=V2,Resp=V3,
                  Temperatura=V4,RStudent=V7,CooksD=distc2,
                  Leverage=lev2)


ajuste2$Obs <- c(1:23)


g7 <- ggplot(ajuste2,aes(Preditos,Residuo)) + 
  geom_point(na.rm = T) + geom_hline(yintercept=0)+ 
  theme_classic()+labs(x = "Valores Ajustados", y="Res?duos") 

g8 <- ggplot(ajuste2,aes(Preditos,RStudent)) + geom_point(na.rm = T) +
  geom_hline(yintercept=c(-2,2))+ theme_classic()+
  labs(x = "Valores Ajustados", y="RStudent") + 
  scale_y_continuous(limits=c(-2,3))

g9 <- ggplot(ajuste2,aes(Leverage,RStudent)) + geom_point(na.rm = T) +
  geom_hline(yintercept=c(-2,2))+ theme_classic()+
  labs(x = "Leverage", y="RStudent") + 
  scale_y_continuous(limits=c(-2,3))

g10 <- ggplot(ajuste2,aes(Preditos,Resp)) + geom_point(na.rm = T) + 
  theme_classic()+labs(x = "Valores Ajustados", y="Resposta") + 
  scale_x_continuous(limits = c(.1,0.5)) + 
  scale_y_continuous(limits = c(.1,.5)) + 
  geom_abline(slope = 1, intercept = 0)

g11 <- ggplot(ajuste2,aes(Obs,CooksD)) + geom_point(na.rm = T) +
  geom_line(na.rm = T)+ theme_classic()+ 
  labs(x = "Observacao", y="Distancia de Cook") + 
  scale_x_discrete(limits = c(1,2,3,4,5,6,7)) + 
  scale_y_continuous(limits=c(-1,2))

g12 <-ggQQ(mod2)  
plot_grid(g7,g8,g9,g10,g11,g12, ncol=3)

ggplot(ajuste2,aes(Temperatura,Residuo)) + geom_point(na.rm = T) + 
  theme_classic()+labs(x = "Temperatura", y="Residuo") + 
  geom_hline(yintercept = 0)

ggplot(ajuste2,aes(Temperatura,Residuo)) + geom_point(na.rm = T) +  
  theme_classic()+labs(x = "Temperatura", y="Residuo") + 
  geom_hline(yintercept = 0)

# inserir intervalo de confian?a

fit <- predict(mod2)
VarCov <- vcov(mod2)
X <- model.matrix(~ Temperatura,data=ajuste2)
se.fit <- sqrt(diag(X %*% VarCov %*% t(X)))

dim(ajuste2) 

ajuste2$lwr <- fit - qnorm(0.975)*se.fit
ajuste2$upr <- fit + qnorm(0.975)*se.fit


ggplot(ajuste2,aes(Temperatura,Preditos)) + geom_line() + 
  theme_classic() + 
  geom_point(aes(Temperatura,Resp)) + 
  scale_y_continuous(limits = c(-.2,.5)) +
  geom_ribbon(data=ajuste2,aes(ymin=as.numeric(lwr),
                               ymax=as.numeric(upr)),
              fill= "slateblue",alpha=0.3) + 
  labs(x = "Temperatura", y="Valores Ajustados")


# Analise CORRETA - Regress?o Log?stica com Zeros

dados$nfalhas <- dados$orings-dados$falhas

mod3 <- glm(cbind(falhas,nfalhas)~ temp, 
            family=binomial(link=logit), 
            data=dados)

# teste verossimilhan?a

mod0 <- glm(cbind(falhas,nfalhas)~1, family=binomial(link=logit), 
            data=dados)
t <- lr.test(mod0,mod3)
t

# teste de wald

thetahat <- mod3$coefficients
kov <- vcov(mod3)
LL <- rbind(c(0,1))

WaldTest = function(L,thetahat,Vn,h=0) {
  WaldTest = numeric(3)
  names(WaldTest) = c("W","df","p-value")
  r = dim(L)[1]
  W = t(L%*%thetahat-h) %*% solve(L%*%Vn%*%t(L)) %*%
    (L%*%thetahat-h)
  W = as.numeric(W)
  pval = 1-pchisq(W,r)
  WaldTest[1] = W; WaldTest[2] = r; WaldTest[3] = pval
  WaldTest}


# teste score

score<-anova(mod0,mod3, test="Rao")
resultados <- cbind(t$LR,1,t$pvalue)
resultados2 <- matrix(WaldTest(LL,thetahat,kov), ncol=3)
resultados3 <- cbind(anova(mod0,mod3, test="Rao")[2,4],
                     anova(mod0,mod3, test="Rao")[2,3],
                     anova(mod0,mod3, test="Rao")[2,6])

testes <- rbind(resultados,resultados2,resultados3)
rownames(testes) <- c("Razao de Verossimilhanca","Wald","Score")
colnames(testes) <- c("Estatistica","GL","P-valor")

testes

mod4 <- summary(mod3)
coef3 <- mod4$coefficients

colnames(coef3) <- c("Estimativa","Erro Padrao","Valor Z","Pr(>|z|)")
rownames(coef3) <- c("Intercepto","Temperatura")

coef3

odds.ratio(mod3)[2,]

# Medidas de qualidade de ajuste

medidas1 <- as.data.frame(cbind(mod4$deviance,mod4$aic, BIC(mod3),
                                logLik(mod3)[1],
                                AIC(mod3)))
colnames(medidas1) <- c("Deviance","AIC","BIC","Full LOg Likelihood","AIC")
medidas1

temp.data <- data.frame(Temperatura =temp)

predicted.data <- as.data.frame(predict(mod3, newdata = temp.data, 
                                        type="link", se=TRUE))

new.data <- cbind(temp.data, predicted.data)

std <- qnorm(0.95 / 2 + 0.5)
new.data$ymin <- mod3$family$linkinv(new.data$fit - std * new.data$se)
new.data$ymax <- mod3$family$linkinv(new.data$fit + std * new.data$se)
new.data$fit <- mod3$family$linkinv(new.data$fit)  # Rescale to 0-1

p <- ggplot(dados, aes(x=temp, y=resp))  
p + geom_point(na.rm = T) + 
  geom_ribbon(data=new.data, aes(y=fit, ymin=ymin, ymax=ymax),
              fill="slateblue", alpha=0.3) +
  geom_line(data=new.data, aes(y=fit)) + 
  labs(x="Temperatura", y="Probabilidade") 