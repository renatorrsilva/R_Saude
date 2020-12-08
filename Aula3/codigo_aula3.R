#Apagar todos os objetos da memoria
rm(list=ls())

#Carregar de bibliotecas
library(tidyverse)

#Ler os dados
dat = read.csv("diabetes.csv", header = TRUE)

dat = dplyr::mutate(dat, Outcome = factor(Outcome))

#Acessar a variavel glicose
dat$Glucose


#Selecionar as colunas excluindo a variavel Outcome
dat2 = dplyr::select(dat,-Outcome)


#Calcular a media de todas variaveis quantitativas
apply(dat2, 2, mean)


#Calcular o logaritmo de todas as variaveis quantitativas
apply(dat2, 2, function(x){ log(x+1)})


summary(dat)


#Intervalo de confiança para media

t.test(dat$BMI, conf.level=0.95)$conf.int


#Teste de hipotese da media populacional ser igual a 25
#Teste Bilateral

t.test(BMI ~ 1, mu=25,data=dat)

t.test(dat$BMI, mu=25)


##Teste de Igualdades de Variâncias
var.test(BMI ~ Outcome, data = dat)


#Teste de Medias Populacional Com Variancias Iguais
t.test(BMI ~ Outcome, data= dat, var.equal = TRUE)

#Teste de Normalidade
shapiro.test(dat$BMI)

#Teste de Não Paramétrico
kruskal.test(BMI ~ Outcome, data = dat)


#Conjunto de Dados
vars = c( "Pregnancies", "Outcome" )
datq = dplyr::select(dat, -one_of(vars))


round(cor(datq),2)

cor(dat$Glucose, dat$BloodPressure)

library(tidyverse)

cor(datq) %>% round(digits=2)


#install.packages("PerformanceAnalytics")

library("PerformanceAnalytics")

#Fazer graficos de correlação
chart.Correlation(
  datq ,
  histogram = TRUE,
  method = c("pearson")
)



#Regressao Linear Simples

mod = lm(Glucose ~ Insulin, data = dat)

summary(mod)

#install.packages("ggpubr")

library("ggpubr")


ggscatter(dat, x = "Insulin", y = "Glucose", 
          add = "reg.line", conf.int = FALSE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Insulina", ylab = "Glicose")


##Regressão Múltipla

mod = lm(Glucose ~ Outcome + Insulin, data = dat)

summary(mod)


qqnorm(residuals(mod))


group_by(dat,Outcome) %>% summarise(medio = mean(BMI))

