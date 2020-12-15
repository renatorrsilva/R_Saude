#Apagar todos os objetos da memoria
rm(list=ls())

#Carregar a biblioteca
library(tidyverse)
#Ler os dados 
dat = read.csv("sivep_atualizado.csv", header= TRUE)
#Ter uma visão geral dos dados
glimpse(dat)

#Tabelas de Frequencias
table( dat$CLASSI_FIN, exclude = NULL)

#Tabelas de Frequencias
table(dat$CRITERIO, exclude = NULL)


#Tabela de Contingencia

table(dat$CRITERIO, dat$CLASSI_FIN, exclude = NULL) %>% addmargins()

#Tabela de Contigência  - Comando xtabs (~ Linhas  + Colunas)

xtabs(~CRITERIO + CLASSI_FIN, data=dat, addNA = TRUE) %>% addmargins()


teste = filter(dat, CLASSI_FIN == 5 & CRITERIO == 1)

#Tabela de Contigencia  
xtabs( ~POS_PCROUT + PCR_SARS2, 
       dat = filter(dat, CLASSI_FIN == 5 & CRITERIO == 1), 
       addNA = TRUE) %>% addmargins()

#Tabela de Contigencia  
xtabs( ~POS_PCROUT + PCR_SARS2, 
       dat = filter(dat, CLASSI_FIN == 5), 
       addNA = TRUE) %>% addmargins()

#Tabela de Contigencia  
xtabs( ~AN_OUTRO + AN_SARS2, data = filter(dat, CLASSI_FIN == 5 & CRITERIO == 1),
       addNA = TRUE) %>% addmargins()

#Tabela de Frequencia
xtabs(~ TP_SOR, data = filter(dat, CLASSI_FIN == 5 & CRITERIO == 1), addNA = TRUE)


#Dados Filtrados 1
datRT_PCR = filter(dat, CLASSI_FIN == 5 & POS_PCROUT == 1 &  PCR_SARS2 == 1)

glimpse(datRT_PCR)


#Os distintos nomes dos municipios do estado de Goias
mn = filter(datRT_PCR, SG_UF_NOT == "GO" ) %>% select(ID_MUNICIP) %>% distinct()

mn

#Filtrar pelos municipios distintos
dat_mn = filter(datRT_PCR, ID_MUNICIP %in% mn$ID_MUNICIP)

#Tabelas de Frequencias
xtabs(~SG_UF_NOT, data = dat_mn, addNA = TRUE)


#Medidas Resumo para verificar os codigos
summary(dat_mn$CO_MUN_NOT)

#Tabelas de Frequencias
xtabs(~EVOLUCAO, data = dat_mn, addNA = TRUE)

#Tabelas de Frequencias

xtabs(~  CARDIOPATI, addNA = TRUE, data = dat_mn)


xtabs(~ FATOR_RISC + CARDIOPATI, addNA = TRUE, data = dat_mn)

#Vetor com os nomes das condicoes pre existentes
morbs = c("CARDIOPATI", "HEMATOLOGI", "HEPATICA", "ASMA", "DIABETES",
          "NEUROLOGIC","PNEUMOPATI", "IMUNODEPRE", "RENAL", "OBESIDADE")

#Funcao para corrigir as morbidades
corr.morbs = function(x){  ifelse(dat_mn$FATOR_RISC ==2, 2, x)    }

#Novo conjunto de dados
dat_mnF = mutate_at(dat_mn, morbs, corr.morbs)

#Tabelas de Contigencia
xtabs(~ FATOR_RISC + CARDIOPATI, addNA = TRUE, data = dat_mnF)



variaveis = c("EVOLUCAO","CARDIOPATI", "HEMATOLOGI", "HEPATICA", 
              "ASMA", "DIABETES", "NEUROLOGIC","PNEUMOPATI", 
              "IMUNODEPRE", "RENAL", "OBESIDADE")

corr.na = function(x){  ifelse(x ==9,NA, x)    }

dat_mnF = mutate_at(dat_mnF, variaveis, corr.na )

xtabs(~ FATOR_RISC + CARDIOPATI, addNA = TRUE, data = dat_mnF)


#install.packages("gmodels")
library(gmodels)
CrossTable(dat_mnF$EVOLUCAO,    digits=3,format = "SPSS")



CrossTable(dat_mnF$EVOLUCAO, dat_mnF$CARDIOPATI, 
           expected = FALSE, prop.chisq = FALSE,
           digits=3, chisq=FALSE,
           prop.r=FALSE,prop.c=FALSE, prop.t = TRUE,
           format = "SPSS")

save.image(file = "Resultados.RData")

rm(list=ls())

load(file = "Resultados.RData")


pp = c(1781530,  849421, 248821, 812707, 286433, 145865, 140321,116841, 112423, 488380,
       199333, 167159 , 167391, 125947, 420427 ,215282, 243991)


dat_tx = group_by(dat_mnF, ID_REGIONA, EVOLUCAO) %>% summarise(obitos = n()) %>% 
  filter(EVOLUCAO==2) %>% 
  select(obitos) %>%ungroup() %>% mutate(pop = pp) %>% 
  mutate(taxa = (obitos/pop)*10000) %>% mutate(taxa = round(taxa, digits=3))

dat_tx



x = c("Renato")
y = c("REnato")


library(textclean)
dat_IBGE = read.csv("Municipios.csv",header=TRUE,skip=1) 
dat_IBGE =filter(dat_IBGE,COD..UF == 52)
dat_IBGE = 
  
  
  filter(COD..UF == 52) %>% 
  mutate(POPULACAO = str_replace_all(POPULAÇÃO.ESTIMADA, ",",".") %>% as.numeric()*1000) %>% 
  mutate(ID_MUNICIP =replace_non_ascii(NOME.DO.MUNICÍPIO) %>% toupper()) %>% 
  select(one_of(c("ID_MUNICIP", "POPULACAO")))
head(dat_IBGE)


dat_tx2 = group_by(dat_mnF, ID_MUNICIP, EVOLUCAO) %>% summarise(OBITOS = n()) %>% ungroup() %>% filter(EVOLUCAO==2) %>% select(-EVOLUCAO) 
head(dat_tx2)


dat_tx2  = left_join(dat_IBGE, dat_tx2) %>% 
  mutate(TX_MORTALIDADE = (OBITOS/POPULACAO)*10^4 ) %>%
  mutate(TX_MORTALIDADE = replace_na(TX_MORTALIDADE,0)) %>% 
  select(-one_of(c("POPULACAO", "OBITOS")))


library(geobr)
library(sf)
#Carregando todos os municipios de Goias
muniGo = read_municipality(code_muni= "GO", year=2019)


muniGo =  mutate(muniGo, name_muni = replace_non_ascii(name_muni) %>% toupper()) 
dat_tx2 = rename(dat_tx2, name_muni = ID_MUNICIP)
muniGo = inner_join(muniGo, dat_tx2)

no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())
ggplot() +
  geom_sf(data=muniGo, aes(fill=TX_MORTALIDADE), color= NA, size=.15) +
  labs(subtitle="Taxa de Mortalidade de Covid 19 no Estado de Goiás", size=8) +
  scale_fill_distiller(palette = "Spectral", name="Taxa de Mortalidade por 10000 hab", limits = c(0,5)) +
  theme_minimal() +
  no_axis

