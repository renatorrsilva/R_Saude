


x = c(1,4,8,9)


x=1:4

x <= y

x >= y

renato_23 = 1000

ren = 3

x = 1:4

dat = data.frame(
  student = c("Renato", "Joao", "Pedro", "Maria", "Joana"), 
  grades = c(10,2,4,9,9.5), 
  approved = c(T,F,F,T,T))



dat[order(dat$student),]


#Imprimir diretorio
getwd()



dir()



#Lendo os dados
dat = read.table("/Users/renatorodriguessilva/Dropbox/UFG/Extensao/R_Saude/Aula1/Goiaba.txt")

#Configurar diretorio
setwd("/Users/renatorodriguessilva/Dropbox/UFG/Extensao/R_Saude/Aula1")

dat = read.table("Goiaba.txt", header = TRUE)

dat2 = read.table("student-por.csv", sep=",", header = TRUE)


dat2 = read.csv("student-por.csv", header = TRUE)


dat3 = read.csv2("tabela2.1.csv", header = TRUE)


dat4 = read.table("tabela2.1.csv", header = TRUE, sep=";")
