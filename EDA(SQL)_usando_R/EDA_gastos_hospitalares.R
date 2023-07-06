#Diretório
#dir.create("~/Estudos/DSA/FCD/R/Cap17")

setwd("~/Estudos/DSA/FCD/R/Cap17")
list.files()

options(warn = -1)

library(readr)


# Carga de dados
dados <- read_csv("dataset.csv")
head(dados)


# DataWrangling (pré-processamento e tratamento)
str(dados)
dim(dados)

#A edição dos metadados pode estar sendo afetada pela existência de na's.Vamos averiguar
any(is.na(dados))

#Verificando a propoção
table(complete.cases(dados))
colSums(is.na(dados))


#encontrando na
which(!complete.cases(dados))
dados[277, ]

#Eliminando na
dados <- na.omit(dados)

#algumas variáveis precisam ser transformadas em fator
labels <- c("FEMALE", "RACE", "APRDRG")
dados[, labels] <- lapply(dados[, labels], as.factor)


str(dados)

head(dados)

#Parte 1 - análise exploratória com SQL
library(sqldf)
help('sqldf')

#Primeira questão
#Quantas raças estão representadas no dataset?

#Usando SQL
sqldf::sqldf("Select count(*) as qtd_racas 
              from(
              select RACE 
              from dados 
              group by RACE);")

#Usando R
length(levels(dados$RACE))

#Segunda questão
#Qual a idade média dos pacientes?

sqldf::sqldf("select avg(AGE) as idade_media
             from dados;")

#Terceira questão
#Qual a moda da idade dos pacientes?

sqldf::sqldf("select max(freq) as moda_idade, AGE
              from(
                select count(*) as freq, AGE
                from dados
                group by AGE);"
             )

#Quarta questão
#Qual a variância da coluna idade?


#Quinta questão
#Qual o gasto total com internações hospitalares por idade?

sqldf::sqldf("select sum(TOTCHG) as gastoTotal, AGE 
             from dados 
             group by AGE;")


#Sexta questão
#Qual idade gera o maior gasto total com internações hospitalares?

sqldf::sqldf("select max(gastoTotal) as maxIdade, AGE from(
              select sum(TOTCHG) as gastoTotal, AGE
             from dados
             group by AGE);")


#Sétima questão
#Qual o gasto total com internações hospitalares por gênero?


sqldf::sqldf("select sum(TOTCHG) as gastoGenero, FEMALE
             from dados
             group by FEMALE;")


#Oitava questão
#Qual a média de gasto com internações hospitalares por raça do paciente?

sqldf::sqldf("select avg(TOTCHG) as mediaRACA, RACE
             from dados
             group by RACE;")


#Nona questão
#Para  pacientes  acima  de  10  anos,  qual  a  média  de  gasto  total  com  internações hospitalares.

sqldf::sqldf("select sum(TOTCHG) as total10
             from dados
             where AGE > 10;")


#Décima questão
#Considerando o item anterior, qual idade tem média de gastos superior a 3000?

sqldf::sqldf("select avg(TOTCHG) as media3k, AGE
             from dados
             where AGE > 10
             group by AGE
             having media3k > 3000;")



#Parte 2 - Análise exploratória com R