#definindo diretório
setwd('~/Estudos/DSA/FCD/bigdata5/')

# SEGMENTAÇÃO DE CLIENTES COM RFM (RECÊNCIA, FREQUÊNCIA E VALOR MONETÁRIO)

# Instalando e carregando bibliotecas
install.packages('tidyverse')
install.packages('caret')
install.packages('rfm')
install.packages('factoextra')


library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)
library(plotly)
library(readxl)
library(rfm)
library(stats)
library(factoextra)



#Explorando fonte de dados
list.files()

# Conhecendo sheets
readxl::excel_sheets('online_retail_II.xlsx')


#lendo dados
carrega_dados = function(){
  
  setwd('~/Estudos/DSA/FCD/bigdata5/')
  
  sheet1 <- read_xlsx('online_retail_II.xlsx', sheet = "Year 2009-2010")
  
  sheet2 <- read_xlsx('online_retail_II.xlsx', sheet = "Year 2010-2011")
  
  dados_combinados <- rbind(sheet1, sheet2)
  
  return(dados_combinados)
}

# carregando
dados <- carrega_dados()

dim(dados)

View(dados)

# Checando na's

verifica_missing <- function(x){
  
  return(colSums(is.na(x)))
  
}

# Aplicando funcao
verifica_missing(dados)

#Função para pre-processar os dados

pre_processamento <- function(dados1){
  
  #criando atributo total price
  dados1$total_price <- dados1$Quantity * dados1$Price
  
  #limpando na's
  dados1 <- na.omit(dados1)
  
  #limpando dados do atributo invoice com letra 'c'
  dados1 <- dados1[!grepl('C', dados1$Invoice), ]
  
  return(dados1)
  
}  

#aplicando funcao
dataset <- pre_processamento(dados)

View(dataset)

# plotando distribuição dos precos
ggplot(data = dataset,
       aes(x = total_price)) + 
  geom_density(fill = '#69b3a2', color = '#e9ecef', aplha = 3.5) + 
  labs(title = 'Distribuição da variável total_price')

#verificando dados sobre clientes
length(dataset$`Customer ID`)

length(unique(dataset$`Customer ID`))

# Agrupando total de dados por clientes 
total_gasto <- dataset %>% group_by(`Customer ID`) %>%
  summarise(total_cliente = sum(total_price))

View(total_gasto)

#Criando uma data para calcular a recência
date1 <- as.Date.character('25/12/2011', '%d/%m/%Y')

#criando função para tratar dados InvoiceData
converte_data <- function(x){
  
  options(digits.sec = 3)
  return(as.Date(as.POSIXct(x$InvoiceDate, 'GMT')))
  
}

#Aplicando funcao ao atributo InvoiceDate
dataset$InvoiceDate <- converte_data(dataset)

View(dataset)

# -- Aplicando análise RFN
# função para calcular recência, frequência e valor monetário

calcula_rfn <- function(x){
  
  #calcula RFN
  z <- x %>% group_by(`Customer ID`) %>%
    
    summarise(recencia = as.numeric(date1 - max(InvoiceDate)),
              frequencia = n(), #contabiliza a frequência do ID
              valor = sum(total_price), #total gasto pelo ID
              primeira_compra = min(InvoiceDate))
  
  
  # Extrai outliers, com base no valor gasto por cliente
  Q1 <- quantile(z$valor, .25)
  Q3 <- quantile(z$valor, .75)
  IQR <- IQR(z$valor)
  
  
  z <- subset(z, z$valor >= (Q1 - 1.5 * IQR) & z$valor <= (Q3 + 1.5 * IQR))
  
  return(z)
  
  
}

#executa função
valor_rfm <- calcula_rfn(dataset)
head(valor_rfm)


#-- Machine learning - aplicando kusterização com K-means

#Setseed
set.seed(1029)

# criando função para segmentar clientes

segmenta_cliente <- function(rfm){
  
  #cria lista vazia para receber saida
  resultados <- list()
  
  #selecionando comente RFM, da base de dados
  dados_rfm <- select(valor_rfm, c('recencia', 'frequencia', 'valor'))
  
  #criando modelo
  modelo_kmeans <- kmeans(dados_rfm, centers = 5, iter.max = 50)
  
  
  #salvando gráfico em um elemento da lista
  resultados$plot <- fviz_cluster(modelo_kmeans, #modelo calculado
                                  data = dados_rfm, #define dados
                                  geom = c('point'), #estética
                                  ellipse.type = 'euclid') #define distancia euclidiana
  
  #organizando dados
  #cria atributo custumer id em dados
  dados_rfm$`Customer ID` <- rfm$`Customer ID`
  
  #cria atributo cluster
  
  dados_rfm$cluster <- modelo_kmeans$cluster
  
  #salva o dataset em um elemento da lista
  
  resultados$data <- dados_rfm
  
  
  return(resultados)
  
  
}

# Executando função e verificando resultados

#grafico
grafico <- segmenta_cliente(rfm = valor_rfm)[1]
grafico

#tabela
tabela <- segmenta_cliente(rfm = valor_rfm)[2]
tabela
