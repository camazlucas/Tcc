# Limpeza de Memoria ------------------------------------------------------
rm(list=ls(all=TRUE)) # Limpar a memória

# Pacotes Utilizados ------------------------------------------------------

#Todos
library(caret)
library(dplyr)
library(factoextra)

#Redes Neurais

library(neuralnet)

#Arvore de Classificacao
library(arules)
library(caTools)
library(rpart)
library(rpart.plot)

#Maquina de Vetor de Suporte
library(e1071)
library(ggplot2)

# Dados Utilizados -----------------------------------

# Upload dos Dados
dados = read.csv2("https://huggingface.co/datasets/camazlucas/pph2019/resolve/main/PPH%202019%20-%20Banco%20de%20Dados%20V2.csv")
dados[is.na(dados)] = 0

#Filtragem dos Dados --------------------------------

dados_filtrados = tratamento_dos_dados(dados)


# Classificação com os Dados Totais do RJ -----------------------------------

dados_RJ = filtrar_estados("RJ", dados_filtrados)


#Classificação com os Dados Reduzidos por Componentes Principais ------------

dados_cp = reduzir_com_cp(1, 0.13, dados_RJ) 


#Classificação com os Dados Originais da Pesquisa ----------------------------

dados_o = dados_originais(dados, "RJ")







