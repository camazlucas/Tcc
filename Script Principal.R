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

# Funcoes Utilizadas -------------------------------

#Filtragem e Analise dos Dados
source("Bases de Dados/Tratamento da Base de Dados Totais PPH-2019.r")
source("Analise dos Dados.r")

#Preparacao das Bases de Dados aplicadas
source("Bases de Dados/Base de Dados Original.r")
source("Bases de Dados/Base de Dados por Componentes Principais.r")
source("Bases de Dados/Base de Dados por Estado.r")

#Classificadores Utilizados
source("Classificadores/Classificador em Arvore.r")
source("Classificadores/Classificador Redes Neurais.r")
source("Classificadores/Classificador Vetor de Suporte.r")
source("Classificadores/Divisao da Quantidade de Classes Sociais Utilizadas.r")


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

  ## Analise Exploratoria dos Dados

    graficos_dispersao(dados_o)
    
    boxplot_dispersao(dados_o)
    
    estat_dados(dados_o)


    ##Classificacao com Redes Neurais --------------------------------------------

      rn_class_o6 = rn_class(dados_o, 6)

      x11();{plot(class_o6$modelo_rn, show.weights = FALSE)}

      #print(class_o$matriz_de_confusao_rn)

      print(class_o6$metricas_rn)

      rn_class_o3 = rn_class(dados_o, 3)

      x11();{plot(class_o3$modelo_rn, show.weights = FALSE)}

      #print(class_o$matriz_de_confusao_rn)

      print(class_o3$metricas_rn)




