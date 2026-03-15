# Limpeza de Memoria ------------------------------------------------------
rm(list=ls(all=TRUE)) # Limpar a memória

# Pacotes Utilizados ------------------------------------------------------

#Todos
library(caret)
library(dplyr)
library(factoextra)
library(xtable)

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

  ## Analise Exploratoria dos Dados -----------------------------------------

    graficos_dispersao(dados_RJ)

    boxplot_dispersao(dados_RJ)

    estat_dados(dados_RJ)

    ##Classificacao em Arvores de Decisao -------------------------------------
      arvore_class_RJ6 = arvore_class(dados_RJ, 6)

      print(arvore_class_RJ6)

      arvore_class_RJ3 = arvore_class(dados_RJ, 3)

      print(arvore_class_RJ3)

    ##Classificacao com Maquinas de Vetor de Suporte --------------------------
      svm_class_RJ6 = svm_class(dados_RJ, 6)

      print(svm_class_RJ6$summary_svm)

      print(svm_class_RJ6$matriz_de_confusao_svm)

      svm_class_RJ3 = svm_class(dados_RJ, 3)

      print(svm_class_RJ3$summary_svm)

      print(svm_class_RJ3$matriz_de_confusao_svm)

    ##Classificacao com Redes Neurais -----------------------------------------

      rn_class_RJ6 = rn_class(dados_RJ, 6)

      x11();{plot(rn_class_RJ6$modelo_rn, show.weights = FALSE)}

      #print(class_o$matriz_de_confusao_rn)

      print(rn_class_RJ6$metricas_rn)

      rn_class_RJ3 = rn_class(dados_RJ, 3)

      x11();{plot(rn_class_RJ3$modelo_rn, show.weights = FALSE)}

      #print(class_o$matriz_de_confusao_rn)

      print(rn_class_RJ3$metricas_rn)
      
#Classificação com os Dados Originais da Pesquisa ----------------------------
      
  dados_o = dados_originais(dados, "RJ")
      
      ## Analise Exploratoria dos Dados --------------------------------------
      
      graficos_dispersao(dados_o)
      
      boxplot_dispersao(dados_o)
      
      estat_dados(dados_o)
      
      
      ##Classificacao em Arvores de Decisao -------------------------------------
      arvore_class_o6 = arvore_class(dados_o, 6)
      
      print(arvore_class_o6)
      
      arvore_class_o3 = arvore_class(dados_o, 3)
      
      print(arvore_class_o3)
      
      ##Classificacao com Maquinas de Vetor de Suporte --------------------------
      svm_class_o6 = svm_class(dados_o, 6)
      
      print(svm_class_o6$summary_svm)
      
      print(svm_class_o6$matriz_de_confusao_svm)
      
      svm_class_o3 = svm_class(dados_o, 3)
      
      print(svm_class_o3$summary_svm)
      
      print(svm_class_o3$matriz_de_confusao_svm)
      
      ##Classificacao com Redes Neurais -----------------------------------------
      
      rn_class_o6 = rn_class(dados_o, 6)
      
      x11();{plot(rn_class_o6$modelo_rn, show.weights = FALSE)}
      
      #print(class_o$matriz_de_confusao_rn)
      
      print(rn_class_o6$metricas_rn)
      
      rn_class_o3 = rn_class(dados_o, 3)
      
      x11();{plot(rn_class_o3$modelo_rn, show.weights = FALSE)}
      
      #print(class_o$matriz_de_confusao_rn)
      
      print(rn_class_o3$metricas_rn)


#Classificação com os Dados Reduzidos por Componentes Principais ------------

  dados_cp = reduzir_com_cp(1, 0.13, dados_RJ) 

  ## Analise Exploratoria dos Dados -----------------------------------------

    graficos_dispersao(dados_cp)

    boxplot_dispersao(dados_cp)

    estat_dados(dados_cp)

    ##Classificacao em Arvores de Decisao -------------------------------------
      arvore_class_cp6 = arvore_class(dados_cp, 6)

      print(arvore_class_cp6)

      arvore_class_cp3 = arvore_class(dados_cp, 3)

      print(arvore_class_cp3)

    ##Classificacao com Maquinas de Vetor de Suporte --------------------------
      svm_class_cp6 = svm_class(dados_cp, 6)

      print(svm_class_cp6$summary_svm)

      print(svm_class_cp6$matriz_de_confusao_svm)

      svm_class_cp3 = svm_class(dados_cp, 3)

      print(svm_class_cp3$summary_svm)

      print(svm_class_cp3$matriz_de_confusao_svm)

    ##Classificacao com Redes Neurais -----------------------------------------

      rn_class_cp6 = rn_class(dados_cp, 6)

      x11();{plot(class_cp6$modelo_rn, show.weights = FALSE)}

      #print(class_o$matriz_de_confusao_rn)

      print(class_cp6$metricas_rn)

      rn_class_cp3 = rn_class(dados_cp, 3)

      x11();{plot(class_cp3$modelo_rn, show.weights = FALSE)}

      #print(class_o$matriz_de_confusao_rn)

      print(class_cp3$metricas_rn)



      
    




