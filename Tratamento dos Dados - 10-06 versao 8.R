###############Lucas Camaz Ferreira######################################
###############Script Classificacao Monografia###########################


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

dados_RN = tratamento_dos_dados(dados)

{
  indice_classeUF = which(names(dados_UF) == "CLASSE")
  indice_classeRD = which(names(dados_cp) == "CLASSE")
  indice_classeOG = which(names(dados_o) == "CLASSE")
  
  dados_RN_padr_UF = data.frame(scale(dados_UF[,-indice_classeUF]))
  dados_RN = cbind(dados_RN_padr_UF, dados_UF[,indice_classeUF])
  
  dados_RN_padr_RD = data.frame(scale(dados_cp[,-indice_classeRD]))
  dados_RN = cbind(dados_RN_padr_RD, dados_cp[,indice_classeRD])
  
  dados_RN_padr_OG = data.frame(scale(dados_o[,-indice_classeOG]))
  dados_RN = cbind(dados_RN_padr_OG, dados_o[,indice_classeOG])
  
  names(dados_RN)[ncol(dados_RN)] <- "CLASSE"
}

# An?lise Explorat?ria dos Dados ------------------------------------------
{
  x11();boxplot(dados_UF, col=rainbow(ncol(dados_UF)), 
                pch=16, ylim=c(-1, 20)); abline(h=c(-1,1), col="red", lty="dashed")
  
  
  x11();boxplot(dados_UF[,-(ncol(dados_UF))], col=rainbow(ncol(dados_UF)), 
                pch=16, ylim=c(-4, 20)); abline(h=c(-1,1), col="red", lty="dashed")
}

# Fun??o para calcular todas as estat?sticas para uma coluna
calcular_estatisticas <- function(x) {
  c(Media = mean(x),
    Maximo = max(x),
    Mediana = median(x),
    `Desvio Padrao` = sd(x))
}

# Aplicar a fun??o para cada coluna e armazenar os resultados em um dataframe
estatisticas <- sapply(dados_RN, calcular_estatisticas)

# Adicionar os nomes das linhas
rownames(estatisticas) <- c("Media", "Maximo", "Mediana", "Desvio Padrao")

#Resultados das Estatisticas para o Latex
xtable(estatisticas)

xtable(t(estatisticas))


