
# Pacotes -----------------------------------------------------------------

library("dplyr")
library("tidyr")
library("ggplot2")
library("factoextra")
library("tidyverse")
library("cluster")
library("biotools")
library("mvnormtest")
library("psych")
library("MVN")
library("NbClust")
library("xtable")

# Limpeza de Memoria ------------------------------------------------------


rm(list=ls(all=TRUE)) # Limpar a mem√≥ria



# Buscando e Organizando os Dados -----------------------------------------

  setwd("G:/Meu Drive/Rural/IC/Material/Base de Dados/Consumo Residencial Atualizado")

  dadosintensos = read.csv2("Dados Intenso e Forte.csv")
  #dadostotais = read.csv2("Dados Totais.csv")
  dadosnome = data.frame(dadosintensos[,3:18], row.names = dadosintensos$UF)
  
  #dadosnome = data.frame(dadostotais[,3:27], row.names = dadostotais$UF)
  
  #dados_eletro_principais = dadosintensos[,11:18]

## Padronizando os dados ---------------------------------------------------

  dadospadr <- scale(dadosnome)
  
#testando

  resultado100 <- mvn(dadospadr, mvnTest = "mardia")
  resultado100

  colnames(dadospadr) <- gsub("\\.", " ", colnames(dadospadr))

## Analise descritiva ------------------------------------------------------
  
x11();boxplot(dadosnome[,12], col="blue", 
                pch=16, ylim=c(-0, 15000)); abline(h=c(-1,1), col="red", lty="dashed")

x11();boxplot(dadosnome[,14], col="darkgreen", 
                pch=16, ylim=c(-0, 3000)); abline(h=c(-1,1), col="red", lty="dashed")

x11();boxplot(dadosnome[,-c(12,14, 15,16)], col=rainbow(ncol(dadospadr)-2), 
              pch=16, ylim=c(-0, 2)); abline(h=c(-1,1), col="red", lty="dashed")
  
  
x11();{boxplot(dadospadr, 
              col=rainbow(ncol(dadospadr)), 
              pch=16, 
              ylim=c(-12, 6),
              names = FALSE); 
              abline(h=c(-1,1), 
              col="red",
              lty="dashed")
              legend("bottomright", 
              legend = colnames(dadospadr), 
              fill = rainbow(ncol(dadospadr)), 
              bty = "n", 
              cex = 0.8)}
              

medias = round(apply(dadosnome, 2, mean), 2)
mediasmil = medias/1000
mediasmil

desviopadrao = round(apply(dadosnome, 2, FUN = sd), 2)
desviopadrao
desviomil = round(desviopadrao/1000, 2)
desviomil


# Metodo Completo com Distancia Euclidiana --------------------------------

  x11(); dadoseucl <- NbClust(dadospadr, distance="euclidean", method="complete", 
                         min.nc=2, max.nc=8, index="all")
  dadoseucl
  
  
  #### ==== numero de clusters hierarquico ==== ####
  resp = NbClust(dadospadr,distance="euclidean",method="complete",min.nc=2,max.nc=7,index="all")
  resp
  resp$Best.partition
  resp$All.index
  resp$Best.nc
  
  
  X.de <- dist(dadospadr, method="euclidean")
  
  #Tabela com as distancias
  matrizdist = as.matrix(X.de)
  
  X.hccomplete <- hclust(X.de, method = "complete")
  X.hccomplete
  #x11();plot(X.hccomplete, labels = dadosnome$head)
  X.cl.complete <- cutree(X.hccomplete, k = 3)
  X.cl.complete
  
  #Discretizando Cluster no Dataframe
  cidadesligcomp = data.frame(dadosnome, Cluster = X.cl.complete)
  cidadesligcomp
  
  #Selecionando 5 cluster
  x11();  {plot(X.hccomplete,
                hang = -1,
                sub="", 
                ylab = "Dist‚ncia M·xima entre Clusters", 
                main ="Dendrograma dos Clusters", 
                xlab = "", 
                cex = 1.1, 
                cex.axis = 1.5, 
                cex.lab = 1.5,
                cex.main = 1.7,
                cex.sub = 1.5); 
    groups = cutree(X.hccomplete, k=3); 
    rect.hclust(X.hccomplete, k=3, border="red")}
  

# Metodo Completo com Distancia Minkowski ---------------------------------
  
  x11(); dadosmink <- NbClust(dadospadr, distance="minkowski", method="complete", 
                              min.nc=2, max.nc=8, index="all")
  dadosmink
  
  
  #### ==== numero de clusters hierarquico ==== ####
  respm = NbClust(dadospadr,distance="minkowski",method="complete",min.nc=2,max.nc=7,index="all")
  respm
  respm$Best.partition
  respm$All.index
  respm$Best.nc

  X.demink<- dist(dadospadr, method="minkowski")
  
  #Tabela com as distancias
  matrizdistmink = as.matrix(X.demink)
  
  X.hccompletemink <- hclust(X.demink, method = "complete")
  X.hccompletemink
  #x11();plot(X.hccomplete, labels = dadosnome$head)
  X.cl.completemink <- cutree(X.hccompletemink, k = 5)
  X.cl.completemink
  
  #Discretizando Cluster no Dataframe
  cidadesligcompmink = data.frame(dadosnome, Cluster = X.cl.completemink)
  cidadesligcompmink

  #Selecionando 5 cluster
    x11();  {plot(X.hccompletemink,
                 hang = -1,
                 sub="", 
                 ylab = "Dist?ncia M?xima entre Clusters", 
                 main ="Dendrograma dos Clusters", 
                 xlab = "", 
                 cex = 1.1, 
                 cex.axis = 1.2, 
                 cex.lab = 1.2); 
      groupsmink = cutree(X.hccompletemink, k=4); 
      rect.hclust(X.hccompletemink, k=4, border="red")}


# Numero de Clusters Pelo wss Kmeans --------------------------------------

  x11();fviz_nbclust(dadospadr, kmeans, method = "wss") + geom_vline(xintercept = 3, linetype = 3)

  set.seed(123)

  # function to compute total within-cluster sum of square 
    k= 3
    wss <- function(k) {
      kmeans(dadospadr, k, nstart = 10 )$tot.withinss
    }

  # Compute and plot wss for k = 1 to k = 15
    k.values <- 1:15

  # extract wss for 2-15 clusters
    wss_values <- map_dbl(k.values, wss)

    x11(); plot(k.values, wss_values,ylim = c(100,400), xlim = c(1,8), main = "Elbow Method - MÈtodo do Cotovelo",
            type="b", pch = 19, frame = T, axes = F, cex.lab=1.5,cex.main =2, cex.axis=3,
            xlab="N˙mero de clusters - k",
            ylab="Soma total dos quadrados dentro dos clusters");axis(2, at=seq(0,200,50), cex.axis=1.5);axis(1, at = seq(1,13,1), cex.axis=1.5); abline(v=3, lty=2)

  #Usando Kmeans
    km100 <- kmeans(dadospadr, 3, nstart = 25)
    km100

    t100 <- aggregate(dadospadr, by=list(cluster=km100$cluster), mean)
    t100

    dd100 <- cbind(dadospadr, cluster = km100$cluster)
    dd100

  #calculando o tamanho de cada cluster:
    cl100 <- km100$size
    cl100

  #Gr√É¬°fico dos Clusters
    x11();{fviz_cluster(km100, 
                       data = dadospadr, 
                       main = "ClusterizaÁ„o pelo MÈtodo de K-MÈdias",
                   palette = c("darkgreen",
                               "blue",
                               "red",
                               "black",
                               "orange",
                               "yellow" ),
                   ellipse.type = "euclid", 
                   star.plot = TRUE, 
                   repel = TRUE, 
                   ggtheme = theme_minimal() +
                     theme(text = element_text(size = 14),    # Ajusta o tamanho da fonte do texto geral
                           axis.text.x = element_text(size = 14),  # Ajusta o tamanho da fonte dos rÛtulos do eixo x
                           axis.text.y = element_text(size = 14),  # Ajusta o tamanho da fonte dos rÛtulos do eixo y
                           axis.title = element_text(size = 14),   # Ajusta o tamanho da fonte dos tÌtulos dos eixos
                           plot.title = element_text(size = 20),   # Ajusta o tamanho da fonte do tÌtulo do gr·fico
                           legend.title = element_text(size = 14), # Ajusta o tamanho da fonte do tÌtulo da legenda
                           legend.text = element_text(size = 14))
    )}
    
    

# Dataframe com Indicacao dos Clusters obtidos ----------------------------


## Ligacao Completa --------------------------------------------------------

    #Cluster de Cada Cidade pela liga??o Completa
    cidadesligcomp[cidadesligcomp$Cluster == 2,]
    
    #Analise das m?dias
    cluster1 = cidadesligcomp[cidadesligcomp$Cluster == 1,]
    cluster2 = cidadesligcomp[cidadesligcomp$Cluster == 2,]
    cluster1$Cluster = "CentroNorte"
    cluster2$Cluster = "Sulsudeste"
    
    #Dataset para treino
    totalclusters = cidadesligcomp
    totalclusters = cbind(totalclusters, totalclusters$Cluster == 1)
    totalclusters = cbind(totalclusters, totalclusters$Cluster == 2)
    
    names(totalclusters)[18] = 'CentroNorte'
    names(totalclusters)[19] = 'SulSudeste'
    
    head(totalclusters)


## Metodo de K-means -------------------------------------------------------

    clustersk = cbind(dadosnome, clusters = km100$cluster)

    
##########################################################################

# MÈtodos Supervisionados - Database -------------------------------------------------

    totalclusters
    clustersk
    
    # Install, this package has some functions grafichs and classification
    #install.packages("caret", dependencies=T)
    library(caret)
    library(dplyr)
    library(neuralnet)
    
    particao = createDataPartition(1:dim(totalclusters)[2],p=.7)
    dataset_treino = totalclusters[particao$Resample1,]
    dataset_teste = totalclusters[- particao$Resample1,]
    
    treino = subset(dataset_treino, select = -Cluster)
    teste = subset(dataset_teste, select = -Cluster)
      
    names(teste)
    table(dataset_teste$Cluster)

# Redes Neurais -----------------------------------------------------------


## Clusters LigaÁ„o Completa -----------------------------------------------

    #modelo_hierarquico = neuralnet(CentroNorte + SulSudeste ~ Celular + Conversor.Digital + Modem.TV.Internet + Notebook + Receptor.de.TV.por.assinatura + Roteador.WIFI + Ventilador.de.Teto + Ventilador.ou.Circulador.de.Ar + Freezer+Ar.Condicionado + Microondas + Consumo.residencial.por.UF..2019. + Densidade.de.moradores.por.comodo + Rendimento.Medio.Anual + Duas.ou.mais.TV.tela.fina + Geladeira, dataset_treino, hidden= c(6,6), act.fct = "logistic")
    
    modelo_h = neuralnet(CentroNorte + SulSudeste ~ ., treino, linear.output = FALSE, hidden = c(13, 8, 8, 8, 6), rep="2", act.fct = "logistic")
    
    prev <- compute(modelo_h, teste)
    resultado = as.data.frame(prev$net.result)
    
    names(resultado)[1] <- 'CentroNorte'
    names(resultado)[2] <- 'Sulsudeste'
    
    resultado    
    
    confusionMatrix(prev, totalclusters)
    

## Custers K-means ---------------------------------------------------------

    totalclustersk = clustersk
    totalclustersk = cbind(totalclustersk, totalclustersk$clusters == 1)
    totalclustersk = cbind(totalclustersk, totalclustersk$clusters == 2)
    totalclustersk = cbind(totalclustersk, totalclustersk$clusters == 3)
    
    names(totalclustersk)[18] = 'CentroNorte'
    names(totalclustersk)[19] = 'SulSudeste'
    names(totalclustersk)[20] = 'Nortao'    
    head(totalclustersk)
    
    particao = createDataPartition(1:dim(totalclustersk)[1],p=.7)
    dataset_treinok = totalclustersk[particao$Resample1,]
    dataset_testek = totalclustersk[- particao$Resample1,]
    
    treinok = subset(dataset_treinok, select = -clusters)
    testek = subset(dataset_testek, select = -clusters)
    
    names(testek)
    table(dataset_teste$Cluster)
    
    modelo_k = neuralnet(CentroNorte + SulSudeste + Nortao ~ ., treinok, linear.output = FALSE, rep = 1000,  hidden = c(3,4), act.fct = "logistic")

    prev <- compute(modelo_k, testek)
    resultado = as.data.frame(prev$net.result)
    resultado    
    
    
# Aplicando Componentes Principais para reducao da dimens√£o ---------------
    
setwd("G:/Meu Drive/Rural/IC/Material/Base de Dados/Procel")

dados_eletro = read.csv2("Todos Equipamentos Eletronicos.csv")

dados_cp = data.frame(dados_eletro[,2:56], row.names = dados_eletro$X)

comp_princ = prcomp(dados_cp, scale = TRUE)

summary(comp_princ)

fviz_eig(comp_princ)

fviz_contrib(comp_princ,choice = "var", axes = 1, top = 27)


# Extrair os pesos dos componentes principais
weights <- comp_princ$rotation


# Obter os nomes das vari·veis originais
nomes_variaveis <- colnames(dados_cp)

# Ordenar os pesos da primeira componente principal
pesos_cp <- data.frame(abs(weights[, 1]))
View(pesos_cp)


#Selecionar Quantidade de Componentes Principais
Quantidade_CP = 1


# Inicializar uma lista para armazenar as vari·veis selecionadas para cada componente principal
variaveis_contribuicao <- vector("list", length = Quantidade_CP)

# Loop sobre as cinco primeiras componentes principais
for (i in 1:Quantidade_CP) {
  # Ordenar os pesos da componente principal atual
  pesos_componente <- weights[, i]
  
  # Identificar as vari·veis que contribuem significativamente para a componente principal atual
  variaveis_contribuicao[[i]] <- nomes_variaveis[abs(pesos_componente) > 0.2]
}

# Exibir as vari·veis selecionadas para cada componente principal
variaveis_contribuicao

# Unir os nomes das vari·veis selecionadas para as 5 componentes principais em um vetor
nomes_variaveis_componentes <- unlist(variaveis_contribuicao[1:Quantidade_CP])

# Remover valores duplicados
nomes_variaveis_unicos <- unique(nomes_variaveis_componentes)

# Exibir os nomes das vari·veis ˙nicas
nomes_variaveis_unicos

#Criando Dataframe com as vari·veis significativas dos componentes selecionados
dados_reduzidos = dados_cp[,nomes_variaveis_unicos]

