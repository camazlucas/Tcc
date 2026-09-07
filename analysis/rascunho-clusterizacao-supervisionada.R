# RASCUNHO -- NAO EXECUTAVEL NO ESTADO ATUAL
#
# Este arquivo preserva dois blocos exploratorios que estavam no script
# original de clusterizacao e que nao rodam. Foram separados para que o
# analysis/02-clusterizacao.R contenha apenas o que executa de ponta a ponta.
#
# BLOCO 1 - Redes neurais sobre os clusters
#
#   Problemas identificados:
#
#   a) createDataPartition(1:dim(totalclusters)[2], p = .7) usa o numero de
#      COLUNAS (19) para sortear indices de LINHA. A base tem 27 UFs, entao a
#      particao ignora as linhas 20 a 27 e o teste fica com 8 observacoes.
#      O correto seria dim(totalclusters)[1] ou nrow(totalclusters).
#
#   b) confusionMatrix(prev, totalclusters) passa o objeto devolvido por
#      compute() e a base inteira. O caret espera dois fatores de mesmo
#      comprimento, ou uma tabela de contingencia.
#
#   c) Com 27 observacoes no total, treinar uma rede com hidden = c(13,8,8,8,6)
#      (cinco camadas ocultas) nao se sustenta estatisticamente.
#
#   d) neuralnet(rep = "2") passa texto onde se espera inteiro, e rep = 1000
#      no modelo de k-medias repete o treino mil vezes.
#
# BLOCO 2 - Componentes principais sobre todos os equipamentos
#
#   Le "Todos Equipamentos Eletronicos.csv" de um diretorio local do Google
#   Drive. Esse arquivo nao esta no repositorio e nao ha copia dele aqui, entao
#   o bloco nao tem como rodar. Para reativa-lo, coloque o CSV em data-raw/ e
#   troque o setwd() por here("data-raw", "...").
#
#   A logica de selecao de variaveis por peso na componente ja existe, pronta e
#   parametrizada, em R/dados-componentes.R (funcao reduzir_com_cp).
#
# O codigo abaixo esta comentado de proposito. O historico completo, antes da
# reestruturacao, esta no Git.

# --- BLOCO 1 --------------------------------------------------------------

# totalclusters <- cidadesligcomp
# totalclusters <- cbind(totalclusters, totalclusters$Cluster == 1)
# totalclusters <- cbind(totalclusters, totalclusters$Cluster == 2)
# names(totalclusters)[18] <- "CentroNorte"
# names(totalclusters)[19] <- "SulSudeste"
#
# particao <- createDataPartition(1:dim(totalclusters)[2], p = .7)  # (a)
# dataset_treino <- totalclusters[particao$Resample1, ]
# dataset_teste <- totalclusters[-particao$Resample1, ]
#
# treino <- subset(dataset_treino, select = -Cluster)
# teste <- subset(dataset_teste, select = -Cluster)
#
# modelo_h <- neuralnet(CentroNorte + SulSudeste ~ ., treino,
#   linear.output = FALSE, hidden = c(13, 8, 8, 8, 6),
#   rep = "2", act.fct = "logistic"                                  # (c) (d)
# )
#
# prev <- compute(modelo_h, teste)
# confusionMatrix(prev, totalclusters)                               # (b)

# --- BLOCO 2 --------------------------------------------------------------

# setwd("G:/Meu Drive/Rural/IC/Material/Base de Dados/Procel")
# dados_eletro <- read.csv2("Todos Equipamentos Eletronicos.csv")
# dados_cp <- data.frame(dados_eletro[, 2:56], row.names = dados_eletro$X)
#
# comp_princ <- prcomp(dados_cp, scale = TRUE)
# summary(comp_princ)
# fviz_eig(comp_princ)
# fviz_contrib(comp_princ, choice = "var", axes = 1, top = 27)
#
# # Equivalente parametrizado ja disponivel:
# #   dados_reduzidos <- reduzir_com_cp(qtd_cp = 1, peso = 0.2, dados = dados_cp)
