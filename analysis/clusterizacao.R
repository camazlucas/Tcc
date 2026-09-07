# Agrupamento das unidades da federacao por perfil de consumo eletrico
#
# Agrupa as UFs a partir da posse de equipamentos e do consumo residencial,
# por ligacao completa com distancia euclidiana e por k-medias.
#
#   Rscript analysis/clusterizacao.R

rm(list = ls(all = TRUE))

library(here)
library(dplyr)
library(ggplot2)
library(factoextra)
library(cluster)
library(MVN)
library(NbClust)
library(xtable)

# Saidas ------------------------------------------------------------------

pasta_figuras <- here("output", "clusterizacao", "figures")
pasta_tabelas <- here("output", "clusterizacao", "tables")
dir.create(pasta_figuras, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_tabelas, recursive = TRUE, showWarnings = FALSE)

unlink(list.files(
  pasta_figuras,
  pattern = "\\.png$", full.names = TRUE
))

# Modo grafico ------------------------------------------------------------

interativo <- interactive() && capabilities("X11") || .Platform$GUI == "Rgui"

.contador_grafico <- 0
.rotulo_grafico <- "grafico"

if (!interativo) {
  x11 <- function(...) {
    if (grDevices::dev.cur() > 1) try(grDevices::dev.off(), silent = TRUE)
    .contador_grafico <<- .contador_grafico + 1
    grDevices::png(
      file.path(
        pasta_figuras,
        sprintf("%02d-%s.png", .contador_grafico, .rotulo_grafico)
      ),
      width = 1400, height = 900, res = 120
    )
  }
}

secao <- function(rotulo) {
  .rotulo_grafico <<- rotulo
}

fechar_grafico <- function() {
  if (!interativo && grDevices::dev.cur() > 1) {
    try(grDevices::dev.off(), silent = TRUE)
  }
  # dev.off() devolve o dispositivo atual; sem isto o "null device" aparece
  # no relatorio a cada grafico fechado.
  invisible(NULL)
}

# Funcoes do projeto ------------------------------------------------------

# Carrega apenas o que este subprojeto usa.
source(here("R", "comum", "estatisticas.R"))
source(here("R", "clusterizacao", "agrupamento.R"))

# Dados -------------------------------------------------------------------

dados_intensos <- read.csv2(
  here("data-raw", "clusterizacao", "dados-intenso-forte.csv")
)
preparados <- preparar_dados_uf(dados_intensos)

dados_nome <- preparados$bruto
dados_padr <- preparados$padronizado

cat("UFs:", nrow(dados_padr), "| variaveis:", ncol(dados_padr), "\n\n")

# Normalidade multivariada ------------------------------------------------

# O MVN 6.x renomeou o argumento mvnTest para mvn_test e o componente
# multivariateNormality para multivariate_normality.
cat("Teste de Mardia de normalidade multivariada\n")
print(mvn(dados_padr, mvn_test = "mardia")$multivariate_normality)

# Analise descritiva ------------------------------------------------------

secao("consumo-residencial")
x11()
boxplot(dados_nome[, 12], col = "blue", pch = 16, ylim = c(0, 15000))
fechar_grafico()

secao("rendimento-medio")
x11()
boxplot(dados_nome[, 14], col = "darkgreen", pch = 16, ylim = c(0, 3000))
fechar_grafico()

secao("posse-equipamentos")
x11()
boxplot(dados_nome[, -c(12, 14, 15, 16)],
  col = rainbow(ncol(dados_padr) - 2), pch = 16, ylim = c(0, 2)
)
abline(h = c(-1, 1), col = "red", lty = "dashed")
fechar_grafico()

secao("variaveis-padronizadas")
x11()
boxplot(dados_padr,
  col = rainbow(ncol(dados_padr)), pch = 16,
  ylim = c(-12, 6), names = FALSE
)
abline(h = c(-1, 1), col = "red", lty = "dashed")
legend("bottomright",
  legend = colnames(dados_padr),
  fill = rainbow(ncol(dados_padr)), bty = "n", cex = 0.8
)
fechar_grafico()

estatisticas <- estat_dados(dados_nome)
write.csv2(
  t(estatisticas),
  file.path(pasta_tabelas, "estatisticas-clusterizacao.csv")
)
print(
  xtable(t(estatisticas), caption = "Estatisticas descritivas por UF"),
  file = file.path(pasta_tabelas, "estatisticas-clusterizacao.tex")
)
print(t(estatisticas))

# Numero de grupos sugerido -----------------------------------------------
# NbClust e o metodo do cotovelo, mais abaixo, sao dois artificios
# CONSULTIVOS: informam a escolha do numero de grupos, nao a determinam. A
# regra da maioria do NbClust aponta 2, o cotovelo aponta 3, e o trabalho
# adota 3. A divergencia e deliberada, nao um descuido.
#
# index = "all" combina 30 indices e responde pela maior parte do tempo de
# execucao deste script.

secao("nbclust")
x11()
melhor_eucl <- NbClust(dados_padr,
  distance = "euclidean", method = "complete",
  min.nc = 2, max.nc = 8, index = "all"
)
fechar_grafico()
cat("\nSugestao do NbClust por indice:\n")
print(melhor_eucl$Best.nc[1, ])

# Ligacao completa --------------------------------------------------------
# Apenas distancia euclidiana. agrupar_hierarquico() aceita qualquer metrica
# de dist(), mas o trabalho usa a euclidiana.

agrup_eucl <- agrupar_hierarquico(dados_padr, "euclidean", k = 3)
secao("dendrograma")
plotar_dendrograma(
  agrup_eucl, 3,
  "Dendrograma dos Clusters - Distancia Euclidiana"
)
fechar_grafico()

ufs_por_grupo <- data.frame(
  UF = rownames(dados_nome),
  Euclidiana = agrup_eucl$grupos
)
write.csv2(
  ufs_por_grupo,
  file.path(pasta_tabelas, "clusters-hierarquicos.csv"),
  row.names = FALSE
)
cat("\nAgrupamento hierarquico das UFs:\n")
print(ufs_por_grupo, row.names = FALSE)

# K-medias ----------------------------------------------------------------

secao("cotovelo-fviz")
x11()
print(
  fviz_nbclust(dados_padr, kmeans, method = "wss") +
    geom_vline(xintercept = 3, linetype = 3)
)
fechar_grafico()

set.seed(123)
cotovelo <- curva_cotovelo(dados_padr, k_max = 15)

secao("cotovelo")
x11()
plot(cotovelo$k, cotovelo$soma_quadrados,
  ylim = c(100, 400), xlim = c(1, 8),
  main = "Metodo do Cotovelo",
  type = "b", pch = 19, frame = TRUE, axes = FALSE,
  cex.lab = 1.5, cex.main = 2,
  xlab = "Numero de clusters - k",
  ylab = "Soma total dos quadrados dentro dos clusters"
)
axis(2, at = seq(0, 400, 50), cex.axis = 1.5)
axis(1, at = seq(1, 13, 1), cex.axis = 1.5)
abline(v = 3, lty = 2)
fechar_grafico()

km <- kmeans(dados_padr, 3, nstart = 25)

cat("\nTamanho de cada grupo (k-medias):\n")
print(km$size)

cat("\nMedia de cada variavel por grupo:\n")
print(aggregate(dados_padr, by = list(cluster = km$cluster), mean))

secao("kmeans")
x11()
print(
  fviz_cluster(km,
    data = dados_padr,
    main = "Clusterizacao pelo Metodo de K-Medias",
    palette = c("darkgreen", "blue", "red"),
    ellipse.type = "euclid",
    star.plot = TRUE,
    repel = TRUE,
    ggtheme = theme_minimal() +
      theme(
        text = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 20),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)
      )
  )
)
fechar_grafico()

ufs_por_grupo$KMedias <- km$cluster
write.csv2(
  ufs_por_grupo,
  file.path(pasta_tabelas, "clusters-por-uf.csv"),
  row.names = FALSE
)

cat("\nAgrupamento final das UFs:\n")
print(ufs_por_grupo, row.names = FALSE)

cat("\nSaida gravada em", here("output", "clusterizacao"), "\n")
