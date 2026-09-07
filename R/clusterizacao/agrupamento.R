#' Padroniza a base de consumo por UF
#'
#' @param dados Base lida de `data-raw/clusterizacao/dados-intenso-forte.csv`.
#' @param colunas Indices das colunas numericas a manter.
#'
#' @return Lista com `bruto` (escala original, UF nas linhas) e `padronizado`.
preparar_dados_uf <- function(dados, colunas = 3:18) {
  bruto <- data.frame(dados[, colunas], row.names = dados$UF)
  padronizado <- scale(bruto)

  # Os nomes vem do CSV com pontos no lugar dos espacos
  colnames(padronizado) <- gsub("\\.", " ", colnames(padronizado))

  list(bruto = bruto, padronizado = padronizado)
}

#' Agrupa as UFs por ligacao completa
#'
#' @param dados_padr Matriz padronizada.
#' @param metrica Metrica de distancia aceita por `dist()`.
#' @param k Numero de grupos a extrair do dendrograma.
#'
#' @return Lista com o objeto `hclust`, a matriz de distancias e os grupos.
agrupar_hierarquico <- function(dados_padr, metrica = "euclidean", k = 3) {
  distancias <- dist(dados_padr, method = metrica)
  arvore <- hclust(distancias, method = "complete")

  list(
    arvore = arvore,
    distancias = as.matrix(distancias),
    grupos = cutree(arvore, k = k)
  )
}

#' Desenha o dendrograma com os grupos destacados
#'
#' @param agrupamento Saida de `agrupar_hierarquico()`.
#' @param k Numero de grupos a destacar.
#' @param titulo Titulo do grafico.
plotar_dendrograma <- function(agrupamento,
                               k = 3,
                               titulo = "Dendrograma dos Clusters") {
  x11()
  plot(agrupamento$arvore,
    hang = -1,
    sub = "",
    ylab = "Distancia Maxima entre Clusters",
    main = titulo,
    xlab = "",
    cex = 1.1,
    cex.axis = 1.5,
    cex.lab = 1.5,
    cex.main = 1.7
  )
  rect.hclust(agrupamento$arvore, k = k, border = "red")
}

#' Sugere o numero de grupos pelo metodo do cotovelo
#'
#' @param dados_padr Matriz padronizada.
#' @param k_max Maior numero de grupos a avaliar.
#'
#' @return Data frame com a soma de quadrados intra-grupo por k.
curva_cotovelo <- function(dados_padr, k_max = 15) {
  soma_quadrados <- vapply(
    seq_len(k_max),
    function(k) kmeans(dados_padr, k, nstart = 10)$tot.withinss,
    numeric(1)
  )

  data.frame(k = seq_len(k_max), soma_quadrados = soma_quadrados)
}
