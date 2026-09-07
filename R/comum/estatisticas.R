#' Boxplot da dispersao de todas as variaveis da base
#'
#' @param dados Base a inspecionar.
boxplot_dispersao <- function(dados) {
  x11()
  boxplot(dados, col = rainbow(ncol(dados)), pch = 16, ylim = c(-1, 20))
  abline(h = c(-1, 1), col = "red", lty = "dashed")
}

#' Estatisticas descritivas de uma variavel
#'
#' @param x Vetor numerico.
#'
#' @return Vetor nomeado com media, maximo, mediana e desvio padrao.
calcular_estatisticas <- function(x) {
  c(
    Media = mean(x),
    Maximo = max(x),
    Mediana = median(x),
    `Desvio Padrao` = sd(x)
  )
}

#' Tabela de estatisticas descritivas da base
#'
#' @param dados Base a resumir.
#'
#' @return Matriz com uma linha por estatistica e uma coluna por variavel.
estat_dados <- function(dados) {
  estatisticas <- sapply(dados, calcular_estatisticas)
  rownames(estatisticas) <- c("Media", "Maximo", "Mediana", "Desvio Padrao")

  estatisticas
}
