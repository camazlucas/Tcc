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

#' Graficos de dispersao nas duas primeiras componentes principais
#'
#' Gera uma versao com 6 classes e outra com 3, para comparar a separacao
#' dos estratos no plano das componentes.
#'
#' @param dados Base com a coluna `CLASSE` codificada de 1 a 6.
graficos_dispersao <- function(dados) {
  dados_cp <- dados[, !(names(dados) %in% "CLASSE")]
  comp_princ <- prcomp(dados_cp, scale = TRUE)

  desenhar <- function(grupo, cores, titulo) {
    x11()
    plot(comp_princ$x[, 1], comp_princ$x[, 2],
      col = cores[grupo],
      xlab = "Dim 1",
      ylab = "Dim 2",
      main = titulo
    )
    legend("bottomright",
      legend = levels(grupo),
      col = cores,
      pch = 1,
      title = "CLASSE"
    )
  }

  grupo6 <- as.factor(divisao_das_classes(dados, 6)$CLASSE)
  desenhar(
    grupo6,
    c("red", "blue", "darkgreen", "orange", "purple", "green"),
    "Grafico de Dispersao com Divisao de 6 Classes"
  )

  # Aqui as classes sao apenas renomeadas, sem a reamostragem que
  # divisao_das_classes() aplica no caminho de 3 classes, para que o grafico
  # mostre todos os pontos da base.
  grupo3 <- as.factor(dplyr::case_when(
    dados$CLASSE %in% c(1, 2) ~ "Alta",
    dados$CLASSE %in% c(3, 4, 5) ~ "Media",
    dados$CLASSE == 6 ~ "Baixa",
    TRUE ~ as.character(dados$CLASSE)
  ))
  desenhar(
    grupo3,
    c("red", "blue", "darkgreen"),
    "Grafico de Dispersao com Divisao de 3 Classes"
  )
}
