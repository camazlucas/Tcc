#' Filtra a base por unidade da federacao
#'
#' @param uf Sigla da UF, por exemplo "RJ".
#' @param dados_filtrados Base ja tratada por `tratamento_dos_dados()`.
#'
#' @return Data frame restrito a UF pedida, sem a coluna `UF`.
filtrar_estados <- function(uf, dados_filtrados) {
  dados_estado <- subset(dados_filtrados, UF == uf)
  dados_estado <- dados_estado[, !(names(dados_estado) %in% "UF")]

  dados_estado
}
