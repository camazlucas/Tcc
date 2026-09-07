#' Normaliza preditoras por min-max usando estatisticas do treino
#'
#' Os minimos e maximos vem exclusivamente do conjunto de treino e sao
#' aplicados tambem ao teste, para nao vazar informacao do teste ao modelo.
#'
#' @param treino,teste Data frames com a coluna `CLASSE`.
#'
#' @return Lista com os elementos `treino` e `teste` normalizados.
normalizar_treino_teste <- function(treino, teste) {
  preditoras <- setdiff(names(treino), "CLASSE")

  minimos <- sapply(treino[preditoras], min)
  maximos <- sapply(treino[preditoras], max)
  amplitude <- maximos - minimos
  amplitude[amplitude == 0] <- 1 # colunas constantes ficam zeradas

  escalar <- function(dados) {
    dados[preditoras] <- as.data.frame(
      scale(dados[preditoras], center = minimos, scale = amplitude)
    )
    dados
  }

  list(treino = escalar(treino), teste = escalar(teste))
}

#' Divide a base em treino e teste
#'
#' @param dados Base a dividir.
#' @param qtd_de_classes 3 ou 6, conforme o agrupamento de classes sociais.
#' @param normalizar Se `TRUE`, aplica `normalizar_treino_teste()`.
#'
#' @return Lista com os elementos `treino` e `teste`.
divisao_dos_dados <- function(dados, qtd_de_classes, normalizar = FALSE) {
  dados_com_classe <- divisao_das_classes(dados, qtd_de_classes)

  # divisao_das_classes devolve tibble quando ha reamostragem por grupo
  dados_com_classe <- as.data.frame(dados_com_classe)

  particao <- createDataPartition(seq_len(nrow(dados_com_classe)), p = 0.7)
  treino <- dados_com_classe[particao$Resample1, ]
  teste <- dados_com_classe[-particao$Resample1, ]

  if (normalizar) {
    escalados <- normalizar_treino_teste(treino, teste)
    treino <- escalados$treino
    teste <- escalados$teste
  }

  list(treino = treino, teste = teste)
}
