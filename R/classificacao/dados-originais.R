#' Seleciona as variaveis originais do Criterio Brasil
#'
#' Estas sao as variaveis que a propria pesquisa usa para atribuir a classe
#' social. Servem como referencia de comparacao para os modelos treinados
#' sobre a posse de equipamentos.
#'
#' @param dados Base bruta da PPH 2019, como lida do CSV.
#' @param UF Sigla da unidade da federacao.
#'
#' @return Data frame com as variaveis do criterio e `CLASSE`.
dados_originais <- function(dados, UF) {
  dados_orig <- dados[, c(3, 7:22, 51)]

  # Colunas numericas que vem como texto com separador de milhar
  dados_orig$P3.1_3 <- as.numeric(gsub(",", "", dados_orig$P3.1_3))
  dados_orig$P3.1_4 <- as.numeric(gsub(",", "", dados_orig$P3.1_4))
  dados_orig$P3.1_5 <- as.numeric(gsub(",", "", dados_orig$P3.1_5))
  dados_orig$P3.1_12 <- as.numeric(gsub(",", "", dados_orig$P3.1_12))
  dados_orig[is.na(dados_orig)] <- 0

  # Mantem apenas residencias sem atividade comercial
  dados_orig <- subset(dados_orig, P5.13 == 1)

  # ATENCAO: a selecao acima traz 18 colunas e este vetor tem 17 nomes. A
  # ultima coluna (P5.13) fica com nome NA, de proposito: ela ja cumpriu seu
  # papel no subset acima e e descartada no final. Por isso o subset vem
  # antes da renomeacao.
  names(dados_orig) <- c(
    "UF",
    "Automoveis",
    "Empregados",
    "Lava_Roupas",
    "Banheiros",
    "DVD",
    "Geladeiras",
    "Freezers",
    "Microcomputadores",
    "Lava_Loucas",
    "Microondas",
    "Motocicletas",
    "Seca_Roupas",
    "Fonte_da_Agua",
    "Rua",
    "Instrucao",
    "CLASSE"
  )

  dados_orig <- filtrar_estados(UF, dados_orig)

  # Apos filtrar_estados() remover UF, a posicao 17 e a coluna P5.13 sem nome
  dados_orig[, -17]
}
