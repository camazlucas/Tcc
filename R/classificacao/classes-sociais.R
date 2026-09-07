#' Converte o codigo numerico da classe social em rotulo
#'
#' Com 6 classes usa os estratos do Criterio Brasil (A, B1, B2, C1, C2, DE).
#' Com 3 classes agrupa em Alta, Media e Baixa e reamostra os grupos para o
#' tamanho do menor deles, equilibrando a base.
#'
#' @param dados_RN Base com a coluna `CLASSE` codificada de 1 a 6.
#' @param x Quantidade de classes desejada: 3 ou 6.
#'
#' @return Data frame com `CLASSE` como rotulo textual.
divisao_das_classes <- function(dados_RN, x) {
  if (x == 6) {
    dados_classe <- dados_RN %>%
      mutate(CLASSE = case_when(
        CLASSE == 1 ~ "A",
        CLASSE == 2 ~ "B1",
        CLASSE == 3 ~ "B2",
        CLASSE == 4 ~ "C1",
        CLASSE == 5 ~ "C2",
        CLASSE == 6 ~ "DE",
        TRUE ~ as.character(CLASSE)
      ))
  } else if (x == 3) {
    dados_UF_3classes <- dados_RN %>%
      mutate(CLASSE = case_when(
        CLASSE == 1 ~ "Alta",
        CLASSE == 2 ~ "Alta",
        CLASSE == 3 ~ "Media",
        CLASSE == 4 ~ "Media",
        CLASSE == 5 ~ "Media",
        CLASSE == 6 ~ "Baixa",
        TRUE ~ as.character(CLASSE)
      ))

    # Reamostra cada grupo para o tamanho do menor, equilibrando as classes
    contagem_classes <- dados_UF_3classes %>%
      group_by(CLASSE) %>%
      summarise(contagem = n())

    tamanho_minimo <- min(contagem_classes$contagem)

    set.seed(42) # reprodutibilidade da reamostragem

    dados_classe <- dados_UF_3classes %>%
      group_by(CLASSE) %>%
      sample_n(tamanho_minimo) %>%
      ungroup()
  } else {
    stop("Selecione 3 ou 6 classes")
  }

  dados_classe
}
