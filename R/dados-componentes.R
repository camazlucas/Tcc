#' Reduz a dimensao selecionando variaveis por componentes principais
#'
#' Calcula os componentes principais das preditoras e mantem as variaveis
#' cujo peso absoluto na componente supera `peso`.
#'
#' @param qtd_cp Quantas componentes considerar.
#' @param peso Peso absoluto minimo para manter a variavel.
#' @param dados Base com a coluna `CLASSE`.
#'
#' @return Data frame com as variaveis selecionadas mais `CLASSE`.
reduzir_com_cp <- function(qtd_cp, peso, dados) {
  ind_class <- which(names(dados) == "CLASSE")
  dados_cp <- dados[, -ind_class]
  comp_princ <- prcomp(dados_cp, scale = TRUE)

  pesos <- comp_princ$rotation
  nomes_variaveis <- colnames(dados_cp)

  variaveis_contribuicao <- vector("list", length = qtd_cp)
  for (i in seq_len(qtd_cp)) {
    pesos_componente <- pesos[, i]
    variaveis_contribuicao[[i]] <-
      nomes_variaveis[abs(pesos_componente) > peso]
  }

  nomes_variaveis_unicos <- unique(unlist(variaveis_contribuicao))

  dados_reduzidos <- dados[, nomes_variaveis_unicos]
  dados_reduzidos$CLASSE <- dados$CLASSE

  dados_reduzidos
}
