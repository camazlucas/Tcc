#' Classifica a classe social com maquina de vetor de suporte
#'
#' @param divisao Lista com `treino` e `teste`, vinda de
#'   `divisao_dos_dados()`. A mesma divisao e usada pelos tres
#'   classificadores, para que a comparacao entre eles seja justa.
#'
#' @return Lista com `matriz_de_confusao_svm` e `summary_svm`.
svm_class <- function(divisao) {
  treino <- divisao$treino
  teste <- divisao$teste

  # O e1071 exige a resposta como fator; os niveis sao ordenados para que
  # treino e teste usem a mesma codificacao.
  codificar_classe <- function(dados) {
    niveis <- sort(unique(dados$CLASSE))
    dados$CLASSE <- as.integer(factor(dados$CLASSE, levels = niveis))
    dados
  }

  treino_svm <- codificar_classe(treino)
  teste_svm <- codificar_classe(teste)

  classif <- svm(
    formula = CLASSE ~ .,
    data = treino_svm,
    type = "C-classification",
    cost = 0.1,
    kernel = "linear"
  )

  prev_svm <- predict(classif, teste[, !(names(teste) %in% "CLASSE")])

  matriz_confusao_svm <- table(teste_svm$CLASSE, prev_svm)

  list(
    matriz_de_confusao_svm = confusionMatrix(matriz_confusao_svm),
    summary_svm = summary(classif)
  )
}
