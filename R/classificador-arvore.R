#' Classifica a classe social com arvore de decisao
#'
#' @param divisao Lista com `treino` e `teste`, vinda de
#'   `divisao_dos_dados()`. A mesma divisao e usada pelos tres
#'   classificadores, para que a comparacao entre eles seja justa.
#'
#' @return Objeto `confusionMatrix` do caret com as metricas no teste.
arvore_class <- function(divisao) {
  treino <- divisao$treino
  teste <- divisao$teste

  classificador <- rpart(
    formula = CLASSE ~ .,
    data = treino,
    parms = list(split = "information")
  )

  x11()
  rpart.plot(classificador, extra = 102, cex = 0.8)

  previsao <- predict(
    classificador,
    teste[, !(names(teste) %in% "CLASSE")],
    type = "class"
  )

  matriz_confusao_arvore <- table(teste$CLASSE, previsao)

  confusionMatrix(matriz_confusao_arvore)
}
