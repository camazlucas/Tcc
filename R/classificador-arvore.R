#' Classifica a classe social com arvore de decisao
#'
#' @param dados Base com a coluna `CLASSE`.
#' @param qtd_de_classes 3 ou 6.
#'
#' @return Objeto `confusionMatrix` do caret com as metricas no teste.
arvore_class <- function(dados, qtd_de_classes) {
  divisao <- divisao_dos_dados(dados, qtd_de_classes)
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
