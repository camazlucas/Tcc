#' Classifica a classe social com rede neural
#'
#' Recebe a mesma divisao dos demais classificadores e normaliza as
#' preditoras internamente. A normalizacao e necessaria porque o rprop nao
#' converge dentro do `stepmax` quando as variaveis estao em escalas
#' diferentes, e fica restrita a esta funcao para nao alterar a arvore nem
#' a maquina de vetor de suporte, que operam sobre os valores originais.
#'
#' @param divisao Lista com `treino` e `teste`, vinda de
#'   `divisao_dos_dados()`.
#' @param numero_de_neuronios Neuronios da camada oculta.
#' @param lr Taxa de aprendizado.
#' @param threshold Limite de parada do rprop na primeira tentativa.
#' @param stepmax Maximo de passos por tentativa.
#'
#' @return Lista com `modelo_rn`, `matriz_de_confusao_rn` e `metricas_rn`.
rn_class <- function(divisao,
                     numero_de_neuronios = 12,
                     lr = 0.01,
                     threshold = 0.05,
                     stepmax = 5e4) {
  escalados <- normalizar_treino_teste(divisao$treino, divisao$teste)
  treino <- escalados$treino
  teste <- escalados$teste

  # Sem convergir, o neuralnet devolve weights = NULL e o compute() seguinte
  # falha com um erro de matriz que nao indica a causa. Por isso a
  # convergencia e verificada aqui, afrouxando o threshold a cada tentativa.
  classificador_RN <- NULL

  for (tentativa_threshold in threshold * c(1, 2, 4)) {
    candidato <- neuralnet(
      CLASSE ~ .,
      treino,
      linear.output = FALSE,
      learningrate = lr,
      hidden = numero_de_neuronios,
      act.fct = "logistic",
      threshold = tentativa_threshold,
      stepmax = stepmax
    )

    if (!is.null(candidato$weights)) {
      if (tentativa_threshold != threshold) {
        message("Rede convergiu apenas com threshold = ", tentativa_threshold)
      }
      classificador_RN <- candidato
      break
    }
  }

  if (is.null(classificador_RN)) {
    stop(
      "A rede nao convergiu com threshold ate ", threshold * 4,
      ". Aumente o stepmax ou reduza numero_de_neuronios."
    )
  }

  prev <- compute(
    classificador_RN,
    teste[, !(names(teste) %in% "CLASSE")]
  )$net.result

  # Os neuronios de saida seguem a ordem alfabetica das classes do treino.
  # Nao usar model.list$response: ele lista as classes na ordem de aparicao
  # no data frame, que nao corresponde as colunas de net.result.
  labels <- sort(unique(treino$CLASSE))

  predicao <- labels[apply(prev, 1, which.max)]

  matriz_confusao_RN <- table(
    factor(teste$CLASSE, levels = labels),
    factor(predicao, levels = labels)
  )

  list(
    modelo_rn = classificador_RN,
    matriz_de_confusao_rn = matriz_confusao_RN,
    metricas_rn = confusionMatrix(matriz_confusao_RN)
  )
}
