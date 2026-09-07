# Ajustando rede neural---------------------------------------------------
rn_class <- function(dados, qtd_de_classes, numero_de_neuronios = 12, lr = 0.01,
                     threshold = 0.05, stepmax = 5e4){

  # Divisao em Treino e Teste, com normalizacao ajustada no treino.
  # Sem normalizar, o rprop nao converge dentro do stepmax nas bases com
  # variaveis de escalas diferentes (por exemplo a base reduzida por CP).
  divisao = divisao_dos_dados(dados, qtd_de_classes, normalizar = TRUE)
  treino = divisao$treino
  teste = divisao$teste

  # Treinando a rede neural.
  # O rprop nem sempre converge dentro do stepmax, e quando isso acontece o
  # neuralnet devolve weights = NULL, o que faz compute() quebrar depois com
  # um erro de matriz que nao indica a causa. Por isso a convergencia e
  # verificada aqui, afrouxando o threshold a cada tentativa.
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
    stop("A rede nao convergiu com threshold ate ", threshold * 4,
         ". Aumente o stepmax ou reduza numero_de_neuronios.")
  }

  # Previsão
  prev <- compute(
    classificador_RN,
    teste[, !(names(teste) %in% "CLASSE")]
  )$net.result

  # Os neuronios de saida seguem a ordem alfabetica das classes do treino.
  # Nao usar model.list$response aqui: ele lista as classes na ordem de
  # aparicao no data.frame, que nao e a ordem das colunas de net.result.
  labels <- sort(unique(treino$CLASSE))

  # Convertendo índices para rótulos
  predicao <- labels[apply(prev, 1, which.max)]

  # Matriz de confusão
  matriz_confusao_RN <- table(factor(teste$CLASSE, levels = labels),
                              factor(predicao,     levels = labels))

  metricas <- confusionMatrix(matriz_confusao_RN)

  return(list(
    modelo_rn = classificador_RN,
    matriz_de_confusao_rn = matriz_confusao_RN,
    metricas_rn = metricas
  ))

}
