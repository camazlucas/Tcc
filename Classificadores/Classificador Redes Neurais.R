# Ajustando rede neural---------------------------------------------------
rn_class <- function(dados, qtd_de_classes, numero_de_neuronios = 12, lr = 0.01){
  
  #Divisao em Treino e Teste
  split = divisao_das_classes(qtd_de_classes, dados)
  
  treino = split$treino
  teste = split$teste
  
  # Treinando a rede neural
  classificador_RN <- neuralnet(
    CLASSE ~ .,
    treino,
    linear.output = FALSE,
    learningrate = lr,
    hidden = numero_de_neuronios,
    act.fct = "logistic"
  )
  
  # Previsão
  prev <- compute(
    classificador_RN,
    teste[, !(names(teste) %in% "CLASSE")]
  )$net.result
  
  # Função para pegar índice do maior valor
  classe_max <- function(x){
    which.max(x)
  }
  
  x <- apply(prev, 1, classe_max)
  
  qtd_classes <- length(unique(x))
  
  # Definindo rótulos
  if(qtd_classes == 6){
    
    labels <- c('A', 'B1', 'B2', 'C1', 'C2', 'DE')
    
  } else if(qtd_classes == 3){
    
    labels <- c('Alta', 'Baixa', 'Media')
    
  } else {
    
    stop("Numero inesperado de classes")
    
  }
  
  # Convertendo índices para rótulos
  predicao <- labels[x]
  
  # Matriz de confusão
  matriz_confusao_RN <- table(teste$CLASSE, predicao)
  
  metricas <- confusionMatrix(matriz_confusao_RN)
  
  return(list(
    modelo_rn = classificador_RN,
    matriz_de_confusao_rn = matriz_confusao_RN,
    metricas_rn = metricas
  ))
  
}
