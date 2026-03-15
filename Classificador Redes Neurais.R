# Ajustando rede neural---------------------------------------------------
rn_class <- function(treino, teste){
  
  # Treinando a rede neural
  classificador_RN <- neuralnet(
    CLASSE ~ .,
    treino,
    rep = 1,
    linear.output = FALSE,
    learningrate = 0.01,
    hidden = 12,
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



# Plots de Resultados da Rede Neural --------------------------------------

rn_class(treino, teste)

#Arquitetura da Rede
x11();{plot(classificador_RN, show.weights = FALSE)}


#Taxa de Aprendizagem
x11();plot(classificador_RN$result.matrix[, 1], 
           type = "l", 
           xlab = "Itera??o", 
           ylab = "Erro",
           xlim = c(1, 200))

print(classificador_RN)