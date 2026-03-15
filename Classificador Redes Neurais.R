# Ajustando rede neural---------------------------------------------------
rn_class = function(treino, teste)
  {classificador_RN = neuralnet(CLASSE ~ ., 
                              treino,  
                              linear.output = TRUE, 
                              #rep = 1,
                              learningrate = 0.01,
                              #algorithm = "backprop",
                              hidden = 0, 
                              act.fct = "logistic")


prev <- compute(classificador_RN, teste[, !(names(teste) %in% "CLASSE")])$net.result
resultado = as.data.frame(prev)

# Criando funcao para rotulacao e comparacao ------------------------------

func = function(x) {
  return(which(x == max(x)))
}
x = apply(prev, c(1), func)

qtd_classes = length(unique(x))

if (qtd_classes == 6){
  predicao_6 = c('A', 'B1', 'B2', 'C1', 'C2', 'DE')[x]
  matriz_confusao_6classes = table(teste$CLASSE, predicao_6)
  confusionMatrix(matriz_confusao_6classes)
  matriz_confusao_RN = matriz_confusao_6classes
}else if(qtd_classes == 3){
  predicao_3 = c('Alta', 'Baixa', 'Media')[x]
  matriz_confusao_3classes = table(teste$CLASSE, predicao_3)
  confusionMatrix(matriz_confusao_3classes)
  matriz_confusao_RN = matriz_confusao_3classes
}

confusionMatrix(matriz_confusao_RN)
}



# Plots de Resultados da Rede Neural --------------------------------------

#Arquitetura da Rede
x11();{plot(classificador_RN, show.weights = FALSE)}


#Taxa de Aprendizagem
x11();plot(classificador_RN$result.matrix[, 1], 
           type = "l", 
           xlab = "Itera??o", 
           ylab = "Erro",
           xlim = c(1, 200))

print(classificador_RN)