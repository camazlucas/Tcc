# Classificacao em Arvore -------------------------------------------------
arvore_class = function(dados, c){
  
  #Divisao em Treino e Teste
  split = divisao_das_classes(c, dados)
  
  treino = split$treino
  teste = split$teste
  
  
  classificador = rpart(formula = CLASSE ~ ., data = treino, parms = list(split = 'information'))
  
  x11();rpart.plot(classificador, 
                   extra=102, 
                   cex=0.8)
  
  
  previsao = predict(classificador, teste[, !(names(teste) %in% "CLASSE")], type='class')
  #previsao
  
  matriz_confusao_arvore = table(teste$CLASSE, previsao)
  #matriz_confusao_arvore
  
  matriz_confusao = confusionMatrix(matriz_confusao_arvore)
  
  return(matriz_confusao)
}
