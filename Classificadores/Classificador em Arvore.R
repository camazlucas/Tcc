# Classificacao em Arvore -------------------------------------------------
arvore_class = function(treino, teste){
  classificador = rpart(formula = CLASSE ~ ., data = treino, parms = list(split = 'information'))
  
  x11();rpart.plot(classificador, 
                   extra=102, 
                   cex=0.8)
  
  
  previsao = predict(classificador, teste[, !(names(teste) %in% "CLASSE")], type='class')
  #previsao
  
  matriz_confusao_arvore = table(teste$CLASSE, previsao)
  #matriz_confusao_arvore
  
  confusionMatrix(matriz_confusao_arvore)
  
  return(summary(classificador))
}