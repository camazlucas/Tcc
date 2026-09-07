# Classificacao em Arvore -------------------------------------------------
arvore_class = function(dados, qtd_de_classes){
  
  #Divisao em Treino e Teste
  divisao = divisao_dos_dados(dados, qtd_de_classes)
  treino = divisao$treino
  teste = divisao$teste
  
  
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
