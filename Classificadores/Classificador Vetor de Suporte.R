# Maquinas de Vetor de Suporte --------------------------------------------
svm_class = function(dados, qtd_de_classes){
  
  #Divisao em Treino e Teste
  split = divisao_das_classes(qtd_de_classes, dados)
  
  treino = split$treino
  teste = split$teste
  
  
  treino_svm = treino
  valores_unicos <- unique(treino_svm$CLASSE)
  valores_unicos_ordenados <- sort(valores_unicos)
  treino_svm$CLASSE <- factor(treino_svm$CLASSE, levels = valores_unicos_ordenados)
  treino_svm$CLASSE <- as.integer(as.factor(treino_svm$CLASSE))
  
  teste_svm = teste
  valores_unicos <- unique(teste_svm$CLASSE)
  valores_unicos_ordenados <- sort(valores_unicos)
  teste_svm$CLASSE <- factor(teste_svm$CLASSE, levels = valores_unicos_ordenados)
  teste_svm$CLASSE <- as.integer(as.factor(teste_svm$CLASSE))
  
  classif = svm(formula = CLASSE ~ ., 
                data = treino_svm,
                type = 'C-classification',
                cost = 0.1,
                kernel = 'linear')
  
  prev_svm = predict(classif, teste[, !(names(teste) %in% "CLASSE")])
  
  matriz_confusao_svm = table(teste_svm$CLASSE, prev_svm)
  #matriz_consufsao_svm
  
  mc_svm = confusionMatrix(matriz_confusao_svm)
  
  return(list(
    matriz_de_confusao_svm = mc_svm,
    summary_svm = summary(classif)
    ))
}
