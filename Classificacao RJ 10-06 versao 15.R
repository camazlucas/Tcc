###############Lucas Camaz Ferreira######################################
###############Script Classificacao Monografia###########################


# Ajustando rede neural---------------------------------------------------
{
  
  
classificador_RN = neuralnet(CLASSE ~ ., 
                             treino,  
                             linear.output = TRUE, 
                             #rep = 1,
                             learningrate = 0.01,
                             #algorithm = "backprop",
                             hidden = 0, 
                             act.fct = "logistic")


prev <- compute(classificador_RN, teste[,-indice_classe])$net.result
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
}else if(qtd_classes == 4){
  predicao_4 = c('A', 'B', 'C','DE')[x]
  matriz_confusao_4classes = table(teste$CLASSE, predicao_4)
  confusionMatrix(matriz_confusao_4classes)
  matriz_confusao_RN = matriz_confusao_4classes
}else if(qtd_classes == 3){
  predicao_3 = c('Alta', 'Baixa', 'Media')[x]
  matriz_confusao_3classes = table(teste$CLASSE, predicao_3)
  confusionMatrix(matriz_confusao_3classes)
  matriz_confusao_RN = matriz_confusao_3classes
}else{
#predicao_2 = c('MediaAlta', 'MediaBaixa')[x]
#matriz_confusao_2classes = table(teste$CLASSE, predicao_2)
#confusionMatrix(matriz_confusao_2classes)
#matriz_confusao_RN = matriz_confusao_2classes
}

confusionMatrix(matriz_confusao_RN)
}



# Plots de Resultados da Rede Neural --------------------------------------

#Arquitetura da Rede
x11();{plot(classificador_RN, show.weights = FALSE)}


#Taxa de Aprendizagem
x11();plot(classificador_RN$result.matrix[, 1], 
     type = "l", 
     xlab = "Iteração", 
     ylab = "Erro",
     xlim = c(1, 200))

print(classificador_RN)






# Classificacao em Arvore -------------------------------------------------
{
classificador = rpart(formula = CLASSE ~ ., data = treino, parms = list(split = 'information'))

x11();rpart.plot(classificador, 
                 extra=102, 
                 cex=0.8)
                 

previsao = predict(classificador, teste[,-indice_classe], type='class')
#previsao

matriz_confusao_arvore = table(teste$CLASSE, previsao)
#matriz_confusao_arvore

confusionMatrix(matriz_confusao_arvore)

summary(classificador)
}



# Maquinas de Vetor de Suporte --------------------------------------------
{
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

prev_svm = predict(classif, teste_svm[,-indice_classe])

matriz_confusao_svm = table(teste_svm$CLASSE, prev_svm)
#matriz_consufsao_svm

confusionMatrix(matriz_confusao_svm)

summary(classif)
}

#Testando cost por validacao cruzada
set.seed(1)
tune.out = tune(svm, 
                CLASSE ~ .,
                data = treino_svm,
                kernel = "linear",
                ranges = list(cost=c(0.001, 0.01, 0.1, 1, 10)))

summary(tune.out)




# Comparacao entre os metodos ---------------------------------------------

classes_RN = confusionMatrix(matriz_confusao_RN)$overall
classes_Arvore = confusionMatrix(matriz_confusao_arvore)$overall
classes_svm = confusionMatrix(matriz_confusao_svm)$overall

tabela_comparacao = data.frame(classes_RN, classes_Arvore, classes_svm)
tabela_comparacao

tabela_comparacao_total = tabela_comparacao
tabela_comparacao_reduzido = tabela_comparacao
tabela_comparacao_total
tabela_comparacao_reduzido

# Comparacao entre a quantidade de Classes Redes Neurais--------------------------------


classes6 = confusionMatrix(matriz_confusao_6classes)$overall
classes4 = confusionMatrix(matriz_confusao_4classes)$overall
classes3 = confusionMatrix(matriz_confusao_3classes)$overall
classes2 = confusionMatrix(matriz_confusao_2classes)$overall

tabela_RN = data.frame(classes6, classes4, classes3, classes2)
tabela_RN
