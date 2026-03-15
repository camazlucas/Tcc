###############Lucas Camaz Ferreira######################################
###############Script Classificacao Monografia###########################

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
classes3 = confusionMatrix(matriz_confusao_3classes)$overall

tabela_RN = data.frame(classes6, classes4, classes3, classes2)
tabela_RN
