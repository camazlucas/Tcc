divisao_dos_dados = function(dados, qtd_de_classes){
  
  dados_com_classe = divisao_das_classes(dados, qtd_de_classes)
  
  particao = createDataPartition(1:nrow(dados_com_classe),p=0.7)
  treino = dados_com_classe[particao$Resample1,]
  teste = dados_com_classe[- particao$Resample1,]

return(list(treino = treino, 
            teste = teste))
}
