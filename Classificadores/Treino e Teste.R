# Normalizacao min-max ajustada apenas no treino -------------------------
# Os minimos e maximos vem do treino e sao aplicados tambem no teste, para
# nao vazar informacao do conjunto de teste para o modelo.
normalizar_treino_teste = function(treino, teste){

  preditoras = setdiff(names(treino), "CLASSE")

  minimos = sapply(treino[preditoras], min)
  maximos = sapply(treino[preditoras], max)
  amplitude = maximos - minimos
  amplitude[amplitude == 0] = 1 # colunas constantes ficam em zero apos o centro

  escalar = function(dados){
    dados[preditoras] = as.data.frame(scale(dados[preditoras],
                                            center = minimos,
                                            scale = amplitude))
    dados
  }

  return(list(treino = escalar(treino),
              teste = escalar(teste)))
}

divisao_dos_dados = function(dados, qtd_de_classes, normalizar = FALSE){

  dados_com_classe = divisao_das_classes(dados, qtd_de_classes)

  # divisao_das_classes devolve tibble quando ha reamostragem por grupo
  dados_com_classe = as.data.frame(dados_com_classe)

  particao = createDataPartition(1:nrow(dados_com_classe),p=0.7)
  treino = dados_com_classe[particao$Resample1,]
  teste = dados_com_classe[- particao$Resample1,]

  if (normalizar) {
    escalados = normalizar_treino_teste(treino, teste)
    treino = escalados$treino
    teste = escalados$teste
  }

return(list(treino = treino,
            teste = teste))
}
