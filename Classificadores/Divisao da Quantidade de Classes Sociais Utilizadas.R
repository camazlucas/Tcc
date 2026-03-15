# Definindo a quantidade de Classes Sociais Utilizadas --------------------
divisao_das_classes = function(x, dados_RN){
if (x == 6) {
  ## Classificacao em 6 classes ----------------------------------------------
  ### Removendo valor numerico das Classes ------------------------------------
  
  dados_UF_6classes = dados_RN %>% mutate(CLASSE = case_when(
    CLASSE == 1 ~ "A",
    CLASSE == 2 ~ "B1",
    CLASSE == 3 ~ "B2",
    CLASSE == 4 ~ "C1",
    CLASSE == 5 ~ "C2",
    CLASSE == 6 ~ "DE",
    TRUE ~ as.character(CLASSE)
  ))
  
  
  ### Particao de 70% dos dados -----------------------------------------------
  
  particao_6 = createDataPartition(1:nrow(dados_UF_6classes),p=0.7)
  dataset_treino_6 = dados_UF_6classes[particao_6$Resample1,]
  dataset_teste_6 = dados_UF_6classes[- particao_6$Resample1,]

  treino = dataset_treino_6
  teste = dataset_teste_6
  
}
  
else if (x == 3) {  
  ## Classificando em Tres Classes ------------------------------------
  
  ### Removendo valor numerico das Classes ------------------------------------
  
  dados_UF_3classes = dados_RN %>% mutate(CLASSE = case_when(
    CLASSE == 1 ~ "Alta",
    CLASSE == 2 ~ "Alta",
    CLASSE == 3 ~ "Media",
    CLASSE == 4 ~ "Media",
    CLASSE == 5 ~ "Media",
    CLASSE == 6 ~ "Baixa",
    TRUE ~ as.character(CLASSE)
  ))
  
  
  ### Redimensionando dados proporcionalmente ---------------------------------
  
  # Contar o n?mero de elementos em cada classe
  contagem_classes <- dados_UF_3classes %>% 
    group_by(CLASSE) %>% 
    summarise(contagem = n())
  
  # Encontrar a menor contagem
  tamanho_minimo <- min(contagem_classes$contagem)
  
  # Amostrar aleatoriamente elementos de cada grupo para igualar ao tamanho m?nimo
  set.seed(42)  # Para reprodutibilidade
  
  dados_bal <- dados_UF_3classes %>% 
    group_by(CLASSE) %>% 
    sample_n(tamanho_minimo) %>% 
    ungroup()
  
  dados_UF_3classes = dados_bal
  
  
  ### Particao de 70% dos dados -----------------------------------------------
  
  particao_3 = createDataPartition(1:nrow(dados_UF_3classes),p=0.7)
  dataset_treino_3 = dados_UF_3classes[particao_3$Resample1,]
  dataset_teste_3 = dados_UF_3classes[- particao_3$Resample1,]

  treino = dataset_treino_3
  teste = dataset_teste_3
}
else {
  stop("Selecione 3 ou 6 classes")
}
  
  return(list(treino = treino, teste = teste))

}
