# Aplicando Componentes Principais para reducao da dimensão ---------------
reduzir_com_cp = function(qtd_cp, peso){
  ind_class = which(names(dados_UF) == "CLASSE")
  dados_cp = dados_UF[,-ind_class] #removendo a coluna de classes sociais
  comp_princ = prcomp(dados_cp, scale = TRUE)
  
  #Visualizacao dos Resultados
  #summary(comp_princ)
  #x11();fviz_eig(comp_princ)
  #x11();fviz_contrib(comp_princ,choice = "var", axes = 1, top = 27)
  
  
  ## Selecionando as variaveis que sao relevantes para a componente ----------
  
  weights <- comp_princ$rotation # Extrair os pesos dos componentes principais
  nomes_variaveis <- colnames(dados_cp) # Obter os nomes das vari?veis originais
  pesos_cp <- data.frame(abs(weights[, 1])) # Ordenar os pesos da primeira componente principal
  #View(pesos_cp)
  nomes_variaveis <- colnames(dados_cp) # Obter os nomes das vari?veis originais
  
  
  ## Criando Dataframe com as vari?veis significativas dos componente --------
  
  # Inicializar uma lista para armazenar as vari?veis selecionadas para cada componente principal
  variaveis_contribuicao <- vector("list", length = qtd_cp)
  for (i in 1:qtd_cp) { # Loop sobre as componentes principais
    pesos_componente <- weights[, i] # Ordenar os pesos da componente principal atual
    variaveis_contribuicao[[i]] <- nomes_variaveis[abs(pesos_componente) > peso] # Identificar as vari?veis que contribuem significativamente para a componente principal atual
  }
  
  variaveis_contribuicao # Exibir as vari?veis selecionadas para cada componente principal
  nomes_variaveis_componentes <- unlist(variaveis_contribuicao[1:qtd_cp]) # Unir os nomes das vari?veis selecionadas para as componentes principais em um vetor
  nomes_variaveis_unicos <- unique(nomes_variaveis_componentes) # Remover valores duplicados
  nomes_variaveis_unicos # Exibir os nomes das vari?veis ?nicas
  dados_reduzidos = dados_UF[,nomes_variaveis_unicos]
  CLASSE = dados_UF$CLASSE
  dados_reduzidos = cbind(dados_reduzidos, CLASSE)
  
  return(dados_reduzidos)
}


## Escolha a Quantidade de Componentes Principais e a porcentagem  --------

dados_cp = reduzir_com_cp(1, 0.13) 
