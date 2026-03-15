# Analise Exploratoria dos Dados --------------------------------------------

# Funcao para grafico de dispersao da base de dados selecionada --------------
boxplot_dispersao = function(dados){
  x11();boxplot(dados, col=rainbow(ncol(dados)), 
                pch=16, ylim=c(-1, 20)); abline(h=c(-1,1), col="red", lty="dashed")
}

# Funcao para calcular todas as estatasticas para uma coluna -------------
calcular_estatisticas <- function(x) {
  c(Media = mean(x),
    Maximo = max(x),
    Mediana = median(x),
    `Desvio Padrao` = sd(x))
}

#Funcao para a tabela de estatisticas dos dados em latex ------------

estat_dados = function(dados){
estatisticas <- sapply(dados, calcular_estatisticas)
rownames(estatisticas) <- c("Media", "Maximo", "Mediana", "Desvio Padrao")
return(estatisticas)
}

# Grafico de Dispersao CP  --------------------------------

graficos_dispersao = function(dados){
  
dados_cp = dados[, !(names(dados) %in% "CLASSE")]
comp_princ = prcomp(dados_cp, scale = TRUE)
  
dados_cp6 = dados %>% mutate(CLASSE = case_when(
    CLASSE == 1 ~ "A",
    CLASSE == 2 ~ "B1",
    CLASSE == 3 ~ "B2",
    CLASSE == 4 ~ "C1",
    CLASSE == 5 ~ "C2",
    CLASSE == 6 ~ "DE",
    TRUE ~ as.character(CLASSE)
))

grupo6 <- as.factor(dados_cp6$CLASSE)

cores6 <- c("red", "blue", "darkgreen", "orange", "purple", "green")  # Defina as cores que voc? deseja atribuir a cada valor

x11();{plot(comp_princ$x[, 1], 
            comp_princ$x[, 2], 
            col = cores6[grupo6], 
            xlab = "Dim 1", 
            ylab = "Dim 2", 
            main = "Grafico de Dispersao com Divisao de 6 Classes")
  legend("bottomright", # topright para dados totais e bottomright para os originais
         legend = levels(grupo6), 
         col = cores6, 
         pch = 1, 
         title = "CLASSE")}

dados_cp3 = dados %>% mutate(CLASSE = case_when(
  CLASSE == 1 ~ "Alta",
  CLASSE == 2 ~ "Alta",
  CLASSE == 3 ~ "Media",
  CLASSE == 4 ~ "Media",
  CLASSE == 5 ~ "Media",
  CLASSE == 6 ~ "Baixa",
  TRUE ~ as.character(CLASSE)
))

grupo3 <- as.factor(dados_cp3$CLASSE)

cores3 <- c("red", "blue", "darkgreen")  

x11();{plot(comp_princ$x[, 1], 
            comp_princ$x[, 2], 
            col = cores3[grupo3], 
            xlab = "Dim 1", 
            ylab = "Dim 2", 
            main = "Grafico de Dispersao com Divisao de 3 Classes")
  legend("bottomright", # topright para dados totais e bottomright para os originais
         legend = levels(grupo3), 
         col = cores3, 
         pch = 1, 
         title = "CLASSE")}
}
