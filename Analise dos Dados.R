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

grafico_dispersao = function(dados){

dados_cp1 = dados %>% mutate(CLASSE = case_when(
    CLASSE == 1 ~ "A",
    CLASSE == 2 ~ "B1",
    CLASSE == 3 ~ "B2",
    CLASSE == 4 ~ "C1",
    CLASSE == 5 ~ "C2",
    CLASSE == 6 ~ "DE",
    TRUE ~ as.character(CLASSE)
))
dados_cp = dados[, !(names(dados) %in% "CLASSE")]
comp_princ = prcomp(dados_cp, scale = TRUE)

grupo <- as.factor(dados_cp1$CLASSE)

cores <- c("red", "blue", "darkgreen", "orange", "purple", "green")  # Defina as cores que voc? deseja atribuir a cada valor

x11();{plot(comp_princ$x[, 1], 
            comp_princ$x[, 2], 
            col = cores[grupo], 
            xlab = "Dim 1", 
            ylab = "Dim 2", 
            main = "Grafico de Dispersao")
  legend("bottomright", # topright para dados totais e bottomright para os originais
         legend = levels(grupo), 
         col = cores, 
         pch = 1, 
         title = "CLASSE")}
}

grafico_dispersao(dados_UF)
