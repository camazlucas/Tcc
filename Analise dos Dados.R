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
estatisticas <- sapply(dados_RN, calcular_estatisticas)
rownames(estatisticas) <- c("Media", "Maximo", "Mediana", "Desvio Padrao")
return(estatisticas)
}