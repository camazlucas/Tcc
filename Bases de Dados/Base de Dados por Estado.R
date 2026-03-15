# Filtrar dados por estado ----------------------------------------------
filtrar_estados <- function(uf, dados_filtrados){
  # Filtrar UF
  
  dados_estado <- subset(dados_filtrados, UF == uf)
  
  dados_estado <- dados_estado[, !(names(dados_estado) %in% "UF")]
  
  return(dados_estado)
}

