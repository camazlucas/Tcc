# Filtrar dados por estado ----------------------------------------------
filtrar_estados <- function(uf){
  # Filtrar UF
  dados_filtrados = tratamento_dos_dados(dados)
  
  dados_estado <- subset(dados_filtrados, UF == uf)
  
  dados_estado <- dados_estado[, !(names(dados_estado) %in% "UF")]
  
  return(dados_estado)
}

dados_UF = filtrar_estados("RJ")

