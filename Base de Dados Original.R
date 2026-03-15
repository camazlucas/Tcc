# Selecionando dados padroes para a analise de classes --------------------
dados_originais = function(dados, UF){
  dados_orig = dados[, c(3, 7:22, 51)]
  
  summary(dados_orig)
  
  #Alterando dados do tipo character para dados numericos
  dados_orig$P3.1_3 = as.numeric(gsub(",", "", dados_orig$P3.1_3))
  dados_orig$P3.1_4 = as.numeric(gsub(",", "", dados_orig$P3.1_4))
  dados_orig$P3.1_5 = as.numeric(gsub(",", "", dados_orig$P3.1_5))
  dados_orig$P3.1_12 = as.numeric(gsub(",", "", dados_orig$P3.1_12))
  dados_orig[is.na(dados_orig)] = 0
  
  
  dados_orig = subset(dados_orig, P5.13 == 1)
  
  names(dados_orig) = c("UF",
                        "Automoveis", 
                        "Empregados", 
                        "Lava_Roupas", 
                        "Banheiros", 
                        "DVD", 
                        "Geladeiras", 
                        "Freezers", 
                        "Microcomputadores", 
                        "Lava_Loucas", 
                        "Microondas", 
                        "Motocicletas", 
                        "Seca_Roupas",
                        "Fonte_da_Agua",
                        "Rua",
                        "Instrucao",
                        "CLASSE")
  dados_orig = filtrar_estados(UF, dados_orig)
  dados_orig = dados_orig[,-17]
  
  return(dados_orig)
}


