#' Trata a base bruta da PPH 2019
#'
#' Seleciona as colunas de interesse, renomeia para nomes legiveis, descarta
#' domicilios com atividade comercial e remove variaveis redundantes.
#'
#' @param dados Base bruta da PPH 2019, como lida do CSV.
#'
#' @return Data frame com `UF`, as variaveis de posse de equipamentos e
#'   `CLASSE`.
tratamento_dos_dados <- function(dados) {
  colunas_de_interesse <- c(
    3, 24, 51, 9, 12, 13, 14, 15, 16, 18, 1429, 1437, 1445, 1506, 1538,
    1545, 1579, 1582, 1586, 1589, 1592, 1595, 1598, 1601, 1604, 1607, 1610,
    1613, 1616, 1619, 1622, 1625, 1628, 1632, 1635, 1638, 1642, 1646, 1650,
    1654, 1658, 1662, 1666, 1670, 1674, 1677, 1680, 1683, 1686, 1689, 1692,
    1695, 1698, 1701, 1704, 1707, 1736, 1765, 1793, 1821, 1849, 1878, 1907,
    1936, 1964, 1993, 2022, 2051, 2080, 2109, 2138, 2182, 2183, 22
  )
  dados_select <- dados[, colunas_de_interesse]

  # Algumas colunas vem como texto com separador de milhar e precisam virar
  # numero antes de qualquer conta.
  dados_select$P3.1_4 <- as.numeric(gsub(",", "", dados_select$P3.1_4))
  dados_select$P3.1_5 <- as.numeric(gsub(",", "", dados_select$P3.1_5))
  dados_select$P3.1_12 <- as.numeric(gsub(",", "", dados_select$P3.1_12))
  dados_select[is.na(dados_select)] <- 0

  nomesvariaveis <- c(
    "UF",
    "Qtd_Moradores",
    "Comercio",
    "Maquina_de_Lavar",
    "Geladeiras",
    "Freezer",
    "Microcomputador",
    "Lava_Loucas",
    "Microondas",
    "Secadora_de_Roupa",
    "Geladeiras",
    "Freezer",
    "Ar_Condicionado",
    "Televisao",
    "Microondas",
    "Maquina_de_Lavar",
    "Batedeira",
    "Cafereira",
    "Sanduicheira",
    "Espremedor",
    "Liquidificador",
    "Multiprocessador",
    "Panela_Eletrica",
    "Triturador_de_Lixo",
    "Faca_Eletrica",
    "Ebulidor",
    "Fogao_Eletrico",
    "Fritadeira_com_Oleo",
    "Fritadeira_sem_Oleo",
    "Enceradeira",
    "Aspirador_de_Po",
    "Panificadora",
    "DVD",
    "Tablet",
    "Celular",
    "Telefone_sem_Fio",
    "Fax",
    "Modem_Wifi",
    "Roteador_WIFI",
    "Impressora",
    "Receptor_de_TV",
    "Conversor_Digital",
    "Receptor_Digital",
    "NoBreak",
    "Serra_Eletrica",
    "Maquina_de_Solda",
    "Furadeira",
    "Portao_Eletronico",
    "Projetores",
    "Lava_Jato",
    "Filtro_de_Piscina",
    "Bomba_Dagua",
    "Maquina_de_Costura",
    "Chapinha",
    "Secador_de_Cabelo",
    "Forno_Eletrico",
    "Lava_Loucas",
    "Ferro_Eletrico_Seco",
    "Ferro_Eletrico_Vapor",
    "Ferro_Eletrico_sem_Vapor",
    "Secadora_Aquecimento",
    "Secadora_Centrifuga",
    "Aquecedor_de_Ambiente",
    "Ventilador_de_Teto",
    "Circulador_de_Ar",
    "Videogame",
    "Notebook",
    "Som_Radio",
    "Computador",
    "Filtro_de_Agua",
    "Adega",
    "Chuveiros",
    "Aquecimento_Chuveiro",
    "CLASSE"
  )
  names(dados_select) <- nomesvariaveis

  # Mantem apenas residencias sem atividade comercial
  dados_select <- subset(dados_select, Comercio == 1)
  dados_select <- dados_select[, !(names(dados_select) %in% "Comercio")]

  # ATENCAO: nomesvariaveis tem nomes repetidos de proposito (a base traz a
  # mesma variavel em blocos diferentes do questionario). A deduplicacao
  # acontece porque setdiff() devolve valores unicos, entao a selecao abaixo
  # fica com a PRIMEIRA ocorrencia de cada nome repetido. O filtro de
  # sufixos existe para o caso de o R ter desambiguado nomes com ".1".
  colunas_com_sufixos <- names(dados_select)[
    grepl("\\.\\d+$", names(dados_select))
  ]
  colunas_sem_sufixos <- setdiff(names(dados_select), colunas_com_sufixos)
  dados_select <- dados_select[, colunas_sem_sufixos]

  # Um chuveiro so conta como eletrico se o aquecimento for eletrico
  dados_select$Chuveiros_Eletricos <- ifelse(
    dados_select$Aquecimento_Chuveiro == 1,
    dados_select$Chuveiros,
    0
  )

  # Computador e Notebook, e Lava_Loucas e Secadora_de_Roupa, sao redundantes
  # com Microcomputador e com a lava-e-seca ja contabilizada.
  redundantes <- c(
    "Computador",
    "Notebook",
    "Secadora_de_Roupa",
    "Lava_Loucas",
    "Aquecimento_Chuveiro",
    "Chuveiros"
  )
  dados_select <- dados_select[, !(names(dados_select) %in% redundantes)]

  dados_select
}
