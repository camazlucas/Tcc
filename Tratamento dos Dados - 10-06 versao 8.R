###############Lucas Camaz Ferreira######################################
###############Script Classificacao Monografia###########################


# Pacotes Utilizados ------------------------------------------------------

#Todos
library(caret)
library(dplyr)
library(factoextra)

#Redes Neurais

library(neuralnet)

#Arvore de Classificacao
library(arules)
library(caTools)
library(rpart)
library(rpart.plot)

#Maquina de Vetor de Suporte
library(e1071)
library(ggplot2)


# Limpeza de Memoria ------------------------------------------------------

rm(list=ls(all=TRUE)) # Limpar a memÃ³ria

# Dados -------------------------------------------------------------------

setwd("G:/Meu Drive/Rural/IC/Monografia/Dados")
setwd("C:/Users/olive/OneDrive/Documentos/Rural/Lucas")
dados = read.csv2("PPH 2019 - Banco de Dados V2.csv")


# Filtragem dos Dados por Estado -----------------------------------------------------

dataframes_estados <- split(dados, dados$UF)

## Escolha o estado de estudo ----------------------------------------------
dadosUF = dataframes_estados$"RJ"
dadosUF[is.na(dadosUF)] = 0


# Selecionados dados Quantitativos de Eletrodomesticos --------------------
{
dados_UF = dadosUF[, c(24, 51, 9, 12, 13, 14, 15, 16, 18,  1429, 1437, 1445, 1506, 1538, 1545, 1579, 1582, 1586, 1589, 1592, 1595, 1598, 1601, 1604, 1607, 1610, 1613, 1616, 1619, 1622, 1625, 1628, 1632, 1635, 1638, 1642, 1646, 1650, 1654, 1658, 1662, 1666, 1670, 1674, 1677, 1680, 1683, 1686, 1689, 1692, 1695, 1698, 1701, 1704, 1707, 1736, 1765, 1793, 1821, 1849, 1878, 1907, 1936, 1964, 1993, 2022, 2051, 2080, 2109, 2138, 2182, 2183, 22)]


#Alguns dados estao sendo lidos como character, por isso tem de ser convertidos para valores numericos.

summary(dados_UF)
dados_UF$P3.1_4 = as.numeric(gsub(",", "", dados_UF$P3.1_4))
dados_UF$P3.1_5 = as.numeric(gsub(",", "", dados_UF$P3.1_5))
dados_UF$P3.1_12 = as.numeric(gsub(",", "", dados_UF$P3.1_12))
dados_UF[is.na(dados_UF)] = 0

summary(dados_UF)

nomesvariaveis = c("Qtd_Moradores", "Comercio","Maquina_de_Lavar", "Geladeiras", "Freezer", "Microcomputador", "Lava_Loucas", "Microondas", "Secadora_de_Roupa", "Geladeiras", "Freezer", "Ar_Condicionado", "Televisao", "Microondas", "Maquina_de_Lavar", "Batedeira", "Cafereira", "Sanduicheira", "Espremedor", "Liquidificador", "Multiprocessador", "Panela_Eletrica", "Triturador_de_Lixo", "Faca_Eletrica", "Ebulidor", "Fogao_Eletrico", "Fritadeira_com_Oleo", "Fritadeira_sem_Oleo", "Enceradeira", "Aspirador_de_Po", "Panificadora", "DVD", "Tablet", "Celular", "Telefone_sem_Fio", "Fax", "Modem_Wifi", "Roteador_WIFI", "Impressora", "Receptor_de_TV", "Conversor_Digital", "Receptor_Digital", "NoBreak", "Serra_Eletrica", "Maquina_de_Solda", "Furadeira", "Portao_Eletronico", "Projetores", "Lava_Jato", "Filtro_de_Piscina", "Bomba_Dagua", "Maquina_de_Costura", "Chapinha", "Secador_de_Cabelo", "Forno_Eletrico", "Lava_Loucas", "Ferro_Eletrico_Seco", "Ferro_Eletrico_Vapor", "Ferro_Eletrico_sem_Vapor", "Secadora_Aquecimento", "Secadora_Centrifuga", "Aquecedor_de_Ambiente", "Ventilador_de_Teto", "Circulador_de_Ar", "Videogame", "Notebook", "Som_Radio", "Computador", "Filtro_de_Agua", "Adega", "Chuveiros","Aquecimento_Chuveiro", "CLASSE")
names(dados_UF) = nomesvariaveis


## Removendo Residencias que possuem atividade comercial -------------------

dados_UF = subset(dados_UF, Comercio == 1)
dados_UF = dados_UF[,-2] #Removendo a coluna de comercio apos a selecao


## Removendo dados Duplicados ----------------------------------------------

colunas_com_sufixos <- names(dados_UF)[grepl("\\.\\d+$", names(dados_UF))] # Identificar as colunas que tem sufixos adicionados
colunas_sem_sufixos <- setdiff(names(dados_UF), colunas_com_sufixos) # Selecionar apenas as colunas que nao tem sufixos
dados_UF <- dados_UF[, colunas_sem_sufixos] # Criar um novo dataframe apenas com as colunas que nao tem sufixos
dados_UF$Chuveiros_Eletricos <- ifelse(dados_UF$Aquecimento_Chuveiro == 1, dados_UF$Chuveiros, 0) #Identificando Chuveiros Eletricos
identificando_duplicados  = sort(names(dados_UF)) #Identificando colunas duplicadas
identificando_duplicados


#Removendo Computador, Notebook, Lava Loucas, Secadora de Roupa (considerando lava e seca) e as Colunas do Chuveiro

dados_UF <- dados_UF[, !(names(dados_UF) %in% c("Computador", 
                                                "Notebook", 
                                                "Secadora_de_Roupa", 
                                                "Lava_Loucas", 
                                                "Aquecimento_Chuveiro", 
                                                "Chuveiros"))]

#dados_RN = dados_UF

}
# Aplicando Componentes Principais para reducao da dimensÃ£o ---------------
{
ind_class = which(names(dados_UF) == "CLASSE")
dados_cp = dados_UF[,-ind_class] #removendo a coluna de classes sociais
comp_princ = prcomp(dados_cp, scale = TRUE)

#Visualização dos Resultados
#summary(comp_princ)
#x11();fviz_eig(comp_princ)
#x11();fviz_contrib(comp_princ,choice = "var", axes = 1, top = 27)


## Selecionando as variaveis que sao relevantes para a componente ----------

weights <- comp_princ$rotation # Extrair os pesos dos componentes principais
nomes_variaveis <- colnames(dados_cp) # Obter os nomes das variáveis originais
pesos_cp <- data.frame(abs(weights[, 1])) # Ordenar os pesos da primeira componente principal
#View(pesos_cp)
nomes_variaveis <- colnames(dados_cp) # Obter os nomes das variáveis originais

## Escolha a Quantidade de Componentes Principais e a porcentagem  --------

Quantidade_CP = 1
peso_considerado = 0.13


## Criando Dataframe com as variáveis significativas dos componente --------

# Inicializar uma lista para armazenar as variáveis selecionadas para cada componente principal
variaveis_contribuicao <- vector("list", length = Quantidade_CP)
for (i in 1:Quantidade_CP) { # Loop sobre as componentes principais
  pesos_componente <- weights[, i] # Ordenar os pesos da componente principal atual
  variaveis_contribuicao[[i]] <- nomes_variaveis[abs(pesos_componente) > peso_considerado] # Identificar as variáveis que contribuem significativamente para a componente principal atual
}

variaveis_contribuicao # Exibir as variáveis selecionadas para cada componente principal
nomes_variaveis_componentes <- unlist(variaveis_contribuicao[1:Quantidade_CP]) # Unir os nomes das variáveis selecionadas para as componentes principais em um vetor
nomes_variaveis_unicos <- unique(nomes_variaveis_componentes) # Remover valores duplicados
nomes_variaveis_unicos # Exibir os nomes das variáveis únicas
dados_reduzidos = dados_UF[,nomes_variaveis_unicos]
CLASSE = dados_UF$CLASSE
dados_reduzidos = cbind(dados_reduzidos, CLASSE)
}

# Selecionando dados padroes para a analise de classes --------------------
{
dados_orig = dadosUF[, c(7:22, 51)]

summary(dados_orig)

#Alterando dados do tipo character para dados numericos
dados_orig$P3.1_3 = as.numeric(gsub(",", "", dados_orig$P3.1_3))
dados_orig$P3.1_4 = as.numeric(gsub(",", "", dados_orig$P3.1_4))
dados_orig$P3.1_5 = as.numeric(gsub(",", "", dados_orig$P3.1_5))
dados_orig$P3.1_12 = as.numeric(gsub(",", "", dados_orig$P3.1_12))
dados_orig[is.na(dados_orig)] = 0


dados_orig = subset(dados_orig, P5.13 == 1)

names(dados_orig) = c("Automoveis", 
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

dados_orig = dados_orig[,-17]
}

# Escolha: Dataframe com todos os Dados, com base de dados reduzido ou com os dados originais --------

dim(dados_UF)
#dim(dados_reduzidos)
dim(dados_orig)

###############Dados Sobre Eletrodomesticos##############
dados_RN = dados_UF

###############Dados Reduzidos por Componentes Principais##############
#dados_RN = dados_reduzidos

###############Dados Originais da Pesquisa para a Classificacao##############
dados_RN = dados_orig


# Definindo a quantidade de Classes Sociais Utilizadas --------------------
{
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


indice_classe6 = which(names(dataset_treino_6) == "CLASSE")


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

# Contar o número de elementos em cada classe
contagem_classes <- dados_UF_3classes %>% 
  group_by(CLASSE) %>% 
  summarise(contagem = n())

# Encontrar a menor contagem
tamanho_minimo <- min(contagem_classes$contagem)

# Amostrar aleatoriamente elementos de cada grupo para igualar ao tamanho mínimo
set.seed(123)  # Para reprodutibilidade

dados_bal <- dados_UF_3classes %>% 
  group_by(CLASSE) %>% 
  sample_n(tamanho_minimo) %>% 
  ungroup()

dados_UF_3classes = dados_bal


### Particao de 70% dos dados -----------------------------------------------

particao_3 = createDataPartition(1:nrow(dados_UF_3classes),p=0.7)
dataset_treino_3 = dados_UF_3classes[particao_3$Resample1,]
dataset_teste_3 = dados_UF_3classes[- particao_3$Resample1,]

indice_classe3 = which(names(dataset_treino_3) == "CLASSE")
}


# Escolha: a quantidade de classes sociais ----------------------------

###############Padrao 6 classes sociais##############
{
treino = dataset_treino_6
teste = dataset_teste_6
indice_classe = indice_classe6
}

############### 3 classes sociais##############
{
treino = dataset_treino_3
teste = dataset_teste_3
indice_classe = indice_classe3
}

# Escolher Dataframe com padronizacao dos dados ---------------------------
{
  indice_classeUF = which(names(dados_UF) == "CLASSE")
  indice_classeRD = which(names(dados_reduzidos) == "CLASSE")
  indice_classeOG = which(names(dados_orig) == "CLASSE")
  
  dados_RN_padr_UF = data.frame(scale(dados_UF[,-indice_classeUF]))
  dados_RN = cbind(dados_RN_padr_UF, dados_UF[,indice_classeUF])
  
  dados_RN_padr_RD = data.frame(scale(dados_reduzidos[,-indice_classeRD]))
  dados_RN = cbind(dados_RN_padr_RD, dados_reduzidos[,indice_classeRD])
  
  dados_RN_padr_OG = data.frame(scale(dados_orig[,-indice_classeOG]))
  dados_RN = cbind(dados_RN_padr_OG, dados_orig[,indice_classeOG])
  
  names(dados_RN)[ncol(dados_RN)] <- "CLASSE"
}

# Análise Exploratória dos Dados ------------------------------------------
{
  x11();boxplot(dados_UF, col=rainbow(ncol(dados_UF)), 
                pch=16, ylim=c(-1, 20)); abline(h=c(-1,1), col="red", lty="dashed")
  
  
  x11();boxplot(dados_RN[,-(ncol(dados_RN))], col=rainbow(ncol(dados_RN)), 
                pch=16, ylim=c(-4, 20)); abline(h=c(-1,1), col="red", lty="dashed")
}

# Função para calcular todas as estatísticas para uma coluna
calcular_estatisticas <- function(x) {
  c(Média = mean(x),
    Máximo = max(x),
    Mediana = median(x),
    `Desvio Padrão` = sd(x))
}

# Aplicar a função para cada coluna e armazenar os resultados em um dataframe
estatisticas <- sapply(dados_RN, calcular_estatisticas)

# Adicionar os nomes das linhas
rownames(estatisticas) <- c("Média", "Máximo", "Mediana", "Desvio Padrão")

#Resultados das Estatisticas para o Latex
xtable(estatisticas)

xtable(t(estatisticas))

# Grafico de Dispersao CP  --------------------------------

dados_cp = dados_UF[,-61] #removendo a coluna de classes sociais

dados_cp = dados_orig[,-17] #Utilizando os dados originais da classificacao

comp_princ = prcomp(dados_cp, scale = TRUE)

grupo <- as.factor(dados_UF_6classes$CLASSE)

cores <- c("red", "blue", "darkgreen", "orange", "purple", "green")  # Defina as cores que você deseja atribuir a cada valor

x11();{plot(comp_princ$x[, 1], 
            comp_princ$x[, 2], 
            col = cores[grupo], 
            xlab = "Dim 1", 
            ylab = "Dim 2", 
            main = "Gráfico de Dispersão")
        legend("bottomright", # topright para dados totais e bottomright para os originais
            legend = levels(grupo), 
            col = cores, 
            pch = 1, 
            title = "CLASSE")
}



dados_cp = dados_UF_3classes[,-61] #removendo a coluna de classes sociais

comp_princ = prcomp(dados_cp, scale = FALSE)

grupo <- as.factor(dados_UF_3classes$CLASSE)

cores <- c("blue", "darkgreen","red" )  # Defina as cores que você deseja atribuir a cada valor

x11();{plot(comp_princ$x[, 1],
           comp_princ$x[, 2], 
           col = cores[grupo], 
           xlab = "Dim 1", 
           ylab = "Dim 2", 
           main = "Gráfico de Dispersão")
        legend("topright", 
            legend = levels(grupo), 
            col = cores, 
            pch = 1, 
            title = "CLASSE")
        }





# o primeiro componente principal representa o tamanho dos pardais
escores1 = comp_princ$x[, 1]
names(escores1) = dados_UF
ordem = order(escores1, decreasing = TRUE)
x11()
barplot(escores1[ordem], ylab = "Escore do CP1", las = 2)
box()