# Classificacao do perfil socioeconomico a partir da posse de equipamentos
#
# Compara tres classificadores sobre tres bases derivadas da PPH 2019, com
# as classes agrupadas em 6 estratos do Criterio Brasil ou em 3 faixas.
#
# Funciona nos dois modos:
#   - no RStudio, os graficos abrem em janelas
#   - via Rscript, os graficos vao para output/figures/ em PNG
#
#   Rscript analysis/01-classificacao.R

rm(list = ls(all = TRUE))

library(here)
library(caret)
library(dplyr)
library(factoextra)
library(xtable)
library(neuralnet)
library(arules)
library(caTools)
library(rpart)
library(rpart.plot)
library(e1071)
library(ggplot2)

# Saidas ------------------------------------------------------------------

pasta_figuras <- here("output", "figures")
pasta_tabelas <- here("output", "tables")
dir.create(pasta_figuras, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_tabelas, recursive = TRUE, showWarnings = FALSE)

# Graficos numerados na ordem em que aparecem; sobras de execucoes
# anteriores se misturariam com as novas.
unlink(list.files(pasta_figuras, pattern = "\\.png$", full.names = TRUE))

# Modo grafico ------------------------------------------------------------
# As funcoes de R/ chamam x11(). Sem interface grafica, x11() e substituido
# por um dispositivo PNG, de modo que os mesmos scripts servem aos dois modos.

interativo <- interactive() && capabilities("X11") || .Platform$GUI == "Rgui"

.contador_grafico <- 0
.rotulo_grafico <- "grafico"

if (!interativo) {
  x11 <- function(...) {
    if (grDevices::dev.cur() > 1) try(grDevices::dev.off(), silent = TRUE)
    .contador_grafico <<- .contador_grafico + 1
    grDevices::png(
      file.path(
        pasta_figuras,
        sprintf("%02d-%s.png", .contador_grafico, .rotulo_grafico)
      ),
      width = 1400, height = 900, res = 120
    )
  }
}

secao <- function(rotulo) {
  .rotulo_grafico <<- rotulo
}

fechar_grafico <- function() {
  if (!interativo && grDevices::dev.cur() > 1) {
    try(grDevices::dev.off(), silent = TRUE)
  }
  # dev.off() devolve o dispositivo atual; sem isto o "null device" aparece
  # no relatorio a cada grafico fechado.
  invisible(NULL)
}

# Funcoes do projeto ------------------------------------------------------

for (arquivo in list.files(here("R"), pattern = "\\.R$", full.names = TRUE)) {
  source(arquivo)
}

# Dados -------------------------------------------------------------------
# O CSV tem cerca de 70 MB e fica fora do controle de versao. Baixe uma vez
# com data-raw/download-pph2019.R.

arquivo_dados <- here("data-raw", "pph2019.csv")

if (!file.exists(arquivo_dados)) {
  stop(
    "Base nao encontrada em ", arquivo_dados,
    ". Rode primeiro: Rscript data-raw/download-pph2019.R"
  )
}

dados <- read.csv2(arquivo_dados)
dados[is.na(dados)] <- 0
cat("Base bruta:", nrow(dados), "linhas x", ncol(dados), "colunas\n")

dados_filtrados <- tratamento_dos_dados(dados)
cat(
  "Apos filtragem:", nrow(dados_filtrados), "linhas x",
  ncol(dados_filtrados), "colunas\n\n"
)

# Registro das metricas ---------------------------------------------------

resumo <- data.frame()

registrar <- function(base, modelo, classes, metricas) {
  resumo <<- rbind(resumo, data.frame(
    Base = base,
    Modelo = modelo,
    Classes = classes,
    Acuracia = round(unname(metricas$overall["Accuracy"]), 4),
    Kappa = round(unname(metricas$overall["Kappa"]), 4),
    IC_inferior = round(unname(metricas$overall["AccuracyLower"]), 4),
    IC_superior = round(unname(metricas$overall["AccuracyUpper"]), 4)
  ))
}

# Isola falhas para que uma base problematica nao derrube a execucao inteira
tentar <- function(rotulo, expressao) {
  cat("\n>>>", rotulo, "\n")
  inicio <- Sys.time()
  resultado <- try(expressao, silent = FALSE)
  fechar_grafico()
  duracao <- as.numeric(difftime(Sys.time(), inicio, units = "secs"))

  if (inherits(resultado, "try-error")) {
    cat("    FALHOU em", round(duracao), "s\n")
    return(NULL)
  }

  cat("    ok em", round(duracao), "s\n")
  resultado
}

# Analise de uma base -----------------------------------------------------

analisar_base <- function(nome, prefixo, base) {
  cat("\n\n==========================================================\n")
  cat("BASE:", nome, "-", nrow(base), "linhas x", ncol(base), "colunas\n")
  cat("==========================================================\n")

  secao(paste0(prefixo, "-dispersao"))
  tentar("Graficos de dispersao", graficos_dispersao(base))

  secao(paste0(prefixo, "-boxplot"))
  tentar("Boxplot", boxplot_dispersao(base))

  estatisticas <- tentar("Estatisticas descritivas", estat_dados(base))
  if (!is.null(estatisticas)) {
    write.csv2(
      t(estatisticas),
      file.path(pasta_tabelas, paste0("estatisticas-", prefixo, ".csv"))
    )
    print(t(estatisticas))
  }

  for (k in c(6, 3)) {
    secao(paste0(prefixo, "-arvore-", k, "classes"))
    arvore <- tentar(
      paste("Arvore de decisao,", k, "classes"),
      arvore_class(base, k)
    )
    if (!is.null(arvore)) {
      print(arvore)
      registrar(nome, "Arvore de Decisao", k, arvore)
    }

    svm_resultado <- tentar(
      paste("Maquina de vetor de suporte,", k, "classes"),
      svm_class(base, k)
    )
    if (!is.null(svm_resultado)) {
      print(svm_resultado$summary_svm)
      print(svm_resultado$matriz_de_confusao_svm)
      registrar(nome, "Vetor de Suporte", k, svm_resultado$matriz_de_confusao_svm)
    }

    secao(paste0(prefixo, "-rede-neural-", k, "classes"))
    rede <- tentar(
      paste("Rede neural,", k, "classes"),
      rn_class(base, k, 12)
    )
    if (!is.null(rede)) {
      # rep = "best" evita que plot.nn abra um dispositivo proprio
      x11()
      plot(rede$modelo_rn, rep = "best", show.weights = FALSE)
      fechar_grafico()
      print(rede$matriz_de_confusao_rn)
      print(rede$metricas_rn)
      registrar(nome, "Rede Neural", k, rede$metricas_rn)
    }
  }
}

# Execucao ----------------------------------------------------------------

if (!interativo) {
  sink(file.path(pasta_tabelas, "relatorio.txt"), split = TRUE)
}

dados_RJ <- filtrar_estados("RJ", dados_filtrados)
analisar_base("Dados Totais RJ", "totais-rj", dados_RJ)

dados_o <- dados_originais(dados, "RJ")
analisar_base("Dados Originais da Pesquisa", "originais", dados_o)

dados_cp <- reduzir_com_cp(1, 0.13, dados_RJ)
analisar_base("Reduzidos por Componentes Principais", "componentes", dados_cp)

# Resumo comparativo ------------------------------------------------------

cat("\n\n==========================================================\n")
cat("RESUMO COMPARATIVO\n")
cat("==========================================================\n")

resumo <- resumo[order(resumo$Classes, -resumo$Acuracia), ]
print(resumo, row.names = FALSE)

write.csv2(
  resumo,
  file.path(pasta_tabelas, "resumo-metricas.csv"),
  row.names = FALSE
)
print(
  xtable(resumo, caption = "Desempenho dos classificadores"),
  file = file.path(pasta_tabelas, "resumo-metricas.tex"),
  include.rownames = FALSE
)

cat("\nSaida gravada em", here("output"), "\n")

if (!interativo) {
  sink()
}
