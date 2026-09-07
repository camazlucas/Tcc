# Classificacao do perfil socioeconomico a partir da posse de equipamentos
#
# Compara tres classificadores sobre as bases derivadas da PPH 2019.
#
#   Rscript analysis/classificacao.R [--base=...] [--classes=...]
#
#   --base     totais | originais | componentes | todas   (padrao: todas)
#   --classes  6 | 3 | ambas                              (padrao: ambas)
#   --uf       sigla da unidade da federacao              (padrao: RJ)
#   --help     mostra esta ajuda
#
# Exemplos:
#   Rscript analysis/classificacao.R --base=componentes --classes=6
#   Rscript analysis/classificacao.R --base=todas --classes=3
#
# Funciona nos dois modos: no RStudio os graficos abrem em janelas; via
# Rscript vao para output/classificacao/figures/ em PNG.

rm(list = ls(all = TRUE))

# Argumentos --------------------------------------------------------------

argumentos <- commandArgs(trailingOnly = TRUE)

ajuda <- function() {
  cat(
    "Uso: Rscript analysis/classificacao.R [opcoes]\n\n",
    "  --base=totais|originais|componentes|todas   base a analisar\n",
    "  --classes=6|3|ambas                         divisao das classes\n",
    "  --uf=RJ                                     unidade da federacao\n",
    "  --help                                      mostra esta ajuda\n\n",
    "Sem argumentos, roda as tres bases com 6 e 3 classes.\n",
    sep = ""
  )
}

if ("--help" %in% argumentos || "-h" %in% argumentos) {
  ajuda()
  quit(save = "no")
}

valor_do_argumento <- function(nome, padrao) {
  achado <- grep(paste0("^--", nome, "="), argumentos, value = TRUE)
  if (length(achado) == 0) {
    return(padrao)
  }
  sub(paste0("^--", nome, "="), "", achado[1])
}

exigir_valor_valido <- function(nome, valor, aceitos) {
  if (!valor %in% aceitos) {
    ajuda()
    stop(
      "Valor invalido para --", nome, ": '", valor, "'. ",
      "Aceitos: ", paste(aceitos, collapse = ", "),
      call. = FALSE
    )
  }
  valor
}

base_escolhida <- exigir_valor_valido(
  "base",
  valor_do_argumento("base", "todas"),
  c("totais", "originais", "componentes", "todas")
)

classes_escolhidas <- exigir_valor_valido(
  "classes",
  valor_do_argumento("classes", "ambas"),
  c("6", "3", "ambas")
)

uf_escolhida <- toupper(valor_do_argumento("uf", "RJ"))

valores_de_k <- if (classes_escolhidas == "ambas") {
  c(6, 3)
} else {
  as.integer(classes_escolhidas)
}

cat(
  "Base:", base_escolhida, "| classes:", classes_escolhidas,
  "| UF:", uf_escolhida, "\n\n"
)

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

pasta_figuras <- here("output", "classificacao", "figures")
pasta_tabelas <- here("output", "classificacao", "tables")
dir.create(pasta_figuras, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_tabelas, recursive = TRUE, showWarnings = FALSE)

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

# Carrega apenas o que este subprojeto usa: as estatisticas compartilhadas
# e as funcoes da classificacao.
for (pasta in c("comum", "classificacao")) {
  for (arquivo in list.files(
    here("R", pasta),
    pattern = "\\.R$", full.names = TRUE
  )) {
    source(arquivo)
  }
}

# Dados -------------------------------------------------------------------
# O CSV tem cerca de 70 MB e fica fora do controle de versao. Baixe uma vez
# com data-raw/classificacao/download-pph2019.R.

arquivo_dados <- here("data-raw", "classificacao", "pph2019.csv")

if (!file.exists(arquivo_dados)) {
  stop(
    "Base nao encontrada em ", arquivo_dados,
    ". Rode primeiro: Rscript data-raw/classificacao/download-pph2019.R",
    call. = FALSE
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

# Bases disponiveis -------------------------------------------------------
# Construidas sob demanda: componentes depende de totais, e originais parte
# da base bruta, sem passar por tratamento_dos_dados().

construir_base <- function(qual) {
  if (qual == "originais") {
    return(dados_originais(dados, uf_escolhida))
  }

  base_uf <- filtrar_estados(uf_escolhida, dados_filtrados)

  if (qual == "totais") {
    return(base_uf)
  }
  reduzir_com_cp(1, 0.13, base_uf)
}

catalogo <- list(
  totais = list(
    nome = paste("Dados Totais", uf_escolhida),
    prefixo = paste0("totais-", tolower(uf_escolhida))
  ),
  originais = list(nome = "Dados Originais da Pesquisa", prefixo = "originais"),
  componentes = list(
    nome = "Reduzidos por Componentes Principais",
    prefixo = "componentes"
  )
)

bases_a_rodar <- if (base_escolhida == "todas") {
  c("totais", "originais", "componentes")
} else {
  base_escolhida
}

# Limpa apenas as figuras das bases que serao regeradas, para nao apagar o
# resultado de uma execucao anterior com outra base.
prefixos_a_limpar <- vapply(
  bases_a_rodar, function(b) catalogo[[b]]$prefixo, character(1)
)
for (prefixo in prefixos_a_limpar) {
  unlink(list.files(
    pasta_figuras,
    pattern = paste0("^\\d+-", prefixo, "-.*\\.png$"), full.names = TRUE
  ))
}

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

  for (k in valores_de_k) {
    # A divisao e criada UMA vez e entregue aos tres classificadores, para
    # que a diferenca entre eles nao venha do sorteio.
    divisao <- tentar(
      paste("Divisao treino/teste,", k, "classes"),
      divisao_dos_dados(base, k)
    )
    if (is.null(divisao)) next

    cat(
      "    treino:", nrow(divisao$treino),
      "| teste:", nrow(divisao$teste), "\n"
    )

    secao(paste0(prefixo, "-arvore-", k, "classes"))
    arvore <- tentar(
      paste("Arvore de decisao,", k, "classes"),
      arvore_class(divisao)
    )
    if (!is.null(arvore)) {
      print(arvore)
      registrar(nome, "Arvore de Decisao", k, arvore)
    }

    svm_resultado <- tentar(
      paste("Maquina de vetor de suporte,", k, "classes"),
      svm_class(divisao)
    )
    if (!is.null(svm_resultado)) {
      print(svm_resultado$summary_svm)
      print(svm_resultado$matriz_de_confusao_svm)
      registrar(
        nome, "Vetor de Suporte", k, svm_resultado$matriz_de_confusao_svm
      )
    }

    secao(paste0(prefixo, "-rede-neural-", k, "classes"))
    rede <- tentar(
      paste("Rede neural,", k, "classes"),
      rn_class(divisao, 12)
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

for (qual in bases_a_rodar) {
  base <- construir_base(qual)
  analisar_base(catalogo[[qual]]$nome, catalogo[[qual]]$prefixo, base)
}

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

cat("\nSaida gravada em", here("output", "classificacao"), "\n")

if (!interativo) {
  sink()
}
