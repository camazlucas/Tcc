# Baixa a base bruta da PPH 2019
#
# O arquivo tem cerca de 70 MB e por isso fica fora do controle de versao.
# Rode uma vez antes da primeira analise:
#
#   Rscript data-raw/classificacao/download-pph2019.R

library(here)

url_dados <- paste0(
  "https://huggingface.co/datasets/camazlucas/pph2019/resolve/main/",
  "PPH%202019%20-%20Banco%20de%20Dados%20V2.csv"
)

destino <- here("data-raw", "classificacao", "pph2019.csv")

if (file.exists(destino)) {
  cat("Base ja existe em", destino, "\n")
  cat("Apague o arquivo para baixar de novo.\n")
} else {
  cat("Baixando de", url_dados, "\n")
  download.file(url_dados, destino, mode = "wb")
  cat("Gravado em", destino, "\n")
}
