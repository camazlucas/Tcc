# Ambiente de execucao do TCC
#
# O renv.lock fixa as versoes dos pacotes R, mas nao fixa a versao do R, o
# compilador, a BLAS nem as bibliotecas graficas. Esta imagem fecha essa
# lacuna e congela o ambiente inteiro em que a analise foi produzida.
#
#   docker compose build
#   docker compose run --rm tcc Rscript analysis/clusterizacao.R

FROM rocker/r-ver:4.5.1

# Bibliotecas de sistema exigidas pelos pacotes do renv.lock:
#   curl, openssl, httr -> libcurl, libssl
#   gsl                 -> libgsl
#   stringi             -> libicu
#   nloptr              -> cmake
#   dispositivo png()   -> libpng, libjpeg, libtiff, cairo, freetype
RUN apt-get update && apt-get install -y --no-install-recommends \
        cmake \
        libcairo2-dev \
        libcurl4-openssl-dev \
        libfontconfig1-dev \
        libfreetype6-dev \
        libgsl-dev \
        libicu-dev \
        libjpeg-dev \
        libpng-dev \
        libssl-dev \
        libtiff-dev \
        zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# O renv.lock registra https://packagemanager.posit.co/cran/latest, que serve
# binarios no Windows e *codigo-fonte* no Linux. Como o restore usa os
# repositorios do proprio lockfile, sem este override os 183 pacotes seriam
# compilados do zero. O caminho __linux__/noble corresponde ao Ubuntu 24.04
# da imagem base.
ENV RENV_CONFIG_REPOS_OVERRIDE=https://packagemanager.posit.co/cran/__linux__/noble/latest

# A biblioteca fica fora de /project para nao ser encoberta pelo bind mount do
# codigo, e o cache do renv fica desligado para que os pacotes sejam copiados
# para dentro da imagem em vez de virarem symlink para um cache externo.
ENV RENV_PATHS_LIBRARY=/opt/renv/library \
    RENV_CONFIG_CACHE_ENABLED=FALSE

WORKDIR /project

# As dependencias sao restauradas antes de o codigo entrar: assim uma
# alteracao em analysis/ ou R/ nao invalida a camada dos 183 pacotes.
COPY renv.lock .Rprofile ./
COPY renv/activate.R renv/settings.json renv/
RUN Rscript -e 'renv::restore(prompt = FALSE)'

COPY . .

CMD ["bash"]
