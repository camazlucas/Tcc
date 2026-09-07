# TCC — Classificação do perfil socioeconômico a partir da posse de equipamentos elétricos

Este projeto implementa diferentes métodos de aprendizado de máquina para classificação do perfil socioeconômico utilizando dados da Pesquisa de Posse e Hábitos de Uso de Equipamentos Elétricos (PPH) 2019.

Foram avaliados classificadores baseados em Árvores de Decisão, Máquinas de Vetores de Suporte (SVM) e Redes Neurais, além de diferentes estratégias de pré-processamento e redução de dimensionalidade.

## Objetivo

Verificar se a **classe social de um domicílio pode ser inferida a partir dos eletrodomésticos que ele possui**, sem recorrer às variáveis do Critério Brasil (automóveis, empregados domésticos, banheiros, grau de instrução).

A pergunta prática por trás disso: uma concessionária de energia conhece o perfil de consumo e a posse de equipamentos de seus clientes, mas não conhece a renda deles. Se a posse de equipamentos for suficiente para estimar o estrato socioeconômico, ela pode planejar programas de eficiência energética sem depender de pesquisa de renda.

## Metodologia

A base da PPH 2019 traz 27.826 domicílios e 2.319 variáveis. O tratamento seleciona as colunas de posse de equipamentos, remove domicílios com atividade comercial e elimina variáveis redundantes, chegando a 18.544 domicílios e 61 variáveis. A análise é feita sobre o Rio de Janeiro (1.096 domicílios).

Três bases são comparadas:

| Base | Variáveis | Papel |
|---|---|---|
| **Totais RJ** | 61 equipamentos | A hipótese em teste |
| **Originais** | 16 do Critério Brasil | Teto de referência |
| **Componentes Principais** | 23 selecionadas por PCA | Redução de dimensionalidade |

Cada base é classificada em **6 estratos** (A, B1, B2, C1, C2, DE) e em **3 faixas** (Alta, Média, Baixa). No caminho de 3 faixas os grupos são reamostrados para o tamanho do menor deles, equilibrando a base.

A divisão é 70/30, **estratificada pela própria `CLASSE`** e criada **uma única vez** por base e por quantidade de classes. Os três classificadores recebem exatamente as mesmas linhas de treino e de teste, de modo que a diferença de acurácia entre eles seja diferença de modelo e não do sorteio. A estratificação importa porque a classe A representa só 3,6% da base do Rio de Janeiro; sem ela, sua participação no conjunto de teste oscila de um sorteio para outro.

> **Nota de interpretação.** A base Originais alcança as maiores acurácias, mas isso não é mérito do modelo: essas variáveis são justamente as entradas da fórmula que *define* a `CLASSE`. Ela serve como limite superior de comparação. O resultado que sustenta o trabalho é a comparação entre Totais RJ e Componentes Principais.

## Algoritmos implementados

- **Árvore de decisão** (`rpart`), com critério de ganho de informação
- **Máquina de vetor de suporte** (`e1071`), kernel linear, custo 0,1
- **Rede neural** (`neuralnet`), 12 neurônios na camada oculta, ativação logística

As preditoras são normalizadas por min-max para a rede neural, com mínimos e máximos calculados **apenas no conjunto de treino** e aplicados ao teste, evitando vazamento de informação. Sem essa normalização o algoritmo rprop não converge dentro do `stepmax` na base reduzida por componentes principais.

### Estudo independente: agrupamento das UFs

O TCC tem **dois subprojetos independentes**. Além da classificação, o repositório abriga o agrupamento das 27 unidades da federação por perfil de consumo elétrico, a partir de `data-raw/clusterizacao/dados-intenso-forte.csv`. Usa agrupamento hierárquico por ligação completa, com distância euclidiana, e k-médias.

**Sobre a escolha do número de grupos.** Dois artifícios informam essa decisão, e nenhum dos dois a determina: o `NbClust`, que combina 30 índices e devolve uma sugestão por regra da maioria, e o gráfico do método do cotovelo, construído iterando o número de grupos e observando a soma de quadrados intra-grupo. Aqui os dois divergem — a regra da maioria do `NbClust` aponta **2**, o cotovelo aponta **3** — e o trabalho adota **3**, apoiado no cotovelo e na interpretabilidade dos grupos resultantes. A divergência é deliberada: o `NbClust` entra como opinião a ponderar, não como critério de decisão.

Os dois não compartilham dados, nem diretório de saída, nem funções — exceto as estatísticas descritivas de `R/comum/estatisticas.R`. Cada um tem o seu próprio script em `analysis/`, e nenhum depende do outro para rodar.

## Estrutura do projeto

O layout segue a convenção de *research compendium* para projetos de análise em R (Marwick, Boettiger & Mullen, 2018), com os **dois subprojetos separados** em cada nível.

```
├── R/                          Funções reutilizáveis, sem efeitos colaterais
│   ├── comum/
│   │   └── estatisticas.R          descritivas, usadas pelos dois
│   ├── classificacao/
│   │   ├── dados-pph2019.R         tratamento da base bruta
│   │   ├── dados-originais.R       variáveis do Critério Brasil
│   │   ├── dados-estado.R          recorte por UF
│   │   ├── dados-componentes.R     seleção por componentes principais
│   │   ├── dispersao.R             dispersão no plano das componentes
│   │   ├── classes-sociais.R       agrupamento em 3 ou 6 classes
│   │   ├── treino-teste.R          divisão estratificada e normalização
│   │   └── classificador-*.R       árvore, SVM e rede neural
│   └── clusterizacao/
│       └── agrupamento.R           hierárquico e k-médias
│
├── analysis/                   Scripts executáveis, um por subprojeto
│   ├── classificacao.R
│   ├── clusterizacao.R
│   └── clusterizacao-rascunho.R    blocos preservados, não executam
│
├── data-raw/                   Dados de entrada
│   ├── classificacao/
│   │   ├── download-pph2019.R      baixa a base (~70 MB, fora do Git)
│   │   └── pph2019.csv             (gerado pelo download)
│   └── clusterizacao/
│       ├── dados-intenso-forte.csv
│       └── dados-totais.csv
│
└── output/                     Gerado pelos scripts, fora do Git
    ├── classificacao/  figures/  tables/
    └── clusterizacao/  figures/  tables/
```

Cada script de `analysis/` carrega apenas `R/comum/` e a pasta do seu próprio subprojeto — a separação é imposta pelo código, não só pela convenção de nomes. Os arquivos não são numerados porque os dois subprojetos são independentes: não há ordem de execução entre eles.

> Subpastas dentro de `R/` funcionam num compendium, mas **não** num pacote R, onde `R/` precisa ser plano. O `DESCRIPTION` aqui está no papel de metadado do compendium, não de pacote instalável.

## Como replicar

**Requisitos:** R 4.5 ou superior. No Windows, o R normalmente fica em `C:/Program Files/R/R-4.5.1/bin/`.

**1. Clone o repositório e restaure as dependências.** O `renv` instala exatamente as versões registradas em `renv.lock`:

```bash
git clone https://github.com/camazlucas/Tcc.git
cd Tcc
Rscript -e "renv::restore()"
```

**2. Obtenha os dados.** Os dois subprojetos têm origens diferentes:

| Subprojeto | Base | Origem |
|---|---|---|
| Classificação | PPH 2019 | [Hugging Face](https://huggingface.co/datasets/camazlucas/pph2019) |
| Clusterização | consumo por UF | [GitHub](data-raw/clusterizacao/) |

A base da PPH 2019 tem cerca de 70 MB, grande demais para o controle de versão, e por isso fica hospedada no Hugging Face. O script abaixo a baixa para `data-raw/classificacao/pph2019.csv` e só precisa rodar uma vez — a URL está no próprio script:

```bash
Rscript data-raw/classificacao/download-pph2019.R
```

Os dados da clusterização são dois CSVs de poucos kilobytes, versionados junto com o código. Você já os tem depois do `git clone`, e não há nada a baixar para rodar aquele subprojeto.

Feito isso, o ambiente está pronto. A execução em si está na seção seguinte.

### Alternativa: Docker

O `renv` fixa as versões dos pacotes R, mas não fixa o R em si, o compilador, a BLAS nem as bibliotecas gráficas — e delas dependem tanto a convergência do `neuralnet` quanto o desenho dos PNGs. Quem preferir não instalar nada disso na própria máquina pode rodar o compendium em container, com o ambiente inteiro congelado.

**Requisitos:** Docker. Nada de R, nem de RStudio.

```bash
git clone https://github.com/camazlucas/Tcc.git
cd Tcc
docker compose build
```

O build restaura os mesmos pacotes do `renv.lock`, agora sobre R 4.5.1 e Ubuntu 24.04 fixos. Leva alguns minutos na primeira vez e fica em cache depois.

Daí em diante, **todo comando da seção seguinte vale igual, prefixado por `docker compose run --rm tcc`**:

```bash
docker compose run --rm tcc Rscript data-raw/classificacao/download-pph2019.R
docker compose run --rm tcc Rscript analysis/classificacao.R
docker compose run --rm tcc Rscript analysis/clusterizacao.R
```

O projeto é montado como volume, então as figuras e tabelas aparecem em `output/` no seu próprio disco, e o código pode ser editado no host sem refazer a imagem. Os scripts detectam a ausência de interface gráfica e gravam PNG — o mesmo comportamento de rodar `Rscript` fora do container, sem necessidade de X11.

> Em host Linux os arquivos de `output/` saem pertencendo ao `root`. Para evitar isso, acrescente `--user "$(id -u):$(id -g)"` ao `docker compose run`. No Windows e no macOS não é preciso.

## Execução

Cada subprojeto tem um script, e os dois são independentes — rode o que precisar, em qualquer ordem:

| Para obter | Rode | Saída |
|---|---|---|
| Resultados da **classificação** | `Rscript analysis/classificacao.R` | `output/classificacao/` |
| Resultados da **clusterização** | `Rscript analysis/clusterizacao.R` | `output/clusterizacao/` |

Cada execução grava, dentro da pasta do subprojeto:

- `figures/` — os gráficos em PNG, numerados na ordem em que aparecem
- `tables/relatorio.txt` — toda a saída de console, incluindo as matrizes de confusão
- `tables/resumo-metricas.csv` e `.tex` — a tabela comparativa, a última pronta para o LaTeX
- `tables/estatisticas-*.csv` — as descritivas de cada base

### Classificação

Sem argumentos, roda as três bases com 6 e 3 classes:

```bash
Rscript analysis/classificacao.R
```

Você pode escolher a base e a divisão das classes:

| Opção | Valores | Padrão |
|---|---|---|
| `--base` | `totais`, `originais`, `componentes`, `todas` | `todas` |
| `--classes` | `6`, `3`, `ambas` | `ambas` |
| `--uf` | sigla da unidade da federação | `RJ` |
| `--help` | mostra a ajuda | — |

Por exemplo, só a base reduzida por componentes principais, em 6 estratos:

```bash
Rscript analysis/classificacao.R --base=componentes --classes=6
```

Rodar uma base isolada apaga apenas as figuras daquela base, preservando as das demais.

### Clusterização

Não tem parâmetros — usa a base própria do subprojeto:

```bash
Rscript analysis/clusterizacao.R
```

### Notas de execução

Os scripts detectam o ambiente: via `Rscript` os gráficos vão para PNG; abertos no RStudio, abrem em janelas.

A classificação completa leva cerca de 4 minutos, e uma base isolada menos de 1. A clusterização leva cerca de 1 minuto — o `NbClust` com `index = "all"` responde pela maior parte.

## Resultados

Acurácia no conjunto de teste. Os valores saem de `output/classificacao/tables/resumo-metricas.csv`.

### 6 estratos (A, B1, B2, C1, C2, DE)

| Base | Modelo | Acurácia | Kappa | IC 95% |
|---|---|---|---|---|
| Originais | Rede Neural | 0,939 | 0,922 | 0,907 – 0,962 |
| Originais | SVM | 0,905 | 0,879 | 0,868 – 0,935 |
| Originais | Árvore | 0,653 | 0,555 | 0,599 – 0,705 |
| Totais RJ | SVM | 0,528 | 0,392 | 0,472 – 0,583 |
| Componentes Principais | Árvore | 0,518 | 0,379 | 0,463 – 0,574 |
| Totais RJ | Árvore | 0,506 | 0,368 | 0,451 – 0,562 |
| Componentes Principais | SVM | 0,503 | 0,366 | 0,447 – 0,559 |
| Componentes Principais | Rede Neural | 0,436 | 0,283 | 0,381 – 0,491 |
| Totais RJ | Rede Neural | 0,420 | 0,266 | 0,366 – 0,476 |

### 3 faixas (Alta, Média, Baixa)

Aqui a base é reamostrada para equilibrar os grupos, caindo de 1.096 para 345 domicílios — daí os intervalos de confiança mais largos.

| Base | Modelo | Acurácia | Kappa | IC 95% |
|---|---|---|---|---|
| Originais | SVM | 0,971 | 0,956 | 0,916 – 0,994 |
| Originais | Rede Neural | 0,931 | 0,897 | 0,864 – 0,972 |
| Originais | Árvore | 0,863 | 0,794 | 0,780 – 0,923 |
| Totais RJ | SVM | 0,775 | 0,662 | 0,681 – 0,851 |
| Totais RJ | Árvore | 0,765 | 0,647 | 0,670 – 0,843 |
| Componentes Principais | Árvore | 0,755 | 0,632 | 0,660 – 0,835 |
| Componentes Principais | SVM | 0,755 | 0,632 | 0,660 – 0,835 |
| Componentes Principais | Rede Neural | 0,706 | 0,559 | 0,608 – 0,792 |
| Totais RJ | Rede Neural | 0,647 | 0,471 | 0,546 – 0,739 |

**Leitura principal:** reduzir de 61 para 23 variáveis por componentes principais **não custa desempenho de forma detectável**. Os intervalos de confiança da base completa e da reduzida se sobrepõem amplamente em todos os classificadores e nas duas divisões de classe — com 6 estratos, o SVM dá 0,528 (0,472 – 0,583) na completa contra 0,503 (0,447 – 0,559) na reduzida; com 3 faixas, 0,775 (0,681 – 0,851) contra 0,755 (0,660 – 0,835). Ou seja, 38 variáveis a menos sem diferença estatisticamente distinguível.

A posse de equipamentos sozinha sustenta uma separação em 3 faixas com acurácia em torno de 0,77, mas se mostra insuficiente para os 6 estratos do Critério Brasil, onde nenhum modelo passa de 0,53.

Como os três classificadores agora recebem a mesma divisão, a comparação **entre modelos dentro de uma mesma base** é direta. Os valores absolutos ainda variam entre execuções no caminho de 6 classes, porque a divisão é sorteada; no de 3 classes eles são estáveis, pelo motivo descrito em Limitações.

## Tecnologias

R, com `caret` para métricas e partição, `rpart`, `e1071` e `neuralnet` para os classificadores, `NbClust` e `factoextra` para o agrupamento, `dplyr` para manipulação e `xtable` para exportar tabelas em LaTeX. Dependências fixadas com `renv`, estilo conforme o [tidyverse style guide](https://style.tidyverse.org/), verificado com `lintr` e aplicado com `styler`.

## Limitações conhecidas

- Há um `set.seed(42)` dentro de `divisao_das_classes()`, usado para tornar reprodutível a reamostragem das 3 faixas. Como ele reinicia o gerador de números aleatórios, toda a aleatoriedade seguinte fica presa àquela semente: no caminho de 3 classes, a divisão treino/teste é sempre a mesma entre execuções. No caminho de 6 classes, que não passa por esse trecho, ela varia normalmente.
- O arquivo `analysis/clusterizacao-rascunho.R` preserva dois blocos exploratórios que **não executam**: um depende de um CSV ausente do repositório, o outro tem erros de indexação documentados no próprio arquivo.
- A base bruta da PPH 2019 é redistribuída via Hugging Face pelo autor; a fonte original é a Eletrobras/Procel.

## Autor

Lucas Camaz Ferreira — Universidade Federal Rural do Rio de Janeiro
