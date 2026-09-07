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

O repositório abriga também um segundo trabalho, **separado da classificação e com outra base de dados**: o agrupamento das 27 unidades da federação por perfil de consumo elétrico, a partir de `data-raw/dados-intenso-forte.csv`. Usa ligação completa (distâncias euclidiana e de Minkowski) e k-médias. Não compartilha dados nem funções com o pipeline de classificação — só o diretório de saída.

## Estrutura do projeto

O layout segue a convenção de *research compendium* para projetos de análise em R (Marwick, Boettiger & Mullen, 2018).

```
├── R/                      Funções reutilizáveis, sem efeitos colaterais
│   ├── dados-pph2019.R         tratamento da base bruta
│   ├── dados-originais.R       variáveis do Critério Brasil
│   ├── dados-estado.R          recorte por UF
│   ├── dados-componentes.R     seleção por componentes principais
│   ├── analise-exploratoria.R  dispersão, boxplot, estatísticas
│   ├── classes-sociais.R       agrupamento em 3 ou 6 classes
│   ├── treino-teste.R          divisão e normalização
│   ├── classificador-*.R       árvore, SVM e rede neural
│   └── clusterizacao.R         agrupamento das UFs
│
├── analysis/               Scripts executáveis, numerados
│   ├── 01-classificacao.R
│   ├── 02-clusterizacao.R
│   └── rascunho-clusterizacao-supervisionada.R
│
├── data-raw/               Dados de entrada
│   ├── download-pph2019.R      baixa a base (~70 MB, fora do Git)
│   ├── dados-intenso-forte.csv
│   └── dados-totais.csv
│
└── output/                 Gerado pelos scripts, fora do Git
    ├── figures/                gráficos em PNG
    └── tables/                 CSV, LaTeX e relatório
```

## Como replicar

**Requisitos:** R 4.5 ou superior. No Windows, o R normalmente fica em `C:/Program Files/R/R-4.5.1/bin/`.

**1. Clone o repositório e restaure as dependências.** O `renv` instala exatamente as versões registradas em `renv.lock`:

```bash
git clone https://github.com/camazlucas/Tcc.git
cd Tcc
Rscript -e "renv::restore()"
```

**2. Baixe a base da PPH 2019.** São cerca de 70 MB, por isso ela não está no repositório. Só precisa rodar uma vez:

```bash
Rscript data-raw/download-pph2019.R
```

**3. Rode a classificação.** Sem argumentos, roda as três bases com 6 e 3 classes:

```bash
Rscript analysis/01-classificacao.R
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
Rscript analysis/01-classificacao.R --base=componentes --classes=6
```

Rodar uma base isolada apaga apenas as figuras daquela base, preservando as das demais.

**4. Rode a clusterização** (estudo independente, com outra base de dados):

```bash
Rscript analysis/02-clusterizacao.R
```

Os scripts detectam o ambiente. Via `Rscript` os gráficos vão para `output/figures/` em PNG e a saída do console é gravada em `output/tables/relatorio.txt`. Abertos no RStudio, os gráficos abrem em janelas.

A classificação completa leva cerca de 4 minutos; uma base isolada, menos de 1. A clusterização leva cerca de 1 minuto (o `NbClust` com `index = "all"` responde pela maior parte).

## Resultados

Acurácia no conjunto de teste, divisão em 6 estratos:

| Base | Modelo | Acurácia | Kappa |
|---|---|---|---|
| Originais | Rede Neural | 0,945 | 0,930 |
| Originais | SVM | 0,890 | 0,860 |
| Originais | Árvore | 0,729 | 0,653 |
| Componentes Principais | SVM | 0,540 | 0,405 |
| Totais RJ | Árvore | 0,524 | 0,382 |
| Totais RJ | SVM | 0,512 | 0,365 |
| Totais RJ | Rede Neural | 0,491 | 0,350 |
| Componentes Principais | Árvore | 0,448 | 0,293 |
| Componentes Principais | Rede Neural | 0,448 | 0,294 |

Com 3 faixas os valores sobem: a base Originais chega a 0,980 e as demais ficam entre 0,67 e 0,79.

**Leitura principal:** reduzir de 61 para 23 variáveis por componentes principais não custa desempenho — a base reduzida empata ou supera a completa nos três classificadores. A posse de equipamentos sozinha sustenta uma separação em 3 faixas com acurácia perto de 0,75, mas se mostra insuficiente para distinguir os 6 estratos do Critério Brasil.

Os valores exatos variam entre execuções, porque a divisão treino/teste é sorteada a cada chamada.

## Tecnologias

R, com `caret` para métricas e partição, `rpart`, `e1071` e `neuralnet` para os classificadores, `NbClust` e `factoextra` para o agrupamento, `dplyr` para manipulação e `xtable` para exportar tabelas em LaTeX. Dependências fixadas com `renv`, estilo conforme o [tidyverse style guide](https://style.tidyverse.org/), verificado com `lintr` e aplicado com `styler`.

## Limitações conhecidas

- Há um `set.seed(42)` dentro de `divisao_das_classes()`, usado para tornar reprodutível a reamostragem das 3 faixas. Como ele reinicia o gerador de números aleatórios, toda a aleatoriedade seguinte fica presa àquela semente: no caminho de 3 classes, a divisão treino/teste é sempre a mesma entre execuções. No caminho de 6 classes, que não passa por esse trecho, ela varia normalmente.
- O arquivo `analysis/rascunho-clusterizacao-supervisionada.R` preserva dois blocos exploratórios que **não executam**: um depende de um CSV ausente do repositório, o outro tem erros de indexação documentados no próprio arquivo.
- A base bruta da PPH 2019 é redistribuída via Hugging Face pelo autor; a fonte original é a Eletrobras/Procel.

## Autor

Lucas Camaz Ferreira — Universidade Federal Rural do Rio de Janeiro
