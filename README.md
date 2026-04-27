# MPI-LA Brasil: PNAD & PNAD Contínua (1981-2024)

Pipeline reproduzível para cálculo, análise e visualização do **Índice de Pobreza Multidimensional para América Latina (MPI-LA)**, 
adaptado para os microdados da PNAD anual (1981-2015) e PNAD Contínua (2016-2024), se baseando na metodologia de Santos et al. (2015).

---

## Funcionalidades

- **Ingestão**: download e processamento dos microdados da PNAD anual e PNAD Contínua
- **Cálculo**: construção dos indicadores e do score MPI-LA a nível de domicílio
- **Análise descritiva**: gráficos e tabelas de evolução, composição, decomposição, convergência, dominância e concentração
- **Modelagem econométrica**: regressão logística, ZOIB e regressão quantílica
- **Clusterização**: tipologia hierárquica Ward sobre os 5 scores dimensionais do MPI-LA
- **Dashboard interativo**: visualização dos resultados via `bs4Dash`

---

## Instalação

```r
# Instalar pak se necessário
if (!requireNamespace('pak', quietly = TRUE)) install.packages('pak')

# Instalar o pacote
pak::pak('dpierf/mpiPnad')
```

---

## Uso

1. Clone ou baixe este repositório
2. Instale as dependências:

```r
source('pipeline_packages.R')
```

Execute o pipeline completo abrindo o arquivo `pipeline.R`. Siga as instruções do arquivo.

---

## Estrutura do repositório

```
mpiPnad/
├── R/                        # Funções do pacote
│   ├── A_download_PNAD.R     # Download dos microdados
│   ├── B_process_PNAD.R      # Processamento e filtragem
│   ├── C_rowcalc_PNAD.R      # Cálculo dos indicadores por domicílio
│   ├── D_aggcalc_PNAD.R      # Agregação domiciliar
│   ├── E_create_MPI.R        # Construção da base MPI-LA
│   ├── F_resume_MPI.R        # Medidas-resumo
│   ├── G_analyse_MPI.R       # Análise descritiva
│   ├── J_cluster_MPI.R       # Clusterização hierárquica
│   ├── K_models_MPI.R        # Modelagem econométrica
│   └── L_dashboard_MPI.R     # Dashboard interativo
├── pipeline.R                # Script de execução completo
├── pipeline_packages.R       # Instalação e carregamento de dependências
├── DESCRIPTION
├── NAMESPACE
└── LICENSE
```

---

## Dados necessários

Os microdados da PNAD e PNAD Contínua não estão incluídos no repositório e devem ser obtidos diretamente do IBGE. A função `download_pnad()` automatiza o download:

```r
# PNAD anual (1981-2015)
download_pnad(anos = 1981:2015)
```

Para a PNAD Contínua (2016-2024), os microdados necessários já são baixados automaticamente via PNADcIBGE, ao executar a função `process_pnad()`.

---

## Referência metodológica

Santos, M.E., Villatoro, P., Mancero, X., & Gerstenfeld, P. (2015).
*A Multidimensional Poverty Index for Latin America*.
OPHI Working Paper 79. University of Oxford.

---

## Licença

MIT © Pier De Maria
