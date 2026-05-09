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

## Instalação e Uso

```r
# Instalar pak se necessário
if (!requireNamespace('pak', quietly = TRUE)) install.packages('pak')

# Instalar o pacote
pak::pak('dpierf/mpiPnad')
```

O pipeline completo pode ser executado, etapa por etapa, abrindo o arquivo `pipeline.R`. Siga as instruções do arquivo.

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

## Autoria

**Pier Francesco De Maria**  
[![Lattes](https://img.shields.io/badge/Lattes-CNPq-blue)](http://lattes.cnpq.br/8532403786219091)

[![ORCID](https://img.shields.io/badge/ORCID-0000--0003--1389--3082-green)](https://orcid.org/0000-0003-1389-3082)  

[![Email](https://img.shields.io/badge/Email-dpierf%40gmail.com-red)](mailto:dpierf@gmail.com)

[![LinkedIn](https://img.shields.io/badge/LinkedIn-Pier%20F.%20De%20Maria-blue?logo=linkedin)](https://www.linkedin.com/in/dpierf)

---

## Origem e referência teórica

Este pacote foi desenvolvido no âmbito de pesquisa acadêmica em continuidade à dissertação de mestrado:

> MARIA, Pier Francesco De. **Família e pobreza: arranjos no Pós-Real (1995–2014)**. 2016. Dissertação (Mestrado em Demografia) - Universidade Estadual de Campinas, Campinas, 2016.

O desenvolvimento e a implementação do MPI-LA a nível Brasil seguiu, como referencial, o seguinte trabalho do Oxford Poverty and Human Development Initiative (OPHI):

> SANTOS, Maria Emma et al. **A multidimensional poverty index for Latin America**. Oxford: OPHI, 2015. (OPHI Working Paper, 79). Disponível em: https://ophi.org.uk/wp-content/uploads/OPHIWP079.pdf. Acesso em: 27 abr. 2026.

---

## Uso de inteligência artificial generativa

O desenvolvimento deste pacote contou com o auxílio extensivo de inteligência artificial generativa ao longo de todo o processo — incluindo arquitetura do pipeline, escrita e revisão de código, decisões metodológicas e documentação. A ferramenta utilizada foi:

> ANTHROPIC. **Claude Sonnet 4.6**. San Francisco: Anthropic, 2025. Disponível em: https://claude.ai. Acesso em: 27 abr. 2026.

O uso de IA não substitui a responsabilidade intelectual do autor sobre as escolhas metodológicas, interpretações e resultados apresentados.

---

## Licença e Como citar este pacote

**GPL-3.0 © Pier Francesco De Maria**

Este pacote é distribuído sob a GNU General Public License v3.0. Você pode usar, copiar e modificar este código livremente, desde que:

* Atribua o trabalho original ao autor
* Distribua qualquer versão modificada sob a mesma licença GPL-3.0
* Disponibilize o código-fonte de qualquer derivação publicamente
Isso garante que trabalhos derivados deste pacote permaneçam abertos e rastreáveis até a fonte original. Pier Francesco De Maria

Para citação deste pacote em publicações futuras, siga a referência abaixo (conforme NBR 6023:2025)
> DE MARIA, Pier Francesco. **mpiPnad** - MPI-LA Brasil: PNAD & PNAD Contínua (1981–2024). Versão 0.0.0.9000. [S.l.]: GitHub, 2025. Disponível em: https://github.com/dpierf/mpiPnad. Acesso em: 27 abr. 2026.
