# =======================================================================================
# mpiPnad — Pipeline de execução
# Autor: Pier De Maria
# Repositório: https://github.com/dpierf/mpiPnad
#
# Pré-requisito: instalar o pacote usando `pak::pak('dpierf/mpiPnad')`
# =======================================================================================


# 0. SETUP ==============================================================================

rm(list = ls())

# Remover bloqueios de instalação
for (lib in .libPaths()) {
  locks <- list.files(lib, pattern = '^00LOCK', full.names = TRUE)
  if (length(locks) > 0) {
    unlink(locks, recursive = TRUE, force = TRUE)
    message('Removidos bloqueios em: ', lib)
  }
}

rm(lib, locks)

# Instalar/atualizar o pacote 'pak'
if (!requireNamespace('pak', quietly = TRUE)) {
  install.packages('pak')
} else {
  pak::pak('pak')
}


# Instalar/Atualizar o pacote 'mpipnad' e dependências
pak::cache_clean()
if (requireNamespace('mpipnad', quietly = TRUE)) {
  try(unloadNamespace('mpipnad'), silent = TRUE)
  remove.packages('mpipnad')
}
pak::pak('dpierf/mpiPnad', upgrade = TRUE, dependencies = TRUE)
library(mpipnad)


# 1. DOWNLOAD E PROCESSAMENTO ===========================================================

# Baixar microdados da PNAD anual (1981–2015) e PNAD Contínua (2016–2024)
download_pnad(anos = c(1981:2015))

# Processar os arquivos brutos
c(
  fs::dir_ls('data/01_raw/pnad',  glob = '*documentoPNAD*'),
  fs::dir_ls('data/01_raw/pnadc', glob = '*PNADC_*.zip')
) |> purrr::walk(process_pnad)


# 2. CONSTRUÇÃO DO MPI-LA ===============================================================

# Criar (ou carregar, se já existir) o banco de dados principal
mpi_pnad <- create_mpi()
# Nota: se 'data/pnad_completa.parquet' e 'data/mpi_dictionary.rds' já existirem,
# a função carrega os objetos diretamente sem reprocessar.


# 3. ANÁLISE DESCRITIVA =================================================================

# Medidas-resumo por recorte desejado (com exportação para CSV)
mpi_summary <- resume_mpi(
  dt       = mpi_pnad,
  grupos   = c('ano'), #Outros grupos: 'setor', 'area', 'arranjo_familiar'
  k_output = 0.33
) |> write.table('mpi_summary.csv', sep = ';', dec = ',')

# Gráficos e tabelas para análise descritiva (salva em output/graphs/)
analyse_mpi(dt = mpi_pnad)


# 4. MODELAGEM ECONOMÉTRICA =============================================================

# Ajustar modelos ZOIB, quantílico e logit
modelos <- models_mpi(
  dt      = mpi_pnad,
  modelos = c('logit', 'quant', 'zoib'),
  rds     = FALSE #Somente trocar para 'TRUE' se o objetivo é salvar os RDS (>1.5GB cada)
)


# 5. CLUSTERIZAÇÃO HIERÁRQUICA (Ward) ===================================================

# Rodar clustering (k = número de clusters desejado)
anos <- c(1981:1985)
grupos <- cluster_mpi(
  dt     = mpi_pnad[ano %in% anos,], 
  k      = 3, 
  reduce = T,
  dicts  = dicts
)

# Visualizar resultados
with(grupos, {
  print(p_dendro)       # Dendrograma Ward
  print(p_perfil)       # Perfil dimensional por cluster
  print(p_comp)         # Composição categórica por cluster
  print(p_score)        # Densidade do score por cluster
  print(sil_global)     # Silhueta global
  print(sil_dt)         # Silhueta por cluster
  print(p_sil_cluster)  # Barras de silhueta
  print(p_sil_obs)      # Silhueta por observação
})


# 6. DASHBOARD INTERATIVO ===============================================================

dashboard_mpi()
