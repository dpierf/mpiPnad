# CARREGAMENTO DE FUNCOES ===============================================================

rm(list=ls())
setwd(here::here())

source('R/0_packages.R')
source('R/0_functions.R')


# TRATAMENTO DE ARQUVOS =================================================================

# Baixando arquivos PNAD anual
download_pnad(anos = c(1981:2015))

# Processando os arquivos
c(
  fs::dir_ls('data/01_raw/pnad',  glob = '*documentoPNAD*'),
  fs::dir_ls('data/01_raw/pnadc', glob = '*PNADC_*.zip')
) |> purrr::walk(process_pnad)


# CONSTRUCAO E ANALISE DO MPI-LA ========================================================

# Criando (ou carregando) o banco de dados
mpi_pnad <- create_mpi()

# Obtendo medidas-resumo para o período e recortes desejados
mpi_summary <- resume_mpi(
  dt       = mpi_pnad, 
  grupos   = c('ano'), #grupos = c('ano','setor','area','arranjo_familiar')
  k_output = 0.33
) 

# INTERPRETAÇÃO DE DADOS ================================================================

# Gerando gráficos e tabelas para análise descritiva
analyse_mpi(dt = mpi_pnad)

# Modelagem econométrica
mods <- models_mpi(dt = mpi_pnad, modelos = c('logit','quant','zoib'), rds = F)

# Criando o dashboard
dashboard_mpi()

# MODELAGEM MULTIDIMENSIONAL ============================================================

# Clusterização Hierárquica (Ward)
clus_input <- prep_cluster(dt = mpi_pnad)

clus_mpi <- cluster_mpi(dt = clus_input)

with(clus_mpi, {
  print(p_dendro)      #Gráfico dos clusters em espaço R2
  print(p_perfil)      #Perfil médio dos indicadores do MPI
  print(p_comp)        #Alocação dos arranjos por categorias
  print(p_score)       #Densidade observada do MPI por grupo
  print(sil_global)    #Silhueta média
  print(sil_dt)        #Silhueta por cluster
  print(p_sil_cluster) #Boxplot das silhuetas
  print(p_sil_obs)     #Silhuetas por observação
})


