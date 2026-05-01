#' Cria tabela-resumo dos resultados do MPI-LA, conforme parametros indicados
#' @param dt Banco de dados que foi carregado, pelo usuario, ao ler o arquivo `pnad_completa.parquet`
#' @param grupos Nome dos atributos (default = 'ano') pelos quais serao feitas as analises segmentadas
#' @param k_output Valor escolhido (default = 0.33) para apresentacao do report
#' @param ks Valores de cutoff K utilizados para os calculos do binario de pobreza
#' @return data.table unico, com calculos de pobreza unidimensional (RPC) e multidimensional (MPI usando cutoff K)
#' @export

resume_mpi <- function(
    dt = NULL,
    grupos = 'ano',
    k_output = 0.33,
    ks = c(0.10, 0.20, 0.25, 0.33, 0.40, 0.50)
) {
  
  # Validações
  if (is.null(dt)) {
    stop("dt não pode ser NULL")
  }
  
  cols_disponiveis <- names(dt)
  invalid_groups <- setdiff(grupos, cols_disponiveis)
  if (length(invalid_groups) > 0) {
    stop("Colunas não encontradas em dt: ", paste(invalid_groups, collapse = ", "))
  }
  
  # Força plano sequencial
  plano_original <- future::plan()
  future::plan(future::sequential)
  on.exit({
    future::plan(plano_original)
    gc(verbose = FALSE)
  }, add = TRUE)
  
  ks_cols <- paste0('pobre_k', ks * 100)
  grupos_nao_ano <- setdiff(grupos, 'ano')
  grupos_ano <- intersect(grupos, 'ano')
  
  # Função auxiliar para métricas que NÃO dependem de k
  calc_metricas_fixas <- function(grps) {
    dt[, .(
      P0  = weighted.mean(PB | EP, peso, na.rm = TRUE),
      P1t = weighted.mean(pmax(0, (sm_real/2 - rpc_real) / (sm_real/2)), peso, na.rm = TRUE),
      P1c = weighted.mean(((sm_real/2 - rpc_real) / (sm_real/2))[PB | EP], 
                          peso[PB | EP], na.rm = TRUE)
    ), by = grps]
  }
  
  # Função auxiliar para métricas que dependem de k
  calc_metricas_k <- function(grps, col, k_val) {
    dt[, .(
      H = weighted.mean(.SD[[col]], peso, na.rm = TRUE),
      A = {
        pobres <- .SD[[col]] == 1
        if (any(pobres, na.rm = TRUE)) {
          weighted.mean(score[pobres], peso[pobres], na.rm = TRUE)
        } else NA_real_
      }
    ), by = grps]
  }
  
  # Função principal
  calc <- function(grps) {
    metricas_fixas <- calc_metricas_fixas(grps)
    
    purrr::map_dfr(ks_cols, function(col) {
      k_val <- as.numeric(stringr::str_extract(col, '[\\d.]+$')) / 100
      metricas_k <- calc_metricas_k(grps, col, k_val)
      
      metricas_k[, `:=`(k = k_val, MPI = H * A)]
      merge(metricas_k, metricas_fixas, by = grps)
    })
  }
  
  # Cálculo principal
  res_principal <- calc(grupos)
  
  # Adiciona totais se houver grupos não-ano
  if (length(grupos_nao_ano) > 0) {
    res_total <- calc(if (length(grupos_ano) > 0) grupos_ano else NULL)
    data.table::setDT(res_total)
    res_total[, (grupos_nao_ano) := 'Total']
    
    # Reordena colunas
    todas_cols <- c(grupos_nao_ano, grupos_ano, names(res_total)[!names(res_total) %in% c(grupos_nao_ano, grupos_ano)])
    res_total <- res_total[, ..todas_cols]
    
    res_principal <- data.table::rbindlist(list(res_principal, res_total), 
                                           use.names = TRUE, fill = TRUE)
  }
  
  # Filtra pelo k de saída
  res_principal[k == k_output]
}
