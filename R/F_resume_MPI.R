#' Cria tabela-resumo dos resultados do MPI-LA, conforme parametros indicados
#' @param dt Banco de dados que foi carregado, pelo usuario, ao ler o arquivo `pnad_completa.parquet`
#' @param grupos Nome dos atributos (default = 'ano') pelos quais serao feitas as analises segmentadas
#' @param k_output Valor escolhido (default = 0.33) para apresentacao do report
#' @param ks Valores de cutoff K utilizados para os calculos do binario de pobreza
#' @return data.table unico, com calculos de pobreza unidimensional (RPC) e multidimensional (MPI usando cutoff K)
#' @export

resume_mpi <- function(
    dt       = NULL,
    grupos   = 'ano',
    k_output = 0.33,
    ks       = c(0.10, 0.20, 0.25, 0.33, 0.40, 0.50)
) {
  plano_original <- future::plan()
  future::plan(future::sequential)
  on.exit({
    future::plan(plano_original)
    gc()
  }, add = T)

  ks_cols        <- paste0('pobre_k', ks * 100)
  grupos_nao_ano <- setdiff(grupos, 'ano')
  grupos_ano     <- intersect(grupos, 'ano')

  calc <- function(grps) {
    purrr::map(setNames(ks_cols, ks_cols), \(col) {
      k_val <- as.numeric(stringr::str_extract(col, '[\\d.]+$')) / 100
      data.table::as.data.table(dt)[,
                                    .(
                                      k   = k_val,
                                      H   = weighted.mean(.SD[[col]], peso, na.rm = T),
                                      A   = weighted.mean(score[.SD[[col]] == 1], peso[.SD[[col]] == 1], na.rm = T),
                                      MPI = weighted.mean(.SD[[col]], peso, na.rm = T) *
                                        weighted.mean(score[.SD[[col]] == 1], peso[.SD[[col]] == 1], na.rm = T),
                                      P0  = weighted.mean(PB | EP, peso, na.rm = T),
                                      P1t = weighted.mean(pmax(0, (sm_real/2 - rpc_real) / (sm_real/2)), peso, na.rm = T),
                                      P1c = weighted.mean(((sm_real/2 - rpc_real) / (sm_real/2))[PB | EP], peso[PB | EP], na.rm = T)
                                      ),
                                    by = grps
      ]
    }) |> purrr::list_rbind()
  }

  res_principal <- calc(grupos)

  if (length(grupos_nao_ano) > 0) {
    res_total <- calc(if (length(grupos_ano) > 0) grupos_ano else NULL) |>
      data.table::as.data.table()
    res_total[, (grupos_nao_ano) := 'Total']
    res_principal <- data.table::rbindlist(list(res_principal, res_total), use.names = T)
  }

  res_principal[k == k_output]
}
