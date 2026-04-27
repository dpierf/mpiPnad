# ── Pesos unificados ──────────────────────────────────────────────────────────

.w_pnad <- c(
  D1 = 2/27, D2 = 2/27, D3 = 2/27,
  B1 = 1/18, B2 = 1/18, B3 = 1/18, B4 = 1/18,
  V1 = 4/27, V2 = 2/27,
  E1 = 2/27, E2 = 2/27, E3 = 2/27,
  P1 = 2/27, P2 = 1/27
)


# ── Função pública ────────────────────────────────────────────────────────────

#' Calcula os indicadores do MPI-LA e o score de privação a nível domiciliar
#'
#' Detecta automaticamente o tipo de pesquisa a partir do ano presente nos dados.
#' @param df data.frame ou data.table, criado por `rowcalc_pnad`, com atributos intermediários
#' @param type Tipo de pesquisa: `'anual'` ou `'continua'`. Se `NULL` (padrão),
#'   detectado automaticamente a partir de `max(df$ano)`
#' @return data.table com score de privação e indicadores do MPI-LA a nível domiciliar
#' @export

aggcalc_pnad <- function(df, type = NULL) {
  
  dt <- data.table::as.data.table(df)
  
  if (is.null(type)) {
    ano_max <- max(dt[['ano']], na.rm = TRUE)
    type    <- if (ano_max >= 2016) 'continua' else 'anual'
  } else {
    type <- match.arg(type, c('anual', 'continua'))
  }
  
  w    <- .w_pnad
  inds <- names(w)
  
  # REFERENCIA DO DOMICILIO (pessoa de referência) ----
  cols_ref <- c(
    'domicilioid', 'ano', 'uf', 'setor', 'area', 'psu', 'strata', 'peso',
    'pessoas_dom', 'pessoas_fam', 'arranjo_familiar', 'agregados',
    'sexo', 'raca', 'idade', 'escol', 'rpc',
    'D1', 'D2', 'D3', 'B1', 'B2', 'B3', 'B4', 'V1', 'V2'
  )
  ref <- dt[cond_familia == 1, .SD, .SDcols = cols_ref]
  
  # INDICADORES INDIVIDUAIS (valor máximo por domicílio) ----
  fam <- dt[, lapply(.SD, \(x) {
    v <- max(x, na.rm = TRUE)
    fifelse(is.infinite(v), NA_real_, v)
  }), .SDcols = c('E1','E2','E3','P1','P2'), by = domicilioid]
  
  # JOIN E CÁLCULO DO SCORE ----
  result <- merge(ref, fam, by = 'domicilioid', all.x = TRUE)
  
  mat <- as.matrix(result[, .SD, .SDcols = inds])
  result[, score := as.numeric(mat %*% w)]
  
  # SELECT FINAL ----
  result[, .SD, .SDcols = c(
    'ano', 'uf', 'setor', 'area', 'domicilioid', 'psu', 'strata', 'peso',
    'pessoas_dom', 'pessoas_fam', 'arranjo_familiar', 'agregados',
    'sexo', 'raca', 'idade', 'escol', 'rpc',
    'D1', 'D2', 'D3', 'B1', 'B2', 'B3', 'B4',
    'V1', 'V2', 'E1', 'E2', 'E3', 'P1', 'P2', 'score'
  )]
}