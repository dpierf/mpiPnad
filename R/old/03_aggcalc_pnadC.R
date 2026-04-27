.w_pnadc <- c(
  D1 = 2/27, D2 = 2/27, D3 = 2/27,
  B1 = 1/18, B2 = 1/18, B3 = 1/18, B4 = 1/18,
  V1 = 4/27, V2 = 2/27,
  E1 = 2/27, E2 = 2/27, E3 = 2/27,
  P1 = 2/27, P2 = 1/27
)

#' Calcula os indicadores do MPI-LA e o score de privacao a nivel domiciliar - PNAD continua (a partir de 2015)
#' @param df Banco de dados, criado pela funcao `rowcalc_pnadC`, ja com atributos intermediarios
#' @return data.table processado, com os 15 indicadores para as 5 dimensoes do MPI-LA, e score de privacao
#' @export

aggcalc_pnadC <- function(df) {

  dt <- data.table::as.data.table(df)

  # REFERENCIA DO DOMICILIO (pessoa de referência) ----
  ref <- dt[cond_familia == 1, .(
    domicilioid, ano, uf, setor, area, peso, pessoas_dom, pessoas_fam,
    arranjo_familiar, agregados, sexo, raca, idade, escol, rpc, 
    D1, D2, D3, B1, B2, B3, B4, V1, V2
  )]

  # INDICADORES INDIVIDUAIS (valor maximo por domicílio) ----
  fam <- dt[, lapply(.SD, \(x) {
    v <- max(x, na.rm = T)
    fifelse(is.infinite(v), NA_real_, v)
  }), .SDcols = c('E1','E2','E3','P1','P2'), by = domicilioid]

  # JOIN E CALCULO DO SCORE ----
  result <- merge(ref, fam, by = 'domicilioid', all.x = T)

  inds <- names(.w_pnadc)

  result[, score := rowSums(
    as.matrix(mapply(`*`, .SD, .w_pnadc)),
    na.rm = T
  ), .SDcols = inds]

  # SELECT FINAL ----
  result[, .(ano, uf, setor, area, domicilioid, peso, pessoas_dom, pessoas_fam,
             arranjo_familiar, agregados,
             sexo, raca, idade, escol, rpc,
             D1, D2, D3, B1, B2, B3, B4, V1, V2, E1, E2, E3, P1, P2, score)]
}

