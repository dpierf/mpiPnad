.w_pnad <- c(
  D1 = 2/27, D2 = 2/27, D3 = 2/27,
  B1 = 2/45, B2 = 2/45, B3 = 2/45, B4 = 2/45, B5 = 2/45,
  V1 = 4/27, V2 = 2/27,
  E1 = 2/27, E2 = 2/27, E3 = 2/27,
  P1 = 2/27, P2 = 1/27
)

#' Calcula os indicadores do MPI-LA e o score de privacao a nivel domiciliar - PNAD anual (ate 2015)
#' @param df Banco de dados, criado pela funcao `rowcalc_pnadA`, ja com atributos intermediarios
#' @return data.table processado, com os 15 indicadores para as 5 dimensoes do MPI-LA, e score de privacao
#' @export

aggcalc_pnadA <- function(df) {

  dt <- data.table::as.data.table(df)

  # REFERENCIA DO DOMICILIO (pessoa de referência) ----
  ref <- dt[cond_familia == 1, .(
    domicilioid, ano, uf, setor, area, peso, pessoas_dom, pessoas_fam,
    arranjo_familiar, agregados, sexo, raca, idade, escol, rpc, 
    D1, D2, D3, B1, B2, B3, B4, B5, V1, V2
  )]

  # INDICADORES INDIVIDUAIS (valor maximo por domicílio) ----
  fam <- dt[, lapply(.SD, \(x) {
    v <- max(x, na.rm = T)
    fifelse(is.infinite(v), NA_real_, v)
  }), .SDcols = c('E1','E2','E3','P1','P2'), by = domicilioid]

  # JOIN E CALCULO DO SCORE ----
  result <- merge(ref, fam, by = 'domicilioid', all.x = T)

  inds <- names(.w_pnad)

  result[, score := rowSums(
    as.matrix(mapply(`*`, .SD, .w_pnad)),
    na.rm = T
  ), .SDcols = inds]

  # SELECT FINAL ----
  result[, .(ano, uf, setor, area, domicilioid, peso, pessoas_dom, pessoas_fam,
             arranjo_familiar, agregados,
             sexo, raca, idade, escol, rpc,
             D1, D2, D3, B1, B2, B3, B4, V1, V2, E1, E2, E3, P1, P2, score)]
}
