# Salario Minimo PNAD Contínua (2016–2024) ----
.sm_pnadc <- data.table::data.table(
  ano = 2016:2025,
  valor = as.double(c(880, 937, 954, 998, 1045, 1100, 1212, 1320, 1412, 1518))
)

#' Prepara os indicadores intermediarios para calculo do MPI-LA a nivel individual - PNAD continua (a partir de 2015)
#' @param df Banco de dados, criado pela funcao `process_pnadC`, ja filtrado com atributos
#' @return data.table processado, com indicadores intermediarios
#' @export

rowcalc_pnadC <- function(df) {

  dt <- data.table::as.data.table(df)

  # JOIN SALARIO MINIMO ----
  dt <- merge(dt, .sm_pnadc, by = 'ano', all.x = T)

  chr_cols <- names(dt)[sapply(dt, is.character)]
  dt[, (chr_cols) := lapply(.SD, \(x) {
    y <- suppressWarnings(as.double(x))
    if (sum(is.na(y)) == sum(is.na(x))) y else x
  }), .SDcols = chr_cols]

  ## TRATAMENTOS INICIAIS =========================================================================

  # VARIAVEIS SOCIODEMOGRAFICAS ----
  data.table::setnames(dt, 'uf', 'uf_ref')

  dt[, uf := fcase(
    uf_ref == 11, 'RO', uf_ref == 12, 'AC', uf_ref == 13, 'AM', uf_ref == 14, 'RR',
    uf_ref == 15, 'PA', uf_ref == 16, 'AP', uf_ref == 17, 'TO', uf_ref == 21, 'MA',
    uf_ref == 22, 'PI', uf_ref == 23, 'CE', uf_ref == 24, 'RN', uf_ref == 25, 'PB',
    uf_ref == 26, 'PE', uf_ref == 27, 'AL', uf_ref == 28, 'SE', uf_ref == 29, 'BA',
    uf_ref == 31, 'MG', uf_ref == 32, 'ES', uf_ref == 33, 'RJ', uf_ref == 35, 'SP',
    uf_ref == 41, 'PR', uf_ref == 42, 'SC', uf_ref == 43, 'RS', uf_ref == 50, 'MS',
    uf_ref == 51, 'MT', uf_ref == 52, 'GO', uf_ref == 53, 'DF'
  )]

  dt[, `:=` (
    setor = fcase(v1022 == 1, 'U', default = 'R'),
    area  = fcase(v1023 %in% c(1,2), 'RM', default = 'RM')
    )]
  
  dt[, `:=` (
    domicilioid  = paste0(upa, v1008, v1014),
    pessoas_dom  = as.double(v2001),
    sexo         = fcase(v2007 == 1, 'H', v2007 == 2, 'M'),
    raca         = fcase(v2010 == 1, 'Br',
                         v2010 == 2, 'Pr',
                         v2010 == 3, 'Pd',
                         v2010 == 4, 'Am',
                         v2010 == 5, 'In',
                         default  =  'Nd'),
    idade        = as.double(v2009),
    num_familia  = NA_real_,
    familiaid    = NA_character_
  )]
  
  data.table::setnames(dt, 'v1032', 'peso')
  
  # CONDICAO NA FAMILIA (recode de V2005) ----
  dt[, pessoas_fam := sum(!v2005 %in% c(16,17,18,19), na.rm = TRUE), by = domicilioid]
  
  dt[, cond_familia := fcase(
    v2005 == 1,              1,   # pessoa de referência
    v2005 %in% c(2,3),       2,   # cônjuge
    v2005 %in% c(4,5),       3,   # filho
    v2005 %between% c(6,19), 4    # outros
  )]
  
  # RPC ----
  dt[, rpc := round(vd5007 / pessoas_fam, 2)]
  
  # ARRANJO FAMILIAR ----
  dt[, `:=` (
    sexo_chefe  = sexo[cond_familia == 1][1],
    tem_conjuge = fifelse(any(cond_familia == 2, na.rm = T), 'S', 'N'),
    tem_filhos  = fifelse(any(cond_familia == 3, na.rm = T), 'S', 'N'),
    agregados   = fifelse(any(cond_familia == 4, na.rm = T), 'S', 'N')
  ), by = domicilioid]
  
  dt[, `:=` (
    arranjo_familiar = paste0(sexo_chefe, tem_conjuge, tem_filhos),
    convivencia      = 'U'   # PNADC não tem subfamílias
  )]
  
  dt[, domicilioid := as.character(domicilioid)]
  
  
  ## ATRIBUTOS DO MPI-LA ==========================================================================
  
  # GRUPO D - MORADIA ----
  dt[, `:=`(
    d1a = fcase(
      is.na(s01002),                       NA_real_,
      s01002 %in% c(1,4),                  0,
      s01002 == 2,                         0.5,
      s01002 %in% c(3,5,6),                1
    ),
    d1b = fcase(
      is.na(s01004),                       NA_real_,
      s01004 %in% c(1,2,3),                0,
      s01004 %in% c(4,5),                  1
    ),
    d1c = fcase(
      is.na(s01003),                       NA_real_,
      s01003 %in% c(1,2,3,4,5),            0,
      s01003 == 6,                         1
    )
  )]
  
  dt[, D1 := {
    vals <- cbind(d1a, d1b, d1c)
    res  <- rowMeans(vals, na.rm = T)
    res[is.nan(res)] <- NA_real_
    res
  }]
  
  dt[, D2 := {
    comodos <- as.integer(s01005)
    razao   <- pessoas_dom / comodos
    fcase(
      is.na(comodos) | comodos > 30 | comodos <= 0 | is.na(pessoas_dom) | pessoas_dom <= 0, NA_real_,
      razao >= 3, 1,
      razao >= 2, 0.5,
      default = 0
    )
  }]
  
  dt[, D3 := fcase(
    is.na(s01017),                NA_real_,
    s01017 %in% c(1,2,3),         0,
    s01017 %in% c(4,5,6,7),       1
  )]
  
  
  # GRUPO B - SERVICOS BASICOS ----
  dt[, B1 := fcase(
    is.na(s01007) | is.na(s01010),                               NA_real_,
    setor == 'U' & s01007 == 1 & s01010 == 1,                    0,
    setor == 'U' & s01007 == 1 & s01010 %in% c(2,3),             0.5,
    setor == 'U' & s01007 >= 2,                                  1,
    setor == 'R' & s01007 %in% c(1,2,3,4) & s01010 == 1,         0,
    setor == 'R' & s01007 %in% c(1,2,3,4) & s01010 %in% c(2,3),  0.5,
    setor == 'R' & s01007 >= 5,                                  1,
    default = 1
  )]
  
  dt[, `:=` (
    ban_ref = if ('s01011a' %in% names(dt)) as.double(dt[['s01011a']]) else as.double(dt[['s01011']]),
    esgoto_ref = if ('s01012a' %in% names(dt)) as.double(dt[['s01012a']]) else as.double(dt[['s01012']])
  )]
  
  dt[, `:=` (
    B2 = fcase(
      setor == 'U' & ban_ref > 0 & esgoto_ref == 1,                               0,
      setor == 'U' & (ban_ref == 0 | (esgoto_ref > 1 | is.na(esgoto_ref))),       1,
      setor == 'R' & ban_ref > 0 & esgoto_ref %in% c(1,2,3),                      0,
      setor == 'R' & (ban_ref == 0 | (esgoto_ref > 3 | is.na(esgoto_ref))),       1,
      default = 1
    ),
    B3 = fcase(
      is.na(s01014),  NA_real_,
      s01014 == 1,    0,
      s01014 == 2,    1
    ),
    B4 = fcase(
      is.na(s01013),         NA_real_,
      s01013 %in% c(1,2),    0,
      default = 1
    ),
    B5 = -1 # Indisponivel na PNADC
  )]
  
  
  # GRUPO V - PADRAO DE VIDA ----
  dt[, V1 := fcase(
    is.na(rpc) | pessoas_fam <= 0, NA_real_,
    rpc < 0.5 * valor,             1,
    rpc < valor,                   0.5,
    rpc >= valor,                  0
  )]
  
  dt[, `:=` (
    v2a = fcase(
      is.na(s010311) & is.na(s010312),  NA_real_,
      s010311 == 1 | s010312 == 1,      0,
      default = 1
    ),
    v2b = fcase(
      is.na(s01023),         NA_real_,
      s01023 %in% c(1,2),   0,
      default = 1
    ),
    v2c = fcase(
      is.na(s01024),         NA_real_,
      s01024 == 1,           0,
      default = 1
    )
  )]
  
  dt[, V2 := fcase(
    is.na(v2a) & is.na(v2b) & is.na(v2c), NA_real_,
    v2a == 0  & v2b == 0  & v2c == 0,     0,
    default = 1
  )]
  
  
  # GRUPO E - EDUCACIONAL ----
  dt[, E1 := fcase(
    !idade %between% c(6,17), 0,
    is.na(v3002),             1,
    v3002 == 2,               1,
    v3002 == 1,               0,
    default = NA_real_
  )]
  
  dt[, E2 := fcase(
    !idade %between% c(6,17),                             0,
    is.na(as.double(vd3005)),                             1,
    as.double(vd3005) == 16,                              0,
    ((idade - 5) - (as.double(vd3005) - 1)) <= 1,           0,
    ((idade - 5) - (as.double(vd3005) - 1)) >  1,           1,
    default = NA_real_
  )]
  
  dt[, `:=` ( #Ensino Fundamental de 9 anos
    E3a = fcase(
      idade %between% c(18,59) & (is.na(vd3005) | as.integer(vd3005) < 9),   1,
      idade %between% c(18,59) & as.integer(vd3005) >= 9,                    0,
      default = NA_real_
    ),
    E3b = fcase(
      idade >= 60 & (is.na(vd3005) | as.integer(vd3005) < 5),   1,
      idade >= 60 & as.integer(vd3005) >= 5,                    0,
      default = NA_real_
    )
  )]
  
  dt[, E3 := fcase(
    E3a == 1 | E3b == 1, 1,
    E3a == 0 | E3b == 0, 0
  )]
  
  
  # GRUPO P - PROTECAO SOCIAL ----
  dt[, P1 := fcase(
    !idade %between% c(15,65),                                                       0,
    is.na(vd4001) & is.na(vd4002) & is.na(v4029) & is.na(v4012),                     NA_real_,
    vd4001 == 1 & vd4002 == 1 & v4029 == 1,                                          0,
    vd4001 == 1 & vd4002 == 1 & (v4029 == 2 | v4012 %in% c(6,7) | !is.na(v40121)),   0.5,
    vd4001 != 1 | vd4002 != 1,                                                       1,
    default = 1
  )]
  
  dt[, P2 := fcase(
    idade < 16,                    0,
    vd4012 == 1 | v5004a == 1,     0,
    default = 1
  )]
  
  dt[, escol := fcase(
    idade %between% c(6,17), as.integer(vd3005-1), 
    default                = as.integer(vd3005)
  )]
  
  
  ## SELECT FINAL =================================================================================
  
  dt[, .(ano, uf, setor, area, domicilioid, peso, pessoas_dom, pessoas_fam,
         arranjo_familiar, agregados, cond_familia, 
         sexo, raca, idade, escol, rpc,
         D1, D2, D3, B1, B2, B3, B4, B5, V1, V2, E1, E2, E3, P1, P2)]
}
