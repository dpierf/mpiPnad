#Salario Minimo, ano a ano
.sm_pnad <- data.table::data.table(
  ano = 1981:2015,
  valor = as.double(c(8464.80, 16608, 34776, 97176, 333120, 804, 2400, 18960, 249.48, 6056.31, 42000, 522186.90, 9606,
                      70, 100, 112, 120, 130, 136, 151, 180, 200, 240, 260, 300, 350, 380, 415, 465, 510, 545, 622, 678, 724, 788))
)

#' Prepara os indicadores intermediarios para calculo do MPI-LA a nivel individual - PNAD anual (ate 2015)
#' @param df Banco de dados, criado pela funcao `process_pnadA`, ja filtrado com atributos
#' @return data.table processado, com indicadores intermediarios
#' @export

rowcalc_pnadA <- function(df) {

  dt <- data.table::as.data.table(df)

  safe_col <- function(nm) {
    if (nm %in% names(dt)) {
      v <- dt[[nm]]
      if (is.numeric(v)) as.double(v) else v
    } else NA_real_
  }

  # JOIN SALARIO MINIMO ----
  dt <- merge(dt, data.table::as.data.table(.sm_pnad), by = 'ano', all.x = T)


  ## TRATAMENTOS INICIAIS =========================================================================

  # FILTRO DE RESPOSTA ----
  dt[, resposta := fcase(
    ano %between% c(1981,1990), safe_col('v0107'),
    ano %between% c(1992,2015), safe_col('v0104')
  )]

  dt <- dt[(ano %between% c(1981,1990) & !is.na(resposta)) | (ano %between% c(1992,2015) & resposta == 1)]

  # VARIAVEIS INTERMEDIARIAS ----
  dt[, `:=` (
    peso = fcase( #Peso do domicilio
      ano %between% c(1981,1989), safe_col('v9981'),
      ano == 1990,                safe_col('v1091'),
      ano %between% c(1992,2015), safe_col('v4729')
    ),
    sexo_ref = fcase(
      ano %between% c(1981,1990), safe_col('v0303'),
      ano %between% c(1992,2015), safe_col('v0302')
    ),
    cor_ref = fcase(
      ano == 1982,                safe_col('v6302'),
      ano == 1986,                safe_col('v2201'),
      ano %between% c(1987,1990), safe_col('v0304'),
      ano %between% c(1992,2015), safe_col('v0404') 
    ),
    uf_ref = fcase(
      ano %between% c(1981,1990), safe_col('v0010'),
      ano %between% c(1992,2015), safe_col('uf')
    ),
    local1 = fcase(
      ano %between% c(1981,1990), safe_col('v0003'),
      ano %between% c(1992,2015), safe_col('v4105')
    ),
    local2 = fcase(
      ano %between% c(1981,1990), safe_col('v0005'),
      ano %between% c(1992,2015), safe_col('v4107')
    ),
    num_familia = fcase(
      ano %between% c(1981,1990), safe_col('v0307'),
      ano %between% c(1992,2015), safe_col('v0403')
    ),
    cond_familia = fcase(
      ano %between% c(1981,1990), safe_col('v0306'),
      ano %between% c(1992,2015), safe_col('v0402')
    ),
    pessoas_dom = fcase(
      ano %between% c(1981,1990), safe_col('v0107'),
      ano %between% c(1992,2015), safe_col('v0105')
    ),
    comodos = fcase(
      ano %between% c(1981,1990), safe_col('v0211'),
      ano %between% c(1992,2015), safe_col('v0205')
    ),
    telhado = fcase(
      ano %between% c(1981,1990), safe_col('v0205'),
      ano %between% c(1992,2015), safe_col('v0204')
    ),
    posse = fcase(
      ano %between% c(1981,1990), safe_col('v0212'),
      ano %between% c(1992,2015), safe_col('v0207')
    ),
    luz = fcase(
      ano %between% c(1981,1990), safe_col('v0210'),
      ano %between% c(1992,2015), safe_col('v0219')
    ),
    lixo = fcase(
      ano %between% c(1981,1990), safe_col('v0209'),
      ano %between% c(1992,2015), safe_col('v0218')
    ),
    agua = fcase(
      ano %between% c(1981,1990), safe_col('v0214'),
      ano %between% c(1992,2015), safe_col('v0224')
    ),
    renda_raw = fcase(
      ano %between% c(1981,1990), as.numeric(safe_col('v0410')),
      ano %between% c(1992,2015), as.numeric(safe_col('v4614'))
    ),
    geladeira = fcase(
      ano %between% c(1981,1990), safe_col('v0216'),
      ano %between% c(1992,2015), safe_col('v0228')
    ),
    fogao = fcase(
      ano %between% c(1981,1990), safe_col('v0207'),
      ano %between% c(1992,2015), pmin(safe_col('v0221'), safe_col('v0222'), na.rm = T)
    ),
    idade = fcase(
      ano %between% c(1981,1990), safe_col('v0805'),
      ano %between% c(1992,2015), safe_col('v8005')
    ),
    freq = fcase(
      ano %between% c(1981,1990), safe_col('v0314'),
      ano %between% c(1992,2015), safe_col('v0602')
    ),
    anos = fcase(
      ano %between% c(1981,1990), safe_col('v0318'),
      ano %between% c(1992,2006), safe_col('v4703'),
      ano %between% c(2007,2015), safe_col('v4803')
    ),
    ocup = fcase(
      ano == 2001,                safe_col('v4755'),
      ano %between% c(1981,1990), safe_col('v0501'),
      ano %between% c(1992,2006), safe_col('v4705'),
      ano %between% c(2007,2015), safe_col('v4805')
    ),
    remun = fcase(
      ano == 2001,                safe_col('v4756'),
      ano %between% c(1981,1990), safe_col('v0505'),
      ano %between% c(1992,2015), safe_col('v4706')
    ),
    ativo = fcase(
      ano == 2001,                safe_col('v4754'),
      default =                   safe_col('v4704')
    ),
    inss = fcase(
      ano == 2001,                safe_col('v4761'),
      ano %between% c(1981,1990), safe_col('v0511'),
      ano %between% c(1992,2015), safe_col('v4711')
    ),
    apos = fcase(
      ano %between% c(1981,1990), safe_col('v5280'),
      ano %between% c(1992,2015), safe_col('v9122')
    ),
    pens = fcase(
      ano %between% c(1981,1990), safe_col('v5281'),
      ano %between% c(1992,2015), safe_col('v9123')
    )
  )]

  # VARIAVEIS SOCIODEMOGRAFICAS ----
  dt[, `:=` (
    sexo = fcase(
      ano %between% c(1981,1990) & sexo_ref == 1, 'H',
      ano %between% c(1981,1990) & sexo_ref == 3, 'M',
      ano %between% c(1992,2015) & sexo_ref == 2, 'H',
      ano %between% c(1992,2015) & sexo_ref == 4, 'M'
    ),
    
    raca = fcase(
      ano %in% c(1981,1983,1984,1985),           'Nd', #Não determinado
      ano == 1982 & cor_ref == 1,                'Br',
      ano == 1982 & cor_ref == 3,                'Pr',
      ano == 1982 & cor_ref == 5,                'Pd',
      ano == 1982 & cor_ref == 7,                'Am',
      ano == 1982,                               'Nd',
          
      ano == 1986 & cor_ref == 2,                'Br',
      ano == 1986 & cor_ref == 4,                'Pr',
      ano == 1986 & cor_ref == 6,                'Pd',
      ano == 1986 & cor_ref == 8,                'Am',
      ano == 1986,                               'Nd',
      
      ano %between% c(1987,2015) & cor_ref == 8, 'Am',
      ano %between% c(1987,2015) & cor_ref == 2, 'Br',
      ano %between% c(1987,2015) & cor_ref == 4, 'Pr',
      ano %between% c(1987,2015) & cor_ref == 6, 'Pd',
      ano %between% c(1987,2015) & cor_ref == 0, 'In',
      ano %between% c(1987,2015),                'Nd'
    ),

    uf = fcase(
      (ano %between% c(1981,1990) & uf_ref == 71) | (ano %between% c(1992,2015) & uf_ref == 11),            'RO',
      (ano %between% c(1981,1990) & uf_ref == 72) | (ano %between% c(1992,2015) & uf_ref == 12),            'AC',
      (ano %between% c(1981,1990) & uf_ref == 73) | (ano %between% c(1992,2015) & uf_ref == 13),            'AM',
      (ano %between% c(1981,1990) & uf_ref == 74) | (ano %between% c(1992,2015) & uf_ref == 14),            'RR',
      (ano %between% c(1981,1990) & uf_ref == 75) | (ano %between% c(1992,2015) & uf_ref == 15),            'PA',
      (ano %between% c(1981,1990) & uf_ref == 76) | (ano %between% c(1992,2015) & uf_ref == 16),            'AP',
       ano %between% c(1992,2015) & uf_ref == 17,                                                           'TO',
      (ano %between% c(1981,1990) & uf_ref == 51) | (ano %between% c(1992,2015) & uf_ref == 21),            'MA',
      (ano %between% c(1981,1990) & uf_ref == 52) | (ano %between% c(1992,2015) & uf_ref == 22),            'PI',
      (ano %between% c(1981,1990) & uf_ref == 53) | (ano %between% c(1992,2015) & uf_ref == 23),            'CE',
      (ano %between% c(1981,1990) & uf_ref == 54) | (ano %between% c(1992,2015) & uf_ref == 24),            'RN',
      (ano %between% c(1981,1990) & uf_ref == 55) | (ano %between% c(1992,2015) & uf_ref == 25),            'PB',
      (ano %between% c(1981,1990) & uf_ref == 56) | (ano %between% c(1992,2015) & uf_ref == 26),            'PE',
      (ano %between% c(1981,1990) & uf_ref == 57) | (ano %between% c(1992,2015) & uf_ref == 27),            'AL',
      (ano %between% c(1981,1990) & uf_ref == 58) | (ano %between% c(1992,2015) & uf_ref == 28),            'SE',
      (ano %between% c(1981,1990) & uf_ref %in% c(59,60))   | (ano %between% c(1992,2015) & uf_ref == 29),  'BA',
      (ano %between% c(1981,1990) & uf_ref %in% c(41,42,44))| (ano %between% c(1992,2015) & uf_ref == 31),  'MG',
      (ano %between% c(1981,1990) & uf_ref == 43)            | (ano %between% c(1992,2015) & uf_ref == 32), 'ES',
      (ano %between% c(1981,1990) & uf_ref %between% c(11,14))|(ano %between% c(1992,2015) & uf_ref == 33), 'RJ',
      (ano %between% c(1981,1990) & uf_ref %between% c(20,29))|(ano %between% c(1992,2015) & uf_ref == 35), 'SP',
      (ano %between% c(1981,1990) & uf_ref %in% c(30,31,37)) |(ano %between% c(1992,2015) & uf_ref == 41),  'PR',
      (ano %between% c(1981,1990) & uf_ref == 32)            | (ano %between% c(1992,2015) & uf_ref == 42), 'SC',
      (ano %between% c(1981,1990) & uf_ref %between% c(33,35))|(ano %between% c(1992,2015) & uf_ref == 43), 'RS',
      (ano %between% c(1981,1990) & uf_ref == 81)            | (ano %between% c(1992,2015) & uf_ref == 50), 'MS',
      (ano %between% c(1981,1990) & uf_ref == 82)            | (ano %between% c(1992,2015) & uf_ref == 51), 'MT',
      (ano %between% c(1981,1990) & uf_ref == 83)            | (ano %between% c(1992,2015) & uf_ref == 52), 'GO',
      (ano %between% c(1981,1990) & uf_ref == 61)            | (ano %between% c(1992,2015) & uf_ref == 53), 'DF'
    ),

    setor = fcase(
      ano %between% c(1981,1990) & local1 %in% c(1,3),   'U',
      ano %between% c(1992,2015) & local1 %in% c(1,2,3), 'U',
      default = 'R'
    ),
    
    area = fcase(
      local2 == 1, 'RM',
      local2 != 1, 'UF'
    ),

    familiaid = paste(domicilioid, num_familia, sep = '_')
  )]

  # ARRANJO FAMILIAR E COMPOSICAO DO DOMICILIO ----
  dt[, pessoas_fam := sum(!cond_familia %in% c(6, 7, 8), na.rm = TRUE), by = domicilioid]

  dt[, `:=` (
    sexo_chefe  = sexo[cond_familia == 1][1],
    tem_conjuge = fifelse(any(cond_familia == 2, na.rm = T), 'S', 'N'),
    tem_filhos  = fifelse(any(cond_familia == 3, na.rm = T), 'S', 'N'),
    agregados   = fifelse(any(cond_familia > 4,  na.rm = T), 'S', 'N')
  ), by = domicilioid]

  dt[, arranjo_familiar := paste0(sexo_chefe, tem_conjuge, tem_filhos)]
  dt[, n_familias_dom   := uniqueN(familiaid), by = domicilioid]
  dt[, convivencia      := fcase(
    n_familias_dom == 1,                   'U',
    n_familias_dom > 1 & num_familia == 1, 'P',
    n_familias_dom > 1 & num_familia != 1, 'S'
  )]

  dt[, `:=` (
    domicilioid = as.character(domicilioid),
    familiaid = as.character(familiaid)
    )]


  ## ATRIBUTOS DO MPI-LA ==========================================================================

  # GRUPO D - MORADIA ----
  dt[, `:=`(
    d1a = fcase(
      is.na(safe_col('v0203')) | safe_col('v0203') == 9,            NA_real_,
      ano %between% c(1981,1990) & safe_col('v0203') %in% c(0,2),   0,
      ano %between% c(1992,2015) & safe_col('v0203') %in% c(1,2),   0,
      default = 1
    ),
    d1b = fcase(
      ano > 1990,                                                    NA_real_,
      is.na(safe_col('v0204')) | safe_col('v0204') == 9,             NA_real_,
      safe_col('v0204') %in% c(1,3,5),                               0,
      default = 1
    ),
    d1c = fcase(
      is.na(telhado) | telhado == 9,                                NA_real_,
      ano %between% c(1981,1990) & telhado %in% c(0,2,4,6),         0,
      ano %between% c(1992,2015) & telhado %in% c(1,2,3,4),         0,
      default = 1
    )
  )]

  dt[, D1 := {
    vals <- cbind(d1a, d1b, d1c)
    res  <- rowMeans(vals, na.rm = T)
    res[is.nan(res)] <- NA_real_
    res
  }]

  dt[, D2 := {
    razao <- pessoas_dom / comodos
    fcase(
      is.na(comodos) | comodos == 99 | comodos <= 0 | is.na(pessoas_dom) | pessoas_dom <= 0, NA_real_,
      razao >= 3, 1,
      razao >= 2, 0.5,
      default = 0
    )
  }]

  dt[, D3 := fcase(
    is.na(posse) | posse == 9,                        NA_real_,
    ano %between% c(1981,1990) & posse %in% c(0,2,4), 0,
    ano %between% c(1992,2015) & posse %in% c(1,2,3), 0,
    default = 1
  )]

  # GRUPO B - SERVICOS BASICOS ----
  dt[, B1 := fcase(
    ano %between% c(1981,1990) & (is.na(safe_col('v0206')) | safe_col('v0206') == 9),                               NA_real_,
    ano %between% c(1981,1990) & safe_col('v0206') == 1          & setor == 'U',                                    0,
    ano %between% c(1981,1990) & safe_col('v0206') %in% c(1,2)  & setor == 'R',                                     0,
    ano %between% c(1981,1990) & safe_col('v0206') == 4          & setor == 'U',                                    0.5,
    ano %between% c(1981,1990) & safe_col('v0206') %in% c(4,5)  & setor == 'R',                                     0.5,
    ano %between% c(1981,1990),                                                                                     1,
    ano %between% c(1992,2015) & (is.na(safe_col('v0212')) | safe_col('v0212') == 9),                               NA_real_,
    ano %between% c(1992,2015) & safe_col('v0212') == 2          & safe_col('v0211') == 1 & setor == 'U',           0,
    ano %between% c(1992,2015) & safe_col('v0212') %in% c(2,4)  & safe_col('v0211') == 1 & setor == 'R',            0,
    ano %between% c(1992,2015) & safe_col('v0212') == 2          & safe_col('v0211') == 3 & setor == 'U',           0.5,
    ano %between% c(1992,2015) & safe_col('v0212') %in% c(2,4)  & safe_col('v0211') == 3 & setor == 'R',            0.5,
    ano %between% c(1992,2015),                                                                                     1
  )]

  dt[, B2 := fcase(
    ano %between% c(1981,1990) &
      (is.na(safe_col('v0207')) | safe_col('v0207') == 9) &
      (is.na(safe_col('v0208')) | safe_col('v0208') == 9),                        NA_real_,
    ano %between% c(1981,1990) & safe_col('v0207') == 0 & setor == 'U',           0,
    ano %between% c(1981,1990) & safe_col('v0207') %in% c(0,2)  & setor == 'R',   0,
    ano %between% c(1981,1990),                                                   1,
    ano %between% c(1992,2015) &
      (is.na(safe_col('v0215')) | safe_col('v0215') == 9) &
      (is.na(safe_col('v0216')) | safe_col('v0216') == 9) &
      (is.na(safe_col('v0217')) | safe_col('v0217') == 9),                        NA_real_,
    ano %between% c(1992,2015) & safe_col('v0215') == 1 &
      safe_col('v0216') == 2 & safe_col('v0217') == 1 & setor == 'U',             0,
    ano %between% c(1992,2015) & safe_col('v0215') == 1 &
      safe_col('v0216') == 2 & safe_col('v0217') %in% c(1,2,3) & setor == 'R',    0,
    ano %between% c(1992,2015),                                                   1
  )]

  dt[, `:=` (
    B3 = fcase(
      is.na(luz) | luz == 9, NA_real_,
      luz == 1,              0,
      default = 1
    ),
    B4 = fcase(
      is.na(lixo) | lixo == 9,                       NA_real_,
      ano %between% c(1981,1990) & lixo == 0,         0,
      ano %between% c(1992,2015) & lixo %in% c(1,2), 0,
      default = 1
    ),
    B5 = fcase(
      is.na(agua) | agua == 9,                        NA_real_,
      ano %between% c(1981,1990) & agua == 1,         0,
      ano %between% c(1992,2015) & agua == 2,         0,
      default = 1
    )
  )]

  # GRUPO V - PADRAO DE VIDA ----
  dt[, renda := fcase(
    ano %between% c(1981,1984) & (is.na(renda_raw) | renda_raw == 9999999      | renda_raw < 0), NA_real_,
    ano %between% c(1985,1990) & (is.na(renda_raw) | renda_raw == 999999999    | renda_raw < 0), NA_real_,
    ano %between% c(1992,2015) & (is.na(renda_raw) | renda_raw == 999999999999 | renda_raw < 0), NA_real_,
    default = as.double(renda_raw)
  )]

  dt[, rpc := round(renda / pessoas_fam, 2)]

  dt[, V1 := fcase(
    is.na(rpc) | pessoas_fam <= 0, NA_real_,
    rpc < 0.5 * valor,             1,
    rpc < valor,                   0.5,
    rpc >= valor,                  0
  )]

  dt[, `:=`(
    v2a = fcase(
      is.na(fogao) | fogao == 9,                      NA_real_,
      ano %between% c(1981,1990) & fogao == 2,        0,
      ano %between% c(1992,2015) & fogao %in% c(1,2), 0,
      default = 1
    ),
    v2b = fcase(
      is.na(geladeira) | geladeira == 9,                      NA_real_,
      ano %between% c(1981,1990) & geladeira == 1,            0,
      ano %between% c(1992,2015) & geladeira %in% c(2,4),     0,
      default = 1
    ),
    v2c = fcase(
      ano <= 1990,                                            NA_real_,
      is.na(safe_col('v0230')) | safe_col('v0230') == 9,      NA_real_,
      safe_col('v0230') == 2,                                 0,
      default = 1
    )
  )]

  dt[, V2 := fcase(
    ano %between% c(1981,1990) & is.na(v2a) & is.na(v2b),                NA_real_,
    ano %between% c(1981,1990) & v2a == 0 & v2b == 0,                    0,
    ano %between% c(1981,1990),                                          1,
    ano %between% c(1992,2015) & is.na(v2a) & is.na(v2b) & is.na(v2c),   NA_real_,
    ano %between% c(1992,2015) & v2a == 0 & v2b == 0 & v2c == 0,         0,
    ano %between% c(1992,2015),                                          1
  )]

  # GRUPO E - EDUCACIONAL ----
  dt[, E1 := fcase(
    !idade %between% c(6,17),                          0,
    ano %between% c(1981,1990) & freq %in% c(0,99),    1,
    ano %between% c(1981,1990) & freq %in% 1:15,       0,
    ano %between% c(1981,1990),                        NA_real_,
    ano %between% c(1992,2015) & freq == 4,            1,
    ano %between% c(1992,2015) & freq == 2,            0,
    ano %between% c(1992,2015),                        NA_real_
  )]

  dt[, E2 := fcase(
    !idade %between% c(6,17),                                               0,
    ano %between% c(1981,1990) & (anos %in% c(12,13) | is.na(anos)),        1,
    ano %between% c(1981,1990) & anos %in% c(10,11),                        0,
    ano %between% c(1981,1990) & ((idade - 5) - (anos - 1)) <= 1,           0,
    ano %between% c(1981,1990) & ((idade - 5) - (anos - 1)) >  1,           1,
    ano %between% c(1981,1990),                                             NA_real_,
    ano %between% c(1992,2015) & anos == 17,                                1,
    ano %between% c(1992,2015) & ((idade - 5) - (anos - 1)) <= 1,           0,
    ano %between% c(1992,2015) & ((idade - 5) - (anos - 1)) >  1,           1,
    ano %between% c(1992,2015),                                             NA_real_
  )]

  dt[, `:=`(
    E3a = fifelse(idade %between% c(18,59) & !is.na(anos) & anos < 9, 1, 0),
    E3b = fifelse(idade >= 60              & !is.na(anos) & anos < 5, 1, 0)
  )]

  dt[, E3 := fcase(
    E3a == 1L | E3b == 1L, 1,
    E3a == 0L & E3b == 0L, 0
  )]

  # GRUPO P - PROTECAO SOCIAL ----
  dt[, P1 := fcase(
    !idade %between% c(15,65),                                                              0,
    ano %between% c(1981,1990) & (ocup == 9 | is.na(ocup)),                                 NA_real_,
    ano %between% c(1981,1990) & ocup %in% c(1,2) & remun %in% c(1,7),                      0,
    ano %between% c(1981,1990) & ocup %in% c(1,2) & remun %in% c(2,3,4,5,6,8),              0.5,
    ano %between% c(1981,1990) & ocup %in% c(1,2) & remun == 0,                             1,
    ano %between% c(1981,1990) & ocup %in% c(3,4,5,6,7),                                    1,
    ano %between% c(1981,1990),                                                             1,
    ano %between% c(1992,2015) & is.na(ocup) & is.na(ativo) & (remun == 14 | is.na(remun)), NA_real_,
    ano %between% c(1992,2015) & ativo == 1 & ocup == 1 & remun %in% c(1,2,3,6,10),         0,
    ano %between% c(1992,2015) & ativo == 1 & ocup == 1 & remun %in% c(4,5,7,8,9,11,12),    0.5,
    ano %between% c(1992,2015) & (ativo == 2 | ocup == 2 | remun == 13),                    1,
    ano %between% c(1992,2015),                                                             1
  )]

  dt[, P2 := fcase(
    idade < 16,                                                        0,
    ano %between% c(1981,1990) & (inss == 1 | apos == 1 | pens == 2),  0,
    ano %between% c(1981,1990),                                        1,
    ano %between% c(1992,2015) & (inss == 1 | apos == 2 | pens == 1),  0,
    ano %between% c(1992,2015),                                        1
  )]
  
  dt[, escol := anos - 1]


  ## SELECT FINAL =================================================================================

  dt[, .(ano, uf, setor, area, domicilioid, peso, pessoas_dom, pessoas_fam,
         arranjo_familiar, agregados, cond_familia, 
         sexo, raca, idade, escol, rpc,
         D1, D2, D3, B1, B2, B3, B4, B5, V1, V2, E1, E2, E3, P1, P2)]
}
