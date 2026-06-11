.sm <- data.table::data.table(
  ano      = 1981:2025,
  valor    = as.double(c(
    8464.80, 16608, 34776, 97176, 333120, 804, 2400, 18960,
    249.48, 6056.31, 42000, 522186.90, 9606, 70, 100, 112,
    120, 130, 136, 151, 180, 200, 240, 260, 300, 350, 380,
    415, 465, 510, 545, 622, 678, 724, 788, 880, 937, 954,
    998, 1045, 1100, 1212, 1320, 1412, 1518
  )),
  conversor = c(
    rep(1 / 2750000000000, 5),  # 1981-1985: Cruzeiro
    rep(1 / 2750000000,    3),  # 1986-1988: Cruzado
    rep(1 / 2750000,       4),  # 1989-1992: Cruzado Novo e Cruzeiro
    rep(1 / 2750,          1),  # 1993:      Cruzeiro Real
    rep(1,                32)   # 1994-2025: Real
  )
)

.get_sm_real_raw <- function() {
  rbcb::get_series(code = 433, start_date = '1981-01-01', end_date = '2025-12-01') |>
    data.table::as.data.table() |>
    data.table::setnames('433', 'value') |>
    dplyr::mutate(
      indice    = cumprod(1 + value / 100),
      deflator  = dplyr::last(indice) / indice,
      ano       = lubridate::year(date),
      mes       = lubridate::month(date)
    ) |>
    dplyr::filter((ano <= 2015 & mes == 9) | (ano >= 2016 & mes == 7)) |>
    dplyr::select(ano, deflator) |>
    dplyr::right_join(.sm, by = 'ano') |>
    dplyr::mutate(
      multiplicador = conversor * deflator,
      valor_real    = valor * multiplicador
    ) |>
    dplyr::select(ano, multiplicador, sm_nominal = valor, sm_real = valor_real)
}

.get_sm_real <- memoise::memoise(.get_sm_real_raw)

#' Calcula o MPI-LA para todos os anos da PNAD (anual ou continua) disponiveis
#' @param input_dir Caminho logico onde se encontram os arquivos anuais ja filtrados, que serao tratados pelas funcoes `rowcalc` e `aggcalc`
#' @param output_file Nome e local do diretorio onde sera salva a base empilhada (com nome padrao `pnad_completa.parquet`)
#' @param ks Valores de cutoff K utilizados para os calculos do binario de pobreza
#' @param anos Nulo por default, permite criar a base so com anos selecionados
#' @return data.table unico, com todos os anos empilhados, e todos os indicadores e cutoffs calculados
#' @export

create_mpi <- function(
    input_dir   = 'data/03_filtered',
    output_file = 'data/pnad_completa.parquet',
    ks          = c(0.10, 0.20, 0.25, 0.33, 0.40, 0.50),
    anos        = NULL
) {
  
  # Atalho: se os arquivos finais já existem, carrega e retorna ----
  dict_file <- 'data/mpi_dictionary.rds'
  if (fs::file_exists(output_file) && fs::file_exists(dict_file)) {
    cli::cli_alert_info('Arquivos encontrados: carregando sem reprocessar')
    return(list(
      mpi_pnad = arrow::read_parquet(output_file) |> data.table::setDT(),
      dicts = readRDS(dict_file)
    ))
    cli::cli_alert_success('Base completa e dicionário carregados no ambiente global')
    return(invisible(mpi_pnad))
  }
  
  cli::cli_alert_info('Arquivos não encontrados: criando objetos')
  
  plano_anterior    <- future::plan()
  handler_anterior  <- progressr::handlers()
  
  n_workers <- max(1, parallelly::availableCores() - 1)
  future::plan(future::multisession, workers = n_workers)
  progressr::handlers(progressr::handler_progress(
    format = '[:bar] :current/:total arquivos | :message | :eta restante'
  ))
  
  on.exit({
    future::plan(plano_anterior)
    progressr::handlers(handler_anterior)
  }, add = TRUE)
  
  sm_real <- .get_sm_real()
  
  cli::cli_h1('Carregando arquivos')
  
  arquivos <- fs::dir_ls(input_dir, glob = '*_filtered.parquet')
  
  if (length(arquivos) == 0) {
    stop('Nenhum arquivo .parquet encontrado em ', input_dir)
  }
  
  if (!is.null(anos)) {
    arquivos <- purrr::keep(
      arquivos,
      \(f) as.integer(stringr::str_extract(f, '\\d{4}')) %in% anos
    )
  }
  
  if (length(arquivos) == 0) {
    stop('Nenhum arquivo encontrado para os anos especificados')
  }
  
  cutoffs <- setNames(ks, paste0('pobre_k', ks * 100))
  
  progressr::with_progress({
    p <- progressr::progressor(along = arquivos)
    
    resultado <- furrr::future_map(arquivos, \(arquivo) {
      
      ano_arq  <- stringr::str_extract(arquivo, '\\d{4}') |> as.integer()
      is_pnadc <- stringr::str_detect(arquivo, 'pnadc')
      type_arq <- if (is_pnadc) 'continua' else 'anual'
      
      p(message = paste(ifelse(is_pnadc, 'PNADC', 'PNAD'), ano_arq))
      
      arrow::read_parquet(arquivo)          |>
        rowcalc_pnad(type = type_arq)       |>
        aggcalc_pnad(type = type_arq)       |>
        dplyr::mutate(
          domicilioid = as.character(domicilioid),
          !!!purrr::imap(cutoffs, \(k, nm) rlang::expr(as.integer(score >= !!k)))
        )
      
    }) |> data.table::rbindlist(fill = TRUE)
  })
  
  cli::cli_alert_success('Arquivos filtrados carregados com sucesso')
  
  cli::cli_h1('Preparando a base final')
  resultado <- resultado |>
    dplyr::left_join(sm_real, by = 'ano') |>
    dplyr::mutate(
      rpc_real = rpc * multiplicador,
      NP       = rpc_real >= sm_real,
      VP       = rpc_real >= sm_real / 2 & rpc_real < sm_real,
      PB       = rpc_real >= sm_real / 4 & rpc_real < sm_real / 2,
      EP       = rpc_real >= 0           & rpc_real < sm_real / 4
    )
  
  .mac <- c(AC=1L,AP=1L,AM=1L,PA=1L,RO=1L,RR=1L,TO=1L,
            AL=2L,BA=2L,CE=2L,MA=2L,PB=2L,PE=2L,PI=2L,RN=2L,SE=2L,
            ES=3L,MG=3L,RJ=3L,SP=3L,
            PR=4L,RS=4L,SC=4L,
            DF=5L,GO=5L,MS=5L,MT=5L)
  
  .rac <- c(Br=1L, Pr=2L, Pd=3L, Am=4L, In=5L, Nd=9L)
  
  resultado[, `:=` (
    sexo_dec    = data.table::fifelse(substr(arranjo_familiar,1,1)=='H', 1L, 2L),
    arranjo_dec = data.table::fcase(
      substr(arranjo_familiar,2,3)=='SS', 1L,  #Casal Com Filhos
      substr(arranjo_familiar,2,3)=='SN', 2L,  #Casal Sem Filhos
      substr(arranjo_familiar,2,3)=='NN', 3L,  #Domicilio Unipessoal
      substr(arranjo_familiar,2,3)=='NS', 4L), #Genitor Solteiro
    setor_dec   = data.table::fifelse(setor=='U', 1L, 2L),
    area_dec    = data.table::fifelse(area=='RM', 1L, 2L),
    regiao      = .mac[uf],
    raca        = .rac[raca],
    periodo     = data.table::fcase(
      ano %in% 1981:1993, 1L,
      ano %in% 1995:2002, 2L,
      ano %in% 2003:2007, 3L,
      ano %in% 2008:2014, 4L,
      ano %in% 2015:2018, 5L,
      ano %in% 2019:2025, 6L)
  )]
  resultado[, arranjo_full := arranjo_dec * 10L + sexo_dec]
  
  cols <- c('ano', 'periodo', 'regiao', 'uf', 'setor_dec', 'area_dec',
            'domicilioid', 'psu', 'strata', 'peso', 
            'pessoas_dom', 'pessoas_fam', 'arranjo_full', 'agregados',
            'sexo_dec', 'raca', 'idade', 'escol',
            'D1','D2','D3','B1','B2','B3','B4','V1','V2','E1','E2','E3','P1','P2','score',
            'pobre_k10','pobre_k20','pobre_k25','pobre_k33','pobre_k40','pobre_k50',
            'rpc','multiplicador','sm_real','rpc_real','NP','VP','PB','EP')
  
  resultado_final <- resultado[, ..cols]
  
  data.table::setkey(resultado_final, 
                     ano, periodo, regiao, uf, setor_dec, area_dec, sexo_dec, raca, arranjo_full)
  cli::cli_alert_success('Base final preparada com êxito')
  
  arrow::write_parquet(resultado_final, output_file)
  cli::cli_alert_success('Base final salva em data/')
  
  .dicts <- list(
    sexo_dec    = c('1'='Homem',              '2'='Mulher'),
    arranjo_dec = c('1'='Casal Com',          '2'='Casal Sem',
                    '3'='Unipessoal',         '4'='Monoparental'),
    setor_dec   = c('1'='Urbano',             '2'='Rural'),
    area_dec    = c('1'='Reg. Metropolitana', '2'='Resto da UF'),
    regiao      = c('1'='Norte',              '2'='Nordeste',
                    '3'='Sudeste',            '4'='Sul',    '5'='Centro-Oeste'),
    raca        = c('1'='Branco',             '2'='Preto',
                    '3'='Pardo',              '4'='Amarelo',
                    '5'='Indígena',           '9'='Não definido'),
    periodo     = c('1'='1981–1993',          '2'='1995–2002',
                    '3'='2003–2007',          '4'='2008–2014',
                    '5'='2015–2018',          '6'='2019–2025'),
    arranjo_full= c('11'='Casal Com: Homem',     '12'='Casal Com: Mulher',
                    '21'='Casal Sem: Homem',     '22'='Casal Sem: Mulher',
                    '31'='Unipessoal: Homem',    '32'='Unipessoal: Mulher',
                    '41'='Monoparental: Homem',  '42'='Monoparental: Mulher')
  )
  saveRDS(.dicts, 'data/mpi_dictionary.rds')
  cli::cli_alert_success('Dicionário salvo com sucesso')
  
  gc(verbose = FALSE)
  invisible(list(
    mpi_pnad = resultado_final,
    dicts    = .dicts
  ))
}
