.vars_pnadA <- list(
  '1981_1990' = c(
    'domicilioid','v0010','v0003','v0005','v0307','v0410','v9991','v3080','v9981',
    'v1091','v5281','v9971','v0201','v0202','v0305','v5010','v0205','v0206','v0207',
    'v0208','v0209','v0210','v6302','v0211','v0231','v0212','v0214','v0215','v0216',
    'v0218','v0303','v0217','v0805','v0304','v0311','v0312','v0314','v0315','v0317',
    'v0318','v0501','v0505','v0511','v5280','v0203','v0204','v0306','v0107','v2201'
  ),

  '1992_2015' = c(
    'domicilioid','uf','v4105','v4107','v0403','v0104','v4729','v4611','v4732','v4617',
    'v4618','v4614','v0201','v0202','v0203','v0211','v0212','v0213','v0214','v0217','v0215',
    'v0216','v0218','v0219','v0205','v0206','v0207','v0224','v0221','v0204','v4722','v0222',
    'v0228','v0226','v0227','v0302','v0401','v0402','v8005','v0404','v0601','v0602','v0606',
    'v0605','v0603','v0208','v0610','v0607','v4703','v4803','v4704','v4705','v4805','v4706',
    'v4754','v4755','v4756','v4711','v4761','v9122','v9123','v0105','v0230'
  )
)

#' Processa microdados da PNAD anual (até 2015)
#' @param file_path Caminho logico onde se encontram o(s) arquivo(s)
#' @param proc_dir Nome do diretorio onde serao salvos os arquivos anuais processados (em Parquet)
#' @param out_dir Nome do diretorio onde serao salvos os arquivos anuais (em Parquet) com os atributos necessarios
#' @param verbose Booleano (default = TRUE) mostrando a evolucao da funcao
#' @return data.table processado
#' @export

process_pnadA <- function(
    file_path,
    proc_dir   = 'data/02_processed',
    out_dir    = 'data/03_filtered',
    verbose    = T
){

  # ETAPA 1 - Importacao de dados brutos ================================================

  # Deteccao do ano
  ano <- as.integer(stringr::str_extract(basename(file_path), '\\d{4}'))

  # Mapeamento de variaveis por periodo
  get_vars <- function(ano) {
    if (ano %between% c(1981, 1991)) return(.vars_pnadA[['1981_1990']])
    if (ano %between% c(1992, 2015)) return(.vars_pnadA[['1992_2015']])
    stop('Ano fora do mapeamento: ', ano)
  }

  # Extracao e leitura
  if (verbose) message('Lendo bases do ano ', ano)

  tmp <- tempdir()
  archive::archive_extract(file_path, dir = tmp)
  folder <- file.path(tmp, paste0('PNAD ', ano))

  dom_file <- list.files(folder, pattern = 'pnad\\.dom', full.names = T, ignore.case = T)
  pes_file <- list.files(folder, pattern = 'pnad\\.pes', full.names = T, ignore.case = T)

  dom <- as.data.table(data.table::fread(dom_file))
  pes <- as.data.table(data.table::fread(pes_file))


  # Join
  df <- pes[dom, on = 'domicilioid']


  # ETAPA 2 - Geracao de arquivo unico em Parquet =======================================

  # Criacao do diretorio de destino
  fs::dir_create(proc_dir)
  raw_path <- file.path(proc_dir, paste0('pnad_', ano, '.parquet'))

  arrow::write_parquet(df, raw_path)
  if (verbose) message('Base raw (parquet) de ', ano, ' salva com sucesso')


  # ETAPA 3 - Filtragem de atributos para criacao de MPI ================================

  # Criacao do diretorio de output
  fs::dir_create(out_dir)
  filtered_path <- file.path(out_dir, paste0('pnad_', ano, '_filtered.parquet'))

  df |>
    dplyr::rename_with(tolower) |>
    dplyr::select(dplyr::any_of(get_vars(ano))) |>
    dplyr::mutate(ano = ano) |>
    arrow::write_parquet(filtered_path)

  if (verbose) message('Base filtrada de ', ano, ' salva com sucesso')

  gc()
  invisible(list(raw = raw_path, filtered = filtered_path))
}
