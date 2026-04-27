.vars_pnadC <- c(
  'UF','V1023','V1022','V1032','V1008','UPA','V1014','V2001','VD2003','V2005',
  'V2007','V2009','V2010','S01017','S01002','S01003','S01004','S01005','S01007',
  'S01010','S01011','S01011A','S01011B','S01012','S01012A','S01013','S01014',
  'S010141','VD5007','S01023','S01024','S010311','S010312','V3002','VD3005',
  'VD4001','VD4002','VD4007','V4029','V5004A','VD4012','V4012','V40121'
)


#' Processa microdados da PNAD Continua (a partir de 2015)
#' @param file_path Caminho logico onde se encontram o(s) arquivo(s)
#' @param proc_dir Nome do diretorio onde serao salvos os arquivos anuais processados (em Parquet)
#' @param out_dir Nome do diretorio onde serao salvos os arquivos anuais (em Parquet) com os atributos necessarios
#' @param vars Lista de atributos que serao extraidos dos arquivos da PNAD Continua
#' @param verbose Booleano (default = TRUE) mostrando a evolucao da funcao
#' @return data.table processado
#' @export

process_pnadC <- function(
    file_path,
    proc_dir = 'data/02_processed',
    out_dir  = 'data/03_filtered',
    vars     = .vars_pnadC,
    verbose  = T
){

  # ETAPA 1 - Importacao de dados brutos ================================================

  # Deteccao do ano
  ano <- as.integer(stringr::str_extract(basename(file_path), '\\d{4}(?=_visita)'))

  fs::dir_create(dirname(file_path))

  # Leitura e download dos dados do IBGE
  if (verbose) message('Lendo bases do ano ', ano)
  df_raw <- PNADcIBGE::get_pnadc(
    year      = ano,
    interview = 1,
    labels    = F,
    design    = F,
    deflator  = F,
    reload    = F,
    savedir   = dirname(file_path)
  )

  # Conversao para data.table e padronizacao de nomes
  dt_full <- data.table::as.data.table(df_raw)
  data.table::setnames(dt_full, tolower(names(dt_full)))


  # ETAPA 2 - Geracao de arquivo unico em Parquet =======================================

  # Criacao do diretorio de destino
  fs::dir_create(proc_dir)
  proc_path <- fs::path(proc_dir, paste0('pnadc_', ano, '.parquet'))

  arrow::write_parquet(dt_full, proc_path)
  if (verbose) message('Base raw (parquet) de ', ano, ' salva com sucesso')


  # ETAPA 3 - Filtragem de atributos para criacao de MPI ================================

  # Criacao do diretorio de output
  fs::dir_create(out_dir)

  cols_sel <- intersect(tolower(vars), names(dt_full))
  dt_filt  <- dt_full[, ..cols_sel]
  dt_filt[, ano := ano]

  out_path <- fs::path(out_dir, paste0('pnadc_', ano, '_filtered.parquet'))

  arrow::write_parquet(dt_filt, out_path)

  if (verbose) message('Base filtrada de ', ano, ' salva com sucesso')

  # Exclusao de arquivos extraidos
  txt_extraido <- fs::path(dirname(file_path), glue::glue('PNADC_{ano}_visita1.txt'))
  if (fs::file_exists(txt_extraido)) fs::file_delete(txt_extraido)

  gc()
  invisible(dt_filt)
}
