# ── Dicionários de variáveis ──────────────────────────────────────────────────

.vars_pnadA <- list(
  '1981_1990' = c(
    'domicilioid','v0010','v0003','v0005','v0307','v0410','v9991','v3080','v9981','v0101',
    'v1091','v5281','v9971','v0201','v0202','v0305','v5010','v0205','v0206','v0207','v0102',
    'v0208','v0209','v0210','v6302','v0211','v0231','v0212','v0214','v0215','v0216','v0103',
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

.vars_pnadC <- c(
  'UF','V1023','V1022','V1032','V1008','UPA','Estrato','V1014','V2001','VD2003','V2005',
  'V2007','V2009','V2010','S01017','S01002','S01003','S01004','S01005','S01007',
  'S01010','S01011','S01011A','S01011B','S01012','S01012A','S01013','S01014',
  'S010141','VD5007','S01023','S01024','S010311','S010312','V3002','VD3005',
  'VD4001','VD4002','VD4007','V4029','V5004A','VD4012','V4012','V40121','VD4020'
)


# ── Helper interno ────────────────────────────────────────────────────────────

.get_vars_pnad <- function(type, ano) {
  if (type == 'continua') return(.vars_pnadC)
  if (ano %between% c(1981, 1991)) return(.vars_pnadA[['1981_1990']])
  if (ano %between% c(1992, 2015)) return(.vars_pnadA[['1992_2015']])
  stop('Ano fora do mapeamento para PNAD Anual: ', ano)
}


# ── Função principal ──────────────────────────────────────────────────────────

#' Processa microdados da PNAD (anual ou contínua)
#'
#' @param file_path Caminho lógico do arquivo bruto (.zip para anual; arquivo PNADC para contínua)
#' @param type Tipo de pesquisa: `'anual'` (1981–2015) ou `'continua'` (2015–).
#'   Se `NULL` (padrão), detectado automaticamente a partir do nome do arquivo.
#' @param proc_dir Diretório onde serão salvos os arquivos processados completos (Parquet)
#' @param out_dir  Diretório onde serão salvos os arquivos filtrados (Parquet)
#' @param vars     Vetor de variáveis a selecionar. `NULL` aplica o dicionário padrão do período
#' @param verbose  Booleano (default = `TRUE`) controlando mensagens de progresso
#' @return `data.table` filtrado, invisível
#' @export

process_pnad <- function(
    file_path,
    type     = NULL,
    proc_dir = 'data/02_processed',
    out_dir  = 'data/03_filtered',
    vars     = NULL,
    verbose  = TRUE
) {
  
  
  # ETAPA 0 - Detecção do tipo de pesquisa =====================================
  
  if (is.null(type)) {
    fname <- basename(file_path)
    type  <- dplyr::case_when(
      stringr::str_detect(fname, stringr::regex('PNADC_', ignore_case = TRUE)) ~ 'continua',
      stringr::str_detect(fname, stringr::regex('PNAD',   ignore_case = TRUE)) ~ 'anual',
      .default = NA_character_
    )
    if (is.na(type)) stop(
      'Nao foi possivel detectar o tipo de PNAD a partir do arquivo: ', fname,
      '\nPasse `type = "anual"` ou `type = "continua"` explicitamente.'
    )
  } else {
    type <- match.arg(type, c('anual', 'continua'))
  }
  
  
  # ETAPA 1 - Importação de dados brutos =======================================
  
  if (type == 'continua') {
    
    ano    <- as.integer(stringr::str_extract(basename(file_path), '\\d{4}(?=_visita)'))
    prefix <- 'pnadc'
    
    fs::dir_create(dirname(file_path))
    if (verbose) message('Lendo bases da PNAD Contínua — ano ', ano)
    
    df_raw  <- PNADcIBGE::get_pnadc(
      year      = ano,
      interview = 1,
      labels    = F,
      design    = F,
      deflator  = F,
      reload    = F,
      savedir   = dirname(file_path)
    )
    dt_full <- data.table::as.data.table(df_raw)
    data.table::setnames(dt_full, tolower(names(dt_full)))
    
  } else {
    
    ano    <- as.integer(stringr::str_extract(basename(file_path), '\\d{4}'))
    prefix <- 'pnad'
    
    if (verbose) message('Lendo bases da PNAD Anual — ano ', ano)
    
    tmp    <- tempdir()
    archive::archive_extract(file_path, dir = tmp)
    folder <- file.path(tmp, paste0('PNAD ', ano))
    
    dom_file <- list.files(folder, pattern = 'pnad\\.dom', full.names = T, ignore.case = T)
    pes_file <- list.files(folder, pattern = 'pnad\\.pes', full.names = T, ignore.case = T)
    
    dom     <- data.table::as.data.table(data.table::fread(dom_file))
    pes     <- data.table::as.data.table(data.table::fread(pes_file))
    dt_full <- pes[dom, on = 'domicilioid']
    data.table::setnames(dt_full, tolower(names(dt_full)))
    
  }
  
  
  # ETAPA 2 - Parquet completo =================================================
  
  fs::dir_create(proc_dir)
  proc_path <- fs::path(proc_dir, paste0(prefix, '_', ano, '.parquet'))
  
  arrow::write_parquet(dt_full, proc_path)
  if (verbose) message('Base raw (parquet) de ', ano, ' salva em: ', proc_path)
  
  
  # ETAPA 3 - Filtragem de atributos ===========================================
  
  fs::dir_create(out_dir)
  
  vars_sel <- if (!is.null(vars)) vars else .get_vars_pnad(type, ano)
  cols_sel <- intersect(tolower(vars_sel), names(dt_full))
  
  dt_filt      <- dt_full[, ..cols_sel]
  dt_filt[, ano := ano]
  
  out_path <- fs::path(out_dir, paste0(prefix, '_', ano, '_filtered.parquet'))
  arrow::write_parquet(dt_filt, out_path)
  if (verbose) message('Base filtrada de ', ano, ' salva em: ', out_path)
  
  
  # Limpeza pós-download (apenas PNAD Contínua) ================================
  
  if (type == 'continua') {
    txt_extraido <- fs::path(dirname(file_path), glue::glue('PNADC_{ano}_visita1.txt'))
    if (fs::file_exists(txt_extraido)) fs::file_delete(txt_extraido)
  }
  
  gc()
  invisible(dt_filt)
}