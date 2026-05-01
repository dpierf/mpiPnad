#' Baixa arquivos compactados da PNAD Anual a partir do CEM/USP
#' 
#' @param anos Vetor de anos a baixar (ex: c(1981, 1990, 2001))
#' @param destino Pasta de destino (default: 'data/01_raw/pnad')
#' @param timeout Tempo máximo de download em segundos (default: 300)
#' @param force_redownload Se TRUE, baixa mesmo se arquivo existir (default: FALSE)
#' @return Vetor de caminhos dos arquivos baixados (invisivelmente)
#' @export

download_pnad <- function(anos, 
                          destino = 'data/01_raw/pnad', 
                          timeout = 300,
                          force_redownload = FALSE) {
  
  # Catálogos de URLs (mantive os seus)
  .cem_urls <- c(
    '1981' = 'https://centrodametropole.fflch.usp.br/pt-br/file/16141/download?token=jjkZtEOP',
    '1982' = 'https://centrodametropole.fflch.usp.br/pt-br/file/16142/download?token=WGa0eNI2',
    '1983' = 'https://centrodametropole.fflch.usp.br/pt-br/file/16143/download?token=tJDwwRqV',
    '1984' = 'https://centrodametropole.fflch.usp.br/pt-br/file/16144/download?token=2OoOz7sf',
    '1985' = 'https://centrodametropole.fflch.usp.br/pt-br/file/16145/download?token=-Vh4mk-k',
    '1987' = 'https://centrodametropole.fflch.usp.br/pt-br/file/16148/download?token=eN2RUwxj',
    '1988' = 'https://centrodametropole.fflch.usp.br/pt-br/file/16149/download?token=aDnrAbSF',
    '1989' = 'https://centrodametropole.fflch.usp.br/pt-br/file/16150/download?token=5ytNC0UH',
    '1990' = 'https://centrodametropole.fflch.usp.br/pt-br/file/16151/download?token=3viQ3U3Q',
    '1992' = 'https://centrodametropole.fflch.usp.br/pt-br/file/16152/download?token=s5WyH6dz',
    '1993' = 'https://centrodametropole.fflch.usp.br/pt-br/file/16153/download?token=EHiLzoMb',
    '1995' = 'https://centrodametropole.fflch.usp.br/pt-br/file/16154/download?token=lVSZFuI1',
    '1996' = 'https://centrodametropole.fflch.usp.br/pt-br/file/16156/download?token=UOn9wRJ1',
    '1997' = 'https://centrodametropole.fflch.usp.br/pt-br/file/16157/download?token=pfdhXUGF',
    '1998' = 'https://centrodametropole.fflch.usp.br/pt-br/file/16158/download?token=9Z7YDocC',
    '1999' = 'https://centrodametropole.fflch.usp.br/pt-br/file/16159/download?token=BfZFW4MR'
  )
  
  .gdrive_ids <- c(
    '1986' = '1_r_7ip9GU1_Ua7jsdFYHE2K9Tx8S9-pP',
    '2001' = '1Xt4e5C2VFfAU7VZ-mRjkLLjoa0iRmu3X',
    '2002' = '1HmIBAk7diKidQTedXNqw__Ky_JGThZ39',
    '2003' = '1G15CO4MmtZSoxS3QnW6IQRtc3zZXfrlp',
    '2004' = '1b9LbeiakbR8sbycKtQDdNIvD1hVHE0rG',
    '2005' = '1KjX-43DgDgYGuVZMahB3D_CGwIKQyMqn',
    '2006' = '1p1PGWDdgTOGB3jBqD7KuJpIx8lJX0vNT',
    '2007' = '1gGNvSoHxvRthcLJePYAbfYg4Wfgxvsbi',
    '2008' = '1B03p-Yrlk0JT2gox4fu1gtmGKHFpEytl',
    '2009' = '14EnyDSLAYGAoW8vRczsJP8OmkF_2bMbx',
    '2011' = '1WdMzXYvehQ2JuzuukZXYX9hd6x9f6Vc-',
    '2012' = '1hP9EkC9oa5u7sbLKDaLlifLqOD1CrkGP',
    '2013' = '1ARge_hi2s6T5zWYl-KDAPFQ_bNedtBvb',
    '2014' = '1EMPvdpU1m0qPJjLzw9If3hzC3N_zsEVz',
    '2015' = '1ZAc0D3y4BF3s7o8vg_VsT7JscDkOKkpJ'
  )
  
  # Função auxiliar para detectar extensão via cabeçalho HTTP
  detect_extensao_url <- function(url) {
    headers <- tryCatch(
      curlGetHeaders(url),
      error = function(e) return(character(0))
    )
    if (length(headers) == 0) return('.rar')
    
    cd_line <- headers[grepl('content-disposition', headers, ignore.case = TRUE)]
    ext <- regmatches(cd_line, regexpr('\\.(rar|zip|7z)', cd_line, ignore.case = TRUE))
    if (length(ext) == 0) '.rar' else tolower(ext)
  }
  
  # Validação dos anos
  anos_chr <- as.character(anos)
  todos_anos <- union(names(.cem_urls), names(.gdrive_ids))
  anos_invalidos <- setdiff(anos_chr, todos_anos)
  
  if (length(anos_invalidos) > 0) {
    warning('Anos não disponíveis no catálogo: ',
            paste(anos_invalidos, collapse = ', '))
  }
  
  anos_cem <- intersect(anos_chr, names(.cem_urls))
  anos_gdrive <- intersect(anos_chr, names(.gdrive_ids))
  
  # Cria diretório se não existir
  fs::dir_create(destino)
  
  # Função para baixar com retry
  baixar_com_retry <- function(url, dest, max_tentativas = 3) {
    for (tentativa in seq_len(max_tentativas)) {
      resultado <- tryCatch({
        download.file(url, destfile = dest, mode = 'wb', 
                      quiet = FALSE, timeout = timeout)
        TRUE
      }, error = function(e) {
        message("  Tentativa ", tentativa, " falhou: ", e$message)
        FALSE
      })
      
      if (resultado) return(TRUE)
      if (tentativa < max_tentativas) Sys.sleep(2)
    }
    stop("Falha ao baixar ", basename(dest), " após ", max_tentativas, " tentativas")
  }
  
  # Downloads via CEM
  if (length(anos_cem) > 0) {
    purrr::walk2(.cem_urls[anos_cem], anos_cem, function(url, ano) {
      ext <- detect_extensao_url(url)
      dest <- fs::path(destino, paste0('documentoPNAD_', ano, ext))
      
      if (!fs::file_exists(dest) || force_redownload) {
        if (force_redownload && fs::file_exists(dest)) {
          fs::file_delete(dest)
        }
        message('Baixando ', ano, ' via CEM (', ext, ')...')
        baixar_com_retry(url, dest)
      } else {
        message(ano, ' já existe, pulando.')
      }
    })
  }
  
  # Downloads via Google Drive
  if (length(anos_gdrive) > 0) {
    googledrive::drive_deauth()
    
    purrr::walk2(.gdrive_ids[anos_gdrive], anos_gdrive, function(id, ano) {
      # CORRIGIDO: usar padrão correto 'documentoPNAD_'
      existentes <- destino |>
        fs::dir_ls(regexp = paste0('documentoPNAD_', ano, '\\.(rar|zip|7z)'))
      
      if (length(existentes) == 0 || force_redownload) {
        if (force_redownload && length(existentes) > 0) {
          fs::file_delete(existentes)
        }
        
        message('Baixando ', ano, ' via Google Drive...')
        tmp <- fs::file_temp()
        meta <- googledrive::drive_download(googledrive::as_id(id),
                                            path = tmp, overwrite = TRUE)
        
        # Detecta extensão pelo MIME type
        ext <- switch(meta$drive_resource[[1]]$mimeType,
                      'application/x-rar-compressed' = '.rar',
                      'application/zip' = '.zip',
                      'application/x-7z-compressed' = '.7z',
                      '.rar')
        
        dest <- fs::path(destino, paste0('documentoPNAD_', ano, ext))
        fs::file_move(tmp, dest)
        message('  Salvo como ', fs::path_file(dest))
      } else {
        message(ano, ' já existe, pulando.')
      }
    })
  }
  
  # Retorna lista de arquivos baixados
  invisible(fs::dir_ls(destino, regexp = 'documentoPNAD_.*\\.(rar|zip|7z)'))
}
