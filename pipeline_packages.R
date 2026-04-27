# =======================================================================================
# mpiPnad: Função para instalação e carregamento de pacotes necessários em R
# Autor: Pier De Maria
# Repositório: https://github.com/dpierf/mpiPnad
#
# Pré-requisito: instalar o pacote usando `pak::pak('dpierf/mpiPnad')`
# =======================================================================================

libs <- .libPaths()

for (lib in libs) {
  locks <- list.files(lib, pattern = '^00LOCK', full.names = T)

  if (length(locks) > 0) {
    unlink(locks, recursive = T, force = T)
    message('Removidos bloqueios em: ', lib)
  }
}

pacotes <- c(
  'tidyverse','archive','WDI','sf','tidyr','stringr','lubridate','estimatr','sandwich','gamlss',
  'marginaleffects','furrr','webshot2','sysfonts','here','fixest','parallelly','Rcpp','rbcb',
  'promises','cachem','data.table','PNADcIBGE','readr','survey','ordinal','renv','usethis','fs',
  'ggplot2','dplyr','cli','VGAM','convey','glue','quarto','showtext','arrow','shinycssloaders',
  'googledrive','ggiraph','MASS','callr','sidrar','purrr','deflateBR','tidyLPA','bs4Dash','srvyr',
  'cluster','targets','geobr','progressr','duckdb','ggrepel','devtools','plotly','withr','shinyjs',
  'ggdendro','mclust','FactoMineR','scales','quantreg','frontier','shiny','RColorBrewer','gt'
)

# Atualizar pacotes desatualizados antes de instalar
update.packages(ask = FALSE, checkBuilt = TRUE)

instalados <- rownames(installed.packages())
faltando <- setdiff(pacotes, instalados)

if (length(faltando) > 0) {
  capture.output(
    suppressMessages(
      install.packages(faltando, quiet = TRUE)
    )
  )
}

suppressPackageStartupMessages(
  invisible(lapply(pacotes, library, character.only = TRUE))
)

nao_carregados <- setdiff(pacotes, rownames(installed.packages()))
if (length(nao_carregados) == 0) {
  cli::cli_alert_success('Pacotes carregados e instalados com sucesso:')
  cli::cli_ul(pacotes)
} else {
  cli::cli_alert_danger('Pacotes ausentes:')
  cli::cli_ul(nao_carregados)
  quit(status = 1)
}

rm(instalados, faltando, pacotes, lib, libs, locks, nao_carregados)
