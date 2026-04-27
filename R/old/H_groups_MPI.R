#' Produz a tabela com medidas-resumo normalizadas para todos os indicadores do MPI
#' @param dt Banco de dados que foi carregado, pelo usuario, ao ler o arquivo `pnad_completa.parquet`
#' @param sexo Booleano (default = F), para saber se o arranjo familiar será analisado por gênero ou não

#' @return data.table unico, com valores resumidos e normalizados, incluindo também Score MPI, Renda PC Média e Peso Total
#' @export

groups_mpi <- function(dt,
                       sexo = FALSE
  ){

  dicts   <- readRDS('data/mpi_dictionary.rds')
  .dec <- function(x, dict) unname(dict[as.character(x)])
  
  dt <- data.table::setDT(dt)
  
  by_cols <- c('ano', 'uf', 'setor', 'area', if (sexo) 'arranjo_full' else 'arranjo', 'tamanho')
  
  # Base completa
  dt[, arranjo_dec := as.integer(substr(as.character(arranjo_full), 1, 1))]
  
  dt[, `:=` (
    ano          = as.integer(ano),
    periodo      = .dec(periodo,                   dicts$periodo),
    regiao       = .dec(regiao,                    dicts$regiao),
    setor        = .dec(setor_dec,                 dicts$setor_dec),
    area         = .dec(area_dec,                  dicts$area_dec),
    sexo         = .dec(sexo_dec,                  dicts$sexo_dec),
    raca         = .dec(raca,                      dicts$raca),
    arranjo      = .dec(substr(arranjo_full,1,1),  dicts$arranjo_dec),
    arranjo_full = .dec(arranjo_full,              dicts$arranjo_full)
  )]
  
  dt[, `:=` (
    nivel = fcase(
      escol <= 0,  '0 anos',
      escol <= 4,  '1 a 4 anos',
      escol <= 7,  '5 a 7 anos',
      escol <= 10, '8 a 10 anos',
      escol <= 14, '11 a 14 anos',
      default =    '15+ anos'
    ),
    
    faixa = fcase(
      idade %between% c(850,998), fcase(
        as.integer(ano - (idade+1000)) %between% c(15,29),  '15-29',
        as.integer(ano - (idade+1000)) %between% c(30,49),  '30-49',
        as.integer(ano - (idade+1000)) %between% c(50,64),  '50-64',
        as.integer(ano - (idade+1000)) %between% c(65,120), '65+',
        default = 'NA'
      ),
      idade %between% c(15,29),  '15-29',
      idade %between% c(30,49),  '30-49',
      idade %between% c(50,64),  '50-64',
      idade %between% c(65,120), '65+',
      default = 'NA'
    ),
    
    tamanho = fcase(
      pessoas_fam %between% c(2,3),  'Pequena',
      pessoas_fam %between% c(4,5),  'Média',
      pessoas_fam %between% c(6,98), 'Grande'
    )
  )]
  
  
  indicadores <- c('D1','D2','D3', 'B1','B2','B3','B4', 'V1','V2', 'E1','E2','E3', 'P1','P2')
  
  dt |>
    dplyr::filter(!is.na(uf) & !is.na(setor) & !is.na(area) & !is.na(arranjo_full) &
                    faixa != 'NA' & arranjo != 'Unipessoal') |>
    dplyr::reframe(
      dplyr::across(
        dplyr::all_of(indicadores),
        \(x) weighted.mean(x, w = peso, na.rm = T)
      ),
      renda_pc = weighted.mean(rpc_real,    w = peso, na.rm = T),
      pessoas  = weighted.mean(pessoas_fam, w = peso, na.rm = T),
      scorempi = weighted.mean(score,       w = peso, na.rm = T),
      pop_tot  = sum(peso, na.rm = T),
      .by = all_of(by_cols)
    ) |>
    dplyr::mutate(renda_pc = -log1p(renda_pc)) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(indicadores, 'renda_pc', 'pessoas')),
        \(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
      ),
      .by = ano
    ) |>
    dplyr::arrange(!!!syms(by_cols))
}
