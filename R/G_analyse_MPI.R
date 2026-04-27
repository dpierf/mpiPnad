#' Produz os graficos e tabelas relacionados aos resultados do MPI
#' @param dt Banco de dados que foi carregado, pelo usuario, ao ler o arquivo `pnad_completa.parquet`
#' @param grupos Tipos de graficos que serao produzidos pela funcao
#' @param k_ref Valor escolhido (default = 0.33) para apresentacao do report
#' @param cutoffs Valores de cutoff K utilizados para os calculos do binario de pobreza
#' @param dir_out Local onde os objetos serao salvos (default = 'output')

#' @param anos_tabs Anos escolhidos para apresentar nas tabelas (default = de 1981 em diante, a cada 6 anos, alem do ano mais recente)
#' @param anos_comp Anos escolhidos para comparativos (default = todo ano terminado em 5, alem do ano mais recente)
#' @param anos_maps Anos escolhidos para criacao dos mapas (default = de 1981 em diante, a cada 14 anos, alem do ano mais recente)
#' @param anos_dens Anos escolhidos para analises de dominancia e densidade (default = todo ano terminado em 5, alem do ano mais recente)
#' @param anos_decp Lista de pares de ano para analise das decomposicoes (default = 1981-1990, 1990-2001, 2001-2011, 2011-2019, 2019-2024)


#' @return data.table unico, com calculos de pobreza unidimensional (RPC) e multidimensional (MPI usando cutoff K)
#' @export


analyse_mpi <- function(
    dt                   = NULL,
    grupos               = 'all',
    k_ref                = 0.33,
    cutoffs              = c(0.10, 0.20, 0.25, 0.33, 0.40, 0.50),

    anos_tabs            = c(1981, 1987, 1993, 1999, 2005, 2011, 2017, 2024), #Anos-padrão para as tabelas
    anos_comp            = c(1985, 1995, 2005, 2015, 2024), #Anos-padrão para evolução da composição
    anos_maps            = c(1981, 1995, 2009, 2024), #Anos-padrão para plotagem de mapas
    anos_dens            = c(1985, 1995, 2005, 2015, 2024), #Anos-padrão para Lorenz e densidades
    anos_beta            = c(1981, 2024), #Anos-padrão de início e fim da análise de convergência-beta
    anos_decp            = list(c(1981,1986), c(1986,1995), c(1995,2002), c(2002,2008),
                                c(2008,2014), c(2014,2019), c(2019,2024)),

    dpi                  = 200, #DPI bom para visualização online sem prejuízos
    dir_out              = 'output/graphs',
    shapefiles           = NULL,
    base_size            = 10,
    usar_fonte           = T,
    ext                  = 'png',
    verbose              = T
) {


  # == 0. Validação e setup ====================================================

  stopifnot(data.table::is.data.table(dt))
  fs::dir_create(dir_out)

  grupos_validos <- c(
    'mapas', 'composicao', 'evolucao', 'sensibilidade', 'decomposicao',
    'convergencia', 'granularidade', 'densidade', 'dominancia', 'concentracao'
  )

  if (!identical(grupos, 'all')) {
    invalid_g <- setdiff(grupos, grupos_validos)
    if (length(invalid_g) > 0)
      stop(
        'Grupo(s) inválido(s): ', paste(invalid_g, collapse = ', '),
        '\nDisponíveis: ', paste(grupos_validos, collapse = ', ')
      )
  }

  anos_dt  <- sort(unique(dt$ano))
  ano_min  <- min(anos_dt)
  ano_max  <- max(anos_dt)

  caption_base <- 'Fonte: PNAD anual (1981-2015) e PNAD contínua, visita 1 (2016-2024). Elaboração própria.'


  # == 1. Setup tipográfico ====================================================

  if (usar_fonte) {
    if (!requireNamespace('showtext', quietly = T) ||
        !requireNamespace('sysfonts',  quietly = T)) {
      warning('Pacotes showtext/sysfonts não disponíveis. Usando fonte padrão.')
      usar_fonte <- F
    } else {
      sysfonts::font_add_google('Source Sans 3', 'source_sans')
      showtext::showtext_auto()
      showtext::showtext_opts(dpi = dpi)
    }
  }
  fonte <- if (usar_fonte) 'source_sans' else ''


  # == 2. Constantes internas ==================================================

  # Pesos
  .w_pnad <- c(
    D1 = 2/27, D2 = 2/27, D3 = 2/27,
    B1 = 1/18, B2 = 1/18, B3 = 1/18, B4 = 1/18,
    V1 = 4/27, V2 = 2/27,
    E1 = 2/27, E2 = 2/27, E3 = 2/27,
    P1 = 2/27, P2 = 1/27
  )
  
  pesos_long <- dplyr::bind_rows(
    tibble::enframe(.w_pnad, name = 'indicador', value = 'peso') |> dplyr::mutate(periodo = 'pnad'),
    tibble::enframe(.w_pnad, name = 'indicador', value = 'peso') |> dplyr::mutate(periodo = 'pnadc')
  )

  # Dimensões e indicadores
  dim_map <- c(
    D1 = 'Moradia',         D2 = 'Moradia',         D3 = 'Moradia',
    B1 = 'Serviços',        B2 = 'Serviços',
    B3 = 'Serviços',        B4 = 'Serviços',
    V1 = 'Padrão de Vida',  V2 = 'Padrão de Vida',
    E1 = 'Educação',        E2 = 'Educação',         E3 = 'Educação',
    P1 = 'Proteção Social', P2 = 'Proteção Social'
  )
  
  ordem_dim <- c('Moradia', 'Serviços', 'Padrão de Vida', 'Educação', 'Proteção Social')
  ordem_ind <- names(dim_map)
  
  labels_ind <- c(
    D1 = 'D1: Estrutura',     D2 = 'D2: Densidade',    D3 = 'D3: Propriedade',
    B1 = 'B1: Água',          B2 = 'B2: Esgoto',
    B3 = 'B3: Iluminação',    B4 = 'B4: Lixo',
    V1 = 'V1: Bens Duráveis', V2 = 'V2: Renda PC',
    E1 = 'E1: Frequência',    E2 = 'E2: Atraso',       E3 = 'E3: Escolaridade',
    P1 = 'P1: Trabalho',      P2 = 'P2: Seguridade'
  )
  
  # Arranjos
  ordem_arranjo  <- c(32L, 31L, 42L, 41L, 22L, 21L, 12L, 11L)
  
  labels_arranjo <- c(
    `32` = 'Feminina', `31` = 'Masculina',
    `42` = 'Feminina', `41` = 'Masculina',
    `22` = 'Feminina', `21` = 'Masculina',
    `12` = 'Feminina', `11` = 'Masculina'
  )
  
  tipo_mapa_vec <- c(
    `11` = 'Casal com Filhos', `12` = 'Casal com Filhos',
    `21` = 'Casal sem Filhos', `22` = 'Casal sem Filhos',
    `31` = 'Unipessoal',       `32` = 'Unipessoal',
    `41` = 'Monoparental',     `42` = 'Monoparental'
  )
  
  sexo_mapa_vec <- c(
    `11` = 'Masculina', `12` = 'Feminina',
    `21` = 'Masculina', `22` = 'Feminina',
    `31` = 'Masculina', `32` = 'Feminina',
    `41` = 'Masculina', `42` = 'Feminina'
  )
  
  .tipo_arranjo_map <- tipo_mapa_vec
  .arranjo_labels   <- labels_arranjo
  
  .cores_arranjo8 <- c(
    `32` = '#00bcd4', `31` = '#01579b',
    `42` = '#ef5350', `41` = '#b71c1c',
    `22` = '#66bb6a', `21` = '#1b5e20',
    `12` = '#ce93d8', `11` = '#4a148c'
  )
  
  niveis_tipo <- c('Unipessoal', 'Monoparental', 'Casal sem Filhos', 'Casal com Filhos')
  niveis_sexo <- c('Masculina', 'Feminina')
  
  # Geografia
  niveis_macro  <- c('Norte', 'Nordeste', 'Sudeste', 'Sul', 'Centro-Oeste')
  
  regiao_labels <- c(
    `1` = 'Norte', `2` = 'Nordeste', `3` = 'Sudeste',
    `4` = 'Sul',   `5` = 'Centro-Oeste'
  )
  
  macro_map <- c(
    AC='Norte', AM='Norte', AP='Norte', PA='Norte',
    RO='Norte', RR='Norte', TO='Norte',
    AL='Nordeste', BA='Nordeste', CE='Nordeste', MA='Nordeste', PB='Nordeste',
    PE='Nordeste', PI='Nordeste', RN='Nordeste', SE='Nordeste',
    ES='Sudeste', MG='Sudeste', RJ='Sudeste', SP='Sudeste',
    PR='Sul', RS='Sul', SC='Sul',
    DF='Centro-Oeste', GO='Centro-Oeste', MS='Centro-Oeste', MT='Centro-Oeste'
  )
  
  # Cores
  labs_hamp  <- c('H (Incidência)', 'A (Intensidade)', 'MPI')
  cores_hamp <- setNames(c('navyblue', 'palegreen3', 'tomato2'), labs_hamp)
  
  cores_cutoff <- setNames(
    c('blue4', 'cyan2', 'green2', 'orange2', 'red3', 'purple4'),
    paste0('k = ', cutoffs)
  )
  
  cores_arranjo <- c(
    'Unipessoal'       = 'blue3',
    'Monoparental'     = 'red3',
    'Casal sem Filhos' = 'green3',
    'Casal com Filhos' = 'purple3'
  )
  .cores_tipo_arranjo <- cores_arranjo
  
  cores_dim <- c(
    'Moradia'         = '#2e7d32',
    'Serviços'        = '#0277bd',
    'Padrão de Vida'  = '#ef6c00',
    'Educação'        = '#6a1b9a',
    'Proteção Social' = '#c62828'
  )
  
  cores_ind <- c(
    D1 = '#2e7d32', D2 = '#388e3c', D3 = '#66bb6a',
    B1 = '#0277bd', B2 = '#0288d1', B3 = '#039be5', B4 = '#29b6f6',
    V1 = '#ef6c00', V2 = '#ffa726',
    E1 = '#6a1b9a', E2 = '#9c27b0', E3 = '#ce93d8',
    P1 = '#c62828', P2 = '#ef9a9a'
  )
  
  cores_macro <- c(
    'Norte'        = '#ef6c00',
    'Nordeste'     = '#c62828',
    'Sudeste'      = '#6a1b9a',
    'Sul'          = '#0277bd',
    'Centro-Oeste' = '#2e7d32'
  )
  

  # == 3. Setup geográfico =====================================================

  siglas_uf <- c(
    'Rondônia' = 'RO', 'Acre' = 'AC', 'Amazonas' = 'AM', 'Roraima' = 'RR',
    'Pará' = 'PA', 'Amapá' = 'AP', 'Maranhão' = 'MA', 'Piauí' = 'PI',
    'Ceará' = 'CE', 'Rio Grande do Norte' = 'RN', 'Paraíba' = 'PB',
    'Pernambuco' = 'PE', 'Alagoas' = 'AL', 'Sergipe' = 'SE', 'Bahia' = 'BA',
    'Minas Gerais' = 'MG', 'Espírito Santo' = 'ES', 'Rio de Janeiro' = 'RJ',
    'São Paulo' = 'SP', 'Paraná' = 'PR', 'Santa Catarina' = 'SC',
    'Rio Grande do Sul' = 'RS', 'Mato Grosso do Sul' = 'MS', 'Mato Grosso' = 'MT',
    'Goiás' = 'GO', 'Distrito Federal' = 'DF', 'Tocantins' = 'TO',
    'Território de Roraima' = 'RR', 'Território de Rondônia' = 'RO',
    'Litígio PI/CE' = 'PI', 'Brasília' = 'DF', 'Território do Amapá' = 'AP'
  )

  .normalizar_geo <- function(uf_geo) {
    if ('abbrev_state' %in% names(uf_geo)) return(uf_geo)
    dplyr::mutate(uf_geo, abbrev_state = dplyr::recode(name_state, !!!siglas_uf))
  }
  .ano_para_shapefile <- function(a) dplyr::case_when(a <= 1988~1980L, a <= 2000~1991L, T~2020L)

  if (is.null(shapefiles)) {
    if (verbose) message('Baixando shapefiles...')
    shapefiles <- list(
      '1980' = geobr::read_state(year = 1980, simplified = T, showProgress = F, cache = F),
      '1991' = geobr::read_state(year = 1991, simplified = T, showProgress = F, cache = F),
      '2020' = geobr::read_state(year = 2020, simplified = T, showProgress = F, cache = F)
    ) |> purrr::map(.normalizar_geo)
  }

  limites_escala <- dt |>
    dplyr::filter(ano %in% anos_maps) |>
    dplyr::reframe(
      score_medio = weighted.mean(score, w = peso, na.rm = T),
      .by = c(ano, uf, arranjo_full)
    ) |>
    dplyr::reframe(lim = range(score_medio, na.rm = T)) |>
    dplyr::pull(lim)


  # == 4. Helpers analíticos ===================================================

  .hamp_serie <- function(k = k_ref) {
    dt |>
      dplyr::summarise(
        H   = 100 * weighted.mean(score  >=  k,        w = peso,           na.rm = T),
        A   = 100 * weighted.mean(score[score >= k],   w = peso[score >= k], na.rm = T),
        MPI = H * A / 100,
        .by = ano
      ) |> dplyr::arrange(ano)
  }

  .mpi_uf_serie <- function(k = k_ref) {
    dt |>
      dplyr::summarise(
        H   = weighted.mean(score  >=  k,        w = peso,           na.rm = T),
        A   = weighted.mean(score[score >= k],   w = peso[score >= k], na.rm = T),
        MPI = 100 * H * A,
        .by = c(ano, uf)
      ) |>
      dplyr::mutate(macro = factor(macro_map[uf], levels = niveis_macro)) |>
      dplyr::filter(!is.na(macro))
  }

  .composicao_dados <- function(by_vars) {
    dt |>
      dplyr::mutate(periodo = dplyr::if_else(ano <= 2015,'pnad', 'pnadc')) |>
      dplyr::reframe(
        dplyr::across(dplyr::any_of(names(dim_map)), \(x) weighted.mean(x,w = peso,na.rm = T)),
        periodo = dplyr::first(periodo),
        .by = dplyr::all_of(by_vars)
      ) |>
      tidyr::pivot_longer(dplyr::any_of(names(dim_map)), names_to = 'indicador', values_to = 'media_privacao') |>
      dplyr::left_join(pesos_long, by = c('indicador', 'periodo')) |>
      dplyr::mutate(
        parcela  = dplyr::if_else(peso == 0|is.na(peso), 0, media_privacao*peso),
        dimensao = dim_map[indicador]
      ) |>
      dplyr::reframe(parcela_dim = sum(parcela), .by = c(dplyr::all_of(by_vars),'dimensao')) |>
      dplyr::mutate(
        score_total  = sum(parcela_dim, na.rm = T),
        contribuicao = 100 * parcela_dim / score_total,
        .by = dplyr::all_of(by_vars)
      )
  }

  .privacao_ind_ano <- function() {
    dt |>
      dplyr::mutate(periodo = dplyr::if_else(ano <= 2015,'pnad', 'pnadc')) |>
      dplyr::reframe(
        dplyr::across(dplyr::any_of(names(dim_map)), \(x) weighted.mean(x,w = peso,na.rm = T)),
        periodo = dplyr::first(periodo),
        .by = ano
      ) |>
      tidyr::pivot_longer(dplyr::any_of(names(dim_map)), names_to = 'indicador', values_to = 'taxa_privacao') |>
      dplyr::mutate(
        dimensao      = factor(dim_map[indicador], levels = ordem_dim),
        indicador     = factor(indicador, levels = ordem_ind),
        label_ind     = labels_ind[indicador]
      )
  }

  .lorenz_ponderada <- function(score, peso, n_pontos = 500) {
    score <- as.numeric(score)
    peso  <- as.numeric(peso)

    ok    <- is.finite(score) & is.finite(peso) & peso > 0 & score  >=  0
    score <- score[ok]
    peso  <- peso[ok]

    pg <- seq(0, 1, length.out = n_pontos)

    # Dados insuficientes ou score todo zero → devolve diagonal (Gini = 0)
    soma_sw <- sum(score * peso)
    if (length(score) < 2 || soma_sw  ==  0)
      return(tibble::tibble(pop_cum = pg, score_cum = pg))

    ord    <- order(score)
    w_cum  <- cumsum(peso[ord])               / sum(peso[ord])
    sw_cum <- cumsum(score[ord] * peso[ord])  / soma_sw

    # ties = 'ordered': curva monotônica mesmo com muitos scores idênticos
    tibble::tibble(
      pop_cum   = pg,
      score_cum = approx(c(0, w_cum), c(0, sw_cum), xout = pg, ties = 'ordered')$y
    )
  }

  .gini_ponderado <- function(score, peso) {
    l    <- .lorenz_ponderada(as.numeric(score), as.numeric(peso), n_pontos = 2000)
    n    <- nrow(l)
    area <- sum(diff(l$pop_cum) * (l$score_cum[-1] + l$score_cum[-n]) / 2)
    1 - 2 * area
  }

  .curvas_hk <- function(dt_sub, col_grupo, niveis_grupo, n_pontos = 200) {
    ks <- seq(0,1,length.out = n_pontos)
    purrr::map(niveis_grupo, \(grp) {
      d <- dt_sub[dt_sub[[col_grupo]] == grp,]
      purrr::map(ks, \(ki) tibble::tibble(
        grupo = grp, k = ki,
        H = 100*weighted.mean(d$score >= ki, w = d$peso, na.rm = T)
      )) |> purrr::list_rbind()
    }) |> purrr::list_rbind()
  }

  .prep_grupo <- function(dt_sub, atributo, labels) {
    dt_sub |>
      dplyr::mutate(grupo = factor(dplyr::recode(.data[[atributo]],!!!labels), levels = unname(labels))) |>
      dplyr::filter(!is.na(grupo))
  }

  .auc_absoluta <- function(score, peso, n_pontos = 200) {
    ks <- seq(0, 1, length.out = n_pontos)
    hs <- purrr::map_dbl(ks, \(k) weighted.mean(score >= k, w = peso, na.rm = T))
    sum(diff(ks) * (hs[-1] + hs[-length(hs)]) / 2) * 100
  }

  .fsd <- function(score_a, peso_a, score_b, peso_b, n_pontos = 200) {
    ks <- seq(0, 1, length.out = n_pontos)
    ha <- purrr::map_dbl(ks, \(k) weighted.mean(score_a >= k, w = peso_a, na.rm = T))
    hb <- purrr::map_dbl(ks, \(k) weighted.mean(score_b >= k, w = peso_b, na.rm = T))
    mean(ha > hb)
  }


  # == 5. Tema e escalas =======================================================

  theme_artigo <- function(bs = base_size) {
    f <- fonte
    ggplot2::theme_minimal(base_size = bs) +
      ggplot2::theme(
        text          = ggplot2::element_text(family = f),
        plot.title    = ggplot2::element_text(family = f, face = 'bold', size = 20, margin = ggplot2::margin(b = 4)),
        plot.subtitle = ggplot2::element_text(family = f, size = 16, color = 'grey30', margin = ggplot2::margin(b = 8)),
        plot.caption  = ggplot2::element_text(family = f, size = 11, color = 'grey40', hjust = 1),
        plot.margin   = ggplot2::margin(10,10,8,10),
        axis.title    = ggplot2::element_text(family = f, size = 14),
        axis.text     = ggplot2::element_text(family = f, size = 12),
        axis.text.x   = ggplot2::element_text(family = f, size = 12, angle = 90, hjust = 1),
        axis.text.y   = ggplot2::element_text(family = f, size = 12),
        axis.title.y  = ggplot2::element_text(family = f, size = 14, margin = ggplot2::margin(r = 8)),
        axis.title.x  = ggplot2::element_text(family = f, size = 14, margin = ggplot2::margin(t = 6)),
        legend.text   = ggplot2::element_text(family = f, size = 12),
        legend.title  = ggplot2::element_text(family = f, size = 12),
        legend.position  = 'bottom',
        strip.text       = ggplot2::element_text(family = f, size = 12),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(color = 'grey92')
      )
  }

  theme_mapa <- function() {
    f <- fonte
    ggplot2::theme_void(base_size = base_size) +
      ggplot2::theme(
        text              = ggplot2::element_text(family = f),
        plot.title        = ggplot2::element_text(family = f, face = 'bold', size = 20, margin = ggplot2::margin(b = 4)),
        plot.subtitle     = ggplot2::element_text(family = f, size = 16, color = 'grey30', margin = ggplot2::margin(b = 8)),
        plot.caption      = ggplot2::element_text(family = f, size = 11, color = 'grey40', hjust = 1),
        strip.text.x      = ggplot2::element_text(family = f, face = 'bold', size = 12, margin = ggplot2::margin(b = 5)),
        strip.text.y.left = ggplot2::element_text(family = f, size = 12, angle = 0, hjust = 1, margin = ggplot2::margin(r = 5)),
        strip.placement   = 'outside',
        panel.spacing     = ggplot2::unit(0.8,'lines'),
        legend.position   = 'bottom',
        legend.text       = ggplot2::element_text(family = f, size = 12),
        legend.title      = ggplot2::element_text(family = f, size = 12),
        plot.margin       = ggplot2::margin(10,10,8,10)
      )
  }

  scale_x_anos <- function() {
    ggplot2::scale_x_continuous(breaks = anos_dt, expand = ggplot2::expansion(mult = c(0.01,0.01)))
  }

  colorbar_padrao <- function(titulo) {
    ggplot2::guide_colorbar(
      title = titulo, title.position = 'top',
      barwidth = ggplot2::unit(12,'cm'), barheight = ggplot2::unit(0.4,'cm')
    )
  }


  # == 6. Geradores internos  ==================================================

  # 6.1 Tabelas ----------------------------------------------------------------

  .tab_arranjo <- function() {
    cols_ord <- unname(labels_arranjo[as.character(ordem_arranjo)])
    dt |>
      dplyr::filter(ano %in% anos_tabs) |>
      dplyr::reframe(
        mpi_medio = 100 * weighted.mean(score, w = peso, na.rm = TRUE),
        .by = c(ano, arranjo_full)) |>
      dplyr::mutate(
        ano          = factor(ano, levels = sort(unique(ano))),
        arranjo_lbl  = factor(
          labels_arranjo[as.character(arranjo_full)],
          levels = cols_ord)) |>
      dplyr::select(-arranjo_full) |>
      tidyr::pivot_wider(names_from = arranjo_lbl, values_from = mpi_medio) |>
      dplyr::select(ano, dplyr::all_of(cols_ord)) |>
      dplyr::arrange(ano) |>
      gt::gt(rowname_col = 'ano') |>
      gt::tab_header(
        title    = 'Índice de Pobreza Multidimensional (MPI-LA)',
        subtitle = 'Score médio (ponderado) de privação, por arranjo domiciliar e anos selecionados'
      ) |>
      gt::fmt_number(columns = dplyr::where(is.numeric), decimals = 1) |>
      gt::tab_spanner(label = 'Unipessoal',       columns = c('Unipessoal M',       'Unipessoal F'))       |>
      gt::tab_spanner(label = 'Monoparental',     columns = c('Monoparental M',     'Monoparental F'))     |>
      gt::tab_spanner(label = 'Casal sem Filhos', columns = c('Casal sem Filhos M', 'Casal sem Filhos F')) |>
      gt::tab_spanner(label = 'Casal com Filhos', columns = c('Casal com Filhos M', 'Casal com Filhos F')) |>
      gt::tab_stubhead(label = 'Ano') |>
      gt::tab_source_note(source_note = caption_base) |>
      gt::opt_stylize(style = 6, color = 'blue')
  }
  .tab_sensibilidade <- function() {
    dt |>
      dplyr::filter(ano %in% anos_tabs) |>
      dplyr::reframe(
        purrr::map(cutoffs, \(k) tibble::tibble(
          k = k, mpi = {
            h <- weighted.mean(score >= k, w = peso, na.rm = T)
            a <- weighted.mean(score[score >= k], w = peso[score >= k], na.rm = T)
            100*(h*a)
          }
        )) |> purrr::list_rbind(),
        .by = ano
      ) |>
      dplyr::mutate(ano = factor(ano,levels = sort(unique(ano))), k_lab = factor(100*k,levels = 100*cutoffs)) |>
      tidyr::pivot_wider(id_cols = ano,names_from = k_lab,values_from = mpi) |>
      dplyr::arrange(ano) |>
      gt::gt(rowname_col = 'ano') |>
      gt::tab_header(
        title    = 'Análise de Sensibilidade do MPI-LA',
        subtitle = 'Índice de Pobreza Multidimensional ponderado por cutoff k e anos selecionados'
      ) |>
      gt::fmt_number(columns = dplyr::where(is.numeric), decimals = 1) |>
      gt::tab_spanner(label = 'Cutoff (k %)', columns = dplyr::everything()) |>
      gt::tab_stubhead(label = 'Ano') |>
      gt::tab_source_note(source_note = caption_base) |>
      gt::opt_stylize(style = 6, color = 'blue')
  }

  .tab_auc_arranjo <- function() {

    dados_wide <- purrr::map_dfr(ordem_arranjo, \(arr) {
      purrr::map_dfr(anos_tabs, \(yr) {
        d <- dt[ano == yr & arranjo_full == arr]
        if (nrow(d) == 0) return(tibble::tibble(arranjo = arr, ano = yr, auc = NA_real_))
        tibble::tibble(arranjo = arr, ano = yr, auc = .auc_absoluta(d$score, d$peso))
      })
    }) |>
      dplyr::mutate(
        sexo         = factor(sexo_mapa_vec[arranjo],  levels = niveis_sexo),
        tipo_arranjo = factor(tipo_mapa_vec[arranjo],  levels = niveis_tipo)
      ) |>
      dplyr::arrange(tipo_arranjo, sexo) |>
      tidyr::pivot_wider(
        id_cols     = c(tipo_arranjo, sexo, arranjo),
        names_from  = ano,
        values_from = auc
      ) |>
      dplyr::select(-arranjo)

    # Range global calculado antes do gt
    vals   <- dplyr::select(dados_wide, dplyr::where(is.numeric))
    domain <- c(min(vals, na.rm = T), max(vals, na.rm = T))

    dados_wide |>
      gt::gt(rowname_col = 'tipo_arranjo', groupname_col = 'sexo') |>
      gt::tab_header(
        title    = 'AUC absoluta por arranjo domiciliar e ano',
        subtitle = 'Área sob a curva H(k) para k ∈ [0,1] (×100); maior valor = maior carga de privação'
      ) |>
      gt::fmt_number(columns = dplyr::where(is.numeric), decimals = 1) |>
      gt::data_color(
        columns  = dplyr::where(is.numeric),
        method   = 'numeric',
        palette = c('#1b5e20', '#7cb342', '#f9a825', '#f46d43', '#b71c1c'),
        domain   = domain,
        na_color = 'grey70'
      ) |>
      gt::tab_spanner(label = 'Anos selecionados', columns = dplyr::where(is.numeric)) |>
      gt::tab_stubhead(label = 'Arranjo') |>
      gt::tab_source_note(source_note = caption_base) |>
      gt::opt_stylize(style = 6, color = 'blue')
  }

  # 6.2 Evolução H-A-MPI -------------------------------------------------------

  .plot_score_mpi <- function() {
    dt |>
      dplyr::summarise(
        score_medio = 100*weighted.mean(score,w = peso,na.rm = T),
        H  = 100*weighted.mean(score >= k_ref,w = peso,na.rm = T),
        A  = 100*weighted.mean(score[score >= k_ref],w = peso[score >= k_ref],na.rm = T),
        .by = ano
      ) |>
      dplyr::mutate(MPI = H*A/100) |> dplyr::arrange(ano) |>
      ggplot2::ggplot(ggplot2::aes(x = ano)) +
      ggplot2::geom_line(ggplot2::aes(y = score_medio,color = 'Score médio'),linewidth = 0.75) +
      ggplot2::geom_point(ggplot2::aes(y = score_medio,color = 'Score médio'),fill = 'white',shape = 23,size = 2,stroke = 0.75) +
      ggplot2::geom_line(ggplot2::aes(y = MPI,color = 'MPI (H × A)'),linewidth = 0.75) +
      ggplot2::geom_point(ggplot2::aes(y = MPI,color = 'MPI (H × A)'),fill = 'white',shape = 23,size = 2,stroke = 0.75) +
      ggplot2::scale_color_manual(values = c('Score médio' = 'brown4', 'MPI (H × A)' = 'purple3')) +
      scale_x_anos() +
      ggplot2::scale_y_continuous(limits = c(0,NA), labels = scales::label_number(decimal.mark = ',',accuracy = 1), expand = ggplot2::expansion(mult = c(0,0.05))) +
      ggplot2::labs(title = 'Evolução do score médio de privação e do MPI: Brasil',
                    subtitle = paste0('k de referência: ',k_ref), x = NULL, y = 'Valores (%)', color = NULL, caption = caption_base) +
      theme_artigo()
  }

  .plot_h_a_mpi <- function() {
    dt |>
      dplyr::summarise(
        H = 100*weighted.mean(score >= k_ref,w = peso,na.rm = T),
        A = 100*weighted.mean(score[score >= k_ref],w = peso[score >= k_ref],na.rm = T),
        MPI = H*A/100, .by = ano
      ) |> dplyr::arrange(ano) |>
      tidyr::pivot_longer(c(H,A,MPI),names_to = 'indicador',values_to = 'valor') |>
      dplyr::mutate(indicador = factor(indicador,levels = c('H', 'A', 'MPI'),labels = labs_hamp)) |>
      ggplot2::ggplot(ggplot2::aes(x = ano,y = valor,color = indicador,group = indicador)) +
      ggplot2::geom_line(linewidth = 0.75) +
      ggplot2::geom_point(ggplot2::aes(fill = indicador),shape = 23,size = 2,stroke = 0.75,fill = 'white') +
      ggplot2::scale_color_manual(values = cores_hamp) +
      scale_x_anos() +
      ggplot2::scale_y_continuous(limits = c(0,NA), labels = scales::label_number(decimal.mark = ',',accuracy = 1), expand = ggplot2::expansion(mult = c(0,0.2))) +
      ggplot2::labs(title = 'Evolução do MPI-Brasil e suas componentes (H e A)',
                    subtitle = paste0('k de referência: ',k_ref), x = NULL, y = 'Valor (%)', color = NULL, fill = NULL, caption = caption_base) +
      theme_artigo()
  }

  .plot_mpi_macrorregiao <- function() {
    dt |>
      dplyr::mutate(macro = factor(macro_map[uf],levels = niveis_macro)) |>
      dplyr::filter(!is.na(macro)) |>
      dplyr::summarise(
        H = 100*weighted.mean(score >= k_ref,w = peso,na.rm = T),
        A = 100*weighted.mean(score[score >= k_ref],w = peso[score >= k_ref],na.rm = T),
        MPI = H*A/100, .by = c(ano,macro)
      ) |> dplyr::arrange(ano) |>
      ggplot2::ggplot(ggplot2::aes(x = ano,y = MPI,color = macro,group = macro)) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point(ggplot2::aes(fill = macro),shape = 21,size = 2.2,stroke = 0.7,color = 'white') +
      ggplot2::scale_color_manual(values = cores_macro,name = 'Macrorregião') +
      ggplot2::scale_fill_manual(values = cores_macro,guide = 'none') +
      scale_x_anos() +
      ggplot2::scale_y_continuous(limits = c(0,NA), labels = scales::label_number(decimal.mark = ',',accuracy = 1), expand = ggplot2::expansion(mult = c(0,0.05))) +
      ggplot2::labs(title = 'MPI por macrorregião: Brasil',subtitle = paste0('k de referência: ',k_ref),
                    x = NULL,y = 'MPI (%)',caption = caption_base) +
      theme_artigo() +
      ggplot2::guides(color = ggplot2::guide_legend(nrow = 1,override.aes = list(linewidth = 2.5)))
  }

  # 6.3 Sensibilidade ----------------------------------------------------------

  .plot_sensibilidade <- function() {
    dt |>
      dplyr::reframe(
        purrr::map(cutoffs, \(k) tibble::tibble(
          k = k,
          H = 100*weighted.mean(score >= k,w = peso,na.rm = T),
          A = 100*weighted.mean(score[score >= k],w = peso[score >= k],na.rm = T),
          MPI = H*A/100
        )) |> purrr::list_rbind(), .by = ano
      ) |>
      dplyr::mutate(k_lab = factor(paste0('k = ',k),levels = paste0('k = ',cutoffs))) |>
      dplyr::arrange(ano) |>
      ggplot2::ggplot(ggplot2::aes(x = ano,y = MPI,color = k_lab,group = k_lab)) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::geom_point(fill = 'white',shape = 23,size = 2,stroke = 0.75) +
      ggplot2::scale_color_manual(values = cores_cutoff,name = 'Cutoff (k)') +
      ggplot2::guides(color = ggplot2::guide_legend(nrow = 1,override.aes = list(linewidth = 2,shape = NA))) +
      scale_x_anos() +
      ggplot2::scale_y_continuous(limits = c(0,NA), labels = scales::label_number(decimal.mark = ',',accuracy = 1), expand = ggplot2::expansion(mult = c(0,0.05))) +
      ggplot2::labs(title = 'Análise de sensibilidade do MPI: Brasil',
                    x = NULL,y = 'MPI (%)',color = 'Cutoff (k)',caption = caption_base) +
      theme_artigo()
  }

  .plot_sensibilidade_arranjo <- function() {
    k_levels <- paste0('k = ',scales::percent(cutoffs,accuracy = 1,decimal.mark = ','))
    dt |>
      dplyr::mutate(tipo_arranjo = tipo_mapa_vec[as.character(arranjo_full)]) |>
      dplyr::reframe(
        purrr::map(cutoffs,\(k){
          pobres <- score >= k
          tibble::tibble(k = k, H = weighted.mean(pobres,w = peso,na.rm = T),
                         A = weighted.mean(score[pobres],w = peso[pobres],na.rm = T), MPI = H*A)
        }) |> purrr::list_rbind(), .by = c(ano,tipo_arranjo)
      ) |>
      dplyr::mutate(
        tipo_arranjo = factor(tipo_arranjo, levels = niveis_tipo),
        k_lab = factor(paste0('k = ',scales::percent(k,accuracy = 1,decimal.mark = ',')),levels = k_levels)
      ) |>
      ggplot2::ggplot(ggplot2::aes(x = ano,y = 100*MPI,color = tipo_arranjo,group = tipo_arranjo)) +
      ggplot2::geom_line(linewidth = 0.5) +
      ggplot2::geom_point(ggplot2::aes(fill = tipo_arranjo),shape = 21,size = 1.8,stroke = 0.5,color = 'white') +
      ggplot2::facet_wrap(~k_lab,ncol = 3) +
      ggplot2::scale_color_manual(values = cores_arranjo,name = 'Arranjo') +
      ggplot2::scale_fill_manual(values = cores_arranjo,guide = 'none') +
      scale_x_anos() +
      ggplot2::scale_y_continuous(limits = c(0,50), labels = scales::label_number(decimal.mark = ',',accuracy = 0.1), expand = ggplot2::expansion(mult = c(0,0.02))) +
      ggplot2::labs(title = 'Análise de sensibilidade do MPI por arranjo domiciliar',
                    subtitle = 'MPI (H × A) ponderado por cutoff k',
                    x = NULL,y = 'MPI (%)',caption = caption_base) +
      theme_artigo() +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(), panel.spacing = ggplot2::unit(1.2,'lines')) +
      ggplot2::guides(color = ggplot2::guide_legend(nrow = 1,override.aes = list(linewidth = 2.5)))
  }

  # 6.4 Composição -------------------------------------------------------------

  .plot_composicao <- function() {
    .composicao_dados('ano') |>
      dplyr::mutate(dimensao = factor(dimensao,levels = ordem_dim)) |>
      ggplot2::ggplot(ggplot2::aes(x = ano,y = contribuicao,fill = dimensao)) +
      ggplot2::geom_area(alpha = 1,color = 'white',linewidth = 0.3) +
      ggplot2::scale_fill_manual(values = cores_dim) +
      scale_x_anos() +
      ggplot2::scale_y_continuous(limits = c(0,100),oob = scales::squish,
                                  labels = scales::label_number(suffix = '%',decimal.mark = ','), expand = ggplot2::expansion(mult = c(0,0))) +
      ggplot2::labs(title = 'Composição do score médio de privação: Brasil',
                    subtitle = 'Contribuição relativa de cada dimensão (% do score total)',
                    x = NULL,y = NULL,fill = NULL,caption = caption_base) +
      theme_artigo() + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
  }

  .plot_composicao_arranjo <- function() {
    .composicao_dados(c('ano', 'arranjo_full')) |>
      dplyr::filter(ano %in% anos_comp) |>
      dplyr::mutate(
        tipo_arranjo = factor(tipo_mapa_vec[as.character(arranjo_full)],levels = niveis_tipo),
        sexo = factor(sexo_mapa_vec[as.character(arranjo_full)],levels = niveis_sexo),
        dimensao = factor(dimensao,levels = ordem_dim)
      ) |>
      ggplot2::ggplot(ggplot2::aes(x = sexo,y = contribuicao,fill = dimensao)) +
      ggplot2::geom_col(position = ggplot2::position_fill(),width = 0.7) +
      ggplot2::geom_text(
        ggplot2::aes(label = dplyr::if_else(contribuicao >= 5,
                                            scales::number(contribuicao,accuracy = 0.1,suffix = '%',decimal.mark = ','),'')),
        position = ggplot2::position_fill(vjust = 0.5),size = 2.5,color = 'black'
      ) +
      ggplot2::facet_grid(tipo_arranjo~ano,switch = 'y') +
      ggplot2::scale_fill_manual(values = cores_dim) +
      ggplot2::scale_y_continuous(labels = scales::label_percent(decimal.mark = ',',accuracy = 1),
                                  oob = scales::squish, expand = ggplot2::expansion(mult = c(0,0.02))) +
      ggplot2::coord_flip() +
      ggplot2::labs(title = 'Composição do MPI por arranjo domiciliar',
                    subtitle = 'Contribuição relativa de cada dimensão (% do score total)',
                    x = NULL,y = NULL,fill = NULL,caption = caption_base) +
      theme_artigo() +
      ggplot2::theme(
        strip.text.x = ggplot2::element_text(face = 'bold',margin = ggplot2::margin(b = 3)),
        strip.text.y.left = ggplot2::element_text(angle = 90,hjust = 0.5,face = 'bold',margin = ggplot2::margin(r = 5)),
        strip.placement = 'outside', axis.text.x = ggplot2::element_text(angle = 0,hjust = 0.5),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.spacing.x = ggplot2::unit(0.5,'lines'), panel.spacing.y = ggplot2::unit(2,'lines')
      )
  }

  .plot_contribuicao_ind <- function() {
    dt |>
      dplyr::mutate(periodo = dplyr::if_else(ano <= 2015,'pnad', 'pnadc')) |>
      dplyr::reframe(
        dplyr::across(dplyr::any_of(names(dim_map)),\(x) weighted.mean(x,w = peso,na.rm = T)),
        periodo = dplyr::first(periodo), .by = ano
      ) |>
      tidyr::pivot_longer(dplyr::any_of(names(dim_map)),names_to = 'indicador',values_to = 'media_privacao') |>
      dplyr::left_join(pesos_long,by = c('indicador', 'periodo')) |>
      dplyr::mutate(
        parcela = dplyr::if_else(peso == 0|is.na(peso),0,media_privacao*peso),
        dimensao = factor(dim_map[indicador],levels = ordem_dim),
        indicador = factor(indicador,levels = ordem_ind)
      ) |>
      dplyr::mutate(score_total = sum(parcela,na.rm = T), contribuicao = 100*parcela/score_total, .by = ano) |>
      ggplot2::ggplot(ggplot2::aes(x = ano,y = contribuicao,fill = indicador)) +
      ggplot2::geom_area(color = 'white',linewidth = 0.2,alpha = 0.95) +
      ggplot2::facet_wrap(~dimensao,ncol = 2,scales = 'free_y') +
      ggplot2::scale_fill_manual(values = cores_ind,guide = 'none') +
      scale_x_anos() +
      ggplot2::scale_y_continuous(limits = c(0,NA),
                                  labels = scales::label_number(suffix = '%',decimal.mark = ',',accuracy = 1),
                                  expand = ggplot2::expansion(mult = c(0,0.05))) +
      ggplot2::labs(title = 'Contribuição individual dos indicadores ao score: Brasil',
                    subtitle = 'Contribuição relativa de cada indicador (% do score total), por dimensão',
                    x = NULL,y = NULL,caption = caption_base) +
      theme_artigo() +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(), panel.spacing = ggplot2::unit(1.2,'lines'))
  }

  .plot_contrib_macro_serie <- function() {
    dt |>
      dplyr::mutate(macro = factor(macro_map[uf],levels = niveis_macro)) |>
      dplyr::filter(!is.na(macro)) |>
      dplyr::summarise(
        H = weighted.mean(score >= k_ref,w = peso,na.rm = T),
        A = weighted.mean(score[score >= k_ref],w = peso[score >= k_ref],na.rm = T),
        MPI = H*A, pop = sum(peso,na.rm = T), .by = c(ano,uf,macro)
      ) |>
      dplyr::mutate(pop_total = sum(pop), contrib = 100*(pop/pop_total)*MPI, .by = ano) |>
      dplyr::reframe(contrib_macro = sum(contrib), .by = c(ano,macro)) |>
      dplyr::arrange(ano) |>
      ggplot2::ggplot(ggplot2::aes(x = ano,y = contrib_macro,fill = macro)) +
      ggplot2::geom_area(color = 'white',linewidth = 0.25,alpha = 0.95) +
      ggplot2::scale_fill_manual(values = cores_macro,name = 'Macrorregião') +
      scale_x_anos() +
      ggplot2::scale_y_continuous(labels = scales::label_number(decimal.mark = ',',accuracy = 0.1,suffix = ' p.p.'), expand = ggplot2::expansion(mult = c(0,0.03))) +
      ggplot2::labs(title = 'Contribuição das macrorregiõees ao MPI nacional: Brasil',
                    subtitle = paste0('Contribuição absoluta = (pop. região / pop. total) × MPI_região; k = ',k_ref),
                    x = NULL,y = 'Contribuição ao MPI (p.p.)',caption = caption_base) +
      theme_artigo() + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank()) +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))
  }

  .plot_contrib_macro_relativa <- function() {
    dt |>
      dplyr::mutate(macro = factor(macro_map[uf],levels = niveis_macro)) |>
      dplyr::filter(!is.na(macro)) |>
      dplyr::summarise(
        H = weighted.mean(score >= k_ref,w = peso,na.rm = T),
        A = weighted.mean(score[score >= k_ref],w = peso[score >= k_ref],na.rm = T),
        MPI = H*A, pop = sum(peso,na.rm = T), .by = c(ano,uf,macro)
      ) |>
      dplyr::mutate(pop_total = sum(pop), contrib = (pop/pop_total)*MPI, .by = ano) |>
      dplyr::reframe(contrib_macro = sum(contrib), .by = c(ano,macro)) |>
      dplyr::mutate(contrib_rel = 100*contrib_macro/sum(contrib_macro), .by = ano) |>
      dplyr::arrange(ano) |>
      ggplot2::ggplot(ggplot2::aes(x = ano,y = contrib_rel,fill = macro)) +
      ggplot2::geom_area(color = 'white',linewidth = 0.25,alpha = 0.95) +
      ggplot2::scale_fill_manual(values = cores_macro,name = 'Macrorregião') +
      scale_x_anos() +
      ggplot2::scale_y_continuous(limits = c(0,100),oob = scales::squish,
                                  labels = scales::label_number(decimal.mark = ',',accuracy = 1,suffix = '%'),
                                  expand = ggplot2::expansion(mult = c(0,0))) +
      ggplot2::labs(title = 'Composição regional do MPI nacional: Brasil',
                    subtitle = paste0('Participação relativa de cada macrorregião no MPI nacional (%); k = ',k_ref),
                    x = NULL,y = '% do MPI nacional',caption = caption_base) +
      theme_artigo() + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank()) +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))
  }

  .plot_contrib_uf_ranking <- function() {

    top_n     <- 27

    # MPI por UF × ano
    mpi_uf <- dt |>
      dplyr::filter(ano %in% anos_tabs) |>
      dplyr::mutate(macro = factor(macro_map[uf], levels = niveis_macro)) |>
      dplyr::filter(!is.na(macro)) |>
      dplyr::summarise(
        H   = weighted.mean(score >= k_ref,        w = peso,             na.rm = T),
        A   = weighted.mean(score[score >= k_ref], w = peso[score >= k_ref], na.rm = T),
        MPI = 100 * H * A,
        .by = c(ano, uf, macro)
      )

    # Ranking por ano — rank 1 = maior MPI
    ranking <- mpi_uf |>
      dplyr::mutate(rank = dplyr::min_rank(dplyr::desc(MPI)), .by = ano)

    # União das UFs que entraram no top-N em qualquer ano
    ufs_union <- ranking |>
      dplyr::filter(rank <= top_n) |>
      dplyr::distinct(uf)

    # Macro de cada UF (invariante ao ano)
    macro_uf <- mpi_uf |>
      dplyr::distinct(uf, macro)

    # Grade completa: todos os anos × todas as UFs da união
    # rank = NA quando a UF não está no top-N → quebra a linha
    dados <- tidyr::expand_grid(ano = anos_tabs, uf = ufs_union$uf) |>
      dplyr::left_join(
        ranking |> dplyr::filter(rank <= top_n) |> dplyr::select(ano, uf, rank),
        by = c('ano', 'uf')
      ) |>
      dplyr::left_join(macro_uf, by = 'uf') |>
      dplyr::mutate(macro = factor(macro, levels = niveis_macro))

    anos_label <- range(anos_tabs)

    dados |>
      ggplot2::ggplot(ggplot2::aes(x = ano, y = rank, color = macro, group = uf)) +
      ggplot2::geom_line(linewidth = 0.9, na.rm = T) +
      ggplot2::geom_point(
        ggplot2::aes(fill = macro),
        shape = 21, size = 3, stroke = 0.5, color = 'white',
        na.rm = T
      ) +
      ggplot2::geom_text(
        data = \(d) dplyr::filter(d, ano == min(anos_tabs), !is.na(rank)),
        ggplot2::aes(label = uf),
        hjust = 1.4, size = 3, fontface = 'bold', show.legend = F
      ) +
      ggplot2::geom_text(
        data = \(d) dplyr::filter(d, ano == max(anos_tabs), !is.na(rank)),
        ggplot2::aes(label = uf),
        hjust = -0.4, size = 3, fontface = 'bold', show.legend = F
      ) +
      ggplot2::scale_y_reverse(
        breaks = 1:top_n,
        labels = paste0(1:top_n, '\u00ba'),
        expand = ggplot2::expansion(mult = c(0.08, 0.08))
      ) +
      ggplot2::scale_x_continuous(
        breaks = anos_tabs,
        expand = ggplot2::expansion(mult = c(0.12, 0.12))
      ) +
      ggplot2::scale_color_manual(values = cores_macro, name = 'Macrorregião') +
      ggplot2::scale_fill_manual(values  = cores_macro, guide = 'none') +
      ggplot2::labs(
        title    = paste0('Ranking de MPI por UF: top ', top_n, ' por ano'),
        subtitle = paste0(
          'Posição das UFs pelo MPI em cada ano selecionado',
          '\n k de referência: ', k_ref
        ),
        x = NULL, y = 'Posição no ranking', caption = caption_base
      ) +
      theme_artigo() +
      ggplot2::theme(
        axis.text.x        = ggplot2::element_text(angle = 45, hjust = 1),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(color = 'grey70', linetype = 'dashed')
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(nrow = 1, override.aes = list(linewidth = 2.5))
      )
  }

  .plot_contrib_arranjo_serie <- function() {
    dt |>
      dplyr::mutate(tipo_arranjo = factor(.tipo_arranjo_map[as.character(arranjo_full)],levels = niveis_tipo)) |>
      dplyr::filter(!is.na(tipo_arranjo)) |>
      dplyr::summarise(
        H = weighted.mean(score >= k_ref,w = peso,na.rm = T),
        A = weighted.mean(score[score >= k_ref],w = peso[score >= k_ref],na.rm = T),
        MPI = H*A, pop = sum(peso,na.rm = T), .by = c(ano,tipo_arranjo)
      ) |>
      dplyr::mutate(contrib_abs = (pop/sum(pop))*MPI, contrib_rel = 100*contrib_abs/sum(contrib_abs), .by = ano) |>
      dplyr::arrange(ano) |>
      ggplot2::ggplot(ggplot2::aes(x = ano,y = contrib_rel,fill = tipo_arranjo)) +
      ggplot2::geom_area(color = 'white',linewidth = 0.25,alpha = 0.95) +
      ggplot2::scale_fill_manual(values = .cores_tipo_arranjo,name = 'Arranjo') +
      scale_x_anos() +
      ggplot2::scale_y_continuous(limits = c(0,100),oob = scales::squish,
                                  labels = scales::label_number(decimal.mark = ',',accuracy = 1,suffix = '%'),
                                  expand = ggplot2::expansion(mult = c(0,0))) +
      ggplot2::labs(title = 'Composição do MPI nacional por arranjo domiciliar: Brasil',
                    subtitle = paste0('Participação relativa de cada arranjo no MPI nacional (%); k = ',k_ref),
                    x = NULL,y = '% do MPI nacional',caption = caption_base) +
      theme_artigo() + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank()) +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))
  }

  # 6.5 Mapas ------------------------------------------------------------------

  .mapa_uf <- function(ano_ref) {
    ano_geo  <- .ano_para_shapefile(ano_ref)
    uf_geo   <- shapefiles[[as.character(ano_geo)]]
    score_uf <- dt |>
      dplyr::filter(ano == ano_ref) |>
      dplyr::summarise(score_medio = weighted.mean(score,w = peso,na.rm = T), .by = c(uf,arranjo_full)) |>
      tidyr::complete(uf = unique(uf_geo$abbrev_state), arranjo_full = ordem_arranjo, fill = list(score_medio = NA_real_)) |>
      dplyr::mutate(
        tipo_arranjo = factor(tipo_mapa_vec[as.character(arranjo_full)],levels = niveis_tipo),
        sexo = factor(sexo_mapa_vec[as.character(arranjo_full)],levels = niveis_sexo)
      )
    uf_geo |>
      dplyr::left_join(score_uf,by = c('abbrev_state' = 'uf'),relationship = 'many-to-many') |>
      ggplot2::ggplot(ggplot2::aes(fill = score_medio)) +
      ggplot2::geom_sf(color = 'white',linewidth = 0.2) +
      ggplot2::coord_sf(xlim = c(-73.99,-28.84),ylim = c(-33.75,5.28),expand = F) +
      ggplot2::facet_grid(sexo~tipo_arranjo,switch = 'y') +
      ggplot2::scale_fill_gradientn(
        colours = c('#00b8d4', '#1a3a9e', '#7b1fa2', '#c2185b', '#c62828'),
        na.value = 'grey75', limits = limites_escala,
        labels = scales::label_number(decimal.mark = ',',accuracy = 0.01),
        guide = colorbar_padrao('Score médio')
      ) +
      ggplot2::labs(title = paste0('Score médio de privação por arranjo domiciliar: ',ano_ref), caption = caption_base) +
      theme_mapa()
  }

  .plot_delta_mpi_uf <- function() {
    uf_geo <- shapefiles[['2020']]
    mpi_uf <- dt |>
      dplyr::filter(ano %in% anos_maps) |>
      dplyr::summarise(
        H = weighted.mean(score >= k_ref,w = peso,na.rm = T),
        A = weighted.mean(score[score >= k_ref],w = peso[score >= k_ref],na.rm = T),
        MPI = 100*H*A, .by = c(ano,uf)
      ) |>
      tidyr::pivot_wider(id_cols = uf,names_from = ano,values_from = MPI,names_prefix = 'y_') |>
      dplyr::mutate(delta = .data[[paste0('y_',max(anos_maps))]]-.data[[paste0('y_',min(anos_maps))]])
    lim <- max(abs(mpi_uf$delta),na.rm = T)
    uf_geo |>
      dplyr::left_join(mpi_uf,by = c('abbrev_state' = 'uf'),relationship = 'many-to-many') |>
      ggplot2::ggplot(ggplot2::aes(fill = delta)) +
      ggplot2::geom_sf(color = 'white',linewidth = 0.25) +
      ggplot2::coord_sf(xlim = c(-73.99,-28.84),ylim = c(-33.75,5.28),expand = F) +
      ggplot2::scale_fill_distiller(palette = 'RdBu',direction = -1,na.value = 'grey75',limits = c(-lim,lim),
                                    labels = scales::label_number(decimal.mark = ',',accuracy = 0.1,style_positive = 'plus'),
                                    guide = colorbar_padrao('Δ MPI (p.p.)')) +
      ggplot2::labs(
        title = paste0('Variação do MPI por UF: ',min(anos_maps),' → ',max(anos_maps)),
        subtitle = paste0('Diferença absoluta em pontos percentuais (k = ',k_ref,')'), caption = caption_base
      ) + theme_mapa()
  }

  .plot_ranking_uf <- function() {
    # UFs ordenadas pelo ano mais recente
    ano_ord <- max(anos_maps)

    dados <- dt |>
      dplyr::filter(ano %in% anos_maps) |>
      dplyr::summarise(
        H   = 100 * weighted.mean(score  >=  k_ref,         w = peso,             na.rm = T),
        A   = 100 * weighted.mean(score[score  >=  k_ref],  w = peso[score  >=  k_ref], na.rm = T),
        MPI = H * A / 100,
        .by = c(ano, uf)
      ) |>
      dplyr::mutate(macro = factor(macro_map[uf], levels = niveis_macro))

    ordem_uf <- dados |>
      dplyr::filter(ano  ==  ano_ord) |>
      dplyr::arrange(MPI) |>
      dplyr::pull(uf)

    dados |>
      dplyr::mutate(
        uf  = factor(uf, levels = ordem_uf),
        ano = factor(ano, levels = sort(anos_maps))
      ) |>
      ggplot2::ggplot(ggplot2::aes(x = MPI, y = uf, fill = macro)) +
      ggplot2::geom_col(width = 0.7) +
      ggplot2::geom_text(
        ggplot2::aes(label = scales::number(MPI, accuracy = 0.1, decimal.mark = ',')),
        hjust = -0.15, size = 3
      ) +
      ggplot2::facet_wrap(~ ano, ncol = length(anos_maps), scales = 'free_x') +
      ggplot2::scale_fill_manual(values = cores_macro, name = 'Macrorregião') +
      ggplot2::scale_x_continuous(
        expand = ggplot2::expansion(mult = c(0, 0.15)),
        labels = scales::label_number(decimal.mark = ',', accuracy = 1)
      ) +
      ggplot2::labs(
        title    = paste0('Ranking de UFs por MPI: anos selecionados'),
        subtitle = paste0('k de referência = ', k_ref, '; UFs ordenadas pelo ano de ', ano_ord),
        x = 'MPI (%)', y = NULL, caption = caption_base
      ) +
      theme_artigo() +
      ggplot2::theme(
        axis.text.x        = ggplot2::element_text(angle = 0, hjust = 0.5),
        axis.text.y        = ggplot2::element_text(face = 'bold'),
        strip.text         = ggplot2::element_text(face = 'bold'),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.spacing      = ggplot2::unit(3.5, 'lines')
      ) +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))
  }

  # 6.6 Convergência -----------------------------------------------------------

  .plot_sigma_convergencia <- function() {
    dados <- .mpi_uf_serie() |>
      dplyr::summarise(
        dp  = sd(MPI,   na.rm = T),
        med = mean(MPI, na.rm = T),
        cv  = 100 * dp / med,
        .by = ano
      ) |>
      dplyr::arrange(ano)

    # Fator linear que mapeia CV no eixo do DP para o eixo secundário
    fator <- max(dados$dp, na.rm = T) / max(dados$cv, na.rm = T)

    dados |>
      dplyr::mutate(cv_scaled = cv * fator) |>
      ggplot2::ggplot(ggplot2::aes(x = ano)) +
      ggplot2::geom_line(ggplot2::aes(y = dp,        color = 'Desvio padrão (p.p.)'),        linewidth = 0.8) +
      ggplot2::geom_point(ggplot2::aes(y = dp,       color = 'Desvio padrão (p.p.)'),
                          fill = 'white', shape = 21, size = 2.2, stroke = 0.9) +
      ggplot2::geom_line(ggplot2::aes(y = cv_scaled, color = 'Coeficiente de variação (%)'),
                         linewidth = 0.8, linetype = 'dashed') +
      ggplot2::geom_point(ggplot2::aes(y = cv_scaled, color = 'Coeficiente de variação (%)'),
                          fill = 'white', shape = 21, size = 2.2, stroke = 0.9) +
      ggplot2::scale_color_manual(
        values = c('Desvio padrão (p.p.)' = '#0277bd', 'Coeficiente de variação (%)' = '#b71c1c'),
        name   = NULL
      ) +
      scale_x_anos() +
      ggplot2::scale_y_continuous(
        name     = 'Desvio padrão (p.p.)',
        limits   = c(0, NA),
        labels   = scales::label_number(decimal.mark = ',', accuracy = 0.1),
        expand   = ggplot2::expansion(mult = c(0, 0.08)),
        sec.axis = ggplot2::sec_axis(
          transform = \(x) x / fator,
          name      = 'Coeficiente de variação (%)',
          labels    = scales::label_number(decimal.mark = ',', accuracy = 0.1)
        )
      ) +
      ggplot2::labs(
        title    = 'Sigma-convergência do MPI entre UFs: Brasil',
        subtitle = paste0('Dispersão interestadual do MPI ao longo do tempo (k de referência: ', k_ref, ')'),
        x = NULL, caption = caption_base
      ) +
      theme_artigo() +
      ggplot2::guides(color = ggplot2::guide_legend(nrow = 1, override.aes = list(linewidth = 2.5)))
  }

  .plot_beta_convergencia <- function() {
    serie <- .mpi_uf_serie()
    base  <- dplyr::inner_join(
      serie |> dplyr::filter(ano == min(anos_beta)) |> dplyr::select(uf,macro,mpi_ini = MPI),
      serie |> dplyr::filter(ano == max(anos_beta)) |> dplyr::select(uf,mpi_fim = MPI),
      by = 'uf'
    ) |> dplyr::mutate(delta = mpi_fim-mpi_ini)

    modelo <- lm(delta~mpi_ini,data = base)
    r2     <- summary(modelo)$r.squared
    b1     <- coef(modelo)[['mpi_ini']]
    sinal  <- if(b1<0) 'Convergência (β < 0)' else 'Divergência (β > 0)'

    base |>
      ggplot2::ggplot(ggplot2::aes(x = mpi_ini,y = delta)) +
      ggplot2::geom_hline(yintercept = 0,linetype = 'dashed',color = 'grey60',linewidth = 0.5) +
      ggplot2::geom_smooth(method = 'lm',formula = y~x,se = T,color = 'grey40',fill = 'grey85',linewidth = 0.7) +
      ggplot2::geom_point(ggplot2::aes(fill = macro),shape = 21,size = 3.5,stroke = 0.5,color = 'white') +
      ggrepel::geom_text_repel(ggplot2::aes(label = uf,color = macro),size = 2.8,fontface = 'bold',
                               max.overlaps = Inf,box.padding = 0.3,point.padding = 0.2,
                               segment.size = 0.3,segment.color = 'grey70',show.legend = F) +
      ggplot2::scale_fill_manual(values = cores_macro,name = 'Macrorregião') +
      ggplot2::scale_color_manual(values = cores_macro,guide = 'none') +
      ggplot2::scale_x_continuous(labels = scales::label_number(decimal.mark = ',',accuracy = 1)) +
      ggplot2::scale_y_continuous(labels = scales::label_number(decimal.mark = ',',accuracy = 1,style_positive = 'plus')) +
      ggplot2::annotate('text',x = max(base$mpi_ini,na.rm = T),y = max(base$delta,na.rm = T),
                        label = paste0(sinal,'\nR² = ',scales::number(r2,accuracy = 0.01)),
                        hjust = 1,vjust = 1,size = 3.5,color = 'grey30',fontface = 'italic') +
      ggplot2::labs(
        title = paste0('Beta-convergência do MPI entre UFs: ',min(anos_beta),' → ',max(anos_beta)),
        subtitle = paste0('Variação acumulada do MPI (p.p.) vs. nível inicial (k = ',k_ref,')'),
        x = paste0('MPI em ',min(anos_beta),' (%)'),
        y = paste0('Δ MPI ',min(anos_beta),' → ',max(anos_beta),' (p.p.)'),
        caption = caption_base
      ) +
      theme_artigo() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0,hjust = 0.5)) +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1,override.aes = list(size = 4)))
  }

  .plot_beta_conv_periodos <- function() {
    
    anos_dt   <- sort(unique(dt$ano))
    niveis_par <- purrr::map_chr(anos_decp, \(p) paste0(
      anos_dt[which.min(abs(anos_dt - p[1]))], '\u2192',
      anos_dt[which.min(abs(anos_dt - p[2]))]
    ))
    
    # MPI por arranjo × UF × ano
    serie_uf <- dt |>
      dplyr::mutate(tipo_arranjo = factor(tipo_mapa_vec[as.character(arranjo_full)], levels = niveis_tipo)) |>
      dplyr::filter(!is.na(tipo_arranjo)) |>
      dplyr::summarise(
        H   = weighted.mean(score >= k_ref,        w = peso,             na.rm = T),
        A   = weighted.mean(score[score >= k_ref], w = peso[score >= k_ref], na.rm = T),
        MPI = 100 * H * A,
        .by = c(ano, uf, tipo_arranjo)
      )
    
    # MPI global por UF × ano
    serie_global <- dt |>
      dplyr::summarise(
        H   = weighted.mean(score >= k_ref,        w = peso,             na.rm = T),
        A   = weighted.mean(score[score >= k_ref], w = peso[score >= k_ref], na.rm = T),
        MPI = 100 * H * A,
        .by = c(ano, uf)
      )
    
    # Beta por arranjo × período
    res_arranjo <- purrr::map_dfr(anos_decp, \(p) {
      t0  <- anos_dt[which.min(abs(anos_dt - p[1]))]
      t1  <- anos_dt[which.min(abs(anos_dt - p[2]))]
      lab <- paste0(t0, '\u2192', t1)
      purrr::map_dfr(niveis_tipo, \(tipo) {
        base <- dplyr::inner_join(
          serie_uf |> dplyr::filter(ano == t0, tipo_arranjo == tipo) |> dplyr::select(uf, mpi_ini = MPI),
          serie_uf |> dplyr::filter(ano == t1, tipo_arranjo == tipo) |> dplyr::select(uf, mpi_fim = MPI),
          by = 'uf'
        ) |> dplyr::mutate(delta = mpi_fim - mpi_ini)
        if (nrow(base) < 5) return(NULL)
        modelo <- lm(delta ~ mpi_ini, data = base)
        tibble::tibble(
          periodo      = lab,
          tipo_arranjo = tipo,
          beta         = coef(modelo)[['mpi_ini']],
          r2           = summary(modelo)$r.squared
        )
      })
    }) |>
      dplyr::mutate(
        periodo      = factor(periodo, levels = niveis_par),
        tipo_arranjo = factor(tipo_arranjo, levels = niveis_tipo)
      )
    
    # Beta global por período
    res_global <- purrr::map_dfr(anos_decp, \(p) {
      t0  <- anos_dt[which.min(abs(anos_dt - p[1]))]
      t1  <- anos_dt[which.min(abs(anos_dt - p[2]))]
      lab <- paste0(t0, '\u2192', t1)
      base <- dplyr::inner_join(
        serie_global |> dplyr::filter(ano == t0) |> dplyr::select(uf, mpi_ini = MPI),
        serie_global |> dplyr::filter(ano == t1) |> dplyr::select(uf, mpi_fim = MPI),
        by = 'uf'
      ) |> dplyr::mutate(delta = mpi_fim - mpi_ini)
      if (nrow(base) < 5) return(NULL)
      modelo <- lm(delta ~ mpi_ini, data = base)
      tibble::tibble(
        periodo = factor(lab, levels = niveis_par),
        beta    = coef(modelo)[['mpi_ini']]
      )
    })
    
    ggplot2::ggplot() +
      # Barras por arranjo
      ggplot2::geom_col(
        data     = res_arranjo,
        ggplot2::aes(x = periodo, y = beta, fill = tipo_arranjo),
        position = ggplot2::position_dodge(width = 0.75),
        width    = 0.65, alpha = 0.85
      ) +
      ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', linewidth = 0.5, color = 'grey40') +
      # Labels nas barras
      ggplot2::geom_text(
        data     = res_arranjo,
        ggplot2::aes(
          x     = periodo, y = beta, group = tipo_arranjo,
          label = scales::number(beta, accuracy = 0.01, decimal.mark = ',', style_positive = 'plus'),
          vjust = ifelse(beta < 0, 1.4, -0.4)
        ),
        position = ggplot2::position_dodge(width = 0.75),
        size = 2.5, color = 'grey20'
      ) +
      # Linha global
      ggplot2::geom_line(
        data = res_global,
        ggplot2::aes(x = periodo, y = beta, group = 1),
        color = 'grey20', linewidth = 1.2, linetype = 'solid'
      ) +
      ggplot2::geom_point(
        data = res_global,
        ggplot2::aes(x = periodo, y = beta),
        shape = 23, size = 3.5, fill = 'white', color = 'grey20', stroke = 1
      ) +
      ggplot2::scale_fill_manual(values = cores_arranjo, name = 'Arranjo') +
      ggplot2::scale_y_continuous(
        labels = scales::label_number(decimal.mark = ',', accuracy = 0.01, style_positive = 'plus')
      ) +
      ggplot2::labs(
        title    = 'Beta-convergência do MPI entre UFs por arranjo domiciliar: Brasil',
        subtitle = 'β calculado sobre as UFs para cada período e tipo de arranjo; losango = β global',
        x = NULL, y = 'β (beta-convergência)',
        caption  = caption_base
      ) +
      theme_artigo() +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        axis.text.x        = ggplot2::element_text(angle = 0, hjust = 0.5)
      ) +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))
  }
  
  # 6.7 Granularidade ----------------------------------------------------------

  .plot_heatmap_ind_ano <- function() {
    .privacao_ind_ano() |>
      dplyr::mutate(label_ind = factor(label_ind,levels = rev(labels_ind[ordem_ind]))) |>
      ggplot2::ggplot(ggplot2::aes(x = ano,y = label_ind,fill = taxa_privacao)) +
      ggplot2::geom_tile(color = 'white',linewidth = 0.3) +
      ggplot2::geom_text(ggplot2::aes(
        label = dplyr::if_else(!is.na(taxa_privacao),scales::number(100*taxa_privacao,accuracy = 1),'-'),
        color = dplyr::if_else(taxa_privacao>0.55|is.na(taxa_privacao),'claro', 'escuro')
      ),size = 2.2) +
      ggplot2::facet_grid(dimensao~.,scales = 'free_y',space = 'free_y',switch = 'y') +
      ggplot2::scale_fill_distiller(palette = 'YlOrRd',direction = 1,na.value = 'grey75',limits = c(0,1),
                                    labels = scales::label_percent(decimal.mark = ',',accuracy = 1),
                                    guide = colorbar_padrao('Taxa de privação (%)')) +
      ggplot2::scale_color_manual(values = c('claro' = 'white', 'escuro' = 'grey20'),guide = 'none') +
      ggplot2::scale_x_continuous(breaks = anos_dt,expand = ggplot2::expansion(mult = c(0,0))) +
      ggplot2::labs(title = 'Taxa de privação por indicador e ano: Brasil',
                    subtitle = 'Proporção ponderada da população privada em cada indicador',
                    x = NULL,y = NULL,caption = caption_base) +
      theme_artigo() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90,hjust = 1),
        strip.text.y.left = ggplot2::element_text(face = 'bold',angle = 0,hjust = 1,margin = ggplot2::margin(r = 4)),
        strip.placement = 'outside', panel.grid = ggplot2::element_blank()
      )
  }

  .plot_heatmap_ind_uf <- function(ano_ref = max(anos_maps)) {
    periodo_ref <- if(ano_ref <= 2015) 'pnad' else 'pnadc'
    indicadores <- names(dim_map)
    ordem_uf <- dt |>
      dplyr::filter(ano == ano_ref) |>
      dplyr::summarise(
        H = weighted.mean(score >= k_ref,w = peso,na.rm = T),
        A = weighted.mean(score[score >= k_ref],w = peso[score >= k_ref],na.rm = T),
        MPI = H*A, .by = uf
      ) |> dplyr::arrange(MPI) |> dplyr::pull(uf)

    dt |>
      dplyr::filter(ano == ano_ref) |>
      dplyr::reframe(dplyr::across(dplyr::any_of(indicadores),\(x) weighted.mean(x,w = peso,na.rm = T)), .by = uf) |>
      tidyr::pivot_longer(dplyr::any_of(indicadores),names_to = 'indicador',values_to = 'taxa_privacao') |>
      dplyr::mutate(
        dimensao = factor(dim_map[indicador],levels = ordem_dim),
        indicador = factor(indicador,levels = ordem_ind),
        label_ind = factor(labels_ind[indicador],levels = rev(labels_ind[ordem_ind])),
        uf = factor(uf,levels = ordem_uf)
      ) |>
      ggplot2::ggplot(ggplot2::aes(x = uf,y = label_ind,fill = taxa_privacao)) +
      ggplot2::geom_tile(color = 'white',linewidth = 0.3) +
      ggplot2::geom_text(
        ggplot2::aes(
          label = dplyr::if_else(!is.na(taxa_privacao),
                                 scales::number(100 * taxa_privacao, accuracy = 1), '-'),
          color = dplyr::if_else(taxa_privacao > 0.55 | is.na(taxa_privacao), 'claro', 'escuro')
        ), size = 2.2
      ) +
      ggplot2::facet_grid(dimensao~.,scales = 'free_y',space = 'free_y',switch = 'y') +
      ggplot2::scale_fill_distiller(palette = 'YlOrRd',direction = 1,na.value = 'grey75',limits = c(0,1),
                                    labels = scales::label_percent(decimal.mark = ',',accuracy = 1),
                                    guide = colorbar_padrao('Taxa de privação (%)')) +
      ggplot2::scale_color_manual(values = c('claro' = 'white', 'escuro' = 'grey20'), guide = 'none') +
      ggplot2::labs(title = paste0('Taxa de privação por indicador e UF: ',ano_ref),
                    subtitle = 'UFs ordenadas por MPI total (menor → maior, da esquerda para a direita)',
                    x = NULL,y = NULL,caption = caption_base) +
      theme_artigo() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(face = 'bold',angle = 90,hjust = 1),
        strip.text.y.left = ggplot2::element_text(face = 'bold',angle = 0,hjust = 1,margin = ggplot2::margin(r = 4)),
        strip.placement = 'outside', panel.grid = ggplot2::element_blank()
      )
  }

  .plot_coocorrencia <- function(ano_ref = max(anos_maps)) {
    indicadores <- names(dim_map)
    periodo_ref <- if(ano_ref <= 2015) 'pnad' else 'pnadc'
    sub  <- dt[ano == ano_ref, c('peso',indicadores), with = F]
    lim  <- 0.35
    pares <- tidyr::expand_grid(ind_x = indicadores,ind_y = indicadores) |>
      dplyr::mutate(
        cooc = purrr::map2_dbl(ind_x,ind_y,\(ix,iy)
                               weighted.mean(as.numeric(sub[[ix]]) == 1&as.numeric(sub[[iy]]) == 1,w = sub[['peso']],na.rm = T)),
        label_x = factor(labels_ind[ind_x],levels = labels_ind[indicadores]),
        label_y = factor(labels_ind[ind_y],levels = rev(labels_ind[indicadores])),
        dim_x = factor(dim_map[ind_x],levels = ordem_dim),
        dim_y = factor(dim_map[ind_y],levels = rev(ordem_dim)),
        diagonal = ind_x == ind_y
      )
    pares |>
      ggplot2::ggplot(ggplot2::aes(x = label_x,y = label_y,fill = cooc)) +
      ggplot2::geom_tile(color = 'white',linewidth = 0.4) +
      ggplot2::geom_text(ggplot2::aes(
        label = scales::number(100*cooc,accuracy = 0.1),
        color = dplyr::if_else(cooc >= lim,'claro', 'escuro')
      ),size = 2.2) +
      ggplot2::geom_tile(data = dplyr::filter(pares,diagonal),color = 'grey30',fill = NA,linewidth = 0.6) +
      ggplot2::scale_fill_distiller(palette = 'YlOrRd',direction = 1,na.value = 'grey75',limits = c(0,NA),
                                    labels = scales::label_percent(decimal.mark = ',',accuracy = 1),
                                    guide = colorbar_padrao('Co-ocorrência (%)')) +
      ggplot2::scale_color_manual(values = c('claro' = 'white', 'escuro' = 'grey20'),guide = 'none') +
      ggplot2::facet_grid(dim_y~dim_x,scales = 'free',space = 'free',switch = 'both') +
      ggplot2::labs(title = paste0('Matriz de co-ocorrência de privaçõees: ',ano_ref),
                    subtitle = 'Proporção ponderada da população privada simultaneamente em cada par (%)\nDiagonal = taxa de privação individual',
                    x = NULL,y = NULL,caption = caption_base) +
      theme_artigo() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45,hjust = 1),
        strip.text.x = ggplot2::element_text(face = 'bold',margin = ggplot2::margin(b = 4)),
        strip.text.y.left = ggplot2::element_text(face = 'bold',angle = 0,hjust = 1,margin = ggplot2::margin(r = 4)),
        strip.placement = 'outside', panel.grid = ggplot2::element_blank(), panel.spacing = ggplot2::unit(0.6,'lines')
      )
  }

  .plot_coocorrencia_serie <- function() {
    pares_sel <- list(c('E1', 'P1'),c('E1', 'V1'),c('D1', 'B1'),c('D3', 'B3'),c('P1', 'V1'))
    purrr::map(pares_sel,\(par){
      ix <- par[1]; iy <- par[2]
      dt |>
        dplyr::summarise(
          cooc = weighted.mean(as.numeric(.data[[ix]]) == 1&as.numeric(.data[[iy]]) == 1,w = peso,na.rm = T), .by = ano
        ) |>
        dplyr::mutate(par = paste0(labels_ind[ix],'  ×  ',labels_ind[iy]))
    }) |>
      purrr::list_rbind() |> dplyr::arrange(ano) |>
      ggplot2::ggplot(ggplot2::aes(x = ano,y = 100*cooc,color = par,group = par)) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point(ggplot2::aes(fill = par),shape = 21,size = 2,stroke = 0.6,color = 'white') +
      ggplot2::scale_color_brewer(palette = 'Dark2',name = 'Par de indicadores') +
      ggplot2::scale_fill_brewer(palette = 'Dark2',guide = 'none') +
      scale_x_anos() +
      ggplot2::scale_y_continuous(limits = c(0,NA),labels = scales::label_number(decimal.mark = ',',accuracy = 1,suffix = '%'),expand = ggplot2::expansion(mult = c(0,0.05))) +
      ggplot2::labs(title = 'Evolução da co-ocorrência de privaçõees: Brasil',
                    subtitle = 'Proporção ponderada da população privada simultaneamente em pares selecionados',
                    x = NULL,y = 'Co-ocorrência (%)',caption = caption_base) +
      theme_artigo() +
      ggplot2::guides(color = ggplot2::guide_legend(nrow = 2,override.aes = list(linewidth = 2.5)))
  }

  # 6.8 Densidade --------------------------------------------------------------

  .plot_curva_incidencia <- function() {
    ks <- seq(0,1, length.out = 500)
    cores_anos <- setNames(
      colorRampPalette(RColorBrewer::brewer.pal(11, 'Spectral'))(length(anos_dens)),
      as.character(sort(anos_dens))
    )
    purrr::map(anos_dens,\(yr){
      sub <- dt[ano == yr]
      purrr::map(ks,\(k) tibble::tibble(ano = yr,k = k,H = 100*weighted.mean(sub$score >= k,w = sub$peso,na.rm = T))) |>
        purrr::list_rbind()
    }) |>
      purrr::list_rbind() |>
      dplyr::mutate(ano = factor(ano,levels = sort(anos_dens))) |>
      ggplot2::ggplot(ggplot2::aes(x = k,y = H,color = ano,group = ano)) +
      ggplot2::geom_line(linewidth = 0.8,alpha = 0.9) +
      ggplot2::geom_vline(xintercept = k_ref,linetype = 'dashed',linewidth = 0.5,color = 'grey40') +
      ggplot2::annotate('text',x = k_ref+0.02,y = 95,label = paste0('k = ',k_ref),hjust = 0,size = 3.5,color = 'grey40',fontface = 'italic') +
      ggplot2::scale_color_manual(values = cores_anos,name = 'Ano') +
      ggplot2::scale_x_continuous(labels = scales::label_percent(decimal.mark = ',',accuracy = 1),expand = ggplot2::expansion(mult = c(0.01,0.01))) +
      ggplot2::scale_y_continuous(limits = c(0,100),labels = scales::label_number(decimal.mark = ',',accuracy = 1,suffix = '%'),expand = ggplot2::expansion(mult = c(0,0.02))) +
      ggplot2::labs(title = 'Curva de incidência de privação: Brasil',
                    subtitle = 'Proporção ponderada da população com score ≥ k, para todo k ∈ [0,1]',
                    x = 'Cutoff k',y = 'H(k): Incidência (%)',caption = caption_base) +
      theme_artigo() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0,hjust = 0.5)) +
      ggplot2::guides(color = ggplot2::guide_legend(nrow = 1,override.aes = list(linewidth = 2.5)))
  }

  .plot_densidade_score <- function() {
    cores_todos <- setNames(
      colorRampPalette(c('#4a148c', '#f9a825', '#e65100'))(length(anos_dens)),
      as.character(sort(anos_dens))
    )
    dt |>
      dplyr::filter(ano %in% anos_dens) |>
      dplyr::mutate(
        survey=factor(dplyr::if_else(ano<=2015,'PNAD Anual', 'PNAD Contínua'),levels=c('PNAD Anual', 'PNAD Contínua')),
        ano=factor(ano,levels=sort(anos_dens))
      ) |>
      ggplot2::ggplot(ggplot2::aes(x=score,color=ano,weight=peso/sum(peso))) +
      ggplot2::geom_density(bw=0.02,linewidth=0.8,alpha=0.85) +
      ggplot2::geom_vline(xintercept = k_ref, linetype = 'dashed', linewidth = 0.5, color = 'grey40') +
      ggplot2::annotate('text', x = k_ref + 0.02, y = Inf,
                        label = paste0('k = ', k_ref), hjust = 0, vjust = 1.5,
                        size = 3.5, color = 'grey40', fontface = 'italic') +
      ggplot2::scale_color_manual(values = cores_todos, name = 'Ano') +
      ggplot2::scale_x_continuous(
        limits = c(0, 1),
        labels = scales::label_percent(decimal.mark = ',', accuracy = 1),
        expand = ggplot2::expansion(mult = c(0.01, 0.01))
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::label_number(decimal.mark = ',', accuracy = 0.1),
        expand = ggplot2::expansion(mult = c(0, 0.05))
      ) +
      ggplot2::labs(
        title    = 'Distribuição do score de privação: Brasil',
        subtitle = paste0('Densidade ponderada do score individual; linha tracejada = k de referência (', k_ref, ')'),
        x = 'Score de privação', y = 'Densidade', caption = caption_base
      ) +
      theme_artigo() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5)) +
      ggplot2::guides(color = ggplot2::guide_legend(nrow = 2, override.aes = list(linewidth = 2.5)))
  }

  .plot_densidade_score_grupo <- function(ano_ref = max(anos_maps)) {
    dt[ano  ==  ano_ref] |>
      dplyr::mutate(
        tipo_arranjo = factor(.tipo_arranjo_map[as.character(arranjo_full)], levels = niveis_tipo)
      ) |>
      dplyr::filter(!is.na(tipo_arranjo)) |>
      dplyr::mutate(peso_norm = peso / sum(peso), .by = tipo_arranjo) |>
      ggplot2::ggplot(ggplot2::aes(x = score, color = tipo_arranjo, weight = peso_norm)) +
      ggplot2::geom_density(bw = 0.02, linewidth = 0.8, alpha = 0.85) +
      ggplot2::geom_vline(xintercept = k_ref, linetype = 'dashed', linewidth = 0.5, color = 'grey40') +
      ggplot2::annotate('text', x = k_ref + 0.02, y = Inf,
                        label = paste0('k = ', k_ref), hjust = 0, vjust = 1.5,
                        size = 3.5, color = 'grey40', fontface = 'italic') +
      ggplot2::scale_color_manual(values = .cores_tipo_arranjo, name = 'Arranjo') +
      ggplot2::scale_x_continuous(
        limits = c(0, 1),
        labels = scales::label_percent(decimal.mark = ',', accuracy = 1),
        expand = ggplot2::expansion(mult = c(0.01, 0.01))
      ) +
      ggplot2::scale_y_continuous(
        limits = c(0, NA),
        labels = scales::label_number(decimal.mark = ',', accuracy = 0.1),
        expand = ggplot2::expansion(mult = c(0, 0.05))
      ) +
      ggplot2::labs(
        title    = paste0('Distribuição do score por arranjo domiciliar: ', ano_ref),
        subtitle = paste0('Densidade ponderada por tipo de arranjo; linha tracejada = k (', k_ref, ')'),
        x = 'Score de privação', y = 'Densidade', caption = caption_base
      ) +
      theme_artigo() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5)) +
      ggplot2::guides(color = ggplot2::guide_legend(nrow = 1, override.aes = list(linewidth = 2.5)))
  }

  # 6.9 Decomposição -----------------------------------------------------------

  .plot_decomp_pares <- function() {
    serie <- .hamp_serie()
    # Níveis do factor construídos com o mesmo padrão do campo `par`
    niveis_par <- purrr::map_chr(anos_decp, \(p) paste0(p[1], ' → ', p[2]))

    purrr::map(anos_decp, \(par) {
      t0 <- par[1]; t1 <- par[2]
      s0 <- serie[serie$ano  ==  t0, ]; s1 <- serie[serie$ano  ==  t1, ]
      dH <- s1$H - s0$H; dA <- s1$A - s0$A
      tibble::tibble(
        par       = paste0(t0, ' → ', t1),
        t0        = t0, t1 = t1,
        delta_mpi = s1$MPI - s0$MPI,
        ef_H      = dH * (s0$A + s1$A) / 2 / 100,
        ef_A      = dA * (s0$H + s1$H) / 2 / 100
      )
    }) |>
      purrr::list_rbind() |>
      dplyr::mutate(par = factor(par, levels = niveis_par)) |>
      tidyr::pivot_longer(c(ef_H,ef_A),names_to = 'efeito',values_to = 'valor') |>
      dplyr::mutate(efeito = factor(efeito,levels = c('ef_H', 'ef_A'),labels = c('Efeito H (incidência)', 'Efeito A (intensidade)'))) |>
      ggplot2::ggplot(ggplot2::aes(x = par,y = valor,fill = efeito)) +
      ggplot2::geom_col(width = 0.65,position = 'stack') +
      ggplot2::geom_hline(yintercept = 0,linewidth = 0.4,color = 'grey30') +
      ggplot2::geom_point(ggplot2::aes(y = delta_mpi,group = par),shape = 23,size = 3,fill = 'white',color = 'grey20', stroke = 0.9,
                          data = \(d) dplyr::distinct(d,par,delta_mpi)) +
      ggplot2::scale_fill_manual(values = c('Efeito H (incidência)' = 'navyblue', 'Efeito A (intensidade)' = 'palegreen3')) +
      ggplot2::scale_y_continuous(labels = scales::label_number(decimal.mark = ',',accuracy = 0.1,style_positive = 'plus',suffix = ' p.p.')) +
      ggplot2::labs(title = 'Decomposição temporal do MPI: Brasil',
                    subtitle = paste0('ΔMPI = Efeito H · Ā + Efeito A · H̄ (decomposição simétrica; k de referência: ',k_ref),                    
                    x = NULL,y = 'Contribuição (p.p.)',fill = NULL,caption = caption_base) +
      theme_artigo() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0,hjust = 0.5), panel.grid.major.x = ggplot2::element_blank()) +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))
  }

  .plot_decomp_acumulada <- function() {
    serie    <- .hamp_serie()
    anos_ord <- sort(unique(serie$ano))
    decomp   <- purrr::map2(anos_ord[-length(anos_ord)],anos_ord[-1],\(t0,t1){
      s0 <- serie[serie$ano == t0,]; s1 <- serie[serie$ano == t1,]
      dH <- s1$H-s0$H; dA <- s1$A-s0$A
      tibble::tibble(t0 = t0,t1 = t1,ef_H = dH*(s0$A+s1$A)/2/100, ef_A = dA*(s0$H+s1$H)/2/100)
    }) |>
      purrr::list_rbind() |>
      dplyr::mutate(ef_H_acum = cumsum(ef_H), ef_A_acum = cumsum(ef_A), mpi_acum = ef_H_acum+ef_A_acum)

    decomp |>
      tidyr::pivot_longer(c(ef_H_acum,ef_A_acum),names_to = 'efeito',values_to = 'valor') |>
      dplyr::mutate(efeito = factor(efeito,levels = c('ef_H_acum', 'ef_A_acum'),
                                    labels = c('Efeito H acumulado', 'Efeito A acumulado'))) |>
      ggplot2::ggplot(ggplot2::aes(x = t1,y = valor,color = efeito,group = efeito)) +
      ggplot2::geom_hline(yintercept = 0,linewidth = 0.4,color = 'grey30') +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point(ggplot2::aes(fill = efeito),shape = 21,size = 2.2,stroke = 0.6,color = 'white') +
      ggplot2::geom_line(ggplot2::aes(y = mpi_acum,group = 1),color = 'grey20',linewidth = 0.9,linetype = 'dashed',
                         data = \(d) dplyr::distinct(d,t1,mpi_acum)) +
      ggplot2::annotate('text',x = max(decomp$t1),y = min(decomp$mpi_acum)*0.85,
                        label = 'ΔMPI total acumulado',hjust = 1,size = 3,color = 'grey20',fontface = 'italic') +
      ggplot2::scale_color_manual(values = c('Efeito H acumulado' = 'navyblue', 'Efeito A acumulado' = 'palegreen3')) +
      ggplot2::scale_fill_manual(values = c('Efeito H acumulado' = 'navyblue', 'Efeito A acumulado' = 'palegreen3')) +
      ggplot2::scale_x_continuous(breaks = anos_ord,expand = ggplot2::expansion(mult = c(0.01,0.01))) +
      ggplot2::scale_y_continuous(labels = scales::label_number(decimal.mark = ',',accuracy = 0.1,style_positive = 'plus',suffix = ' p.p.')) +
      ggplot2::labs(title = 'Decomposição acumulada do ΔMPI: Brasil',
                    subtitle = paste0('Contribuição acumulada de H e A desde ',ano_min,'; k de referência: ',k_ref),
                    x = NULL,y = 'Contribuição acumulada (p.p.)',color = NULL,fill = NULL,caption = caption_base) +
      theme_artigo() +
      ggplot2::guides(color = ggplot2::guide_legend(nrow = 1,override.aes = list(linewidth = 2.5)),fill = 'none')
  }

  # 6.10 Dominância estocástica ------------------------------------------------

  .plot_dominancia_arranjo <- function(ano_ref = max(anos_maps)) {
    sub <- dt[ano == ano_ref] |>
      dplyr::mutate(tipo_arranjo = factor(.tipo_arranjo_map[as.character(arranjo_full)],levels = niveis_tipo)) |>
      dplyr::filter(!is.na(tipo_arranjo))
    .curvas_hk(sub,'tipo_arranjo',niveis_tipo) |>
      dplyr::mutate(grupo = factor(grupo,levels = niveis_tipo)) |>
      ggplot2::ggplot(ggplot2::aes(x = k,y = H,color = grupo,group = grupo)) +
      ggplot2::geom_line(linewidth = 0.8,alpha = 0.9) +
      ggplot2::geom_vline(xintercept = k_ref,linetype = 'dashed',linewidth = 0.5,color = 'grey40') +
      ggplot2::annotate('text',x = k_ref+0.02,y = 97,label = paste0('k = ',k_ref),hjust = 0,size = 3.5,color = 'grey40',fontface = 'italic') +
      ggplot2::scale_color_manual(values = .cores_tipo_arranjo,name = 'Arranjo') +
      ggplot2::scale_x_continuous(labels = scales::label_percent(decimal.mark = ',',accuracy = 1),expand = ggplot2::expansion(mult = c(0.01,0.01))) +
      ggplot2::scale_y_continuous(limits = c(0,100),labels = scales::label_number(decimal.mark = ',',accuracy = 1,suffix = '%'),expand = ggplot2::expansion(mult = c(0,0.02))) +
      ggplot2::labs(title = paste0('Dominância estocástica por arranjo domiciliar: ',ano_ref),
                    subtitle = 'H(k) para todo k ∈ [0,1]; curva inferior domina estocasticamente',
                    x = 'Cutoff k',y = 'H(k): Incidência (%)',caption = caption_base) +
      theme_artigo() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0,hjust = 0.5)) +
      ggplot2::guides(color = ggplot2::guide_legend(nrow = 1,override.aes = list(linewidth = 2.5)))
  }

  .plot_dominancia_macro <- function(ano_ref = max(anos_maps)) {
    sub <- dt[ano == ano_ref] |>
      dplyr::mutate(macro = factor(macro_map[uf],levels = niveis_macro)) |>
      dplyr::filter(!is.na(macro))
    .curvas_hk(sub,'macro',niveis_macro) |>
      dplyr::mutate(grupo = factor(grupo,levels = niveis_macro)) |>
      ggplot2::ggplot(ggplot2::aes(x = k,y = H,color = grupo,group = grupo)) +
      ggplot2::geom_line(linewidth = 0.8,alpha = 0.9) +
      ggplot2::geom_vline(xintercept = k_ref,linetype = 'dashed',linewidth = 0.5,color = 'grey40') +
      ggplot2::annotate('text',x = k_ref+0.02,y = 97,label = paste0('k = ',k_ref),hjust = 0,size = 3.5,color = 'grey40',fontface = 'italic') +
      ggplot2::scale_color_manual(values = cores_macro,name = 'Macrorregião') +
      ggplot2::scale_x_continuous(labels = scales::label_percent(decimal.mark = ',',accuracy = 1),expand = ggplot2::expansion(mult = c(0.01,0.01))) +
      ggplot2::scale_y_continuous(limits = c(0,100),labels = scales::label_number(decimal.mark = ',',accuracy = 1,suffix = '%'),expand = ggplot2::expansion(mult = c(0,0.02))) +
      ggplot2::labs(title = paste0('Dominância estocástica por macrorregião: ',ano_ref),
                    subtitle = 'H(k) para todo k ∈ [0,1]; curva inferior domina estocasticamente',
                    x = 'Cutoff k',y = 'H(k): Incidência (%)',caption = caption_base) +
      theme_artigo() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0,hjust = 0.5)) +
      ggplot2::guides(color = ggplot2::guide_legend(nrow = 1,override.aes = list(linewidth = 2.5)))
  }

  # 6.11 Concentração ----------------------------------------------------------

  .plot_lorenz_serie <- function() {
    cores_anos <- setNames(
      colorRampPalette(c('#b71c1c', '#e65100', '#7cb342', '#1b5e20'))(length(anos_dens)),
      as.character(sort(anos_dens))
    )
    purrr::map(anos_dens,\(yr){
      sub <- dt[ano == yr & !is.na(score) & !is.na(peso)]
      .lorenz_ponderada(sub$score,sub$peso) |> dplyr::mutate(ano = as.character(yr))
    }) |>
      purrr::list_rbind() |>
      dplyr::mutate(ano = factor(ano,levels = as.character(sort(anos_dens)))) |>
      ggplot2::ggplot(ggplot2::aes(x = pop_cum,y = score_cum,color = ano,group = ano)) +
      ggplot2::geom_abline(slope = 1,intercept = 0,linetype = 'dashed',linewidth = 0.5,color = 'grey40') +
      ggplot2::geom_line(linewidth = 0.8,alpha = 0.9) +
      ggplot2::scale_color_manual(values = cores_anos,name = 'Ano') +
      ggplot2::scale_x_continuous(limits = c(0,1),labels = scales::label_percent(decimal.mark = ',',accuracy = 1),expand = ggplot2::expansion(mult = c(0,0))) +
      ggplot2::scale_y_continuous(limits = c(0,1),oob = scales::squish,labels = scales::label_percent(decimal.mark = ',',accuracy = 1),expand = ggplot2::expansion(mult = c(0,0))) +
      ggplot2::labs(title = 'Curva de Lorenz do score de privação: Brasil',
                    subtitle = 'Proporção acumulada do score vs. proporção acumulada da população\nQuanto mais afastada da diagonal, maior a concentração da privação',
                    x = 'Proporção acumulada da população (%)',y = 'Proporção acumulada do score (%)',caption = caption_base) +
      theme_artigo() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0,hjust = 0.5), aspect.ratio = 1) +
      ggplot2::guides(color = ggplot2::guide_legend(nrow = 1,override.aes = list(linewidth = 2.5)))
  }

  .plot_gini_serie <- function() {
    dt |>
      dplyr::filter(!is.na(score)&!is.na(peso)) |>
      dplyr::reframe(gini = .gini_ponderado(score,peso), .by = ano) |>
      dplyr::arrange(ano) |>
      ggplot2::ggplot(ggplot2::aes(x = ano,y = gini)) +
      ggplot2::geom_line(linewidth = 0.8,color = '#2166ac') +
      ggplot2::geom_point(fill = 'white',color = '#2166ac',shape = 21,size = 2.5,stroke = 0.9) +
      scale_x_anos() +
      ggplot2::scale_y_continuous(limits = c(0,1),oob = scales::squish,
                                  labels = scales::label_number(decimal.mark = ',',accuracy = 0.01),expand = ggplot2::expansion(mult = c(0,0.05))) +
      ggplot2::labs(title = 'Índice de Gini do score de privação: Brasil',
                    subtitle = 'Concentração da privação multidimensional entre indivíduos ao longo do tempo\nGini = 0 → distribuição uniforme; Gini = 1 → privação totalmente concentrada',
                    x = NULL,y = 'Gini da privação',caption = caption_base) +
      theme_artigo()
  }

  .plot_lorenz_grupo <- function(atributo = 'tipo_arranjo') {
    
    n      <- length(anos_maps)
    ncol_f <- dplyr::case_when(
      n <= 3 ~ n,
      n == 4 ~ 2L,
      n == 5 ~ 3L,
      T      ~ 3L
    )
    
    if (atributo == 'tipo_arranjo') {
      atb    <- 'arranjo domiciliar'
      cores  <- .cores_tipo_arranjo
      niveis <- niveis_tipo
      dt_grp <- dt |>
        dplyr::mutate(grupo = factor(.tipo_arranjo_map[as.character(arranjo_full)], levels = niveis_tipo))
    } else {
      atb    <- 'macrorregião'
      cores  <- cores_macro
      niveis <- niveis_macro
      dt_grp <- dt |>
        dplyr::mutate(grupo = factor(macro_map[uf], levels = niveis_macro))
    }

    purrr::map(anos_maps, \(yr) {
      purrr::map(niveis, \(grp) {
        d <- dt_grp[ano == yr & grupo == grp & is.finite(score) & is.finite(peso)]
        if (nrow(d) == 0) return(NULL)
        .lorenz_ponderada(d$score, d$peso) |>
          dplyr::mutate(grupo = grp, ano = yr)
      }) |> purrr::compact() |> purrr::list_rbind()
    }) |>
      purrr::list_rbind() |>
      dplyr::mutate(
        grupo = factor(grupo, levels = niveis),
        ano   = factor(ano,   levels = sort(anos_maps))
      ) |>
      ggplot2::ggplot(ggplot2::aes(x = pop_cum, y = score_cum, color = grupo, group = grupo)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 'dashed', linewidth = 0.5, color = 'grey40') +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::facet_wrap(~ ano, ncol = ncol_f) +
      ggplot2::scale_color_manual(values = cores, name = NULL) +
      ggplot2::scale_x_continuous(
        limits = c(0, 1),
        labels = scales::label_percent(decimal.mark = ',', accuracy = 1),
        expand = ggplot2::expansion(mult = c(0, 0))
      ) +
      ggplot2::scale_y_continuous(
        limits = c(0, 1),
        oob    = scales::squish,
        labels = scales::label_percent(decimal.mark = ',', accuracy = 1),
        expand = ggplot2::expansion(mult = c(0, 0))
      ) +
      ggplot2::labs(
        title    = paste0('Curva de Lorenz por ', atb),
        subtitle = 'Proporção acumulada do score vs. população\nCurva mais afastada da diagonal → maior concentração interna',
        x        = 'Proporção acumulada da população (%)',
        y        = 'Proporção acumulada do score (%)',
        caption  = caption_base
      ) +
      theme_artigo() +
      ggplot2::theme(
        axis.text.x  = ggplot2::element_text(angle = 0, hjust = 0.5),
        aspect.ratio = 1,
        strip.text   = ggplot2::element_text(face = 'bold'),
        panel.spacing = ggplot2::unit(1.5, 'lines')
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(nrow = 1, override.aes = list(linewidth = 2.5))
      )
  }
  
  .plot_gini_grupo_serie <- function(atributo = 'tipo_arranjo') {
    if (atributo == 'tipo_arranjo') {
      atb    <- 'arranjo domiciliar'
      base   <- dt |> dplyr::mutate(grupo = factor(.tipo_arranjo_map[as.character(arranjo_full)], levels = niveis_tipo))
      cores  <- .cores_tipo_arranjo
    } else {
      atb    <- 'macrorregião'
      base   <- dt |> dplyr::mutate(grupo = factor(macro_map[uf], levels = niveis_macro))
      cores  <- cores_macro
    }

    # Gini por grupo
    por_grupo <- base |>
      dplyr::filter(!is.na(grupo) & is.finite(score) & is.finite(peso)) |>
      dplyr::reframe(gini = .gini_ponderado(score, peso), .by = c(ano, grupo)) |>
      dplyr::mutate(serie = as.character(grupo))

    # Gini total
    total <- dt |>
      dplyr::filter(is.finite(score) & is.finite(peso)) |>
      dplyr::reframe(gini = .gini_ponderado(score, peso), .by = ano) |>
      dplyr::mutate(serie = 'Total')

    cores_plot <- c(cores, 'Total' = 'grey30')

    dplyr::bind_rows(por_grupo, total) |>
      dplyr::arrange(ano) |>
      ggplot2::ggplot(ggplot2::aes(x = ano, y = gini, color = serie, group = serie)) +
      ggplot2::geom_line(data = \(d) dplyr::filter(d, serie != 'Total'), linewidth = 0.8) +
      ggplot2::geom_line(data = \(d) dplyr::filter(d, serie == 'Total'), linewidth = 1.4, linetype = 'dashed') +
      ggplot2::geom_point(
        ggplot2::aes(fill = serie),
        shape = 21, size = 2.2, stroke = 0.6, color = 'white'
      ) +
      ggplot2::scale_color_manual(values = cores_plot, name = NULL) +
      ggplot2::scale_fill_manual(values  = cores_plot, guide = 'none') +
      scale_x_anos() +
      ggplot2::scale_y_continuous(
        limits = c(0, NA), oob = scales::squish,
        labels = scales::label_number(decimal.mark = ',', accuracy = 0.01),
        expand = ggplot2::expansion(mult = c(0, 0.05))
      ) +
      ggplot2::labs(
        title    = paste0('Gini do score de privação por ', atb, ': Brasil'),
        subtitle = 'Concentração interna da privação dentro de cada grupo; linha tracejada = total Brasil',
        x = NULL, y = 'Gini da privação', caption = caption_base
      ) +
      theme_artigo() +
      ggplot2::guides(
        color = ggplot2::guide_legend(nrow = 1, override.aes = list(linewidth = 2.5))
      )
  }

  ############################### GRÁFICOS NOVOS ###############################
  
  # ── 1. Variação do MPI por UF — 8 mapas (um por arranjo/sexo) ────────────────
  
  .plot_delta_mpi_uf_arranjo <- function() {
    
    uf_geo <- shapefiles[['2020']]
    
    mpi_uf <- dt |>
      dplyr::filter(ano %in% c(min(anos_maps), max(anos_maps))) |>
      dplyr::summarise(
        H   = weighted.mean(score >= k_ref,        w = peso,             na.rm = TRUE),
        A   = weighted.mean(score[score >= k_ref], w = peso[score >= k_ref], na.rm = TRUE),
        MPI = 100 * H * A,
        .by = c(ano, uf, arranjo_full)
      ) |>
      tidyr::pivot_wider(
        id_cols      = c(uf, arranjo_full),
        names_from   = ano,
        values_from  = MPI,
        names_prefix = 'y_'
      ) |>
      dplyr::mutate(
        delta        = .data[[paste0('y_', max(anos_maps))]] - .data[[paste0('y_', min(anos_maps))]],
        tipo_arranjo = factor(tipo_mapa_vec[as.character(arranjo_full)], levels = niveis_tipo),
        sexo         = factor(sexo_mapa_vec[as.character(arranjo_full)], levels = niveis_sexo)
      )
    
    lim <- max(abs(mpi_uf$delta), na.rm = TRUE)
    
    uf_geo |>
      dplyr::left_join(mpi_uf, by = c('abbrev_state' = 'uf'), relationship = 'many-to-many') |>
      dplyr::filter(!is.na(tipo_arranjo)) |>
      ggplot2::ggplot(ggplot2::aes(fill = delta)) +
      ggplot2::geom_sf(color = 'white', linewidth = 0.2) +
      ggplot2::coord_sf(xlim = c(-73.99,-28.84), ylim = c(-33.75,5.28), expand = FALSE) +
      ggplot2::facet_grid(sexo ~ tipo_arranjo, switch = 'y') +
      ggplot2::scale_fill_distiller(
        palette    = 'RdBu',
        direction  = -1,
        na.value   = 'grey80',
        limits     = c(-lim, lim),
        labels     = scales::label_number(decimal.mark = ',', accuracy = 0.1, style_positive = 'plus'),
        guide      = ggplot2::guide_colorbar(
          title          = '\u0394 MPI (p.p.)',
          title.position = 'top',
          barwidth       = ggplot2::unit(12, 'cm'),
          barheight      = ggplot2::unit(0.4, 'cm')
        )
      ) +
      ggplot2::labs(
        title    = paste0('Varia\u00e7\u00e3o do MPI por UF e arranjo domiciliar \u2014 ',
                          min(anos_maps), ' \u2192 ', max(anos_maps)),
        subtitle = paste0('Diferen\u00e7a absoluta em pontos percentuais (k = ', k_ref, ')'),
        caption  = caption_base
      ) +
      theme_mapa()
  }
  
  
  # ── 2. Sigma-convergência por arranjo/sexo (DP e CV em painel duplo) ──────────
  
  .plot_sigma_conv_arranjo <- function() {
    
    dados <- dt |>
      dplyr::mutate(
        arranjo8 = factor(arranjo_full, levels = ordem_arranjo)
      ) |>
      dplyr::filter(!is.na(arranjo8)) |>
      dplyr::summarise(
        H   = weighted.mean(score >= k_ref,        w = peso,             na.rm = TRUE),
        A   = weighted.mean(score[score >= k_ref], w = peso[score >= k_ref], na.rm = TRUE),
        MPI = 100 * H * A,
        .by = c(ano, uf, arranjo8)
      ) |>
      dplyr::summarise(
        dp  = sd(MPI,   na.rm = TRUE),
        med = mean(MPI, na.rm = TRUE),
        cv  = 100 * dp / med,
        .by = c(ano, arranjo8)
      ) |>
      dplyr::arrange(ano) |>
      tidyr::pivot_longer(c(dp, cv), names_to = 'metrica', values_to = 'valor') |>
      dplyr::mutate(
        metrica = factor(metrica,
                         levels = c('cv', 'dp'),
                         labels = c('Coeficiente de varia\u00e7\u00e3o (%)', 'Desvio padr\u00e3o (p.p.)')
        )
      )
    
    ggplot2::ggplot(dados, ggplot2::aes(x = ano, y = valor, color = arranjo8, group = arranjo8)) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point(
        ggplot2::aes(fill = arranjo8),
        shape = 21, size = 2, stroke = 0.5, color = 'white'
      ) +
      ggplot2::facet_wrap(~ metrica, ncol = 1, scales = 'free_y') +
      ggplot2::scale_color_manual(values = .cores_arranjo8, name = 'Arranjo') +
      ggplot2::scale_fill_manual(values  = .cores_arranjo8, guide = 'none') +
      scale_x_anos() +
      ggplot2::scale_y_continuous(
        limits = c(0, NA),
        labels = scales::label_number(decimal.mark = ',', accuracy = 0.1),
        expand = ggplot2::expansion(mult = c(0, 0.08))
      ) +
      ggplot2::labs(
        title    = 'Sigma-converg\u00eancia do MPI por arranjo domiciliar \u2014 Brasil',
        subtitle = paste0('Dispers\u00e3o interestadual do MPI por arranjo/sexo (k = ', k_ref, ')'),
        x = NULL, y = NULL, caption = caption_base
      ) +
      theme_artigo() +
      ggplot2::theme(
        strip.text         = ggplot2::element_text(face = 'bold'),
        panel.spacing      = ggplot2::unit(1.5, 'lines'),
        panel.grid.major.x = ggplot2::element_blank()
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(nrow = 2, override.aes = list(linewidth = 2.5))
      )
  }
  
  
  # ── 3. Dominância estocástica — 8 arranjos facetados ──────────────────────────
  # Cada painel = 1 arranjo, com curva H(k) do arranjo + linha de referência Brasil
  
  .plot_dominancia_arranjo8 <- function(ano_ref = max(anos_maps)) {
    
    sub_global <- dt[ano == ano_ref]
    curva_brasil <- purrr::map_dbl(
      seq(0, 1, length.out = 200),
      \(k) 100 * weighted.mean(sub_global$score >= k, w = sub_global$peso, na.rm = TRUE)
    )
    ref <- tibble::tibble(k = seq(0, 1, length.out = 200), H_brasil = curva_brasil)
    
    curvas <- purrr::map_dfr(ordem_arranjo, \(arr) {
      d  <- dt[ano == ano_ref & arranjo_full == arr]
      ks <- seq(0, 1, length.out = 200)
      purrr::map_dbl(ks, \(k) 100 * weighted.mean(d$score >= k, w = d$peso, na.rm = TRUE)) |>
        (\(H) tibble::tibble(
          k            = ks,
          H            = H,
          arranjo      = arr,
          tipo_arranjo = factor(tipo_mapa_vec[as.character(arr)], levels = niveis_tipo),
          sexo         = factor(sexo_mapa_vec[as.character(arr)], levels = niveis_sexo),
          label_arranjo = .arranjo_labels[as.character(arr)]
        ))()
    }) |>
      dplyr::left_join(ref, by = 'k') |>
      dplyr::mutate(
        label_arranjo = factor(label_arranjo, levels = unname(.arranjo_labels[ordem_arranjo]))
      )
    
    ggplot2::ggplot(curvas, ggplot2::aes(x = k)) +
      ggplot2::geom_line(ggplot2::aes(y = H_brasil),
                         color = 'grey60', linewidth = 0.6, linetype = 'dashed') +
      ggplot2::geom_line(ggplot2::aes(y = H, color = tipo_arranjo), linewidth = 0.9) +
      ggplot2::geom_vline(xintercept = k_ref, linetype = 'dashed', linewidth = 0.4, color = 'grey40') +
      ggplot2::facet_grid(sexo ~ tipo_arranjo, switch = 'y') +
      ggplot2::scale_color_manual(values = .cores_tipo_arranjo, name = 'Arranjo', guide = 'none') +
      ggplot2::scale_x_continuous(
        labels = scales::label_percent(decimal.mark = ',', accuracy = 1),
        expand = ggplot2::expansion(mult = c(0.01, 0.01))
      ) +
      ggplot2::scale_y_continuous(
        limits = c(0, 100),
        labels = scales::label_number(decimal.mark = ',', accuracy = 1, suffix = '%'),
        expand = ggplot2::expansion(mult = c(0, 0.02))
      ) +
      ggplot2::labs(
        title    = paste0('Domin\u00e2ncia estoc\u00e1stica por arranjo domiciliar e sexo \u2014 ', ano_ref),
        subtitle = 'H(k) para todo k \u2208 [0,1]; linha cinza tracejada = Brasil total',
        x = 'Cutoff k', y = 'H(k) \u2014 Incid\u00eancia (%)',
        caption = caption_base
      ) +
      theme_artigo() +
      ggplot2::theme(
        axis.text.x       = ggplot2::element_text(angle = 0, hjust = 0.5),
        strip.text.x      = ggplot2::element_text(face = 'bold'),
        strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1),
        strip.placement   = 'outside',
        panel.spacing     = ggplot2::unit(0.8, 'lines')
      )
  }
  
  
  # ── 4. Distribuição do score por arranjo/sexo (2×4, anos selecionados) ────────
  
  .plot_densidade_arranjo8 <- function() {
    
    anos_ref <- anos_dens[seq(1, length(anos_dens), length.out = min(3, length(anos_dens)))] |>
      round() |> as.integer()
    
    cores_anos <- setNames(
      colorRampPalette(c('#b71c1c', '#f9a825', '#1b5e20'))(length(anos_ref)),
      as.character(anos_ref)
    )
    
    base <- dt |>
      dplyr::filter(ano %in% anos_ref, !is.na(score), !is.na(peso), is.finite(score)) |>
      dplyr::mutate(
        ano          = factor(ano, levels = anos_ref),
        tipo_arranjo = factor(tipo_mapa_vec[as.character(arranjo_full)], levels = niveis_tipo),
        sexo         = factor(sexo_mapa_vec[as.character(arranjo_full)], levels = niveis_sexo),
        peso_norm    = peso / sum(peso),
        .by = c(ano, arranjo_full)
      )
    
    y_max <- base |>
      dplyr::group_by(ano, arranjo_full) |>
      dplyr::summarise(
        dens_max = max(density(score, weights = peso_norm / sum(peso_norm), bw = 0.02)$y),
        .groups  = 'drop'
      ) |>
      dplyr::pull(dens_max) |>
      max(na.rm = TRUE)
    
    base |>
      ggplot2::ggplot(ggplot2::aes(x = score, color = ano, weight = peso_norm)) +
      ggplot2::geom_density(bw = 0.02, linewidth = 0.8) +
      ggplot2::geom_vline(xintercept = k_ref, linetype = 'dashed', linewidth = 0.4, color = 'grey40') +
      ggplot2::annotate('text', x = k_ref + 0.02, y = Inf,
                        label = paste0('k = ', k_ref), hjust = 0, vjust = 1.5,
                        size = 3.5, color = 'grey40', fontface = 'italic') +
      ggplot2::facet_grid(sexo ~ tipo_arranjo, switch = 'y') +
      ggplot2::scale_color_manual(values = cores_anos, name = 'Ano') +
      ggplot2::scale_x_continuous(
        limits = c(0, 1),
        labels = scales::label_percent(decimal.mark = ',', accuracy = 1),
        expand = ggplot2::expansion(mult = c(0.01, 0.01))
      ) +
      ggplot2::scale_y_continuous(
        limits = c(0, y_max * 1.05),
        oob    = scales::squish,
        labels = scales::label_number(decimal.mark = ',', accuracy = 0.1),
        expand = ggplot2::expansion(mult = c(0, 0))
      ) +
      ggplot2::labs(
        title    = 'Distribuição do score de privação por arranjo domiciliar e sexo',
        subtitle = paste0(
          'Densidade ponderada; eixos fixos para comparabilidade; ',
          'linha tracejada = k (', k_ref, ')'
        ),
        x = 'Score de privação', y = 'Densidade',
        caption = caption_base
      ) +
      theme_artigo() +
      ggplot2::theme(
        axis.text.x       = ggplot2::element_text(angle = 0, hjust = 0.5),
        strip.text.x      = ggplot2::element_text(face = 'bold'),
        strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1),
        strip.placement   = 'outside',
        panel.spacing     = ggplot2::unit(0.8, 'lines')
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(nrow = 1, override.aes = list(linewidth = 2.5))
      )
  }
  
  
  # == 7. Registry =============================================================
  # Tamanhos padronizados (cm) — referência: página A4 landscape com margem de 2cm

  .w <- 48
  .h <- 32

  registry <- c(
    #Estaticos
    list(
      tab_arranjo                = list(fun = .tab_arranjo,                               w = .w,  h = .h,  type = 'gt', grupo = 'composicao'),
      plot_composicao            = list(fun = .plot_composicao,                           w = .w,  h = .h,  type = 'gg', grupo = 'composicao'),
      plot_composicao_arranjo    = list(fun = .plot_composicao_arranjo,                   w = .w,  h = .h,  type = 'gg', grupo = 'composicao'),
      plot_contribuicao_ind      = list(fun = .plot_contribuicao_ind,                     w = .w,  h = .h,  type = 'gg', grupo = 'composicao'),
      plot_contrib_macro_serie   = list(fun = .plot_contrib_macro_serie,                  w = .w,  h = .h,  type = 'gg', grupo = 'composicao'),
      plot_contrib_macro_relat   = list(fun = .plot_contrib_macro_relativa,               w = .w,  h = .h,  type = 'gg', grupo = 'composicao'),
      plot_contrib_uf_ranking    = list(fun = .plot_contrib_uf_ranking,                   w = .w,  h = .h,  type = 'gg', grupo = 'composicao'),
      plot_contrib_arranjo_serie = list(fun = .plot_contrib_arranjo_serie,                w = .w,  h = .h,  type = 'gg', grupo = 'composicao'),

      plot_score_mpi             = list(fun = .plot_score_mpi,                            w = .w,  h = .h,  type = 'gg', grupo = 'evolucao'),
      plot_h_a_mpi               = list(fun = .plot_h_a_mpi,                              w = .w,  h = .h,  type = 'gg', grupo = 'evolucao'),
      plot_mpi_macrorregiao      = list(fun = .plot_mpi_macrorregiao,                     w = .w,  h = .h,  type = 'gg', grupo = 'evolucao'),

      tab_sensibilidade          = list(fun = .tab_sensibilidade,                         w = .w,  h = .h,  type = 'gt', grupo = 'sensibilidade'),
      plot_sensibilidade         = list(fun = .plot_sensibilidade,                        w = .w,  h = .h,  type = 'gg', grupo = 'sensibilidade'),
      plot_sensibilidade_arranjo = list(fun = .plot_sensibilidade_arranjo,                w = .w,  h = .h,  type = 'gg', grupo = 'sensibilidade'),

      plot_decomp_pares          = list(fun = .plot_decomp_pares,                         w = .w,  h = .h,  type = 'gg', grupo = 'decomposicao'),
      plot_decomp_acumulada      = list(fun = .plot_decomp_acumulada,                     w = .w,  h = .h,  type = 'gg', grupo = 'decomposicao'),

      plot_sigma_convergencia    = list(fun = .plot_sigma_convergencia,                   w = .w,  h = .h,  type = 'gg', grupo = 'convergencia'),
      plot_beta_convergencia     = list(fun = .plot_beta_convergencia,                    w = .w,  h = .h,  type = 'gg', grupo = 'convergencia'),
      plot_beta_conv_periodos    = list(fun = .plot_beta_conv_periodos,                   w = .w,  h = .h,  type = 'gg', grupo = 'convergencia'),
      plot_sigma_conv_arranjo    = list(fun = .plot_sigma_conv_arranjo,                   w = .w,  h = .h,  type = 'gg', grupo = 'convergencia'),
      
      plot_heatmap_ind_ano       = list(fun = .plot_heatmap_ind_ano,                      w = .w,  h = .h,  type = 'gg', grupo = 'granularidade'),
      plot_coocorrencia_serie    = list(fun = .plot_coocorrencia_serie,                   w = .w,  h = .h,  type = 'gg', grupo = 'granularidade'),

      tab_auc_arranjo            = list(fun = .tab_auc_arranjo,                           w = .w,  h = .h,  type = 'gt', grupo = 'dominancia'),

      plot_curva_incidencia      = list(fun = .plot_curva_incidencia,                     w = .w,  h = .h,  type = 'gg', grupo = 'densidade'),
      plot_densidade_score       = list(fun = .plot_densidade_score,                      w = .w,  h = .h,  type = 'gg', grupo = 'densidade'),
      plot_densidade_arranjo8    = list(fun = .plot_densidade_arranjo8,                   w = .w,  h = .h,  type = 'gg', grupo = 'densidade'),
      
      plot_lorenz_serie          = list(fun = .plot_lorenz_serie,                         w = .w,  h = .h,  type = 'gg', grupo = 'concentracao'),
      plot_gini_arranjo_serie    = list(fun = \() .plot_gini_grupo_serie('tipo_arranjo'), w = .w,  h = .h,  type = 'gg', grupo = 'concentracao'),
      plot_gini_macro_serie      = list(fun = \() .plot_gini_grupo_serie('macro'),        w = .w,  h = .h,  type = 'gg', grupo = 'concentracao'),
      lorenz_arranjo             = list(fun = \() .plot_lorenz_grupo('tipo_arranjo'),     w = .w,  h = .h,  type = 'gg', grupo = 'concentracao'),
      lorenz_macro               = list(fun = \() .plot_lorenz_grupo('macro'),            w = .w,  h = .h,  type = 'gg', grupo = 'concentracao'),
      
      plot_delta_mpi_uf          = list(fun = .plot_delta_mpi_uf,                         w = .w,  h = .h,  type = 'gg', grupo = 'mapas'),
      plot_delta_mpi_uf_arranjo  = list(fun = .plot_delta_mpi_uf_arranjo,                 w = .w,  h = .h,  type = 'gg', grupo = 'mapas'),
      ranking_uf                 = list(fun = .plot_ranking_uf,                           w = .w,  h = .h,  type = 'gg', grupo = 'mapas')
    ),
    
    #Dinamicos
    purrr::set_names(
      purrr::map(anos_maps,\(yr)   list(fun = \() .mapa_uf(yr),                           w = .w,  h = .h,  type = 'gg', grupo = 'mapas')),
      paste0('mapa_', anos_maps)
    ),

    purrr::set_names(
      purrr::map(anos_maps,\(yr)   list(fun = \() .plot_heatmap_ind_uf(yr),               w = .w,  h = .h,  type = 'gg', grupo = 'granularidade')),
      paste0('heatmap_ind_uf_', anos_maps)
    ),
    purrr::set_names(
      purrr::map(anos_maps,\(yr)   list(fun = \() .plot_coocorrencia(yr),                 w = .w,  h = .h,  type = 'gg', grupo = 'granularidade')),
      paste0('coocorrencia_', anos_maps)
    ),

    purrr::set_names(
      purrr::map(anos_maps,\(yr)   list(fun = \() .plot_densidade_score_grupo(yr),        w = .w,  h = .h,  type = 'gg', grupo = 'densidade')),
      paste0('densidade_grupo_', anos_maps)
    ),

    purrr::set_names(
      purrr::map(anos_maps,\(yr)   list(fun = \() .plot_dominancia_arranjo(yr),           w = .w,  h = .h,  type = 'gg', grupo = 'dominancia')),
      paste0('dominancia_arranjo_', anos_maps)
    ),
    purrr::set_names(
      purrr::map(anos_maps, \(yr)  list(fun = \() .plot_dominancia_arranjo8(yr),          w = .w,  h = .h,  type = 'gg', grupo = 'dominancia')),
      paste0('dominancia_arranjo8_', anos_maps)
    ),
    purrr::set_names(
      purrr::map(anos_maps,\(yr)   list(fun = \() .plot_dominancia_macro(yr),             w = .w,  h = .h,  type = 'gg', grupo = 'dominancia')),
      paste0('dominancia_macro_', anos_maps)
    )
  )


  # == 8. Filtro por grupos e validação  =======================================

  grupos_sel <- if (identical(grupos,'all')) grupos_validos else grupos
  targets    <- names(registry)[purrr::map_chr(registry,'grupo') %in% grupos_sel]

  if (length(targets)  ==  0) {
    warning('Nenhum output encontrado para os grupos selecionados.')
    return(invisible(list()))
  }

  if (verbose) {
    message('Grupos selecionados: ', paste(grupos_sel, collapse = ', '))
    message('Outputs a gerar: ', length(targets))
  }


  # == 9. Renderização e exportação ============================================

  # ── 9. Renderização e exportação ─────────────────────────────────────────────
  results    <- vector('list', length(targets)) |> setNames(targets)
  targets_gt <- targets[purrr::map_chr(registry[targets], 'type') == 'gt']
  targets_gg <- targets[purrr::map_chr(registry[targets], 'type') == 'gg']
  
  handler_ant <- progressr::handlers()
  progressr::handlers(progressr::handler_progress(
    format = '[:bar] :current/:total outputs | :message | :eta restante'
  ))
  on.exit(progressr::handlers(handler_ant), add = TRUE)
  
  progressr::with_progress({
    
    p <- progressr::progressor(steps = length(targets_gt) + length(targets_gg))
    
    # Tabelas
    for (nm in targets_gt) {
      p(message = nm)
      tryCatch({
        obj  <- registry[[nm]]$fun()
        path <- fs::path(dir_out, paste0(nm, '.', ext))
        gt::gtsave(obj, filename = path, zoom = 4, expand = 10)
        results[[nm]] <- obj
      }, error = \(e) message('\n[ERRO] ', nm, ': ', conditionMessage(e)))
    }
    
    # Graficos
    for (nm in targets_gg) {
      p(message = nm)
      tryCatch({
        obj  <- registry[[nm]]$fun()
        path <- fs::path(dir_out, paste0(nm, '.', ext))
        ggplot2::ggsave(
          filename  = path, plot = obj,
          width     = registry[[nm]]$w,
          height    = registry[[nm]]$h,
          units     = 'cm', dpi = dpi, bg = 'white',
          limitsize = FALSE
        )
        results[[nm]] <- obj
      }, error = \(e) message('\n[ERRO] ', nm, ': ', conditionMessage(e)))
    }
    
  })
  
  invisible(results)
}
