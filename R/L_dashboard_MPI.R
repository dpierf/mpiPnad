#' Cria o dashboard de análise do MPI-Brasil
#' @param path_dict   Caminho onde o dicionário está salvo (defalt = 'data/mpi_dictionary.rds')
#' @param path_pnad   Caminho onde a base PNAD está salva (defalt = 'data/pnad_completa.parquet')
#' @param path_models Caminho onde estão salvos os modelos estatísticos (default = 'output/models')
#' @param port        Porta utilizada para a apresentação do dashboard, no painel
#' @param launch      Booleano (default = T) indicando se o dashboard será visualizado
#' @export

dashboard_mpi <- function(path_dict    = 'data/mpi_dictionary.rds',
                          path_pnad    = 'data/pnad_completa.parquet',
                          path_models  = 'output/models',
                          port         = NULL,
                          launch       = TRUE
){
  
  # 0. Elementos introdutórios ====
  
  # Validação de objetos
  if (!file.exists(path_dict)) stop("Dicionário não encontrado em: ", path_dict)
  if (!file.exists(path_pnad)) stop("Base PNAD não encontrada em: ", path_pnad)
  
  # Objetos a carregar
  dicts   <- readRDS(path_dict)
  mpi_pnad <- arrow::read_parquet(path_pnad) |> data.table::setDT()
  
  .load_model_file <- function(base, suffix, ext = 'parquet') {
    f <- fs::path(path_models, paste0(base, suffix, '.', ext))
    if (fs::file_exists(f)) {
      if (ext == 'parquet') arrow::read_parquet(f) |> data.table::setDT()
      else if (ext == 'rds') readRDS(f)
      else NULL
    } else {
      warning('Arquivo não encontrado: ', f)
      NULL
    }
  }
  
  .zoib_coefs    <- .load_model_file('zoib_coefs', '_global')
  .zoib_grade    <- .load_model_file('zoib_grade', '_global')
  .qr_coefs      <- .load_model_file('quantilica_coefs', '_global')
  .qr_grade      <- .load_model_file('quantilica_grade', '_global')
  .logit_coefs   <- .load_model_file('logit_coefs',   '_global')
  .logit_effects <- .load_model_file('logit_effects', '_global')
  .logit_grade   <- .load_model_file('logit_grade',   '_global')
  
  
  # Helpers
  .grupo_termo <- function(termo) sub('([a-z_]+).*','\\1', termo)
  
  .pesos_ind <- c(D1 = 2/27, D2 = 2/27, D3 = 2/27,
                  B1 = 1/18, B2 = 1/18, B3 = 1/18, B4 = 1/18,
                  V1 = 4/27, V2 = 2/27, 
                  E1 = 2/27, E2 = 2/27, E3 = 2/27,
                  P1 = 2/27, P2 = 1/27) 
  
  .comp_map   <- c(D1 = 'D', D2 = 'D', D3 = 'D', B1 = 'B', B2 = 'B', B3 = 'B', B4 = 'B',
                   V1 = 'V', V2 = 'V', E1 = 'E', E2 = 'E', E3 = 'E', P1 = 'P', P2 = 'P')
  .comp_names <- c(V='Padrão de Vida',P='Emprego e Proteção Social',
                   D='Moradia',B='Serviços Básicos',E='Educação')
  .cols_ind   <- c('D1','D2','D3','B1','B2','B3','B4','V1','V2','E1','E2','E3','P1','P2')
  
  .cutoffs    <- c('0.10','0.20','0.25','0.33','0.40','0.50')
  
  .periodos   <- unname(dicts$periodo)          # labels do dicionário
  .periodo_ids <- names(dicts$periodo)          # ids inteiros ('1','2',...)
  .periodo_anos <- setNames(
    list(1981:1993, 1995:2002, 2003:2007, 2008:2014, 2015:2018, 2019:2024),
    unname(dicts$periodo)
  )
  
  .dec <- function(x, dict) dict[as.character(x)]
  
  # Limpa cache corrompido do geobr se necessário
  .ufs <- tryCatch(
    geobr::read_state(year = 2020, simplified = TRUE, showProgress = FALSE, cache = TRUE),
    error = \(e) {
      cache_dir <- fs::path(tools::R_user_dir('geobr', 'cache'))
      if (fs::dir_exists(cache_dir)) fs::dir_delete(cache_dir)
      message('Cache do geobr limpo — baixando novamente.')
      geobr::read_state(year = 2020, simplified = TRUE, showProgress = FALSE, cache = TRUE)
    }
  )
  
  .calc_mpi <- function(sc,w,k) {
    pob <- sc>=k
    H   <- weighted.mean(pob,w,na.rm=TRUE)
    A   <- if(any(pob,na.rm=TRUE)) weighted.mean(sc[pob],w[pob],na.rm=TRUE) else 0
    H*A
  }
  
  .dens_manual <- function(dt,grupo_col,peso_col='peso',score_col='score') {
    grupos <- unique(dt[[grupo_col]])
    rbindlist(lapply(grupos,\(g) {
      sub <- dt[get(grupo_col)==g]
      w   <- sub[[peso_col]]/sum(sub[[peso_col]])
      d   <- density(sub[[score_col]],weights=w,from=0,to=1,n=256,na.rm=TRUE)
      data.table(grupo=g,x=d$x,y=d$y)
    }))
  }
  
  # Sistema decimal
  options(OutDec=',')
  .fmt_n   <- function(x,d=3) formatC(x,digits=d,format='f',decimal.mark=',',big.mark='.')
  .fmt_pct <- function(x)     paste0(formatC(x*100,digits=1,format='f',decimal.mark=','),'%')
  .comma_br  <- scales::label_number(big.mark='.',decimal.mark=',')
  .pct_br    <- scales::label_percent(accuracy=0.1,decimal.mark=',')
  
  
  # 1. Pré-processamento de dados ====
  
  # Base completa
  mpi_pnad[, arranjo_dec := as.integer(substr(as.character(arranjo_full), 1, 1))]
  
  mpi_pnad[, `:=` (
    ano          = as.integer(ano),
    arranjo_full = unname(.dec(arranjo_full, dicts$arranjo_full)),
    arranjo_dec  = unname(.dec(arranjo_dec,  dicts$arranjo_dec)),
    regiao       = unname(.dec(regiao,       dicts$regiao)),
    setor_dec    = unname(.dec(setor_dec,    dicts$setor_dec)),
    area_dec     = unname(.dec(area_dec,     dicts$area_dec)),
    sexo_dec     = unname(.dec(sexo_dec,     dicts$sexo_dec)),
    raca         = unname(.dec(raca,         dicts$raca)),
    periodo      = unname(.dec(periodo,      dicts$periodo))
  )]
  
  # Base agregada
  mpi_ag <- mpi_pnad[, .(
    score_med = weighted.mean(score,    peso, na.rm=TRUE),
    rpc_real  = weighted.mean(rpc_real, peso, na.rm=TRUE),
    pop       = sum(peso, na.rm=TRUE)
  ), by=.(ano,periodo,regiao,uf,setor_dec,area_dec,sexo_dec,raca,arranjo_full,arranjo_dec,agregados)]
  
  # Temas para o dashboard
  .tema <- theme_minimal(base_size=11)+
    theme(legend.position='bottom',
          plot.title=element_text(size=14,face='bold'),
          axis.title=element_text(size=10),
          axis.text=element_text(size=9),
          legend.text=element_text(size=10),
          legend.title=element_text(size=10))
  
  .tema_mapa <- theme_void(base_size=11)+
    theme(legend.position='right',
          legend.text=element_text(size=10),
          legend.title=element_text(size=10))
  
  .pal_periodo <- c('#e41a1c','#ff7f00','#f0c21a','#4daf4a','#377eb8','#984ea3','#a65628')
  
  .css <- tags$style(HTML('
    div.small-box > div.inner > h3 {
      font-size:2.8rem !important; font-weight:900 !important; line-height:1.1 !important;
    }
    div.small-box > div.inner > p { font-size:0.95rem !important; }
    .card-header > .card-title, .card-header .card-title {
      font-weight:700 !important; font-size:15px !important;
    }
    .filtro-row { background:#f8f9fa; padding:8px 4px; margin-bottom:12px; border-radius:6px; }
    
    #cl_vb_sil, #cl_vb_neg, #cl_vb_max, #cl_vb_min, #cl_vb_within, #cl_vb_cut {
                          height: 130px;
    }
    #cl_vb_sil .small-box, #cl_vb_neg .small-box, #cl_vb_max .small-box,
    #cl_vb_min .small-box, #cl_vb_within .small-box, #cl_vb_cut .small-box {
    height: 100%;
    }
  '))
  
  .h <- 430
  
  # Elementos para clusterização
  .w_dim <- c(D1=2/27, D2=2/27, D3=2/27,
              B1=1/18, B2=1/18, B3=1/18, B4=1/18,
              V1=4/27, V2=2/27,
              E1=2/27, E2=2/27, E3=2/27,
              P1=2/27, P2=1/27)
  
  cluster_input <- mpi_pnad[
    !(arranjo_full %in% c('Unipessoal: Homem', 'Unipessoal: Mulher'))
  ][, tamanho := fcase(
    pessoas_dom <= 3L, '2-3',
    pessoas_dom <= 5L, '4-5',
    default           = '6+'
  )][, .(
    moradia  = weighted.mean((D1 + D2 + D3) / 3,                    peso, na.rm=TRUE),
    servicos = weighted.mean((B1 + B2 + B3 + B4) / 4,               peso, na.rm=TRUE),
    padrao   = weighted.mean(V1 * (2/3) + V2 * (1/3),               peso, na.rm=TRUE),
    educacao = weighted.mean((E1 + E2 + E3) / 3,                    peso, na.rm=TRUE),
    protecao = weighted.mean(P1 * (2/3) + P2 * (1/3),               peso, na.rm=TRUE),
    pop_tot  = sum(peso, na.rm=TRUE)
  ), by = .(periodo, uf, regiao, setor_dec, area_dec, arranjo_dec, tamanho)
  ][, score := moradia*(6/27) + servicos*(4/18) + padrao*(6/27) +
      educacao*(6/27) + protecao*(3/27)
  ][, periodo := stringr::str_replace_all(periodo, '\u2013', '-')]
  
  # Valores únicos para os filtros da aba Grupos
  .periodos_cl  <- sort(unique(cluster_input$periodo))
  .arranjos_cl  <- sort(unique(cluster_input$arranjo_dec))
  .setores_cl   <- sort(unique(cluster_input$setor_dec))
  .areas_cl     <- sort(unique(cluster_input$area_dec))
  
  
  # 2. Modelagem econométrica ====
  
  .tau_map <- c(tau_010=0.10, tau_025=0.25, tau_033=0.33, tau_050=0.50, tau_067=0.67, tau_075=0.75, tau_090=0.90)
  
  # Ordens canônicas
  .grupo_order <- c('Período','Região','Setor','Área','Raça-Cor','Faixa Etária','Escolaridade',
                    'Arranjo Familiar','Tamanho Familiar','Agregados')
  
  # Referências
  .refs_modelos <- c(
    'Período'          = '1981–1993',
    'Região'           = 'Sudeste',
    'Setor'            = 'Urbano',
    'Área'             = 'Resto da UF',
    'Raça-Cor'         = 'Branco',
    'Faixa Etária'     = '30-49',
    'Escolaridade'     = '5 a 7 anos',
    'Arranjo Familiar' = 'Casal Com: Homem',
    'Tamanho Familiar' = 'Pequena',
    'Agregados'        = 'N'
  )
  
  .refs_modais <- list(
    regiao       = 'Sudeste',
    setor        = 'Urbano',
    area         = 'Resto da UF',
    raca         = 'Branco',
    faixa        = '30-49',
    nivel        = '5 a 7 anos',
    tamanho      = 'Pequena',
    agregados    = 'N',
    arranjo_full = 'Casal Com: Homem'
  )
  
  # Labels legíveis
  .attr_labels <- c(
    regiao       = 'Região',
    setor        = 'Setor',
    area         = 'Área',
    raca         = 'Raça-Cor',
    faixa        = 'Faixa Etária',
    nivel        = 'Escolaridade',
    tamanho      = 'Tamanho Familiar',
    agregados    = 'Agregados',
    arranjo_full = 'Arranjo Familiar'
  )
  
  # Parse unificado
  .parse_termos <- function(dt) {
    mapa <- data.table(
      padrao = c('^periodo','^regiao','^setor','^area','^raca',
                 '^faixa','^nivel','^arranjo_full','^tamanho','^agregados'),
      grupo  = c('Período','Região','Setor','Área','Raça-Cor',
                 'Faixa Etária','Escolaridade','Arranjo Familiar',
                 'Tamanho Familiar','Agregados')
    )
    dt <- copy(dt)
    dt[, c('grupo','categoria') := list(NA_character_, termo)]
    for (i in seq_len(nrow(mapa))) {
      idx <- grepl(mapa$padrao[i], dt$termo)
      dt[idx, grupo     := mapa$grupo[i]]
      dt[idx, categoria := sub(mapa$padrao[i], '', termo)]
    }
    dt[, tick_lbl := paste0(grupo, ': ', categoria)]
    dt
  }
  
  # Posições e shapes
  .assign_ypos <- function(dt, order_col, gap=1.5) {
    dt[, grupo := factor(grupo, levels=rev(.grupo_order))]
    dt <- dt[order(grupo, get(order_col))]
    cur_y <- 0; prev_g <- NULL; y_pos <- numeric(nrow(dt))
    for (i in seq_len(nrow(dt))) {
      g <- as.character(dt$grupo[i])
      if (!is.null(prev_g) && g != prev_g) cur_y <- cur_y + gap
      cur_y <- cur_y + 1; y_pos[i] <- cur_y; prev_g <- g
    }
    dt[, y := y_pos]
    dt
  }
  
  .forest_shapes <- function(dt, grp_info) {
    grp_bands <- dt[, .(y_min=min(y)-0.5, y_max=max(y)+0.5), by=grupo]
    c(
      lapply(seq(1, nrow(grp_bands), 2), \(i) list(
        type='rect', xref='paper', x0=0, x1=1,
        y0=grp_bands$y_min[i], y1=grp_bands$y_max[i],
        fillcolor='rgba(200,200,200,0.12)', line=list(width=0)
      )),
      lapply(seq_len(nrow(grp_info)-1), \(i) list(
        type='line', xref='paper', x0=0, x1=1,
        y0=grp_info$y_max[i]+0.75, y1=grp_info$y_max[i]+0.75,
        line=list(color='#dddddd', width=1)
      ))
    )
  }
  
  .forest_annots <- function(grp_info, sub_txt) {
    ref_items <- paste0(names(.refs_modelos), ' = ', .refs_modelos)
    chunks    <- split(ref_items, ceiling(seq_along(ref_items)/4))
    ref_txt   <- paste0('<b>Referências:</b> ',
                        paste(sapply(chunks, paste, collapse=' ; '), collapse='<br>'))
    list(
      list(xref='paper', yref='paper', x=0.25, y=1.01,
           text=ref_txt, showarrow=FALSE,
           xanchor='center', yanchor='bottom', align='center',
           font=list(size=9, color='#555')),
      list(xref='paper', yref='paper', x=0, y=-0.07,
           text=sub_txt, showarrow=FALSE,
           xanchor='left', font=list(size=9, color='#777'))
    )
  }
  
  # Subtítulos semânticos
  .sub_or   <- '<i>Pontos significativos (p<0,05) em azul; não-significativos em cinza.</i>'
  .sub_ame  <- '<i>Vermelho = aumenta P(pobre) · Azul = reduz P(pobre) · Cinza = não significativo</i>'
  .sub_mu   <- '<i>Vermelho = aumenta score (mais pobreza) · Azul = reduz score (menos pobreza)</i>'
  .sub_nu   <- '<i>Azul = aumenta P(score=0) (menos pobreza) · Vermelho = reduz P(score=0) (mais pobreza)</i>'
  
  
  # 3. UI ====
  
  ui <- bs4DashPage(
    title='MPI Dashboard', dark=FALSE,
    header  = bs4DashNavbar(title=bs4DashBrand(title='Dashboard MPI-Brasil',color='primary'),skin='light'),
    sidebar = bs4DashSidebar(skin='dark',
                             bs4SidebarMenu(
                               bs4SidebarMenuItem('Análise Anual', tabName='anual',     icon=icon('calendar')),
                               bs4SidebarMenuItem('Tendências',    tabName='tendencia', icon=icon('chart-line')),
                               bs4SidebarMenuItem('Modelos',       tabName='modelos',   icon=icon('flask')),
                               bs4SidebarMenuItem('Grupos',        tabName='grupos',    icon=icon('object-group'))
                             )),
    body = bs4DashBody(.css, shinyjs::useShinyjs(), 
                       bs4TabItems(
                         
                         ## P1 - Anual ----
                         bs4TabItem(tabName='anual',
                                    div(class='filtro-row',
                                        fluidRow(
                                          column(2,selectInput('ano','Ano',NULL)),
                                          column(2,selectInput('regiao_p1','Região', choices = c('Todas'='0', setNames(names(dicts$regiao), dicts$regiao)))),
                                          column(2,selectInput('uf_p1','UF','Todas')),
                                          column(2,selectInput('setor_p1','Setor', choices = c('Todas'='0', setNames(names(dicts$setor_dec), dicts$setor_dec)))),
                                          column(2,selectInput('area_p1','Área', choices = c('Todas'='0', setNames(names(dicts$area_dec), dicts$area_dec)))),
                                          column(2,selectInput('sexo_p1','Sexo', choices = c('Todos'='0', setNames(names(dicts$sexo_dec), dicts$sexo_dec)))),
                                          column(2,selectInput('raca_p1','Raça-Cor', choices = c('Todas'='0', setNames(names(dicts$raca), dicts$raca)))),
                                          column(2,selectInput('arranjo_p1','Arranjo', choices = c('Todos'='0', setNames(names(dicts$arranjo_dec), dicts$arranjo_dec)))),
                                          column(2,selectInput('cutoff_p1','Cutoff k',.cutoffs,selected='0.33'))
                                        )
                                    ),
                                    fluidRow(
                                      bs4ValueBoxOutput('vb_score',     width=2),
                                      bs4ValueBoxOutput('vb_pct',       width=2),
                                      bs4ValueBoxOutput('vb_domicilios',width=2),
                                      bs4ValueBoxOutput('vb_H',         width=2),
                                      bs4ValueBoxOutput('vb_A',         width=2),
                                      bs4ValueBoxOutput('vb_MPI',       width=2)
                                    ),
                                    fluidRow(
                                      bs4Card(title='Score médio por UF',  width=6, plotOutput('mapa_score',  height=paste0(.h,'px'))),
                                      bs4Card(title='% Pobres por UF',     width=6, plotOutput('mapa_pobres', height=paste0(.h,'px')))
                                    ),
                                    fluidRow(
                                      bs4Card(title='Contribuição das componentes (% do score)', width=4,
                                              plotlyOutput('contrib_comp',  height=.h)),
                                      bs4Card(title='Score médio por arranjo', width=4,
                                              plotlyOutput('score_arranjo', height=.h)),
                                      bs4Card(title='Renda PC média por arranjo', width=4,
                                              plotlyOutput('renda_arranjo', height=.h))
                                    ),
                                    fluidRow(
                                      bs4Card(title='H (Headcount) por UF',   width=6, plotlyOutput('H_uf', height=.h+100)),
                                      bs4Card(title='A (Intensidade) por UF',  width=6, plotlyOutput('A_uf', height=.h+100))
                                    ),
                                    fluidRow(
                                      bs4Card(title='% Pobres por cutoff (todos os cutoffs)', width=6,
                                              plotlyOutput('pobres_cutoffs', height=.h)),
                                      bs4Card(title='Renda PC média por UF', width=6,
                                              plotOutput('mapa_renda', height=paste0(.h,'px')))
                                    ),
                                    fluidRow(
                                      bs4Card(title='Distribuição do score de privação', width=12,
                                              fluidRow(column(3,selectInput('recorte_dens','Recorte',
                                                                            c('Sexo'='sexo_dec','Arranjo'='arranjo_dec',
                                                                              'Região'='regiao','Setor'='setor_dec','Raça-Cor'='raca',
                                                                              'Área'='area_dec')))),
                                              plotlyOutput('dens_p1', height=.h)
                                      )
                                    )
                         ),
                         
                         ## P2 - Tendências ----
                         bs4TabItem(tabName='tendencia',
                                    div(class='filtro-row',
                                        fluidRow(
                                          column(2,selectInput('regiao_p2','Região', choices = c('Todas'='0', setNames(names(dicts$regiao), dicts$regiao)))),
                                          column(2,selectInput('uf_p2','UF','Todas')),
                                          column(2,selectInput('setor_p2','Setor', choices = c('Todas'='0', setNames(names(dicts$setor_dec), dicts$setor_dec)))),
                                          column(2,selectInput('area_p2','Área', choices = c('Todas'='0', setNames(names(dicts$area_dec), dicts$area_dec)))),
                                          column(2,selectInput('sexo_p2','Sexo', choices = c('Todas'='0', setNames(names(dicts$sexo_dec), dicts$sexo_dec)))),
                                          column(2,selectInput('raca_p2','Raça-Cor', choices = c('Todas'='0', setNames(names(dicts$raca), dicts$raca)))),
                                          column(2,selectInput('arranjo_p2','Arranjo', choices = c('Todas'='0', setNames(names(dicts$arranjo_dec), dicts$arranjo_dec)))),
                                          column(2,selectInput('cutoff_p2','Cutoff k',.cutoffs,selected='0.33')),
                                          column(6,selectInput('periodo_p2','Períodos',.periodos,multiple=TRUE,
                                                               selected=c('1981–1993','2003–2007','2019–2024'),width='100%'))
                                        )
                                    ),
                                    fluidRow(
                                      bs4Card(title='MPI e Score médio — evolução anual', width=6,
                                              plotlyOutput('evolucao_mpi_score',height=.h)),
                                      bs4Card(title='H e A — evolução anual', width=6,
                                              plotlyOutput('evolucao_HA',height=.h))
                                    ),
                                    fluidRow(
                                      bs4Card(title='MPI por arranjo e período', width=6,
                                              plotlyOutput('mpi_arranjo_ano',height=.h)),
                                      bs4Card(title='% Pobres por arranjo e período', width=6,
                                              plotlyOutput('pobres_arranjo',height=.h))
                                    ),
                                    fluidRow(
                                      bs4Card(title='MPI por região (com total)', width=6,
                                              plotlyOutput('mpi_regiao',height=.h)),
                                      bs4Card(title='Correlação score × renda PC', width=6,
                                              plotlyOutput('corr_score_renda',height=.h))
                                    ),
                                    fluidRow(
                                      bs4Card(title='Importância das componentes ao longo do tempo', width=6,
                                              plotlyOutput('comp_area',height=.h)),
                                      bs4Card(title='Sigma-convergência entre UFs', width=6,
                                              plotlyOutput('sigma_conv',height=.h))
                                    ),
                                    fluidRow(
                                      bs4Card(title='Densidade do score por período', width=6,
                                              plotlyOutput('dens_p2',height=.h)),
                                      bs4Card(title='Dominância estocástica por período', width=6,
                                              plotlyOutput('dominancia',height=.h))
                                    ),
                                    fluidRow(
                                      bs4Card(title='% Pobres por cutoff — evolução anual (todos os cutoffs)', width=12,
                                              plotlyOutput('pobres_cutoffs_ano',height=.h))
                                    )
                         ),
                         
                         ## P3 - Modelos ----
                         bs4TabItem(tabName='modelos',
                                    div(class='filtro-row',
                                        fluidRow(
                                          column(3, selectInput('modelo_p3', 'Modelo',
                                                                choices=c('Logit Binário' = 'logit',
                                                                          'ZOIB'          = 'zoib',
                                                                          'Quantílica'    = 'quant')))
                                        )
                                    ),
                                    
                                    # ZOIB
                                    conditionalPanel("input.modelo_p3 == 'zoib'",
                                                     fluidRow(
                                                       bs4Card(title='Coeficientes μ — score esperado (escala log-beta)',
                                                               width=6, plotlyOutput('zoib_mu',  height=round(.h*1.7))),
                                                       bs4Card(title='Coeficientes ν — prob. score zero (escala logit)',
                                                               width=6, plotlyOutput('zoib_nu',  height=round(.h*1.7)))
                                                     ),
                                                     fluidRow(
                                                       bs4Card(title='Score predito — atributo × período',
                                                               width=12,
                                                               fluidRow(column(3,
                                                                               selectInput('zoib_attr_p3', 'Exibir no eixo Y',
                                                                                           choices=setNames(names(.attr_labels), unname(.attr_labels)))
                                                               )),
                                                               uiOutput('zoib_refs_ui'),
                                                               plotlyOutput('.zoib_grade_plot', height=.h))
                                                     )
                                    ),
                                    
                                    # QUANTÍLICA
                                    conditionalPanel("input.modelo_p3 == 'quant'",
                                                     fluidRow(
                                                       bs4Card(title='Coeficientes por quantil — preditor selecionado',
                                                               width=12,
                                                               fluidRow(column(4,
                                                                               selectInput('pred_p3', 'Preditor',
                                                                                           choices=setNames(names(.attr_labels), unname(.attr_labels)))
                                                               )),
                                                               plotlyOutput('.qr_coefs_plot', height=.h+120))
                                                     ),
                                                     fluidRow(
                                                       bs4Card(title='Score predito — atributo × período (τ: 0,50, ceteris paribus)',
                                                               width=12,
                                                               fluidRow(column(3,
                                                                               selectInput('qr_heat_attr', 'Exibir no eixo Y',
                                                                                           choices=setNames(names(.attr_labels), unname(.attr_labels)),
                                                                                           selected='arranjo_full')),
                                                                        column(2, selectInput('qr_heat_tau', 'Quantil (τ)',
                                                                                              choices=c('τ: 0,10'='tau_010', 'τ: 0,25'='tau_025',
                                                                                                        'τ: 0,33'='tau_033', 'τ: 0,50'='tau_050',
                                                                                                        'τ: 0,67'='tau_067', 'τ: 0,75'='tau_075',
                                                                                                        'τ: 0,90'='tau_090'),
                                                                                              selected='tau_050'))
                                                               ),
                                                               uiOutput('qr_heat_refs_ui'),
                                                               plotlyOutput('qr_heatmap', height=.h))
                                                     ),
                                                     fluidRow(
                                                       bs4Card(title='Box plot quantílico — distribuição do efeito por preditor',
                                                               width=6,
                                                               fluidRow(column(5,
                                                                               selectInput('pred_box_p3', 'Atributo',
                                                                                           choices=setNames(names(.attr_labels), unname(.attr_labels)),
                                                                                           selected='arranjo_full')
                                                               )),
                                                               plotlyOutput('qr_boxplot', height=.h+120)),
                                                       bs4Card(title='Heterogeneidade do efeito entre quantis',
                                                               width=6,
                                                               fluidRow(
                                                                 column(5, selectInput('pred_heterog_p3', 'Atributo',
                                                                                       choices=setNames(names(.attr_labels), unname(.attr_labels)),
                                                                                       selected='arranjo_full')),
                                                                 column(5, selectInput('cenario_heterog', 'Cenário',
                                                                                       choices=c('τ: 0,90 × τ: 0,10'='90_10',
                                                                                                 'τ: 0,75 × τ: 0,25'='75_25',
                                                                                                 'τ: 0,67 × τ: 0,33'='67_33'),
                                                                                       selected='90_10'))
                                                               ),
                                                               plotlyOutput('qr_heterog', height=.h+120))
                                                     ),
                                                     fluidRow(
                                                       bs4Card(title='Distribuição condicional do score por quantil — ceteris paribus',
                                                               width=12,
                                                               fluidRow(
                                                                 column(3, selectInput('qr_fan_attr', 'Atributo (linhas)',
                                                                                       choices=setNames(names(.attr_labels), unname(.attr_labels)),
                                                                                       selected='arranjo_full')),
                                                                 column(2, selectInput('qr_fan_periodo', 'Período',
                                                                                       choices=.periodos,
                                                                                       selected=.periodos[length(.periodos)]))
                                                               ),
                                                               uiOutput('qr_fan_refs_ui'),
                                                               plotlyOutput('qr_fan', height=.h+80))
                                                     )
                                    ),
                                    
                                    
                                    # LOGIT
                                    conditionalPanel("input.modelo_p3 == 'logit'",
                                                     fluidRow(
                                                       bs4Card(title='Forest Plot — Odds Ratio (IC 95%, SE cluster-robusto por UF)',
                                                               width=7, plotlyOutput('logit_or',  height=round(.h*1.7))),
                                                       bs4Card(title='Efeitos Marginais Médios — AME (p.p.)',
                                                               width=5, plotlyOutput('logit_ame', height=round(.h*1.7)))
                                                     ),
                                                     fluidRow(
                                                       bs4Card(title='Probabilidade predita de pobreza — atributo × período',
                                                               width=12,
                                                               fluidRow(column(3,
                                                                               selectInput('grade_attr_p3', 'Exibir no eixo Y',
                                                                                           choices=setNames(names(.attr_labels), unname(.attr_labels)))
                                                               )),
                                                               uiOutput('grade_refs_ui'),
                                                               plotlyOutput('.logit_grade_plot', height=.h))
                                                     )
                                    )
                         ),
                         
                         ## P4 - Grupos ----
                         
                         bs4TabItem(tabName='grupos',
                                    div(class='filtro-row',
                                        fluidRow(
                                          column(3, selectInput('cl_periodo', 'Período',
                                                                choices  = c('Todos'='', .periodos_cl),
                                                                multiple = TRUE,
                                                                selected = '')),
                                          column(2, selectInput('cl_arranjo', 'Arranjo',
                                                                choices  = c('Todos'='', .arranjos_cl),
                                                                multiple = TRUE,
                                                                selected = '')),
                                          column(2, selectInput('cl_setor', 'Setor',
                                                                choices  = c('Todos'='', .setores_cl),
                                                                multiple = TRUE,
                                                                selected = '')),
                                          column(2, selectInput('cl_area', 'Área',
                                                                choices  = c('Todas'='', .areas_cl),
                                                                multiple = TRUE,
                                                                selected = '')),
                                          column(1, sliderInput('cl_k', 'k', min=2, max=8, value=3, step=1)),
                                          column(2,
                                                 br(),
                                                 actionButton('cl_rodar', 'Rodar cluster', icon=icon('play'), class='btn-primary btn-block'),
                                                 tags$small(class='text-muted', 'Selecione ao menos um período'))
                                        )
                                    ),
                                    fluidRow(
                                      bs4ValueBoxOutput('cl_vb_sil',    width=2),
                                      bs4ValueBoxOutput('cl_vb_neg',    width=2),
                                      bs4ValueBoxOutput('cl_vb_max',    width=2),
                                      bs4ValueBoxOutput('cl_vb_min',    width=2),
                                      bs4ValueBoxOutput('cl_vb_within', width=2),
                                      bs4ValueBoxOutput('cl_vb_cut',    width=2)
                                    ),
                                    fluidRow(
                                      bs4Card(title='Dendrograma', width=6,
                                              shinycssloaders::withSpinner(
                                                type=4, color='#3c8dbc',
                                                plotOutput('cl_dendro', height=paste0(.h,'px'))
                                              )),
                                      bs4Card(title='Silhueta por observação', width=6,
                                              shinycssloaders::withSpinner(
                                                type=4, color='#3c8dbc',
                                                plotOutput('cl_sil_obs', height=paste0(.h,'px'))
                                              ))
                                    ),
                                    fluidRow(
                                      bs4Card(title='Qualidade por cluster — Silhueta', width=4,
                                              shinycssloaders::withSpinner(
                                                type=4, color='#3c8dbc',
                                                plotOutput('cl_sil_cluster', height=paste0(.h,'px'))
                                              )),
                                      bs4Card(title='Perfil dimensional', width=8,
                                              shinycssloaders::withSpinner(
                                                type=4, color='#3c8dbc',
                                                plotOutput('cl_perfil', height=paste0(.h,'px'))
                                              ))
                                    ),
                                    fluidRow(
                                      bs4Card(title='Distribuição do score por cluster', width=4,
                                              shinycssloaders::withSpinner(
                                                type=4, color='#3c8dbc',
                                                plotOutput('cl_score', height=paste0(.h+150,'px'))
                                              )),
                                      bs4Card(title='Composição categórica', width=8,
                                              shinycssloaders::withSpinner(
                                                type=4, color='#3c8dbc',
                                                plotOutput('cl_comp', height=paste0(.h+150,'px'))
                                              ))
                                    )
                         )
                         
                       )
    )
  )
  
  
  # 4. Server ====
  
  server <- function(input, output, session) {
    
    # Geral  
    observe({
      anos <- sort(unique(mpi_pnad$ano))
      ufs  <- sort(unique(mpi_pnad$uf))
      updateSelectInput(session,'ano',   choices=anos,  selected=max(anos))
      updateSelectInput(session,'uf_p1', choices=c('Todas',ufs))
      updateSelectInput(session,'uf_p2', choices=c('Todas',ufs))
    })
    
    observe({
      if (length(input$cl_periodo) == 0 || all(input$cl_periodo == ''))
        shinyjs::disable('cl_rodar')
      else
        shinyjs::enable('cl_rodar')
    })
    
    
    ## P1 - Anual ----
    dados_p1_raw <- reactive({
      dt <- mpi_pnad[ano==as.integer(input$ano)]
      if (input$regiao_p1 != '0')               dt <- dt[regiao      == dicts$regiao[input$regiao_p1]]
      if (input$uf_p1     != 'Todas')           dt <- dt[uf          == input$uf_p1]
      if (input$setor_p1  != '0')               dt <- dt[setor_dec   == dicts$setor_dec[input$setor_p1]]
      if (input$area_p1   != '0')               dt <- dt[area_dec    == dicts$area_dec[input$area_p1]]
      if (input$sexo_p1   != '0')               dt <- dt[sexo_dec    == dicts$sexo_dec[input$sexo_p1]]
      if (input$raca_p1   != '0')               dt <- dt[raca        == dicts$raca[input$raca_p1]]
      if (input$arranjo_p1!= '0')               dt <- dt[arranjo_dec == dicts$arranjo_dec[input$arranjo_p1]]
      dt
    })
    dados_p1  <- dados_p1_raw  |> debounce(400)
    cutoff_p1 <- reactive(as.numeric(input$cutoff_p1)) |> debounce(400)
    
    metricas_p1 <- reactive({
      dt <- dados_p1(); k <- cutoff_p1()
      w <- dt$peso; sc <- dt$score; pob <- sc>=k
      H <- weighted.mean(pob,w,na.rm=TRUE)
      A <- if(any(pob,na.rm=TRUE)) weighted.mean(sc[pob],w[pob],na.rm=TRUE) else 0
      list(score_med=weighted.mean(sc,w,na.rm=TRUE),
           pct=H,n_dom=sum(w[pob],na.rm=TRUE),H=H,A=A,MPI=H*A)
    })
    
    .vb <- function(val) HTML(paste0('<span style="font-size:2.6rem;font-weight:900;line-height:1">',val,'</span>'))
    
    output$vb_score      <- renderbs4ValueBox(bs4ValueBox(.vb(.fmt_n(metricas_p1()$score_med)),
                                                          'Score médio',icon=icon('gauge'),color='primary',width=12))
    output$vb_pct        <- renderbs4ValueBox(bs4ValueBox(.vb(.fmt_pct(metricas_p1()$pct)),
                                                          paste0('% Pobres (k=',input$cutoff_p1,')'),icon=icon('people-group'),color='danger',width=12))
    output$vb_domicilios <- renderbs4ValueBox(bs4ValueBox(.vb(.comma_br(round(metricas_p1()$n_dom))),
                                                          'Domicílios pobres',icon=icon('house'),color='warning',width=12))
    output$vb_H          <- renderbs4ValueBox(bs4ValueBox(.vb(.fmt_n(metricas_p1()$H)),
                                                          'H — Headcount',icon=icon('users'),color='info',width=12))
    output$vb_A          <- renderbs4ValueBox(bs4ValueBox(.vb(.fmt_n(metricas_p1()$A)),
                                                          'A — Intensidade',icon=icon('arrow-trend-up'),color='secondary',width=12))
    output$vb_MPI        <- renderbs4ValueBox(bs4ValueBox(.vb(.fmt_n(metricas_p1()$MPI,4)),
                                                          'MPI = H × A',icon=icon('chart-pie'),color='success',width=12))
    
    .mapa_gg <- function(mp,fill_col,fill_label,pal_low,pal_high,fmt=NULL) {
      ggplot(mp)+
        geom_sf(aes(fill = .data[[fill_col]]),colour='white',linewidth=0.3)+
        scale_fill_gradient(low=pal_low,high=pal_high,name=fill_label,
                            labels=if(is.null(fmt)) waiver() else fmt)+
        .tema_mapa
    }
    
    output$mapa_score <- renderPlot({
      ag <- dados_p1()[,.(score=weighted.mean(score,peso,na.rm=TRUE)),by=uf]
      mp <- merge(.ufs,ag,by.x='abbrev_state',by.y='uf')
      .mapa_gg(mp,'score','Score','#FFEDA0','#BD0026')   # vermelho/amarelo
    })
    output$mapa_pobres <- renderPlot({
      k  <- cutoff_p1()
      ag <- dados_p1()[,.(pct=weighted.mean(score>=k,peso,na.rm=TRUE)),by=uf]
      mp <- merge(.ufs,ag,by.x='abbrev_state',by.y='uf')
      .mapa_gg(mp,'pct','% Pobres','#C6DBEF','#084594',.pct_br)  # azul claro/escuro
    })
    output$mapa_renda <- renderPlot({
      ag <- dados_p1()[,.(renda=weighted.mean(rpc_real,peso,na.rm=TRUE)),by=uf]
      mp <- merge(.ufs,ag,by.x='abbrev_state',by.y='uf')
      .mapa_gg(mp,'renda','Renda PC\n(R$2024)','#C7E9C0','#005A32',.comma_br)  # verde claro/escuro
    })
    
    output$contrib_comp <- renderPlotly({
      dt <- dados_p1(); ano <- as.integer(input$ano)
      pw <- .pesos_ind; cols <- intersect(names(pw[pw>0]),names(dt))
      sm <- weighted.mean(dt$score,dt$peso,na.rm=TRUE)
      contrib <- sapply(cols,\(col) weighted.mean(dt[[col]]*pw[col],dt$peso,na.rm=TRUE))
      ag <- data.table(comp=.comp_map[cols],val=contrib/sm*100)[,.(val=sum(val)),by=comp]
      ag[,nome:=.comp_names[comp]]
      p  <- ggplot(ag,aes(x=val,y=reorder(nome,val),fill=comp,
                          text=sprintf('%s: %s',nome,.fmt_pct(val/100))))+
        geom_col()+scale_fill_brewer(palette='Set2',guide='none')+
        labs(x='% do score médio',y=NULL)+.tema
      ggplotly(p,tooltip='text') |> layout(margin=list(l=180))
    })
    
    output$score_arranjo <- renderPlotly({
      dt <- dados_p1()
      ag <- dt[,.(score=weighted.mean(score,peso,na.rm=TRUE)),by=arranjo_full][order(-score)]
      p  <- ggplot(ag,aes(x=reorder(arranjo_full,score),y=score,fill=score,
                          text=sprintf('%s\nScore: %s',arranjo_full,.fmt_n(score))))+
        geom_col()+coord_flip()+
        scale_fill_gradient(low='#FFEDA0',high='#BD0026',guide='none')+
        labs(x=NULL,y='Score médio')+.tema
      ggplotly(p,tooltip='text') |> layout(margin=list(l=160))
    })
    
    output$renda_arranjo <- renderPlotly({
      dt <- dados_p1()
      ag <- dt[,.(renda=weighted.mean(rpc_real,peso,na.rm=TRUE)),by=arranjo_full][order(renda)]
      p  <- ggplot(ag,aes(x=reorder(arranjo_full,renda),y=renda,fill=renda,
                          text=sprintf('%s\nR$ %s',arranjo_full,.comma_br(round(renda)))))+
        geom_col()+coord_flip()+
        scale_fill_gradient(low='#C7E9C0',high='#005A32',guide='none')+
        scale_y_continuous(labels=.comma_br)+
        labs(x=NULL,y='Renda PC (R$ 2024)')+.tema
      ggplotly(p,tooltip='text') |> layout(margin=list(l=160))
    })
    
    output$H_uf <- renderPlotly({
      dt <- dados_p1(); k <- cutoff_p1()
      ag <- dt[,.(H=weighted.mean(score>=k,peso,na.rm=TRUE)),by=uf][order(H)]
      p  <- ggplot(ag,aes(x=reorder(uf,H),y=H,fill=H,text=sprintf('%s: %s',uf,.fmt_pct(H))))+
        geom_col()+coord_flip()+
        scale_fill_gradient(low='#C6DBEF',high='#084594',guide='none')+
        scale_y_continuous(labels=.pct_br)+
        labs(x=NULL,y='H (Headcount)')+.tema
      ggplotly(p,tooltip='text')
    })
    
    output$A_uf <- renderPlotly({
      dt <- dados_p1(); k <- cutoff_p1()
      ag <- dt[score>=k,.(A=weighted.mean(score,peso,na.rm=TRUE)),by=uf][order(A)]
      p  <- ggplot(ag,aes(x=reorder(uf,A),y=A,fill=A,text=sprintf('%s: %s',uf,.fmt_n(A))))+
        geom_col()+coord_flip()+
        scale_fill_gradient(low='#FFEDA0',high='#BD0026',guide='none')+
        labs(x=NULL,y='A (Intensidade)')+.tema
      ggplotly(p,tooltip='text')
    })
    
    output$pobres_cutoffs <- renderPlotly({
      dt <- dados_p1(); cortes <- as.numeric(.cutoffs)
      ag <- rbindlist(lapply(cortes,\(k)
                             data.table(cutoff=as.character(k),
                                        pct=weighted.mean(dt$score>=k,dt$peso,na.rm=TRUE))))
      p  <- ggplot(ag,aes(x=cutoff,y=pct,fill=cutoff,
                          text=sprintf('k=%s: %s',cutoff,.fmt_pct(pct))))+
        geom_col()+scale_y_continuous(labels=.pct_br)+
        scale_fill_brewer(palette='RdYlBu',guide='none')+
        labs(x='Cutoff k',y='% Pobres')+.tema
      ggplotly(p,tooltip='text')
    })
    
    output$dens_p1 <- renderPlotly({
      dt  <- dados_p1(); col <- input$recorte_dens; req(col %in% names(dt))
      dt[,grp:=get(col)]
      ag  <- .dens_manual(dt,'grp')
      p   <- ggplot(ag,aes(x=x,y=y,colour=grupo,group=grupo,
                           text=sprintf('Grupo: %s\nScore: %s\nDensidade: %s',
                                        grupo,.fmt_n(x,2),.fmt_n(y,3))))+
        geom_line(linewidth=0.8)+scale_x_continuous(limits=c(0,1))+
        labs(x='Score de privação',y='Densidade',colour=NULL)+.tema
      ggplotly(p,tooltip='text') |> layout(legend=list(orientation='h',y=-0.15))
    })
    
    
    ## P2 - Tendências ----
    .filtrar_p2 <- function(dt) {
      if (input$regiao_p2 != '0')               dt <- dt[regiao      == dicts$regiao[input$regiao_p2]]
      if (input$uf_p2     != 'Todas')           dt <- dt[uf          == input$uf_p2]
      if (input$setor_p2  != '0')               dt <- dt[setor_dec   == dicts$setor_dec[input$setor_p2]]
      if (input$area_p2   != '0')               dt <- dt[area_dec    == dicts$area_dec[input$area_p2]]
      if (input$sexo_p2   != '0')               dt <- dt[sexo_dec    == dicts$sexo_dec[input$sexo_p2]]
      if (input$raca_p2   != '0')               dt <- dt[raca        == dicts$raca[input$raca_p2]]
      if (input$arranjo_p2!= '0')               dt <- dt[arranjo_dec == dicts$arranjo_dec[input$arranjo_p2]]
      dt
    }
    
    dados_p2_todos <- reactive({ .filtrar_p2(mpi_ag) }) |> debounce(400)
    dados_p2_ind   <- reactive({ .filtrar_p2(mpi_pnad) }) |> debounce(400)
    
    dados_p2 <- reactive({
      anos_sel <- unlist(.periodo_anos[input$periodo_p2])
      dados_p2_todos()[ano %in% anos_sel]
    })
    cutoff_p2 <- reactive(as.numeric(input$cutoff_p2)) |> debounce(400)
    
    output$evolucao_mpi_score <- renderPlotly({
      dt <- dados_p2_ind(); k <- cutoff_p2()
      evo <- dt[,.(MPI=.calc_mpi(score,peso,k),
                   Score=weighted.mean(score,peso,na.rm=TRUE)),by=ano][order(ano)]
      ag  <- melt(evo,'ano',variable.name='metrica',value.name='valor')
      p   <- ggplot(ag,aes(x=ano,y=valor,colour=metrica,group=metrica,
                           text=sprintf('Ano: %d\n%s: %s',ano,metrica,.fmt_n(valor,4))))+
        geom_line(linewidth=0.8)+geom_point(size=2)+
        scale_colour_manual(values=c(MPI='#d7191c',Score='#2c7bb6'),name=NULL)+
        labs(x=NULL,y=NULL)+.tema
      ggplotly(p,tooltip='text') |> layout(legend=list(orientation='h',y=-0.15))
    })
    
    output$evolucao_HA <- renderPlotly({
      dt <- dados_p2_ind(); k <- cutoff_p2()
      evo <- dt[,{pob<-score>=k
      H<-weighted.mean(pob,peso,na.rm=TRUE)
      A<-if(any(pob,na.rm=TRUE)) weighted.mean(score[pob],peso[pob],na.rm=TRUE) else 0
      .(H=H,A=A)},by=ano][order(ano)]
      ag  <- melt(evo,'ano',variable.name='metrica',value.name='valor')
      p   <- ggplot(ag,aes(x=ano,y=valor,colour=metrica,group=metrica,
                           text=sprintf('Ano: %d\n%s: %s',ano,metrica,.fmt_n(valor,4))))+
        geom_line(linewidth=0.8)+geom_point(size=2)+
        scale_colour_manual(values=c(H='#1a9641',A='#e6520e'),name=NULL)+
        labs(x=NULL,y=NULL)+.tema
      ggplotly(p,tooltip='text') |> layout(legend=list(orientation='h',y=-0.15))
    })
    
    output$mpi_arranjo_ano <- renderPlotly({
      dt <- dados_p2(); k <- cutoff_p2()
      ag <- dt[,.(MPI=.calc_mpi(score_med,pop,k)),by=.(periodo,arranjo_dec)]
      p  <- ggplot(ag,aes(x=periodo,y=MPI,colour=arranjo_dec,group=arranjo_dec,
                          text=sprintf('%s\n%s: %s',arranjo_dec,periodo,.fmt_n(MPI,4))))+
        geom_line(linewidth=0.7)+geom_point(size=2)+
        scale_colour_brewer(palette='Dark2',name='Arranjo')+
        labs(x=NULL,y='MPI')+.tema+theme(axis.text.x=element_text(angle=30,hjust=1))
      ggplotly(p,tooltip='text') |> layout(legend=list(orientation='h',y=-0.2))
    })
    
    output$pobres_arranjo <- renderPlotly({
      dt <- dados_p2(); k <- cutoff_p2()
      ag <- dt[,.(pct=weighted.mean(score_med>=k,pop,na.rm=TRUE)),by=.(periodo,arranjo_dec)]
      p  <- ggplot(ag,aes(x=periodo,y=pct,colour=arranjo_dec,group=arranjo_dec,
                          text=sprintf('%s\n%s: %s',arranjo_dec,periodo,.fmt_pct(pct))))+
        geom_line(linewidth=0.7)+geom_point(size=2)+
        scale_y_continuous(labels=.pct_br)+
        scale_colour_brewer(palette='Dark2',name='Arranjo')+
        labs(x=NULL,y=paste0('% Pobres (k=',input$cutoff_p2,')'))+.tema+
        theme(axis.text.x=element_text(angle=30,hjust=1))
      ggplotly(p,tooltip='text') |> layout(legend=list(orientation='h',y=-0.2))
    })
    
    output$mpi_regiao <- renderPlotly({
      dt <- dados_p2_ind(); k <- cutoff_p2()
      reg_dt <- dt[,.(MPI=.calc_mpi(score,peso,k)),by=.(ano,regiao)]
      tot_dt <- dt[,.(MPI=.calc_mpi(score,peso,k)),by=ano]
      regioes <- sort(unique(reg_dt$regiao))
      cores   <- RColorBrewer::brewer.pal(max(3,length(regioes)),'Set1')[seq_along(regioes)]
      names(cores) <- regioes
      p <- plot_ly()
      for (rg in regioes) {
        sub <- reg_dt[regiao==rg]
        p <- add_trace(p,data=sub,x=~ano,y=~MPI,type='scatter',mode='lines+markers',
                       name=rg,line=list(color=cores[rg],width=2),
                       marker=list(color=cores[rg],size=5),
                       text=~sprintf('%s\nAno: %d\nMPI: %s',regiao,ano,.fmt_n(MPI,4)),
                       hoverinfo='text')
      }
      p <- add_trace(p,data=tot_dt,x=~ano,y=~MPI,type='scatter',mode='lines',
                     name='Total',line=list(color='black',width=2,dash='dash'),
                     text=~sprintf('Total\nAno: %d\nMPI: %s',ano,.fmt_n(MPI,4)),
                     hoverinfo='text')
      layout(p,xaxis=list(title=''),yaxis=list(title='MPI'),
             legend=list(orientation='h',y=-0.15))
    })
    
    output$corr_score_renda <- renderPlotly({
      dt <- dados_p2()
      ag <- dt[, .(score=weighted.mean(score_med,pop,na.rm=TRUE),
                   renda=weighted.mean(rpc_real,pop,na.rm=TRUE)), by=.(periodo,uf)]
      periodos_sel <- sort(unique(ag$periodo))
      cores <- setNames(RColorBrewer::brewer.pal(max(3,length(periodos_sel)),'Dark2')[seq_along(periodos_sel)],
                        periodos_sel)
      p <- plot_ly()
      for (per in periodos_sel) {
        sub <- ag[periodo==per]
        p <- add_trace(p, data=sub, x=~renda, y=~score, type='scatter', mode='markers',
                       name=per, marker=list(color=cores[per],size=7,opacity=0.8),
                       text=~sprintf('%s — %s\nRenda: R$ %s\nScore: %s',
                                     uf,periodo,.comma_br(round(renda)),.fmt_n(score,3)),
                       hoverinfo='text', legendgroup=per)
        
        if (nrow(sub) >= 2) {
          fit     <- lm(score ~ renda, data=sub)
          x_seq   <- seq(min(sub$renda,na.rm=TRUE), max(sub$renda,na.rm=TRUE), length.out=80)
          y_fit   <- predict(fit, newdata=data.frame(renda=x_seq))
          p <- add_trace(p, x=x_seq, y=y_fit, type='scatter', mode='lines',
                         line=list(color=cores[per],width=1.5,dash='solid'),
                         showlegend=FALSE, hoverinfo='none', legendgroup=per)
        }
      }
      layout(p, xaxis=list(title='Renda PC (R$ 2024)'), yaxis=list(title='Score MPI'),
             legend=list(orientation='h', y=-0.15))
    })
    
    output$comp_area <- renderPlotly({
      dt   <- dados_p2_ind()
      anos <- sort(unique(dt$ano))
      area_dt <- rbindlist(lapply(anos,\(a) {
        sub  <- dt[ano==a]
        pw   <- .pesos_ind
        cols <- intersect(names(pw[pw>0]),intersect(.cols_ind,names(sub)))
        contrib <- sapply(cols,\(col)
                          weighted.mean(sub[[col]]*pw[col],sub$peso,na.rm=TRUE))
        total <- sum(contrib,na.rm=TRUE)
        if(is.na(total)||total==0) return(NULL)
        comps <- unique(.comp_map[cols])
        rbindlist(lapply(comps,\(cmp) {
          idx   <- cols[.comp_map[cols]==cmp]
          share <- sum(contrib[idx],na.rm=TRUE)/total
          data.table(ano=a,comp=cmp,share=share)
        }))
      }))
      req(nrow(area_dt)>0)
      area_dt[,nome:=factor(.comp_names[comp],levels=unname(.comp_names))]
      p <- ggplot(area_dt,aes(x=ano,y=share,fill=nome,group=nome,
                              text=sprintf('Ano: %d\n%s: %s',ano,nome,.fmt_pct(share))))+
        geom_area(position='stack',colour='white',linewidth=0.2)+
        scale_y_continuous(labels=.pct_br)+
        scale_fill_brewer(palette='Set2',name=NULL)+
        labs(x=NULL,y='Participação no score')+.tema
      ggplotly(p,tooltip='text') |> layout(legend=list(orientation='h',y=-0.15))
    })
    
    output$sigma_conv <- renderPlotly({
      dt <- dados_p2_ind(); k <- cutoff_p2()
      uf_ano <- dt[,.(MPI=.calc_mpi(score,peso,k)),by=.(ano,uf)]
      sigma  <- uf_ano[,.(sigma=sd(MPI,na.rm=TRUE),media=mean(MPI,na.rm=TRUE)),by=ano
      ][,CV:=sigma/media][order(ano)]
      escala <- max(sigma$sigma,na.rm=TRUE)/max(sigma$CV,na.rm=TRUE)
      plot_ly() |>
        add_trace(data=sigma,x=~ano,y=~sigma,type='scatter',mode='lines+markers',
                  name='σ (desvio padrão)',line=list(color='#2c7bb6',width=2),
                  marker=list(color='#2c7bb6',size=6),
                  text=~sprintf('Ano: %d\nσ: %s',ano,.fmt_n(sigma,4)),hoverinfo='text') |>
        add_trace(data=sigma,x=~ano,y=~CV*escala,type='scatter',mode='lines+markers',
                  name='CV',yaxis='y2',line=list(color='#d7191c',width=2,dash='dash'),
                  marker=list(color='#d7191c',size=6),
                  text=~sprintf('Ano: %d\nCV: %s',ano,.fmt_n(CV,4)),hoverinfo='text') |>
        layout(yaxis =list(title='σ',titlefont=list(color='#2c7bb6'),
                           tickfont=list(color='#2c7bb6')),
               yaxis2=list(title='CV',overlaying='y',side='right',
                           titlefont=list(color='#d7191c'),tickfont=list(color='#d7191c')),
               xaxis=list(title=''),
               legend=list(orientation='h',y=-0.15),margin=list(r=50))
    })
    
    output$dens_p2 <- renderPlotly({
      dt <- dados_p2(); req(nrow(dt)>0)
      ag <- .dens_manual(dt,'periodo','pop','score_med')
      periodos_sel <- sort(unique(ag$grupo))
      cores <- setNames(.pal_periodo[seq_along(periodos_sel)], periodos_sel)
      p <- plot_ly()
      for (per in periodos_sel) {
        sub <- ag[grupo==per]
        p <- add_trace(p,data=sub,x=~x,y=~y,type='scatter',mode='lines',
                       name=per,line=list(color=cores[per],width=2),
                       text=~sprintf('Período: %s\nScore: %s\nDensidade: %s',
                                     grupo,.fmt_n(x,2),.fmt_n(y,3)),
                       hoverinfo='text')
      }
      layout(p,xaxis=list(title='Score de privação',range=c(0,1)),
             yaxis=list(title='Densidade'),
             legend=list(orientation='h',y=-0.15))
    })
    
    output$dominancia <- renderPlotly({
      dt  <- dados_p2(); req(nrow(dt)>0)
      xs  <- seq(0,1,by=0.01)
      ag  <- rbindlist(lapply(unique(dt$periodo),\(per) {
        sub <- dt[periodo==per]
        data.table(periodo=per,x=xs,
                   cdf=sapply(xs,\(v) weighted.mean(sub$score_med<=v,sub$pop,na.rm=TRUE)))
      }))
      periodos_sel <- sort(unique(ag$periodo))
      cores <- setNames(.pal_periodo[seq_along(periodos_sel)],periodos_sel)
      p <- plot_ly()
      for (per in periodos_sel) {
        sub <- ag[periodo==per]
        p <- add_trace(p,data=sub,x=~x,y=~cdf,type='scatter',mode='lines',
                       name=per,line=list(color=cores[per],width=2),
                       text=~sprintf('Período: %s\nScore ≤ %s: %s',
                                     periodo,.fmt_n(x,2),.fmt_pct(cdf)),
                       hoverinfo='text')
      }
      layout(p,xaxis=list(title='Score de privação',range=c(0,1)),
             yaxis=list(title='F(score)',tickformat='.0%'),
             legend=list(orientation='h',y=-0.15))
    })
    
    output$pobres_cutoffs_ano <- renderPlotly({
      dt <- dados_p2_ind()
      ag <- rbindlist(lapply(.cutoffs, \(ct) {
        k <- as.numeric(ct)
        dt[, .(cutoff=ct, pct=weighted.mean(score>=k, peso, na.rm=TRUE)), by=ano]
      }))[order(ano)]
      cores_c <- setNames(RColorBrewer::brewer.pal(length(.cutoffs),'RdYlBu'), .cutoffs)
      p <- plot_ly()
      for (ct in .cutoffs) {
        sub <- ag[cutoff==ct]
        p <- add_trace(p, data=sub, x=~ano, y=~pct, type='scatter', mode='lines+markers',
                       name=ct, line=list(color=cores_c[ct],width=2),
                       marker=list(color=cores_c[ct],size=5),
                       text=~sprintf('Ano: %d\nk=%s: %s', ano, cutoff, .fmt_pct(pct)),
                       hoverinfo='text')
      }
      layout(p, xaxis=list(title=''), yaxis=list(title='% Pobres', tickformat='.0%'),
             legend=list(orientation='h', y=-0.15))
    })
    
    
    ## P3 - Modelos ----
    
    ### ZOIB ----
    .trunc_thresh <- 3.5
    
    .zoib_coef_plot <- function(comp, invert_colors=FALSE) {
      renderPlotly({
        dt <- .parse_termos(
          as.data.table(.zoib_coefs)[componente == comp & termo != '(Intercept)']
        )
        dt       <- .assign_ypos(dt, 'coef')
        grp_info <- dt[, .(y_min=min(y), y_max=max(y)), by=grupo]
        dt[, col       := ifelse(xor(coef > 0, invert_colors), '#e74c3c', '#2c7bb6')]
        dt[, truncado  := abs(coef) > .trunc_thresh]
        dt[, coef_disp := pmax(pmin(coef, .trunc_thresh), -.trunc_thresh)]
        
        # anotações para barras truncadas — valor real + seta indicando direção
        annots_trunc <- lapply(which(dt$truncado), \(i) list(
          x         = dt$coef_disp[i] + ifelse(dt$coef[i] < 0, 0.08, -0.08),
          y         = dt$y[i],
          text      = ifelse(dt$coef[i] < 0,
                             sprintf('%s →', .fmt_n(dt$coef[i], 3)),
                             sprintf('← %s', .fmt_n(dt$coef[i], 3))),
          showarrow = FALSE,
          xanchor   = ifelse(dt$coef[i] < 0, 'right', 'left'),
          font      = list(size=9, color='white')   # sobre a barra colorida
        ))
        
        sub_txt <- if (!invert_colors) .sub_mu else .sub_nu
        
        # nota de truncamento no subtítulo se houver barras truncadas
        if (any(dt$truncado))
          sub_txt <- paste0(sub_txt,
                            '<br><i style="color:#888">Barras com seta (→/←): valor truncado em ±',
                            .trunc_thresh, ' para visualização</i>')
        
        p <- plot_ly()
        p <- add_segments(p, x=0, xend=0,
                          y=min(dt$y)-0.8, yend=max(dt$y)+0.8,
                          line=list(color='grey50', dash='dash', width=1),
                          showlegend=FALSE, hoverinfo='none')
        p <- add_bars(p, data=dt, x=~coef_disp, y=~y, orientation='h',
                      marker=list(color=~col),
                      # valor fora da barra só para não-truncadas
                      text=~ifelse(truncado, '', .fmt_n(coef, 3)),
                      textposition='outside',
                      textfont=list(size=11),
                      hovertext=~sprintf('<b>%s</b>\nCoef: %s%s',
                                         tick_lbl, .fmt_n(coef, 3),
                                         ifelse(truncado, ' ⚠ truncado na visualização', '')),
                      hoverinfo='text', showlegend=FALSE)
        
        layout(p,
               shapes      = .forest_shapes(dt, grp_info),
               annotations = c(
                 .forest_annots(grp_info[, .(y_max=max(y)), by=grupo], sub_txt),
                 annots_trunc
               ),
               xaxis = list(title='Coeficiente', zeroline=FALSE,
                            range=c(-.trunc_thresh - 0.3, .trunc_thresh + 0.3)),
               yaxis = list(tickmode='array', tickvals=dt$y, ticktext=dt$tick_lbl,
                            showgrid=FALSE),
               margin = list(l=260, b=70, t=80)
        )
      })
    }
    
    output$zoib_mu <- .zoib_coef_plot('mu', invert_colors=FALSE)
    output$zoib_nu <- .zoib_coef_plot('nu', invert_colors=TRUE)
    
    output$zoib_refs_ui <- renderUI({
      dt   <- as.data.table(.zoib_grade)
      attr <- input$zoib_attr_p3; req(attr)
      outros <- setdiff(names(.attr_labels), attr)
      fluidRow(do.call(tagList, lapply(outros, \(col) {
        column(2, selectInput(paste0('zoib_ref_', col), .attr_labels[col],
                              choices=sort(unique(dt[[col]])),
                              selected=.refs_modais[[col]]))
      })))
    })
    
    output$.zoib_grade_plot <- renderPlotly({
      dt   <- as.data.table(.zoib_grade)
      attr <- input$zoib_attr_p3; req(attr %in% names(dt))
      for (col in setdiff(names(.attr_labels), attr)) {
        val <- input[[paste0('zoib_ref_', col)]]
        if (!is.null(val) && col %in% names(dt)) dt <- dt[get(col) == val]
      }
      ag <- dt[, .(score_pred=mean(score_pred, na.rm=TRUE)), by=c(attr, 'periodo')]
      setnames(ag, attr, 'grupo')
      ag[, periodo := factor(periodo, levels=.periodos)]
      ag[, grupo   := factor(grupo)]
      periodos <- levels(ag$periodo); grupos <- levels(ag$grupo)
      mat <- dcast(ag, grupo ~ periodo, value.var='score_pred')
      z   <- as.matrix(mat[, -1, with=FALSE]); rownames(z) <- mat$grupo
      z_vec <- as.numeric(as.vector(t(z)))
      cores_texto <- ifelse(z_vec > 0.4, 'white', '#333333')
      
      annots_cells <- lapply(seq_along(z_vec), \(i) list(
        x         = rep(periodos, times=length(grupos))[i],
        y         = rep(grupos,   each=length(periodos))[i],
        text      = .fmt_pct(z_vec[i]),
        showarrow = FALSE,
        font      = list(size=12, color=ifelse(z_vec[i] > 0.4, 'white', '#333333'))
      ))
      
      plot_ly(x=periodos, y=grupos, z=z, type='heatmap',
              zmin=0, zmax=1,
              colorscale=list(
                list(0.000, '#1a7c3e'),
                list(0.167, '#74c476'),
                list(0.333, '#fed976'),
                list(0.500, '#e31a1c'),
                list(0.667, '#800026'),
                list(1.000, '#49006a')
              ),
              colorbar=list(title='Score pred.', tickformat='.0%'),
              text=matrix(sprintf('%s\n%s\nScore: %s',
                                  rep(grupos,each=length(periodos)),
                                  rep(periodos,times=length(grupos)),
                                  .fmt_pct(as.vector(t(z)))),
                          nrow=length(grupos), ncol=length(periodos), byrow=TRUE),
              hoverinfo='text') |>
        layout(xaxis=list(title=NULL, tickangle=-30),
               yaxis=list(title=NULL),
               margin=list(l=160, b=80, t=20),
               annotations=annots_cells)
    })
    
    
    ### Quantílica ----
    output$.qr_coefs_plot <- renderPlotly({
      dt   <- as.data.table(.qr_coefs)
      pref <- input$pred_p3; req(pref)
      sel  <- dt[grepl(paste0('^', pref), termo) & termo != '(Intercept)']
      req(nrow(sel) > 0)
      sel[, tau_num := .tau_map[as.character(tau)]]
      sel[, label   := sub(paste0('^', pref), '', termo)]
      termos <- sort(unique(sel$termo))
      cores  <- setNames(RColorBrewer::brewer.pal(max(3,length(termos)),'Dark2')[seq_along(termos)], termos)
      p <- plot_ly()
      for (t in termos) {
        sub <- sel[termo == t]
        p <- add_trace(p, data=sub, x=~tau_num, y=~coef,
                       type='scatter', mode='lines+markers',
                       name=unique(sub$label),
                       line=list(color=cores[t], width=2),
                       marker=list(color=cores[t], size=5),
                       text=~sprintf('%s\nτ: %s\nCoef: %s', label, tau_num, .fmt_n(coef,4)),
                       hoverinfo='text')
      }
      layout(p,
             shapes=list(list(type='line', x0=min(.tau_map), x1=max(.tau_map),
                              y0=0, y1=0, line=list(color='grey50', dash='dash', width=1))),
             xaxis=list(title='Quantil (τ)', tickvals=unname(.tau_map),
                        ticktext=as.character(unname(.tau_map))),
             yaxis=list(title='Coeficiente'),
             legend=list(orientation='h', y=-0.15))
    })
    
    output$qr_heat_refs_ui <- renderUI({
      dt   <- as.data.table(.qr_grade)
      attr <- input$qr_heat_attr; req(attr)
      outros <- setdiff(names(.attr_labels), attr)
      fluidRow(do.call(tagList, lapply(outros, \(col) {
        column(2, selectInput(paste0('qr_heat_ref_', col), .attr_labels[col],
                              choices=sort(unique(dt[[col]])),
                              selected=.refs_modais[[col]]))
      })))
    })
    
    output$qr_heatmap <- renderPlotly({
      dt   <- as.data.table(.qr_grade)
      attr <- input$qr_heat_attr; req(attr %in% names(dt))
      for (col in setdiff(names(.attr_labels), attr)) {
        val <- input[[paste0('qr_heat_ref_', col)]]
        if (!is.null(val) && col %in% names(dt)) dt <- dt[get(col) == val]
      }
      tau_sel <- input$qr_heat_tau
      ag <- dt[, .(score=mean(get(tau_sel), na.rm=TRUE)), by=c(attr, 'periodo')]
      setnames(ag, attr, 'grupo')
      ag[, periodo := factor(periodo, levels=.periodos)]
      ag[, grupo   := factor(grupo)]
      periodos <- levels(ag$periodo); grupos <- levels(ag$grupo)
      mat <- dcast(ag, grupo ~ periodo, value.var='score')
      z   <- matrix(as.numeric(as.matrix(mat[,-1,with=FALSE])),
                    nrow=nrow(mat), dimnames=list(mat$grupo, periodos))
      z_t <- as.numeric(t(z))
      annots <- lapply(seq_along(z_t), \(i) list(
        x=rep(periodos, times=length(grupos))[i],
        y=rep(grupos,   each=length(periodos))[i],
        text=.fmt_n(z_t[i], 3), showarrow=FALSE,
        font=list(size=10, color=ifelse(z_t[i] > 0.35, 'white', '#333333'))
      ))
      plot_ly(x=periodos, y=grupos, z=z, type='heatmap',
              zmin=0, zmax=1,
              colorscale=list(list(0.000,'#1a7c3e'), list(0.167,'#74c476'),
                              list(0.333,'#fed976'), list(0.500,'#e31a1c'),
                              list(0.667,'#800026'), list(1.000,'#49006a')),
              colorbar=list(title=paste0('Score (', input$qr_heat_tau, ')')),
              text=matrix(sprintf('%s\n%s\nScore (τ variável): %s',
                                  rep(grupos,each=length(periodos)),
                                  rep(periodos,times=length(grupos)),
                                  .fmt_n(z_t,3)),
                          nrow=length(grupos), ncol=length(periodos), byrow=TRUE),
              hoverinfo='text') |>
        layout(xaxis=list(title=NULL,tickangle=-30), yaxis=list(title=NULL),
               margin=list(l=160,b=80,t=20), annotations=annots)
    })
    
    output$qr_fan_refs_ui <- renderUI({
      dt   <- as.data.table(.qr_grade)
      attr <- input$qr_fan_attr; req(attr)
      outros <- setdiff(names(.attr_labels), attr)
      fluidRow(do.call(tagList, lapply(outros, \(col) {
        column(2, selectInput(paste0('qr_fan_ref_', col), .attr_labels[col],
                              choices=sort(unique(dt[[col]])),
                              selected=.refs_modais[[col]]))
      })))
    })
    
    output$qr_fan <- renderPlotly({
      dt   <- as.data.table(.qr_grade)
      attr <- input$qr_fan_attr; req(attr %in% names(dt))
      per  <- input$qr_fan_periodo; req(!is.null(per))
      dt   <- dt[periodo == per]
      for (col in setdiff(names(.attr_labels), attr)) {
        val <- input[[paste0('qr_fan_ref_', col)]]
        if (!is.null(val) && col %in% names(dt)) dt <- dt[get(col) == val]
      }
      tau_cols <- grep('^tau_', names(dt), value=TRUE)
      ag <- dt[, lapply(.SD, mean, na.rm=TRUE), by=attr, .SDcols=tau_cols]
      setnames(ag, attr, 'categoria')
      cats  <- sort(unique(ag$categoria))
      cores <- setNames(RColorBrewer::brewer.pal(max(3,length(cats)),'Dark2')[seq_along(cats)], cats)
      tau_vals <- unname(.tau_map)
      
      p <- plot_ly()
      for (cat in cats) {
        sub  <- ag[categoria == cat]
        vals <- as.numeric(sub[, tau_cols, with=FALSE])
        col  <- cores[cat]
        p <- add_trace(p, x=tau_vals, y=vals,
                       type='scatter', mode='lines+markers', name=cat,
                       line=list(color=col, width=2.2),
                       marker=list(color=col, size=6),
                       text=sprintf('%s<br>τ: %s: %s', cat, tau_vals, .fmt_n(vals,3)),
                       hoverinfo='text')
      }
      layout(p,
             xaxis=list(title='Quantil (τ)', tickmode='array',
                        tickvals=tau_vals, ticktext=as.character(tau_vals)),
             yaxis=list(title='Score predito', range=c(0, NA)),
             legend=list(orientation='h', y=-0.15),
             margin=list(l=60, b=60, t=20)
      )
    })
    
    output$qr_heterog <- renderPlotly({
      dt   <- as.data.table(.qr_coefs)
      pref <- input$pred_heterog_p3; req(pref)
      cen  <- input$cenario_heterog;  req(!is.null(cen))
      tau_map_cen <- list(
        '90_10' = c(high='tau_090', low='tau_010'),
        '75_25' = c(high='tau_075', low='tau_025'),
        '67_33' = c(high='tau_067', low='tau_033')
      )
      taus_sel <- tau_map_cen[[cen]]
      cen_lbl  <- switch(cen,
                         '90_10'='coef(τ: 0,90) − coef(τ: 0,10)',
                         '75_25'='coef(τ: 0,75) − coef(τ: 0,25)',
                         '67_33'='coef(τ: 0,67) − coef(τ: 0,33)')
      
      sel <- dt[grepl(paste0('^', pref), termo) & termo != '(Intercept)']
      req(nrow(sel) > 0)
      sel[, categoria := sub(paste0('^', pref), '', termo)]
      wide <- dcast(sel, categoria ~ tau, value.var='coef')
      wide[, diff := wide[[taus_sel['high']]] - wide[[taus_sel['low']]]]
      wide <- wide[order(diff)]
      wide[, col := ifelse(abs(diff) < 0.003, '#b4b2a9',
                           ifelse(diff > 0, '#a32d2d', '#185fa5'))]
      wide[, y := seq_len(.N)]
      
      p <- plot_ly()
      p <- add_segments(p, x=0, xend=0, y=0.3, yend=nrow(wide)+0.7,
                        line=list(color='#aaa', dash='dash', width=1),
                        showlegend=FALSE, hoverinfo='none')
      p <- add_bars(p, data=wide, x=~diff, y=~y, orientation='h',
                    marker=list(color=~col),
                    text=~ifelse(diff>=0, paste0('+', .fmt_n(diff,3)), .fmt_n(diff,3)),
                    textposition='outside', textfont=list(size=11),
                    hovertext=~sprintf('<b>%s</b><br>%s: %s', categoria, cen_lbl, .fmt_n(diff,3)),
                    hoverinfo='text', showlegend=FALSE)
      layout(p,
             xaxis=list(title=cen_lbl, zeroline=FALSE),
             yaxis=list(tickmode='array', tickvals=wide$y, ticktext=wide$categoria,
                        showgrid=FALSE),
             margin=list(l=200, b=60, t=20)
      )
    })
    
    output$qr_boxplot <- renderPlotly({
      req(input$pred_box_p3)
      dt <- as.data.table(.qr_coefs)
      pref <- input$pred_box_p3
      sel <- dt[grepl(paste0('^', pref), termo) & termo != '(Intercept)']
      req(nrow(sel) > 0)
      
      # Mapear tau numérico
      sel[, tau_num := .tau_map[as.character(tau)]]
      sel[, categoria := sub(paste0('^', pref), '', termo)]
      
      # Dados para boxplot: cada combinação categoria × quantil
      # Shape: uma coluna "coef", uma coluna "quantil"
      box_data <- sel[, .(categoria, tau_num, coef)]
      
      # Ordenar categorias por mediana (ou por coef médio)
      ord <- box_data[, .(med = median(coef, na.rm=TRUE)), by=categoria][order(med), categoria]
      box_data[, categoria := factor(categoria, levels = ord)]
      
      p <- ggplot(box_data, aes(x = categoria, y = coef)) +
        geom_boxplot(aes(fill = as.factor(tau_num)), outlier.shape = NA) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
        scale_fill_brewer(palette = "RdYlBu", name = "Quantil (τ)") +
        labs(x = NULL, y = "Coeficiente estimado",
             title = paste("Distribuição dos coeficientes por quantil -", .attr_labels[pref]),
             caption = .sub_mu) +
        coord_flip() +
        theme_minimal(base_size = 11) +
        theme(legend.position = "bottom")
      ggplotly(p, tooltip = "y") %>% layout(margin = list(l = 120))
    })
    
    output$.qr_grade_plot <- renderPlotly({
      dt       <- as.data.table(.qr_grade)
      tau_cols <- grep('^tau_', names(dt), value=TRUE)
      ag       <- dt[, lapply(.SD, mean, na.rm=TRUE), by=arranjo_full, .SDcols=tau_cols]
      ag_long  <- melt(ag, id.vars='arranjo_full', variable.name='tau', value.name='score_pred')
      ag_long[, tau_num := .tau_map[as.character(tau)]]
      arranjos <- sort(unique(ag_long$arranjo_full))
      cores    <- setNames(RColorBrewer::brewer.pal(max(3,length(arranjos)),'Dark2')[seq_along(arranjos)], arranjos)
      p <- plot_ly()
      for (arr in arranjos) {
        sub <- ag_long[arranjo_full == arr]
        p <- add_trace(p, data=sub, x=~tau_num, y=~score_pred,
                       type='scatter', mode='lines+markers', name=arr,
                       line=list(color=cores[arr], width=2),
                       marker=list(color=cores[arr], size=5),
                       text=~sprintf('%s\nτ: %s\nScore: %s', arranjo_full, tau_num, .fmt_n(score_pred,3)),
                       hoverinfo='text')
      }
      layout(p,
             xaxis=list(title='Quantil (τ)', tickvals=unname(.tau_map),
                        ticktext=as.character(unname(.tau_map))),
             yaxis=list(title='Score predito'),
             legend=list(orientation='h', y=-0.15))
    })
    
    
    ### Logit ----
    output$logit_or <- renderPlotly({
      dt <- .parse_termos(as.data.table(.logit_coefs)[termo != '(Intercept)'])
      setnames(dt, 'p', 'p_val')
      dt[, sig      := p_val < 0.05]
      dt            <- .assign_ypos(dt, 'OR')
      grp_info      <- dt[, .(y_max=max(y)), by=grupo]
      
      p <- plot_ly()
      p <- add_segments(p, x=1, xend=1, y=min(dt$y)-0.8, yend=max(dt$y)+0.8,
                        line=list(color='#e74c3c', dash='dash', width=1),
                        showlegend=FALSE, hoverinfo='none')
      for (s in c(FALSE, TRUE)) {
        sub <- dt[sig == s]; if (nrow(sub) == 0) next
        col <- if (s) '#2c7bb6' else '#aaaaaa'
        p <- add_segments(p, data=sub, x=~OR_lo, xend=~OR_hi, y=~y, yend=~y,
                          line=list(color=col, width=1.5), showlegend=FALSE, hoverinfo='none')
        p <- add_markers(p, data=sub, x=~OR, y=~y, marker=list(color=col, size=7),
                         text=~sprintf('<b>%s</b>\nOR: %s\nIC 95%%: [%s; %s]\np = %s',
                                       tick_lbl, .fmt_n(OR,3), .fmt_n(OR_lo,3), .fmt_n(OR_hi,3),
                                       formatC(p_val, digits=3, format='g')),
                         hoverinfo='text', showlegend=FALSE)
      }
      layout(p,
             shapes      = .forest_shapes(dt, grp_info),
             annotations = .forest_annots(grp_info, .sub_or),
             xaxis = list(title='Odds Ratio (escala log)', type='log',
                          tickmode='array',
                          tickvals =c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100),
                          ticktext =c('0,1','0,2','0,5','1','2','5','10','20','50','100'),
                          zeroline=FALSE),
             yaxis = list(tickmode='array', tickvals=dt$y, ticktext=dt$tick_lbl,
                          autorange='reversed', showgrid=FALSE),
             margin=list(l=240, b=60, t=80), showlegend=FALSE
      )
    })
    
    output$logit_ame <- renderPlotly({
      dt <- .parse_termos(as.data.table(.logit_effects)[termo != '(Intercept)'])
      setnames(dt, 'p', 'p_val')
      dt[, sig      := p_val < 0.05]
      dt            <- .assign_ypos(dt, 'AME')
      grp_info      <- dt[, .(y_max=max(y)), by=grupo]
      
      p <- plot_ly()
      p <- add_segments(p, x=0, xend=0, y=min(dt$y)-0.8, yend=max(dt$y)+0.8,
                        line=list(color='grey50', dash='dash', width=1),
                        showlegend=FALSE, hoverinfo='none')
      for (s in c(FALSE, TRUE)) {
        sub <- dt[sig == s]; if (nrow(sub) == 0) next
        col <- if (s) '#e74c3c' else '#aaaaaa'
        p <- add_segments(p, data=sub, x=~IC_lo, xend=~IC_hi, y=~y, yend=~y,
                          line=list(color=col, width=1.5), showlegend=FALSE, hoverinfo='none')
        p <- add_markers(p, data=sub, x=~AME, y=~y, marker=list(color=col, size=7),
                         text=~sprintf('<b>%s</b>\nAME: %s p.p.\nIC 95%%: [%s; %s]\np = %s',
                                       tick_lbl, .fmt_n(AME*100,1),
                                       .fmt_n(IC_lo*100,1), .fmt_n(IC_hi*100,1),
                                       formatC(p_val, digits=3, format='g')),
                         hoverinfo='text', showlegend=FALSE)
      }
      layout(p,
             shapes      = .forest_shapes(dt, grp_info),
             annotations = .forest_annots(grp_info, .sub_ame),
             xaxis = list(title='AME (p.p.)', tickformat='.0%', zeroline=FALSE),
             yaxis = list(tickmode='array', tickvals=dt$y, ticktext=dt$tick_lbl,
                          autorange='reversed', showgrid=FALSE),
             margin=list(l=240, b=60, t=80), showlegend=FALSE
      )
    })
    
    output$grade_refs_ui <- renderUI({
      dt   <- as.data.table(.logit_grade)
      attr <- input$grade_attr_p3; req(attr)
      outros <- setdiff(names(.attr_labels), attr)
      fluidRow(do.call(tagList, lapply(outros, \(col) {
        column(2, selectInput(paste0('ref_', col), .attr_labels[col],
                              choices=sort(unique(dt[[col]])),
                              selected=.refs_modais[[col]]))
      })))
    })
    
    output$.logit_grade_plot <- renderPlotly({
      dt   <- as.data.table(.logit_grade)
      attr <- input$grade_attr_p3; req(attr %in% names(dt))
      for (col in setdiff(names(.attr_labels), attr)) {
        val <- input[[paste0('ref_', col)]]
        if (!is.null(val) && col %in% names(dt)) dt <- dt[get(col) == val]
      }
      ag <- dt[, .(p_pobre=mean(p_pobre, na.rm=TRUE)), by=c(attr, 'periodo')]
      setnames(ag, attr, 'grupo')
      ag[, periodo := factor(periodo, levels=.periodos)]
      ag[, grupo   := factor(grupo)]
      periodos <- levels(ag$periodo); grupos <- levels(ag$grupo)
      mat <- dcast(ag, grupo ~ periodo, value.var='p_pobre')
      z   <- as.matrix(mat[, -1, with=FALSE]); rownames(z) <- mat$grupo
      z_vec <- as.numeric(as.vector(t(z)))
      cores_texto <- ifelse(z_vec > 0.4, 'white', '#333333')
      
      annots_cells <- lapply(seq_along(z_vec), \(i) list(
        x         = rep(periodos, times=length(grupos))[i],
        y         = rep(grupos,   each=length(periodos))[i],
        text      = .fmt_pct(z_vec[i]),
        showarrow = FALSE,
        font      = list(size=12, color=ifelse(z_vec[i] > 0.4, 'white', '#333333'))
      ))
      
      plot_ly(x=periodos, y=grupos, z=z, type='heatmap',
              zmin=0, zmax=1,
              colorscale=list(
                list(0.000, '#1a7c3e'),
                list(0.167, '#74c476'),
                list(0.333, '#fed976'),
                list(0.500, '#e31a1c'),
                list(0.667, '#800026'),
                list(1.000, '#49006a')
              ),
              colorbar=list(title='P(pobre)', tickformat='.0%'),
              text=matrix(sprintf('%s\n%s\nP(pobre): %s',
                                  rep(grupos,each=length(periodos)),
                                  rep(periodos,times=length(grupos)),
                                  .fmt_pct(as.vector(t(z)))),
                          nrow=length(grupos), ncol=length(periodos), byrow=TRUE),
              hoverinfo='text') |>
        layout(xaxis=list(title=NULL, tickangle=-30),
               yaxis=list(title=NULL),
               margin=list(l=160, b=80, t=20),
               annotations=annots_cells)
    })
    
    
    ## P4 - Grupos ----
    
    cl_resultado <- eventReactive(input$cl_rodar, {
      req(length(input$cl_periodo) > 0 && !all(input$cl_periodo == ''))
      dt <- data.table::copy(cluster_input)
      
      # Filtros opcionais — só aplica se o usuário selecionou algo
      if (length(input$cl_periodo) > 0 && any(input$cl_periodo != ''))
        dt <- dt[periodo %in% input$cl_periodo]
      if (length(input$cl_arranjo) > 0 && any(input$cl_arranjo != ''))
        dt <- dt[arranjo_dec %in% input$cl_arranjo]
      if (length(input$cl_setor)  > 0 && any(input$cl_setor  != ''))
        dt <- dt[setor_dec %in% input$cl_setor]
      if (length(input$cl_area)   > 0 && any(input$cl_area   != ''))
        dt <- dt[area_dec %in% input$cl_area]
      
      validate(need(nrow(dt) >= input$cl_k,
                    paste0('Filtro resultou em apenas ', nrow(dt),
                           ' células — aumente o número mínimo de linhas ou reduza k.')))
      
      cluster_mpi(dt = dt, k = input$cl_k, reduce = FALSE)
    })
    
    output$cl_dendro      <- renderPlot({ req(cl_resultado()); cl_resultado()$p_dendro      })
    output$cl_sil_obs     <- renderPlot({ req(cl_resultado()); cl_resultado()$p_sil_obs     })
    output$cl_sil_cluster <- renderPlot({ req(cl_resultado()); cl_resultado()$p_sil_cluster })
    output$cl_perfil      <- renderPlot({ req(cl_resultado()); cl_resultado()$p_perfil      })
    output$cl_score       <- renderPlot({ req(cl_resultado()); cl_resultado()$p_score       })
    output$cl_comp        <- renderPlot({ req(cl_resultado()); cl_resultado()$p_comp        })
    
    output$cl_vb_sil <- renderbs4ValueBox({
      r <- cl_resultado()
      bs4ValueBox(.vb(.fmt_n(r$sil_global, 3)),
                  'Silhueta global',
                  icon=icon('chart-bar'), color='primary', width=12)
    })
    
    output$cl_vb_neg <- renderbs4ValueBox({
      r <- cl_resultado()
      bs4ValueBox(
        .vb(r$sil_neg_n),
        HTML(paste0('Casos c/ silhueta negativa<br>',
                    '<span style="font-size:0.82rem;color:#333">',
                    .fmt_pct(r$sil_neg_pop), ' da população</span>')),
        icon  = icon('triangle-exclamation'),
        color = 'orange',
        width = 12)
    })
    
    output$cl_vb_max <- renderbs4ValueBox({
      r <- cl_resultado()
      bs4ValueBox(.vb(.fmt_n(r$score_max_cl, 3)),
                  'Score médio: grupo mais privado',
                  icon=icon('arrow-trend-up'), color='danger', width=12)
    })
    
    output$cl_vb_min <- renderbs4ValueBox({
      r <- cl_resultado()
      bs4ValueBox(.vb(.fmt_n(r$score_min_cl, 3)),
                  'Score médio: grupo menos privado',
                  icon=icon('arrow-trend-down'), color='success', width=12)
    })
    
    output$cl_vb_within <- renderbs4ValueBox({
      r <- cl_resultado()
      bs4ValueBox(.vb(.fmt_n(r$within_ratio, 3)),
                  'Razão within/total',
                  icon=icon('compress'), color='info', width=12)
    })
    
    output$cl_vb_cut <- renderbs4ValueBox({
      r <- cl_resultado()
      bs4ValueBox(.vb(.fmt_n(r$cut_height, 3)),
                  'Altura do corte (dendrograma)',
                  icon=icon('scissors'), color='secondary', width=12)
    })
    
  }
  
  # 5. Lançamento ====
  
  app <- shiny::shinyApp(ui=ui, server=server)
  
  if (launch)
    shiny::runApp(app, port=port)
  else
    invisible(app)   # retorna o objeto para quem quiser fazer deploy
  
}
