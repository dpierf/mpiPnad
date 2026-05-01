#' Estima modelos econométricos do MPI e salva resultados
#' @param dt         Base processada retornada por `creates_mpi` (mpi_pnad)
#' @param modelos    Nome(s) do(s) modelo(s) a ser avaliado ('zoib','quant','logit')
#' @param output_dir Pasta de saída (default: 'output/models')
#' @param n_sample   Proporção para amostragem estratificada (NULL = base completa)
#' @param cutoff_k   Limiar de pobreza multidimensional (default: 0.33)
#' @param n_cyc      Número máximo de ciclos do GAMLSS (default: 50)
#' @param c_crit     Critério de convergência do GAMLSS (default: 0.01)
#' @param seed       Semente para reprodutibilidade
#' @export

models_mpi <- function(dt,
                       modelos    = c('zoib','quant','logit'),
                       output_dir = 'output/models',
                       n_sample   = NULL,
                       rds        = FALSE,
                       cutoff_k   = 0.33,
                       seed       = 186) {
  
  # 0. Elementos Preparatórios ====
  
  # Dicionários e checagens
  dicts <- readRDS('data/mpi_dictionary.rds')
  .dec <- function(x, dict) unname(dict[as.character(x)])
  
  modelos <- match.arg(modelos, c('zoib','quant','logit'), several.ok = TRUE)
  fs::dir_create(output_dir)
  .sufixo <- if (is.null(n_sample)) '_global' else '_amostra'
  
  .ref_periodo   <- '1981–1993'
  .ref_regiao    <- 'Sudeste'
  .ref_setor     <- 'Urbano'
  .ref_area      <- 'Resto da UF'
  .ref_sexo      <- 'Homem'
  .ref_raca      <- 'Branco'
  .ref_arranjo   <- 'Casal Com'
  .ref_full      <- 'Casal Com: Homem'
  
  # 1. Preparação dos dados ====
  cli::cli_h1('Preparando dados')
  
  dt <- dt[
    !is.na(score) & !is.na(arranjo_full) & !is.na(uf) & !is.na(agregados) &
      !is.na(setor_dec) & !is.na(area_dec) & !is.na(regiao) & !is.na(periodo)
  ]
  
  # Decodificação
  dt[, `:=` (
    periodo      = .dec(periodo,                   dicts$periodo),
    regiao       = .dec(regiao,                    dicts$regiao),
    setor_dec    = .dec(setor_dec,                 dicts$setor_dec),
    area_dec     = .dec(area_dec,                  dicts$area_dec),
    sexo_dec     = .dec(sexo_dec,                  dicts$sexo_dec),
    raca         = .dec(raca,                      dicts$raca),
    arranjo_dec  = .dec(substr(arranjo_full,1,1),  dicts$arranjo_dec),
    arranjo_full = .dec(arranjo_full,              dicts$arranjo_full)
  )]
  
  # Checagem dos primeiros atributos
  .check_ref <- function(ref, col) {
    if (!ref %in% unique(dt[[col]]))
      cli::cli_abort(c(
        'Referência inválida em {.field {col}}: {.val {ref}}',
        'i' = 'Valores disponíveis: {.val {sort(unique(dt[[col]]))}}'
      ))
  }
  
  .check_ref(.ref_periodo, 'periodo')
  .check_ref(.ref_regiao,  'regiao')
  .check_ref(.ref_setor,   'setor_dec')
  .check_ref(.ref_area,    'area_dec')
  .check_ref(.ref_sexo,    'sexo_dec')
  .check_ref(.ref_raca,    'raca')
  .check_ref(.ref_arranjo, 'arranjo_dec')
  
  # Criação de atributos intermediários
  n_total <- nrow(dt)
  dt[, `:=` (
    score_zoib = fcase(
      score == 1, (1 * (n_total - 1) + 0.5) / n_total,
      default    = score),
    idade_real      = ifelse(idade %between% c(850,998), ano - (idade + 1000), idade),
    periodo         = relevel(factor(periodo),     ref = .ref_periodo),
    sexo            = relevel(factor(sexo_dec),    ref = .ref_sexo),
    raca            = relevel(factor(raca),        ref = .ref_raca),
    arranjo_simples = relevel(factor(arranjo_dec), ref = .ref_arranjo),
    regiao          = relevel(factor(regiao),      ref = .ref_regiao),
    setor           = relevel(factor(setor_dec),   ref = .ref_setor),
    area            = relevel(factor(area_dec),    ref = .ref_area),
    peso_norm       = peso / mean(peso)
  )]
  
  dt[, `:=` (
    pobre = as.integer(score >= cutoff_k),
    
    nivel = factor(fcase(
      escol <= 0,  '0 anos',
      escol <= 4,  '1 a 4 anos',
      escol <= 7,  '5 a 7 anos',
      escol <= 10, '8 a 10 anos',
      escol <= 14, '11 a 14 anos',
      default =    '15+ anos'
    ), levels = c('0 anos','1 a 4 anos','5 a 7 anos','8 a 10 anos','11 a 14 anos','15+ anos'), ordered = TRUE),
    
    faixa = factor(fcase(
      idade_real %between% c(15,29), '15-29',
      idade_real %between% c(30,49), '30-49',
      idade_real %between% c(50,64), '50-64',
      idade_real %between% c(65,120), '65+',
      default = 'NA'
    ), levels = c('15-29','30-49','50-64','65+','NA'), ordered = TRUE),
    
    tamanho = factor(fcase(
      pessoas_fam %between% c(2,3),  'Pequena',
      pessoas_fam %between% c(4,5),  'Média',
      pessoas_fam %between% c(6,98), 'Grande'
    ), levels = c('Pequena','Média','Grande'), ordered = TRUE)
  )]
  
  # Adicionando referencias para novos atributos
  .ref_faixa     <- '30-49'
  .ref_nivel     <- '5 a 7 anos'
  .ref_tamanho   <- 'Pequena'   
  .ref_agregados <- 'N'
  
  gc(verbose = FALSE, full = TRUE)
  
  # Filtragem da base
  cols_mod <- c('score_zoib','score','pobre','periodo',
                'peso_norm','peso','psu','strata',
                'regiao','uf','setor','area',
                'raca','faixa','nivel',
                'arranjo_full','tamanho','agregados')
  
  reg_dt <- dt[arranjo_simples != 'Unipessoal' & faixa != 'NA', ..cols_mod] |>
    na.omit() |> droplevels()
  
  cols_fator <- c('periodo','regiao','uf','setor','area','raca','faixa',
                  'nivel','arranjo_full','tamanho','agregados')
  reg_dt[, (cols_fator) := lapply(.SD, \(x) factor(x, ordered = FALSE)), .SDcols = cols_fator]
  
  # Relevel após nova fatoração
  reg_dt[, `:=` (
    periodo      = relevel(periodo,      ref = .ref_periodo),
    regiao       = relevel(regiao,       ref = .ref_regiao),
    setor        = relevel(setor,        ref = .ref_setor),
    area         = relevel(area,         ref = .ref_area),
    raca         = relevel(raca,         ref = .ref_raca),
    arranjo_full = relevel(arranjo_full, ref = .ref_full),
    faixa        = relevel(faixa,        ref = .ref_faixa),
    nivel        = relevel(nivel,        ref = .ref_nivel),
    tamanho      = relevel(tamanho,      ref = .ref_tamanho),
    agregados    = relevel(agregados,    ref = .ref_agregados)
  )]
  
  # Criando matriz de saída
  nd <- as.data.frame(data.table::CJ(
    periodo         = levels(reg_dt$periodo),
    regiao          = levels(reg_dt$regiao),
    setor           = levels(reg_dt$setor),
    area            = levels(reg_dt$area),
    raca            = levels(reg_dt$raca),
    faixa           = levels(reg_dt$faixa),
    nivel           = levels(reg_dt$nivel),
    arranjo_full    = levels(reg_dt$arranjo_full),
    tamanho         = levels(reg_dt$tamanho),
    agregados       = levels(reg_dt$agregados)
  ))
  
  
  # 2. Amostragem estratificada ====
  
  if (!is.null(n_sample)) {
    set.seed(seed)
    cli::cli_alert_info(
      'Amostrando {scales::percent(n_sample)} estratificado')
    reg_dt <- reg_dt[
      , .SD[sample(.N, max(1L, round(.N * n_sample)),
                   prob = peso_norm / sum(peso_norm))],
      by = .(periodo, regiao, setor, area, arranjo_full)
    ]
    cli::cli_alert_success('{scales::comma(nrow(reg_dt))} observacoes selecionadas')
    gc(verbose = FALSE, full = TRUE)
  }
  
  cli::cli_alert_info('{scales::comma(nrow(reg_dt))} observacoes no modelo')
  
  
  # 3. Modelo ZOIB ====
  
  out_zoib <- list()
  if ('zoib' %in% modelos) {
    cli::cli_h1('Beta-Regressão Inflada de Zeros (ZOIB)')
    
    mod_zoib <- gamlss::gamlss(
      formula       = score_zoib ~ periodo + regiao + setor + area + raca + faixa + nivel + arranjo_full + tamanho + agregados,
      sigma.formula = ~ 1,
      nu.formula    = ~ periodo + regiao + setor + area + raca + faixa + nivel + arranjo_full + tamanho + agregados,
      family        = gamlss.dist::BEZI,
      data          = reg_dt,
      weights       = reg_dt$peso_norm,
      control       = gamlss::gamlss.control(n.cyc=20, c.crit=0.001, trace=FALSE)
    )
    
    cli::cli_alert_success('Modelo convergiu em {mod_zoib$iter} ciclos')
    gc(verbose = FALSE, full = TRUE)
    
    # coeficientes extraídos
    vcov_mat <- mod_zoib$vcov
    se_zoib  <- if (!is.null(vcov_mat)) {
      se <- sqrt(diag(vcov_mat))
      if (is.null(names(se))) {
        names(se) <- c(names(mod_zoib$mu.coefficients),
                       names(mod_zoib$nu.coefficients))
      }
      se
    } else {
      rep(NA_real_, length(c(mod_zoib$mu.coefficients,
                             mod_zoib$nu.coefficients)))
    }
    
    coef_zoib <- rbindlist(list(
      data.table(componente = 'mu',
                 termo      = names(mod_zoib$mu.coefficients),
                 coef       = mod_zoib$mu.coefficients,
                 se         = se_zoib[names(mod_zoib$mu.coefficients)]
      ),
      data.table(componente = 'nu',
                 termo      = names(mod_zoib$nu.coefficients),
                 coef       = mod_zoib$nu.coefficients,
                 se         = se_zoib[names(mod_zoib$nu.coefficients)]
      )
    ))
    
    # grade de preditos
    nd_zoib <- copy(nd)
    
    nd_zoib$peso_norm  <- 1
    nd_zoib$score_zoib <- mean(reg_dt$score_zoib, na.rm=TRUE)
    
    preds <- gamlss::predictAll(mod_zoib, newdata=nd_zoib, data=reg_dt, type='response')
    
    grade_zoib <- as.data.table(nd_zoib[, c('periodo','regiao','setor','area','raca','faixa','nivel',
                                            'arranjo_full','tamanho','agregados')])
    grade_zoib[, mu_pred    := preds$mu]
    grade_zoib[, nu_pred    := preds$nu]
    grade_zoib[, score_pred := (1 - nu_pred) * mu_pred]
    
    # salvar
    if(rds) saveRDS(mod_zoib,          fs::path(output_dir, paste0('zoib_model', .sufixo, '.rds')))
    arrow::write_parquet(coef_zoib,    fs::path(output_dir, paste0('zoib_coefs', .sufixo, '.parquet')))
    arrow::write_parquet(grade_zoib,   fs::path(output_dir, paste0('zoib_grade', .sufixo, '.parquet')))
    cli::cli_alert_success('Resultados salvos em {output_dir}')
    
    out_zoib <- list(mod_zoib=mod_zoib, coef_zoib=coef_zoib, grade_zoib=grade_zoib)
    
    rm(mod_zoib, coef_zoib, grade_zoib, preds, nd_zoib)
    gc(verbose = FALSE, full = TRUE)    
  }
  
  
  # 4. Regressão Quantílica ====
  
  out_qr <- list()
  if ('quant' %in% modelos) {
    cli::cli_h1('Regressão Quantílica')
    
    taus <- c(0.10, 0.25, 0.33, 0.50, 0.67, 0.75, 0.90)
    
    mod_qr <- quantreg::rq(
      score ~ periodo + regiao + setor + area + raca + faixa + nivel + arranjo_full + tamanho + agregados,
      tau     = taus,
      data    = reg_dt,
      weights = peso,
      method  = 'pfn'
    )
    
    cli::cli_alert_success('Modelo estimado para {length(taus)} quantis')
    gc(verbose = FALSE, full = TRUE)
    
    # coeficientes extraídos — formato longo
    tau_cols <- colnames(coef(mod_qr))   # nomes reais das colunas (ex: "0.1", "0.25")
    tau_ids  <- paste0('tau_', gsub('\\.', '', sprintf('%.2f', taus)))  # ex: "tau_010"
    
    coef_qr <- as.data.table(coef(mod_qr), keep.rownames = 'termo')
    setnames(coef_qr, tau_cols, tau_ids)
    coef_qr <- melt(coef_qr, id.vars = 'termo',
                    variable.name = 'tau', value.name = 'coef',
                    variable.factor = FALSE)
    
    # grade de preditos
    nd_quant <- copy(nd)
    
    preds_qr <- predict(mod_qr, newdata = nd_quant)   # matriz: linhas x taus
    grade_qr <- as.data.table(nd_quant)
    
    for (i in seq_along(taus)) {
      col <- paste0('tau_', gsub('\\.', '', sprintf('%.2f', taus[i])))
      grade_qr[, (col) := preds_qr[, i]]
    }
    
    # salvar
    if(rds) saveRDS(mod_qr,         fs::path(output_dir, paste0('quantilica_model', .sufixo, '.rds')))
    arrow::write_parquet(coef_qr,   fs::path(output_dir, paste0('quantilica_coefs', .sufixo, '.parquet')))
    arrow::write_parquet(grade_qr,  fs::path(output_dir, paste0('quantilica_grade', .sufixo, '.parquet')))
    cli::cli_alert_success('Resultados salvos em {output_dir}')
    
    out_qr <- list(mod_qr=mod_qr, coef_qr=coef_qr, grade_qr=grade_qr)
    rm(mod_qr, coef_qr, grade_qr, preds_qr, nd_quant)
    gc(verbose = FALSE, full = TRUE)    
  }
  
  
  # 5. Logit Binário ====
  
  out_logit <- list()
  if ('logit' %in% modelos) {
    cli::cli_h1('Logit Binário')
    
    suppressWarnings(
      mod_logit <- stats::glm(
        pobre ~ periodo + regiao + setor + area + raca + faixa + nivel + arranjo_full + tamanho + agregados,
        family  = binomial(link = 'logit'),
        data    = reg_dt,
        weights = peso_norm
      )
    )
    
    cli::cli_alert_success('Modelo estimado para k = {cutoff_k}')
    gc(verbose = FALSE, full = TRUE)
    
    # var-covar e SE
    p_hat   <- predict(mod_logit, type = 'response')
    escala  <- mean(p_hat * (1 - p_hat), na.rm = TRUE)
    
    vcov_cl <- sandwich::vcovCL(mod_logit, cluster = ~psu)
    se_cl  <- sqrt(diag(vcov_cl))
    
    # estimando AME
    ame_dt <- data.table(
      termo   = names(coef(mod_logit)),
      AME     = coef(mod_logit) * escala,
      se      = se_cl * escala
    )[, `:=` (
      z     = AME / se,
      p     = 2 * pnorm(-abs(AME / se)),
      IC_lo = AME - 1.96 * se,
      IC_hi = AME + 1.96 * se
    )]
    
    # matriz de coeficientes
    coef_logit <- data.table(
      termo = names(coef(mod_logit)),
      coef  = coef(mod_logit),
      se    = se_cl,                          
      z     = coef(mod_logit) / se_cl,
      p     = 2 * pnorm(-abs(coef(mod_logit) / se_cl)),
      OR    = exp(coef(mod_logit)),
      OR_lo = exp(coef(mod_logit) - 1.96 * se_cl),
      OR_hi = exp(coef(mod_logit) + 1.96 * se_cl)
    )
    
    # grade de preditos
    nd_logit <- copy(nd)
    
    nd_logit$peso_norm <- 1
    nd_logit$pobre     <- 0L
    
    nd_logit$p_pobre <- predict(mod_logit, newdata = nd_logit, type = 'response')
    grade_logit      <- as.data.table(nd_logit[, c('p_pobre','periodo','regiao','setor','area',
                                                   'raca','faixa','nivel','arranjo_full','tamanho','agregados')])
    
    # salvar
    if(rds) saveRDS(mod_logit,          fs::path(output_dir, paste0('logit_model',   .sufixo, '.rds')))
    arrow::write_parquet(coef_logit,    fs::path(output_dir, paste0('logit_coefs',   .sufixo, '.parquet')))
    arrow::write_parquet(ame_dt,        fs::path(output_dir, paste0('logit_effects', .sufixo, '.parquet')))
    arrow::write_parquet(grade_logit,   fs::path(output_dir, paste0('logit_grade',   .sufixo, '.parquet')))
    
    cli::cli_alert_success('Resultados salvos em {output_dir}')
    
    out_logit <- list(mod_logit=mod_logit, coef_logit=coef_logit,
                      ame_dt=ame_dt, grade_logit=grade_logit)
    rm(mod_logit, grade_logit, ame_dt, coef_logit)
    gc(verbose = FALSE, full = TRUE)
  }
  
  
  # 6. Outputs ====
  
  rm(dt, reg_dt, nd)
  gc(verbose = FALSE, full = TRUE)
  invisible(c(out_zoib, out_qr, out_logit))
}
