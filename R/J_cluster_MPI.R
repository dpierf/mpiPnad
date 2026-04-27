#' Realiza a clusterização hierárquica (Ward) sobre os 5 scores dimensionais do MPI-LA
#' @param dt  Tabela agregada produzida pelo pipeline prep_cluster, já filtrada pelo usuário
#' @param k   Número de clusters (inteiro, obrigatório)
#' @return Lista de objetos relacionados à clusterização
#' @export

cluster_mpi <- function(dt, k, reduce = TRUE) {
  
  stopifnot(is.numeric(k), length(k) == 1L, k >= 2L)
  
  dims   <- c('moradia', 'servicos', 'padrao', 'educacao', 'protecao')
  dt     <- data.table::as.data.table(dt)
  dt     <- dt[complete.cases(dt[, ..dims])]
  
  if (reduce) {
    dt <- dt[!(arranjo_full %in% c('Unipessoal: Homem', 'Unipessoal: Mulher'))][
      , tamanho := data.table::fcase(
        pessoas_dom <= 3L, '2-3',
        pessoas_dom <= 5L, '4-5',
        default           = '6+'
      )][, .(
        moradia  = weighted.mean((D1 + D2 + D3) / 3,          peso, na.rm = TRUE),
        servicos = weighted.mean((B1 + B2 + B3 + B4) / 4,     peso, na.rm = TRUE),
        padrao   = weighted.mean(V1 * (2/3) + V2 * (1/3),     peso, na.rm = TRUE),
        educacao = weighted.mean((E1 + E2 + E3) / 3,          peso, na.rm = TRUE),
        protecao = weighted.mean(P1 * (2/3) + P2 * (1/3),     peso, na.rm = TRUE),
        pop_tot  = sum(peso, na.rm = TRUE)
      ), by = .(periodo, uf, regiao, setor_dec, area_dec, arranjo_dec, tamanho)
      ][, score := moradia * (6/27) + servicos * (4/18) + padrao * (6/27) + educacao * (6/27) + protecao * (3/27)
      ][, periodo := stringr::str_replace_all(periodo, '\u2013', '-')]
  }
  
  if (nrow(dt) < k) stop('Número de observações (', nrow(dt), ') menor que k (', k, ').')
  
  mat   <- as.matrix(dt[, ..dims])
  pesos <- dt$pop_tot
  
  # Distância euclidiana ponderada e clustering Ward ----
  mat_w  <- mat * sqrt(pesos / sum(pesos))
  dist_w <- dist(mat_w, method = 'euclidean')
  hc     <- hclust(dist_w, method = 'ward.D2')
  dt[, cluster := cutree(hc, k = k)]
  
  # Silhueta ----
  sil        <- cluster::silhouette(dt$cluster, dist_w)
  sil_global <- mean(sil[, 'sil_width'])
  
  sil_dt <- data.table::as.data.table(sil[, 1:3])[
    , .(sil_media = mean(sil_width),
        sil_min   = min(sil_width),
        sil_max   = max(sil_width)),
    by = cluster
  ][order(cluster)]
  
  # Perfil dimensional por cluster ----
  labels_dim <- c(
    moradia  = 'Moradia',
    servicos = 'Serviços',
    padrao   = 'Padrão de Vida',
    educacao = 'Educação',
    protecao = 'Proteção Social'
  )
  
  perfil_dt <- dt[,
                  lapply(.SD, weighted.mean, w = pop_tot),
                  by      = cluster,
                  .SDcols = dims
  ][order(cluster)]
  
  perfil_long <- data.table::melt(
    perfil_dt,
    id.vars       = 'cluster',
    variable.name = 'dimensao',
    value.name    = 'score_medio'
  )[, dimensao := factor(labels_dim[dimensao], levels = rev(unname(labels_dim)))]
  
  # Tema interno padronizado ----
  .tema_cl <- ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      legend.position  = 'bottom',
      legend.text      = ggplot2::element_text(size = 10),
      legend.title     = ggplot2::element_text(size = 10),
      axis.title       = ggplot2::element_text(size = 10),
      axis.text        = ggplot2::element_text(size = 9),
      plot.title       = ggplot2::element_blank(),
      plot.subtitle    = ggplot2::element_blank()
    )
  
  # Gráficos ----
  
  # Dendrograma
  dend_data <- ggdendro::dendro_data(hc, type = 'rectangle')
  
  # Altura de corte entre o k-ésimo e (k-1)-ésimo merge
  cut_height <- mean(c(
    rev(hc$height)[k - 1L],
    rev(hc$height)[k]
  ))
  
  # Paleta de cores por cluster — mapeia cada folha ao seu cluster
  n_obs      <- nrow(dt)
  leaf_order <- hc$order
  leaf_clust <- cutree(hc, k = k)[leaf_order]
  
  pal <- scales::hue_pal()(k)
  seg <- ggdendro::segment(dend_data)
  
  p_dendro <- ggplot2::ggplot(seg) +
    ggplot2::geom_segment(
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      linewidth = 0.3, colour = 'grey40'
    ) +
    ggplot2::geom_hline(
      yintercept = cut_height,
      linetype   = 'dashed',
      colour     = 'grey30',
      linewidth  = 0.4
    ) +
    ggplot2::annotate(
      'text',
      x      = n_obs * 0.02,
      y      = cut_height,
      label  = paste0('k = ', k),
      hjust  = 0,
      vjust  = -0.4,
      size   = 3.5,
      colour = 'grey30'
    ) +
    ggplot2::labs(x = NULL, y = 'Altura de fusão') +
    .tema_cl +
    ggplot2::theme(
      axis.text.x  = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      panel.grid   = ggplot2::element_blank()
    )
  
  # Silhueta por observação ----
  p_sil_obs <- ggplot2::ggplot(
    data.table::as.data.table(sil[, 1:3])[order(cluster, sil_width)][, obs := .I],
    ggplot2::aes(x = obs, y = sil_width, fill = factor(cluster))
  ) +
    ggplot2::geom_col(width = 1) +
    ggplot2::geom_hline(yintercept = sil_global, linetype = 'dashed', colour = 'grey40') +
    ggplot2::annotate('text', x = Inf, y = sil_global, hjust = 1.1, vjust = -0.5,
                      label = sprintf('Global: %.3f', sil_global), size = 3, colour = 'grey30') +
    ggplot2::scale_fill_discrete(name = 'Cluster') +
    ggplot2::labs(x = NULL, y = 'Silhueta') +
    .tema_cl +
    ggplot2::theme(axis.text.x  = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank())
  
  # Silhueta por cluster (barras com min-max) ----
  p_sil_cluster <- ggplot2::ggplot(
    sil_dt,
    ggplot2::aes(x = factor(cluster), y = sil_media, fill = sil_media)
  ) +
    ggplot2::geom_col(width = 0.6) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = sil_min, ymax = sil_max),
      width = 0.2, colour = 'grey40'
    ) +
    ggplot2::geom_hline(yintercept = sil_global, linetype = 'dashed', colour = 'grey40') +
    ggplot2::annotate('text', x = Inf, y = sil_global, hjust = 1.1, vjust = -0.5,
                      label = sprintf('Global: %.3f', sil_global), size = 3, colour = 'grey30') +
    ggplot2::scale_fill_gradient(low = '#d7191c', high = '#2c7bb6', limits = c(-1, 1)) +
    ggplot2::scale_y_continuous(limits = c(-1, 1)) +
    ggplot2::labs(x = 'Cluster', y = 'Silhueta média') +
    .tema_cl +
    ggplot2::theme(legend.position = 'none')
  
  # Perfil dimensional (heatmap) ----
  p_perfil <- ggplot2::ggplot(
    perfil_long,
    ggplot2::aes(x = factor(cluster), y = dimensao, fill = score_medio)
  ) +
    ggplot2::geom_tile(colour = 'white', linewidth = 0.4) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf('%.3f', score_medio)), size = 3) +
    ggplot2::scale_fill_distiller(palette = 'YlOrRd', direction = 1,
                                  limits = c(0, 1), name = 'Score\ndimensional') +
    ggplot2::labs(x = 'Cluster', y = NULL) +
    .tema_cl +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
  
  # Distribuição do score total por cluster ----
  p_score <- ggplot2::ggplot(
    dt,
    ggplot2::aes(x = score, colour = factor(cluster), weight = pop_tot)
  ) +
    ggplot2::geom_density(linewidth = 0.8, adjust = 1.2) +
    ggplot2::labs(x = 'Score de privação', y = 'Densidade', colour = 'Cluster') +
    .tema_cl
  
  # Composição categórica ----
  cats_map <- c(
    periodo     = 'Período',
    regiao      = 'Região',
    setor_dec   = 'Setor',
    area_dec    = 'Área',
    arranjo_dec = 'Arranjo Domiciliar',
    tamanho     = 'Tamanho do Domicílio'
  )
  cats <- names(cats_map)[sapply(names(cats_map), \(col)
                                 col %in% names(dt) && data.table::uniqueN(dt[[col]]) > 1L
  )]
  
  comp_list <- lapply(cats, \(cat) {
    dt[, .(n = .N), by = c('cluster', cat)][
      , prop    := n / sum(n), by = cluster
    ][, variavel := cats_map[cat]
    ][, data.table::setnames(.SD, cat, 'categoria')]
  }) |> data.table::rbindlist()
  
  comp_list[, variavel := factor(variavel, levels = unname(cats_map[cats]))]
  
  p_comp <- ggplot2::ggplot(
    comp_list,
    ggplot2::aes(x = factor(cluster), y = categoria, fill = prop)
  ) +
    ggplot2::geom_tile(colour = 'white', linewidth = 0.4) +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(prop, accuracy = 1)), size = 3) +
    ggplot2::scale_fill_gradient(low = 'white', high = '#2c7bb6', name = 'Proporção') +
    ggplot2::facet_wrap(~ variavel, scales = 'free_y', ncol = 1) +
    ggplot2::labs(x = 'Cluster', y = NULL) +
    .tema_cl +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
  
  # Métricas para big numbers ----
  sil_neg_n   <- sum(sil[, 'sil_width'] < 0)
  sil_neg_pop <- weighted.mean(sil[, 'sil_width'] < 0, w = pesos)
  
  score_por_cl <- dt[, .(score_cl = weighted.mean(score, w = pop_tot)), by = cluster]
  score_max_cl <- score_por_cl[which.max(score_cl), score_cl]
  score_min_cl <- score_por_cl[which.min(score_cl), score_cl]
  
  # Razão within/total (soma de quadrados) ----
  labels    <- dt$cluster
  dist_sq   <- as.matrix(dist_w)^2
  total_ss  <- sum(dist_sq) / (2 * nrow(dt))
  within_ss <- sum(sapply(seq_len(k), \(cl) {
    idx <- which(as.integer(labels) == cl)
    if (length(idx) < 2) return(0)
    sum(dist_sq[idx, idx]) / (2 * length(idx))
  }))
  within_ratio <- within_ss / total_ss
  
  # Output ----
  list(
    hc            = hc,
    k             = k,
    res_dt        = dt,
    perfil_dt     = perfil_dt,
    sil_dt        = sil_dt,
    sil_global    = sil_global,
    sil_neg_n     = sil_neg_n,
    sil_neg_pop   = sil_neg_pop,
    score_max_cl  = score_max_cl,
    score_min_cl  = score_min_cl,
    within_ratio  = within_ratio,
    cut_height    = cut_height,
    p_dendro      = p_dendro,
    p_perfil      = p_perfil,
    p_sil_obs     = p_sil_obs,
    p_sil_cluster = p_sil_cluster,
    p_score       = p_score,
    comp_list     = comp_list,
    p_comp        = p_comp
  )
}
