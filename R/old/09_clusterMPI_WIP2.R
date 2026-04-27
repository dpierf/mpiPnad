#' Realiza a clusterização, via GMM, dos dados reduzidos do MPI
#' @param pca_obj  Objeto retornado pela função `factors_mpi`
#' @param cols_mpi Vetor de nomes dos indicadores MPI ativos
#' @param G        Número de clusters (NULL = seleção automática via BIC)
#' @param dicts    Lista de dicionários retornada por `readRDS('mpi_dictionary.rds')`
#'                 (opcional; se NULL usa labels internos)
#' @return Lista de objetos relacionados à clusterização
#' @export

cluster_mpi <- function(pca_obj, cols_mpi, G = NULL, dicts = NULL) {
  
  # Preparação dos dados ----
  mat_rot  <- as.matrix(pca_obj$coords_rot)   # [FIX] era coords_pca
  pesos    <- pca_obj$df_pca$pesos
  n_comp   <- pca_obj$n_comp
  
  # Implementação do GMM ----  
  if (is.null(G)) {
    mod_gmm <- mclust::Mclust(mat_rot, G = 2:10, verbose = FALSE)
    message(
      'k ótimo por BIC: ', mod_gmm$G,
      ' | modelo: ',       mod_gmm$modelName,
      ' | BIC: ',          round(mod_gmm$bic, 1),
      '\n[aviso] mclust não suporta pesos de caso — clustering efetivamente não-ponderado'
    )
  } else {
    mod_gmm <- mclust::Mclust(mat_rot, G = G, verbose = FALSE)
    message('modelo: ', mod_gmm$modelName, ' | BIC: ', round(mod_gmm$bic, 1))
  }
  
  k_gmm <- mod_gmm$G
  
  # Decodificadores internos ----
  # Independentes de dicts; dicts sobrescreve se fornecido
  .tipo_arr <- c(
    `11` = 'Casal com Filhos', `12` = 'Casal com Filhos', `21` = 'Casal sem Filhos', `22` = 'Casal sem Filhos',
    `31` = 'Unipessoal',       `32` = 'Unipessoal',       `41` = 'Monoparental',     `42` = 'Monoparental'
  )
  
  .sexo_arr <- c(
    `11` = 'Masculino', `12` = 'Feminino', `21` = 'Masculino', `22` = 'Feminino',
    `31` = 'Masculino', `32` = 'Feminino', `41` = 'Masculino', `42` = 'Feminino'
  )
  
  .macro_uf <- c(
    AC='Norte',  AM='Norte',  AP='Norte',  PA='Norte',  RO='Norte',  RR='Norte',  TO='Norte',
    AL='Nordeste', BA='Nordeste', CE='Nordeste', MA='Nordeste', PB='Nordeste',
    PE='Nordeste', PI='Nordeste', RN='Nordeste', SE='Nordeste',
    ES='Sudeste', MG='Sudeste', RJ='Sudeste', SP='Sudeste',
    PR='Sul', RS='Sul', SC='Sul',
    DF='Centro-Oeste', GO='Centro-Oeste', MS='Centro-Oeste', MT='Centro-Oeste'
  )
  
  .setor_lbl <- if (!is.null(dicts$setor_dec)) dicts$setor_dec else
    c(`1` = 'Urbano', `2` = 'Rural')
  .area_lbl  <- if (!is.null(dicts$area_dec))  dicts$area_dec  else
    c(`1` = 'Reg. Metropolitana', `2` = 'Resto da UF')
  
  meta_cols <- intersect(
    c('ano', 'uf', 'setor_dec', 'area_dec', 'arranjo_full', 'tamanho', 'pesos', 'score'),
    names(pca_obj$df_pca)
  )
  
  # Montagem do resultado ----
  gmm_dt <- cbind(
    pca_obj$df_pca[, ..meta_cols],
    pca_obj$coords_rot,
    data.table::data.table(
      cluster     = mod_gmm$classification,
      uncertainty = round(mod_gmm$uncertainty, 3)
    )
  )
  
  gmm_dt[, `:=`(
    arranjo_tipo = .tipo_arr[as.character(arranjo_full)],
    sexo         = .sexo_arr[as.character(arranjo_full)],
    regiao       = .macro_uf[uf],
    setor_lbl    = .setor_lbl[as.character(setor_dec)],
    area_lbl     = .area_lbl[as.character(area_dec)]
  )]
  
  # Scatterplot 2D ----
  p_scatter <- ggplot2::ggplot(
    gmm_dt,
    ggplot2::aes(x = CP1, y = CP2, colour = factor(cluster), size = pesos)
  ) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::stat_ellipse(
      ggplot2::aes(group = factor(cluster)),
      level = 0.95, linewidth = 0.8, show.legend = FALSE
    ) +
    ggplot2::scale_size_continuous(range = c(1, 8), guide = 'none') +
    ggplot2::labs(
      x        = 'CP1',
      y        = 'CP2',
      colour   = 'Cluster',
      title    = paste0('Clusterização GMM — ', k_gmm, ' grupos'),
      subtitle = paste0(
        'Tamanho \u221d peso populacional | Elipses 95% | ',
        'Modelo: ', mod_gmm$modelName, ' (BIC = ', round(mod_gmm$bic, 1), ')'
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = 'bottom')
  
  # Perfil dos indicadores por cluster ----
  perfil_dt <- cbind(
    pca_obj$df_pca[, .(pesos)],
    pca_obj$df_pca[, ..cols_mpi],
    data.table::data.table(cluster = gmm_dt$cluster)
  )[, lapply(.SD, weighted.mean, w = pesos),
    by      = cluster,
    .SDcols = cols_mpi
  ][order(cluster)]
  
  p_perfil <- ggplot2::ggplot(
    data.table::melt(
      perfil_dt,
      id.vars       = 'cluster',
      variable.name = 'indicador',
      value.name    = 'taxa_privacao'
    ),
    ggplot2::aes(x = factor(cluster), y = indicador, fill = taxa_privacao)
  ) +
    ggplot2::geom_tile(colour = 'white', linewidth = 0.4) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf('%.2f', taxa_privacao)), size = 3
    ) +
    ggplot2::scale_fill_distiller(
      palette  = 'YlOrRd',
      direction = 1,
      limits   = c(0, 1),
      name     = 'Taxa de\nprivação'
    ) +
    ggplot2::scale_y_discrete(limits = rev(cols_mpi)) +
    ggplot2::labs(
      x        = 'Cluster', y = NULL,
      title    = 'Perfil de privação por cluster',
      subtitle = 'Média ponderada da taxa de privação por indicador (0 = sem privação; 1 = privado)'
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = 'right')
  
  # Silhueta ----
  sil        <- cluster::silhouette(mod_gmm$classification, dist(mat_rot))
  sil_global <- mean(sil[, 'sil_width'])
  
  sil_dt <- as.data.table(sil[, 1:3])[
    , .(sil_media = mean(sil_width),
        sil_min   = min(sil_width),
        sil_max   = max(sil_width)),
    by = cluster
  ][order(cluster)]
  
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
    ggplot2::annotate(
      'text', x = Inf, y = sil_global, hjust = 1.1, vjust = -0.5,
      label = sprintf('Global: %.3f', sil_global), size = 3, colour = 'grey30'
    ) +
    ggplot2::scale_fill_gradient(
      low = '#d7191c', high = '#2c7bb6', limits = c(-1, 1), name = 'Silhueta'
    ) +
    ggplot2::scale_y_continuous(limits = c(-1, 1)) +
    ggplot2::labs(
      x = 'Cluster', y = 'Silhueta média',
      title    = 'Qualidade dos clusters — Silhueta',
      subtitle = 'Barra: média | Intervalo: min–max | Tracejado: global'
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = 'right')
  
  p_sil_obs <- ggplot2::ggplot(
    as.data.table(sil[, 1:3])[order(cluster, sil_width)][, obs := seq_len(.N)],
    ggplot2::aes(x = obs, y = sil_width, fill = factor(cluster))
  ) +
    ggplot2::geom_col(width = 1) +
    ggplot2::geom_hline(yintercept = sil_global, linetype = 'dashed', colour = 'grey40') +
    ggplot2::scale_fill_discrete(name = 'Cluster') +
    ggplot2::labs(
      x = NULL, y = 'Silhueta',
      title    = 'Silhueta por observação',
      subtitle = sprintf('Silhueta global: %.3f', sil_global)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x     = ggplot2::element_blank(),
      axis.ticks.x    = ggplot2::element_blank(),
      legend.position = 'bottom'
    )
  
  # Distribuição do score MPI por cluster ----
  p_score <- if ('score' %in% names(gmm_dt)) {
    ggplot2::ggplot(
      gmm_dt,
      ggplot2::aes(x = score, colour = factor(cluster), weight = pesos)
    ) +
      ggplot2::geom_density(linewidth = 0.8, adjust = 1.2) +
      ggplot2::labs(
        x = 'Score de privação', y = 'Densidade', colour = 'Cluster',
        title    = 'Distribuição do score MPI por cluster',
        subtitle = 'Densidade ponderada pelo peso populacional'
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = 'bottom')
  } else {
    message('[aviso] coluna score ausente em df_pca — p_score omitido')
    NULL
  }
  
  # Composição categórica por cluster ----
  cats <- c('regiao', 'setor_lbl', 'area_lbl', 'sexo', 'arranjo_tipo')
  
  rotulos <- c(
    regiao       = 'Região de Residência',
    setor_lbl    = 'Setor da UF',
    area_lbl     = 'Área do Município',
    sexo         = 'Sexo do Responsável',
    arranjo_tipo = 'Arranjo Domiciliar'
  )
  
  comp_list <- lapply(cats, \(cat) {
    gmm_dt[, .(n = .N), by = c('cluster', cat)][
      , prop    := n / sum(n), by = cluster
    ][, variavel := cat
    ][, data.table::setnames(.SD, cat, 'categoria')]
  }) |> data.table::rbindlist()
  
  comp_list[, variavel := rotulos[variavel]]
  comp_list[, variavel := factor(variavel, levels = unname(rotulos))]
  
  p_comp <- ggplot2::ggplot(
    comp_list,
    ggplot2::aes(x = factor(cluster), y = categoria, fill = prop)
  ) +
    ggplot2::geom_tile(colour = 'white', linewidth = 0.4) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(prop, accuracy = 1)), size = 3
    ) +
    ggplot2::scale_fill_gradient(low = 'white', high = '#2c7bb6', name = 'Proporção') +
    ggplot2::facet_wrap(~ variavel, scales = 'free_y', ncol = 1) +
    ggplot2::labs(
      x = 'Cluster', y = NULL,
      title    = 'Composição categórica por cluster',
      subtitle = 'Proporção de casos dentro de cada cluster'
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = 'right')
  
  # Output ----
  list(
    mod_gmm       = mod_gmm,
    k_gmm         = k_gmm,
    gmm_dt        = gmm_dt,
    p_scatter     = p_scatter,
    perfil_dt     = perfil_dt,
    p_perfil      = p_perfil,
    p_score       = p_score,
    comp_list     = comp_list,
    p_comp        = p_comp,
    sil_dt        = sil_dt,
    sil_global    = sil_global,
    p_sil_cluster = p_sil_cluster,
    p_sil_obs     = p_sil_obs
  )
}
