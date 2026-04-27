#' Realiza a clusterizacao, via GMM, dos dados reduzidos do MPI
#' @param pca_obj Objeto retornado pela funcao `factors_mpi`
#' @param cols_mpi Vetor de nomes dos indicadores MPI ativos

#' @return Lista de objetos relacionados à clusterizacao
#' @export

cluster_mpi <- function(pca_obj, cols_mpi, G = NULL) {
  
  # Preparação dos dados ----
  mat_pca <- as.matrix(pca_obj$coords_pca)
  pesos   <- pca_obj$df_pca$pesos
  
  # Implementação do GMM ----
  if (is.null(G)) {
    # busca o k que maximiza silhueta no range 2:10
    sil_scores <- map_dbl(2:10, \(k) {
      m   <- mclust::Mclust(mat_pca, G = k, verbose = F)
      sil <- cluster::silhouette(m$classification, dist(mat_pca))
      mean(sil[, 'sil_width'])
    })
    G <- which.max(sil_scores) + 1L  # +1 porque range começa em 2
    message('k ótimo por silhueta: ', G,
            ' (silhueta = ', round(max(sil_scores), 3), ')')
  }
  
  mod_gmm <- mclust::Mclust(mat_pca, G = G, verbose = F)
  k_gmm   <- mod_gmm$G
  
  gmm_dt <- cbind(
    pca_obj$df_pca[, .(uf, setor, area, arranjo_familiar, pesos, scorempi)],
    pca_obj$coords_pca,
    data.table(cluster     = mod_gmm$classification,
               uncertainty = round(mod_gmm$uncertainty, 3))
  )
  
  # Scatterplot 2D ----
  p_scatter <- ggplot(gmm_dt,
                      aes(x      = Dim.1,
                          y      = Dim.2,
                          colour = factor(cluster),
                          size   = pesos)) +
    geom_point(alpha = 0.7) +
    stat_ellipse(aes(group = factor(cluster)),
                 level = 0.95, linewidth = 0.8, show.legend = F) +
    scale_size_continuous(range = c(1, 8), guide = 'none') +
    labs(x        = 'CP1',
         y        = 'CP2',
         colour   = 'Cluster',
         title    = paste0('Clusterização GMM com ', k_gmm, ' agrupamentos (BIC)'),
         subtitle = 'Tamanho proporcional ao total de pessoas | elipses 95%') +
    theme_minimal() +
    theme(legend.position = 'bottom')
  
  # Perfil dos indicadores por cluster ----
  perfil_dt <- cbind(
    pca_obj$df_pca[, .(pesos)],
    pca_obj$df_pca[, ..cols_mpi],
    data.table(cluster = gmm_dt$cluster)
  )[, lapply(.SD, weighted.mean, w = pesos),
    by      = cluster,
    .SDcols = cols_mpi
  ][order(cluster)]
  
  p_perfil <- ggplot(
    melt(perfil_dt, id.vars = 'cluster',
         variable.name = 'indicador', value.name = 'media_pond'),
    aes(x = factor(cluster), y = indicador, fill = media_pond)
  ) +
    geom_tile(colour = 'white', linewidth = 0.4) +
    geom_text(aes(label = sprintf('%.2f', media_pond)), size = 3) +
    scale_fill_gradient2(low = '#2c7bb6', mid = 'white', high = '#d7191c',
                         midpoint = 0, name = 'Z-score médio \n(menor, melhor)') +
    scale_y_discrete(limits = rev(cols_mpi)) +
    labs(x = 'Cluster', y = NULL,
         title    = 'Perfil de privação por cluster',
         subtitle = 'Média ponderada dos indicadores MPI (z-score)') +
    theme_minimal() +
    theme(panel.grid = element_blank(), legend.position = 'right')
  
  # Medição da silhueta ----
  sil     <- cluster::silhouette(mod_gmm$classification, dist(mat_pca))
  sil_dt  <- as.data.table(sil[, 1:3])[
    , .(sil_media = mean(sil_width), sil_min = min(sil_width), sil_max = max(sil_width)),
    by = .(cluster)
  ][order(cluster)]
  sil_global <- mean(sil[, 'sil_width'])
  
  p_sil_cluster <- ggplot(sil_dt,
                          aes(x = factor(cluster), y = sil_media, fill = sil_media)) +
    geom_col(width = 0.6) +
    geom_errorbar(aes(ymin = sil_min, ymax = sil_max), width = 0.2, colour = 'grey40') +
    geom_hline(yintercept = sil_global, linetype = 'dashed', colour = 'grey40') +
    annotate('text', x = Inf, y = sil_global, hjust = 1.1, vjust = -0.5,
             label = sprintf('Global: %.3f', sil_global), size = 3, colour = 'grey30') +
    scale_fill_gradient(low = '#d7191c', high = '#2c7bb6',
                        limits = c(-1, 1), name = 'Silhueta') +
    scale_y_continuous(limits = c(-1, 1)) +
    labs(x = 'Cluster', y = 'Silhueta média',
         title    = 'Qualidade dos clusters — Silhueta',
         subtitle = 'Barra: média | Intervalo: min–max | Tracejado: global') +
    theme_minimal() +
    theme(legend.position = 'right')
  
  p_sil_obs <- ggplot(as.data.table(sil[, 1:3])[order(cluster, sil_width)][
    , obs := seq_len(.N)],
    aes(x = obs, y = sil_width, fill = factor(cluster))) +
    geom_col(width = 1) +
    geom_hline(yintercept = sil_global, linetype = 'dashed', colour = 'grey40') +
    scale_fill_discrete(name = 'Cluster') +
    labs(x = NULL, y = 'Silhueta',
         title    = 'Silhueta por observação',
         subtitle = sprintf('Silhueta global: %.3f', sil_global)) +
    theme_minimal() +
    theme(axis.text.x  = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = 'bottom')
  
  # Distribuição do score MPI por cluster ----
  p_score <- ggplot(gmm_dt,
                    aes(x      = scorempi,
                        colour = factor(cluster),
                        weight = pesos)) +
    geom_density(linewidth = 0.8, adjust = 1.2) +
    labs(x        = 'Score médio de privação',
         y        = 'Densidade',
         colour   = 'Cluster',
         title    = 'Distribuição do score MPI por cluster',
         subtitle = 'Densidade ponderada pelo total de pessoas') +
    theme_minimal() +
    theme(legend.position = 'bottom')
  
  # Distribuição de atributos categóricos ----
  macro <- list(
    NO = c('AC', 'AP', 'AM', 'PA', 'RO', 'RR', 'TO'),
    NE = c('AL', 'BA', 'CE', 'MA', 'PB', 'PE', 'PI', 'RN', 'SE'),
    CO = c('DF', 'GO', 'MS', 'MT'),
    SE = c('ES', 'MG', 'RJ', 'SP'),
    SU = c('PR', 'RS', 'SC')
  )
  
  uf_macro <- rbindlist(lapply(names(macro), \(m) {
    data.table(uf = macro[[m]], macrorregiao = m)
  }))
  
  gmm_dt[uf_macro, on = 'uf', regiao := macrorregiao]
  gmm_dt[, `:=`(sexo            = substr(arranjo_familiar, 1, 1),
                arranjo_simples = fcase(
                  substr(arranjo_familiar, 2, 3) == 'SS', 'Casal Com',
                  substr(arranjo_familiar, 2, 3) == 'SN', 'Casal Sem',
                  substr(arranjo_familiar, 2, 3) == 'NN', 'Unipessoal',
                  substr(arranjo_familiar, 2, 3) == 'NS', 'Monoparental'
                ))]
  
  cats      <- c('regiao', 'setor', 'area', 'sexo', 'arranjo_simples')
  
  rotulos <- c(
    regiao          = 'Região de Residência',
    setor           = 'Setor da UF',
    area            = 'Área do Município',
    sexo            = 'Sexo do Responsável',
    arranjo_simples = 'Arranjo Domiciliar'
  )
  
  comp_list <- lapply(cats, \(cat) {
    gmm_dt[, .(n = .N), by = c('cluster', cat)][
      , prop     := n / sum(n), by = cluster
    ][, variavel := cat
    ][, setnames(.SD, cat, 'categoria')]
  }) |> rbindlist()
  
  comp_list[, variavel := rotulos[variavel]]
  comp_list[, variavel := factor(variavel, levels = rotulos)]
  
  p_comp <- ggplot(comp_list,
                   aes(x = factor(cluster), y = categoria, fill = prop)) +
    geom_tile(colour = 'white', linewidth = 0.4) +
    geom_text(aes(label = scales::percent(prop, accuracy = 1)), size = 3) +
    scale_fill_gradient(low = 'white', high = '#2c7bb6', name = 'Proporção') +
    facet_wrap(~ variavel, scales = 'free_y', ncol = 1) +
    labs(x = 'Cluster', y = NULL,
         title    = 'Composição categórica por cluster',
         subtitle = 'Proporção de casos dentro de cada cluster') +
    theme_minimal() +
    theme(panel.grid = element_blank(), legend.position = 'right')
  
  # Output ----
  list(
    mod_gmm        = mod_gmm,
    k_gmm          = k_gmm,
    gmm_dt         = gmm_dt,
    p_scatter      = p_scatter,
    perfil_dt      = perfil_dt,
    p_perfil       = p_perfil,
    p_score        = p_score,
    comp_list      = comp_list,
    p_comp         = p_comp,
    sil_dt         = sil_dt,
    sil_global     = sil_global,
    p_sil_cluster  = p_sil_cluster,
    p_sil_obs      = p_sil_obs
  )
}