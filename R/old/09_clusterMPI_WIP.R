#' Realiza a clusterizacao via GMM ou LPA dos dados reduzidos do MPI
#' @param pca_obj Objeto retornado pela funcao `factors_mpi`
#' @param cols_mpi Vetor de nomes dos indicadores MPI ativos
#' @param metodo 'gmm' (default) ou 'lpa'
#' @param G Numero de clusters (NULL = automatico por silhueta no GMM;
#'           no LPA, vetor de k a testar, ex: 2:8)
#' @param lpa_modelo Modelo de covariancia do LPA (1–6, default c(1,2,3,6))
#' @return Lista de objetos relacionados à clusterizacao
#' @export

clusters_mpi <- function(pca_obj,
                         cols_mpi,
                         metodo     = NULL,
                         G          = NULL,
                         lpa_modelo = c(1, 2, 3, 6)) {
  
  # Preparação dos dados ----
  mat_pca <- as.matrix(pca_obj$coords_pca)
  pesos   <- pca_obj$df_pca$pesos
  
  # Clusterização ----
  if (metodo == 'GMM') {
    
    if (is.null(G)) {
      sil_scores <- purrr::map_dbl(2:10, \(k) {
        m   <- mclust::Mclust(mat_pca, G = k, verbose = FALSE)
        sil <- cluster::silhouette(m$classification, dist(mat_pca))
        mean(sil[, 'sil_width'])
      })
      G <- which.max(sil_scores) + 1L
      message('GMM - k ótimo por silhueta: ', G,
              ' (silhueta = ', round(max(sil_scores), 3), ')')
    }
    
    mod     <- mclust::Mclust(mat_pca, G = G, verbose = FALSE)
    classes <- mod$classification
    probs   <- apply(mod$z, 1, max)
    k_final <- mod$G
    label_metodo <- paste0('GMM — ', k_final, ' clusters (silhueta)')
    
  } else if (metodo == 'LPA') {
    
    G_range <- if (is.null(G)) 2:10 else G
    
    lpa_res <- as.data.frame(mat_pca) |>
      tidyLPA::estimate_profiles(n_profiles = G_range,
                                 models     = lpa_modelo)
    
    message('LPA - comparação de soluções:')
    print(tidyLPA::compare_solutions(lpa_res))
    
    # escolha automática: maior entropia com BIC aceitável
    fits <- tidyLPA::get_fit(lpa_res)
    fits_dt <- as.data.table(fits)[order(-Entropy, BIC)]
    k_best  <- fits_dt[1, Classes]
    m_best  <- fits_dt[1, Model]
    message('LPA - solução escolhida: modelo ', m_best, ', k = ', k_best,
            ' (entropia = ', round(fits_dt[1, Entropy], 3), ')')
    
    lpa_fit  <- tidyLPA::get_data(lpa_res, model = m_best, classes = k_best)
    classes  <- lpa_fit$Class
    probs    <- lpa_fit$Probability
    k_final  <- k_best
    label_metodo <- paste0('LPA — ', k_final, ' classes (modelo ', m_best, ')')
    
  } else {
    stop('metodo deve ser "GMM" ou "LPA"')
  }
  
  # Montar base_clus ----
  base_clus <- cbind(
    pca_obj$df_pca[, .(uf, setor, area, arranjo_familiar, pesos, scorempi)],
    pca_obj$coords_pca,
    data.table(cluster     = as.integer(classes),
               probability = round(probs, 3))
  )
  
  # Silhueta ----
  sil        <- cluster::silhouette(base_clus$cluster, dist(mat_pca))
  sil_dt     <- as.data.table(sil[, 1:3])[
    , .(sil_media = mean(sil_width), sil_min = min(sil_width), sil_max = max(sil_width)),
    by = cluster][order(cluster)]
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
         title    = paste0('Qualidade dos clusters — ', label_metodo),
         subtitle = 'Barra: média | Intervalo: min–max | Tracejado: global') +
    theme_minimal() +
    theme(legend.position = 'right')
  
  p_sil_obs <- ggplot(
    as.data.table(sil[, 1:3])[order(cluster, sil_width)][, obs := seq_len(.N)],
    aes(x = obs, y = sil_width, fill = factor(cluster))
  ) +
    geom_col(width = 1) +
    geom_hline(yintercept = sil_global, linetype = 'dashed', colour = 'grey40') +
    scale_fill_discrete(name = 'Cluster') +
    labs(x = NULL, y = 'Silhueta',
         title    = 'Silhueta por observação',
         subtitle = sprintf('Silhueta global: %.3f', sil_global)) +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          legend.position = 'bottom')
  
  # Scatterplot CP1 × CP2 ----
  p_scatter <- ggplot(base_clus,
                      aes(x = Dim.1, y = Dim.2,
                          colour = factor(cluster), size = pesos)) +
    geom_point(alpha = 0.7) +
    stat_ellipse(aes(group = factor(cluster)),
                 level = 0.95, linewidth = 0.8, show.legend = FALSE) +
    scale_size_continuous(range = c(1, 8), guide = 'none') +
    labs(x = 'CP1', y = 'CP2', colour = 'Cluster',
         title    = paste0('Clusterização ', label_metodo),
         subtitle = 'Tamanho proporcional ao total de pessoas | elipses 95%') +
    theme_minimal() +
    theme(legend.position = 'bottom')
  
  # Perfil dos indicadores por cluster ----
  perfil_dt <- cbind(
    pca_obj$df_pca[, .(pesos)],
    pca_obj$df_pca[, ..cols_mpi],
    data.table(cluster = base_clus$cluster)
  )[, lapply(.SD, weighted.mean, w = pesos),
    by = cluster, .SDcols = cols_mpi][order(cluster)]
  
  p_perfil <- ggplot(
    melt(perfil_dt, id.vars = 'cluster',
         variable.name = 'indicador', value.name = 'media_pond'),
    aes(x = factor(cluster), y = indicador, fill = media_pond)
  ) +
    geom_tile(colour = 'white', linewidth = 0.4) +
    geom_text(aes(label = sprintf('%.2f', media_pond)), size = 3) +
    scale_fill_gradient2(low = '#2c7bb6', mid = 'white', high = '#d7191c',
                         midpoint = 0, name = 'Z-score\nmédio') +
    scale_y_discrete(limits = rev(cols_mpi)) +
    labs(x = 'Cluster', y = NULL,
         title    = 'Perfil de privação por cluster',
         subtitle = 'Média ponderada dos indicadores MPI (z-score)') +
    theme_minimal() +
    theme(panel.grid = element_blank(), legend.position = 'right')
  
  # Distribuição do score MPI por cluster ----
  p_score <- ggplot(base_clus,
                    aes(x = scorempi, colour = factor(cluster), weight = pesos)) +
    geom_density(linewidth = 0.8, adjust = 1.2) +
    labs(x = 'Score médio de privação', y = 'Densidade', colour = 'Cluster',
         title    = 'Distribuição do score MPI por cluster',
         subtitle = 'Densidade ponderada pelo total de pessoas') +
    theme_minimal() +
    theme(legend.position = 'bottom')
  
  # Composição categórica ----
  macro <- list(
    NO = c('AC','AP','AM','PA','RO','RR','TO'),
    NE = c('AL','BA','CE','MA','PB','PE','PI','RN','SE'),
    CO = c('DF','GO','MS','MT'),
    SE = c('ES','MG','RJ','SP'),
    SU = c('PR','RS','SC')
  )
  uf_macro <- rbindlist(lapply(names(macro), \(m) {
    data.table(uf = macro[[m]], macrorregiao = m)
  }))
  
  base_clus[uf_macro, on = 'uf', regiao := macrorregiao]
  base_clus[, `:=`(sexo            = substr(arranjo_familiar, 1, 1),
                arranjo_simples = fcase(
                  substr(arranjo_familiar, 2, 3) == 'SS', 'Casal Com',
                  substr(arranjo_familiar, 2, 3) == 'SN', 'Casal Sem',
                  substr(arranjo_familiar, 2, 3) == 'NN', 'Unipessoal',
                  substr(arranjo_familiar, 2, 3) == 'NS', 'Monoparental'
                ))]
  
  rotulos <- c(regiao = 'Região de Residência', setor = 'Setor da UF',
               area = 'Área do Município', sexo = 'Sexo do Responsável',
               arranjo_simples = 'Arranjo Domiciliar')
  
  cats      <- c('regiao', 'setor', 'area', 'sexo', 'arranjo_simples')
  comp_list <- lapply(cats, \(cat) {
    base_clus[, .(n = .N), by = c('cluster', cat)][
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
    metodo        = metodo,
    k_final       = k_final,
    modelo        = if (metodo == 'GMM') mod else lpa_res,
    base_clus        = base_clus,
    sil_dt        = sil_dt,
    sil_global    = sil_global,
    p_sil_cluster = p_sil_cluster,
    p_sil_obs     = p_sil_obs,
    p_scatter     = p_scatter,
    perfil_dt     = perfil_dt,
    p_perfil      = p_perfil,
    p_score       = p_score,
    comp_list     = comp_list,
    p_comp        = p_comp
  )
}

# --- uso -----------------------------------------------------------------
# GMM com k automático por silhueta
cl_gmm <- clusters_mpi(pca_obj = pca_mpi, cols_mpi = rownames(pca_mpi$res_pca$var$coord), metodo = 'GMM')

# GMM com k fixo
cl_gmm <- clusters_mpi(pca_obj = pca_mpi, cols_mpi = rownames(pca_mpi$res_pca$var$coord), G = 4L)

# LPA com range de k e modelos padrão
cl_lpa <- clusters_mpi(pca_obj = pca_mpi, cols_mpi = rownames(pca_mpi$res_pca$var$coord), metodo = 'LPA')

# LPA com k e modelo fixos (após avaliar compare_solutions)
cl_lpa <- clusters_mpi(pca_obj = pca_mpi, cols_mpi = rownames(pca_mpi$res_pca$var$coord),
                       metodo = 'lpa', G = 4, lpa_modelo = 1)