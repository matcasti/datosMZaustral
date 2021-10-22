
# Datos generales -----------------------------------------------------------------------------

## script: 3-clustering.R
## función: generar modelos de clusterización
## autor: Matías Castillo
## fecha: miércoles 13 octubre 2021

# Cargar paquetes -----------------------------------------------------------------------------

  library(data.table)
  library(tm)
  library(igraph)
  library(fpc)
  library(dbscan)

# Funciones auxiliares ------------------------------------------------------------------------
  
  .s <- function(x, ...) {
    stopifnot(inherits(x, "data.table"))
    temp <- substitute(x[...])
    eval(temp)
  }

# Importar datos ------------------------------------------------------------------------------

  lab_instrumentosANID <- readRDS(file = "data/lab_instrumentos/clean/data.RDS") |> 
    .s(j = norm_problema := gsub(pattern = "macrozona-austral", replacement = "", x = norm_problema))
  
# Preparación del corpus ----------------------------------------------------------------------

  ## Creación de un corpus para posterior análisis
  m <- tm::Corpus(x = tm::VectorSource(x = unique(lab_instrumentosANID$norm_problema) ) ) |> 
    tm::TermDocumentMatrix(control = list(minWordLength = c(1, Inf) ) ) |> 
    as.matrix()

# EDA -----------------------------------------------------------------------------------------

  if (FALSE) {
    local({
      
      })
  }
  
# Técnicas de clustering ----------------------------------------------------------------------
  
  # 1. Clusterización jerárquica -----------------------------------------------------------------
    hc <- m |> 
      scale() |> 
      dist() |> 
      hclust(method = "ward.D")
    
    ## Graficamos mediante dendrograma --------------------------------------------------------
      plot(hc)
      rect.hclust(hc, k = 5)
      hc_groups <- cutree(hc, k = 5)
      hc_names <- names(hc_groups)
    
    ## Asignamos grupos del clustering --------------------------------------------------------
      for (i in hc_names) {
        # Identificando el grupo dentro del dendrograma
        k <- hc_groups[hc_names == i][[1]]
        # Asignar el grupo a una nueva columna
        lab_instrumentosANID[norm_problema %like% i, norm_problema_hclust := k]
      }
      
      rm(hc_groups, hc_names)
    
  # 2. Clusterización no jerárquica mediante K-means ---------------------------------------------
  
  # Utilizar el método del codo para determinar la cantidad de centroides
  local({
    set.seed(1234)
    wcss <- vector()
    for (i in 1:20) {
      wcss[i] <- sum(kmeans(t(m), i)$withinss)
    }
    
    #Graficar
    if (require("ggplot2", quietly = TRUE)) {
      ggplot2::ggplot() + ggplot2::geom_point(aes(x = 1:20, y = wcss), color = 'blue') + 
        ggplot2::geom_line(aes(x = 1:20, y = wcss), color = 'blue') + 
        ggplot2::xlab('Cantidad de Centroides k') + 
        ggplot2::ylab('WCSS')
    }
  })
  
  set.seed(12345)
  km <- t(m) |> 
    kmeans(centers = 4, nstart = 1, iter.max = 1000)
  
  ## Graficamos los grupos de kmeans ----------------------------------------------------------
  km |> 
    factoextra::fviz_cluster(data = t(m), 
                             geom = "point",
                             ggtheme = ggplot2::theme_bw())
  
  ## Generamos grupos ----
  lookup_km <- data.table(norm_problema_kmeans = km$cluster, 
                          norm_problema = unique(lab_instrumentosANID$norm_problema))
  
  ## Asignamos grupos
  lab_instrumentosANID <- merge(
    x = lab_instrumentosANID,
    y = lookup_km,
    all.x = TRUE,
    by = "norm_problema"
  )

  rm(lookup_km)
  
  # 3. Density based clustering ---------------------------------------------------------------

  # Buscar un eps óptimo
  dbscan::kNNdistplot(t(m), k = 4); abline(h = 1.74)
  
  set.seed(12345)
  f <- fpc::dbscan(t(m), eps = 2, MinPts = 1); f
  d <- dbscan::dbscan(t(m), 2, minPts = 1); d
  
  factoextra::fviz_cluster(d, t(m), geom = "point")
  
  ## Generamos grupos ----
  lookup_km <- data.table(norm_problema_dbscan = d$cluster, 
                          norm_problema = unique(lab_instrumentosANID$norm_problema))
  
  ## Asignamos grupos
  lab_instrumentosANID <- merge(
    x = lab_instrumentosANID,
    y = lookup_km,
    all.x = TRUE,
    by = "norm_problema"
  )
  
  rm(f, lookup_km)

  # 4. K-mediods ------------------------------------------------------------------------------
  
  factoextra::fviz_nbclust(t(m), cluster::pam, method = "wss", k.max = 20)
  
  set.seed(12345)
  gap_stat <- cluster::clusGap(x = t(m), FUNcluster = cluster::pam, K.max = 20, B = 100)
  
  factoextra::fviz_gap_stat(gap_stat)
  
  kmed <- cluster::pam(x = t(m), k = 4, metric = "euclidean", stand = FALSE)

  factoextra::fviz_cluster(kmed, data = t(m))
  
  ## Generamos grupos ----
  lookup_km <- data.table(norm_problema_kmed = kmed$cluster, 
                          norm_problema = unique(lab_instrumentosANID$norm_problema))
  
  ## Asignamos grupos
  lab_instrumentosANID <- merge(
    x = lab_instrumentosANID,
    y = lookup_km,
    all.x = TRUE,
    by = "norm_problema"
  )
  
  rm(gap_stat, lookup_km, m)
  