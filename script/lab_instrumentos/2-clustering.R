
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

  lab_instrumentosANID <- lab_instrumentosANID[j = norm_problema := gsub(pattern = "macrozona-austral", 
                                                 replacement = "", 
                                                 x = norm_problema)]
  
# Preparación del corpus ----------------------------------------------------------------------

  ## Creación de un corpus para posterior análisis
  m <- tm::Corpus(x = tm::VectorSource(x = unique(lab_instrumentosANID$norm_problema) ) ) |> 
    tm::TermDocumentMatrix(control = list(minWordLength = c(1, Inf) ) ) |> 
    as.matrix()
  
  
  

# EDA -----------------------------------------------------------------------------------------

  if (FALSE) {
    local({
      ## Network Análisis
      m2 <- m[rowSums(m) > 1, ]
      m2[m2 > 1] <- 1
      termM <- m2 %*% t(m2)
      g <- igraph::graph.adjacency(termM, weighted = T, mode = 'undirected')
      g <- igraph::simplify(g)
      igraph::V(g)$label <- igraph::V(g)$name
      igraph::V(g)$degree <- igraph::degree(g)
      
      # Histogram of node degree
      hist(igraph::V(g)$degree,
           breaks = 20,
           col = 'green',
           main = 'Histogram of Node Degree',
           ylab = 'Frequency',
           xlab = 'Degree of Vertices')
      
      # Network diagram
      set.seed(222)
      plot(g, vertex.size = 4)
      
      # Detección de comunidades ----
      comm <- igraph::cluster_edge_betweenness(g)
      plot(comm, g, vertex.size = 4)
      net_k1 <- igraph::membership(comm)
      net_k1_n <- names(net_k1)
      
      # ## Asignamos grupos del clustering ----
      # for (i in net_k1_n) {
      #   # Identificando el grupo dentro del dendrograma
      #   k <- net_k1[net_k1_n == i][[1]]
      #   # Asignar el grupo a una nueva columna
      #   lab_instrumentosANID[norm_problema %like% i, norm_problema_net_k1 := as.numeric(k)]
      # }
      
      prop <- igraph::cluster_label_prop(g)
      plot(prop, g, vertex.size = 4)
      net_k2 <- igraph::membership(prop)
      net_k2_n <- names(net_k2)
      
      # ## Asignamos grupos del clustering ----
      # for (i in net_k2_n) {
      #   # Identificando el grupo dentro del dendrograma
      #   k <- net_k2[net_k2_n == i][[1]]
      #   # Asignar el grupo a una nueva columna
      #   lab_instrumentosANID[norm_problema %like% i, norm_problema_net_k2 := as.numeric(k)]
      # }
      
      set.seed(222)
      greed <- igraph::cluster_fast_greedy(igraph::as.undirected(g))
      plot(greed, igraph::as.undirected(g), vertex.size = 4)
      net_k3 <- igraph::membership(greed)
      net_k3_n <- names(net_k3)
      
      # ## Asignamos grupos del clustering ----
      # for (i in net_k3_n) {
      #   # Identificando el grupo dentro del dendrograma
      #   k <- net_k3[net_k3_n == i][[1]]
      #   # Asignar el grupo a una nueva columna
      #   lab_instrumentosANID[norm_problema %like% i, norm_problema_net_k3 := as.numeric(k)]
      # }
      
      # Hub and authorities
      hs <- igraph::hub_score(g, weights = NA)$vector
      as <- igraph::authority_score(g, weights = NA)$vector
      par(mfrow = c(1,2))
      plot(g, vertex.size = hs * 10, main = 'Hubs',
           vertex.color = rainbow(50))
      plot(g, vertex.size = as * 9, main = 'Authorities',
           vertex.color = rainbow(50))
      par(mfrow = c(1,1))
          # Highlighting degrees
      igraph::V(g)$label.cex <- 2.2 * igraph::V(g)$degree / max(igraph::V(g)$degree) + 0.3
          igraph::V(g)$label.color <- rgb(0, 0, .2, .8)
          igraph::V(g)$frame.color <- NA
          egam <- (log(igraph::E(g)$weight) + .4) / max(log(igraph::E(g)$weight) + .4)
          igraph::E(g)$color <- rgb(.5, .5, 0, egam)
          igraph::E(g)$width <- egam
          plot(g, vertex.size = igraph::V(g)$degree * .5)
          
          # Network of tweets
          m3 <- t(m2) %*% m2
          g <- igraph::graph.adjacency(m3, weighted = T, mode = 'undirected')
          igraph::V(g)$degree <- igraph::degree(g)
          g <- igraph::simplify(g)
          hist(igraph::V(g)$degree,
               breaks = 100,
               col = 'green',
               main = 'Histogram of Degree',
               ylab = 'Freuqency',
               xlab = 'Degree')
          
          # Set labels of vertices to tweet IDs
          igraph::V(g)$label <- igraph::V(g)$name
          igraph::V(g)$label.cex <- 1
          igraph::V(g)$label.color <- rgb(.4, 0, 0, .7)
          igraph::V(g)$size <- 2
          igraph::V(g)$frame.color <- NA
          plot(g, vertex.size = 3)
          
          # Delete vertices
          egam <- (log(igraph::E(g)$weight) + .2) / max(log(igraph::E(g)$weight) + .2)
          igraph::E(g)$color <- rgb(.5, .5, 0, egam)
          igraph::E(g)$width <- egam
          g2 <- igraph::delete.vertices(g, igraph::V(g)[igraph::degree(g) < 2])
          plot(g2,
               vertex.label.cex = .9,
               vertex.label.color = 'black')
          
          # Delete edges
          igraph::E(g)$color <- rgb(.5, .5, 0, egam)
          igraph::E(g)$width <- egam
          g3 <- igraph::delete.edges(g, igraph::E(g)$weight <- 1)
          g3 <- igraph::delete.vertices(g3, igraph::V(g3)[igraph::degree(g3) < 20])
          plot(g3)
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
  