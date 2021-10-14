
# Datos generales -----------------------------------------------------------------------------

## script: 1-import.R
## función: importar datos de laboratorio de instrumentos
## autor: Matías Castillo
## fecha: viernes 8 octubre 2021

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
  
  ## Importamos base de datos
  lab_instrumentosANID <- readRDS(file = "data/lab_instrumentos/clean/data.RDS")
  
  lab_instrumentosANID[, norm_problema := gsub("macrozona-austral", replacement = "", x = norm_problema)]
  
# Preparación del corpus ----------------------------------------------------------------------

  ## Creación de un corpus para posterior análisis
  m <- Corpus(x = VectorSource(x = unique(lab_instrumentosANID$norm_problema) ) ) |> 
    TermDocumentMatrix(control = list(minWordLength = c(1, Inf) ) ) |> 
    as.matrix()
  

# EDA -----------------------------------------------------------------------------------------

  if (FALSE) {
    local({
      ## Network Análisis
      m2 <- m[rowSums(m) > 1, ]
      m2[m2 > 1] <- 1
      termM <- m2 %*% t(m2)
      g <- graph.adjacency(termM, weighted = T, mode = 'undirected')
      g <- simplify(g)
      V(g)$label <- V(g)$name
      V(g)$degree <- degree(g)
      
      # Histogram of node degree
      hist(V(g)$degree,
           breaks = 20,
           col = 'green',
           main = 'Histogram of Node Degree',
           ylab = 'Frequency',
           xlab = 'Degree of Vertices')
      
      # Network diagram
      set.seed(222)
      plot(g, vertex.size = 4)
      
      # Detección de comunidades ----
      comm <- cluster_edge_betweenness(g)
      plot(comm, g, vertex.size = 4)
      net_k1 <- membership(comm)
      net_k1_n <- names(net_k1)
      
      # ## Asignamos grupos del clustering ----
      # for (i in net_k1_n) {
      #   # Identificando el grupo dentro del dendrograma
      #   k <- net_k1[net_k1_n == i][[1]]
      #   # Asignar el grupo a una nueva columna
      #   lab_instrumentosANID[norm_problema %like% i, norm_problema_net_k1 := as.numeric(k)]
      # }
      
      prop <- cluster_label_prop(g)
      plot(prop, g, vertex.size = 4)
      net_k2 <- membership(prop)
      net_k2_n <- names(net_k2)
      
      # ## Asignamos grupos del clustering ----
      # for (i in net_k2_n) {
      #   # Identificando el grupo dentro del dendrograma
      #   k <- net_k2[net_k2_n == i][[1]]
      #   # Asignar el grupo a una nueva columna
      #   lab_instrumentosANID[norm_problema %like% i, norm_problema_net_k2 := as.numeric(k)]
      # }
      
      set.seed(222)
      greed <- cluster_fast_greedy(as.undirected(g))
      plot(greed, as.undirected(g), vertex.size = 4)
      net_k3 <- membership(greed)
      net_k3_n <- names(net_k3)
      
      # ## Asignamos grupos del clustering ----
      # for (i in net_k3_n) {
      #   # Identificando el grupo dentro del dendrograma
      #   k <- net_k3[net_k3_n == i][[1]]
      #   # Asignar el grupo a una nueva columna
      #   lab_instrumentosANID[norm_problema %like% i, norm_problema_net_k3 := as.numeric(k)]
      # }
      
      # Hub and authorities
      hs <- hub_score(g, weights = NA)$vector
      as <- authority_score(g, weights = NA)$vector
      par(mfrow = c(1,2))
      plot(g, vertex.size = hs * 10, main = 'Hubs',
           vertex.color = rainbow(50))
      plot(g, vertex.size = as * 9, main = 'Authorities',
           vertex.color = rainbow(50))
      par(mfrow = c(1,1))
          # Highlighting degrees
          V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree) + 0.3
          V(g)$label.color <- rgb(0, 0, .2, .8)
          V(g)$frame.color <- NA
          egam <- (log(E(g)$weight) + .4) / max(log(E(g)$weight) + .4)
          E(g)$color <- rgb(.5, .5, 0, egam)
          E(g)$width <- egam
          plot(g, vertex.size = V(g)$degree * .5)
          
          # Network of tweets
          m3 <- t(m2) %*% m2
          g <- graph.adjacency(m3, weighted = T, mode = 'undirected')
          V(g)$degree <- degree(g)
          g <- simplify(g)
          hist(V(g)$degree,
               breaks = 100,
               col = 'green',
               main = 'Histogram of Degree',
               ylab = 'Freuqency',
               xlab = 'Degree')
          
          # Set labels of vertices to tweet IDs
          V(g)$label <- V(g)$name
          V(g)$label.cex <- 1
          V(g)$label.color <- rgb(.4, 0, 0, .7)
          V(g)$size <- 2
          V(g)$frame.color <- NA
          plot(g, vertex.size = 3)
          
          # Delete vertices
          egam <- (log(E(g)$weight) + .2) / max(log(E(g)$weight) + .2)
          E(g)$color <- rgb(.5, .5, 0, egam)
          E(g)$width <- egam
          g2 <- delete.vertices(g, V(g)[degree(g) < 2])
          plot(g2,
               vertex.label.cex = .9,
               vertex.label.color = 'black')
          
          # Delete edges
          E(g)$color <- rgb(.5, .5, 0, egam)
          E(g)$width <- egam
          g3 <- delete.edges(g, E(g)$weight <- 1)
          g3 <- delete.vertices(g3, V(g3)[degree(g3) < 20])
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
      rect.hclust(hc, k = 10)
      hc_groups <- cutree(hc, k = 14)
      hc_names <- names(hc_groups)
    
    ## Asignamos grupos del clustering --------------------------------------------------------
      for (i in hc_names) {
        # Identificando el grupo dentro del dendrograma
        k <- hc_groups[hc_names == i][[1]]
        # Asignar el grupo a una nueva columna
        lab_instrumentosANID[norm_problema %like% i, norm_problema_hclust := k]
      }
    
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
      ggplot() + geom_point(aes(x = 1:20, y = wcss), color = 'blue') + 
        geom_line(aes(x = 1:20, y = wcss), color = 'blue') + 
        xlab('Cantidad de Centroides k') + 
        ylab('WCSS')
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


  # 3. Density based clustering ---------------------------------------------------------------

  # Buscar un eps óptimo
  kNNdistplot(t(m), k = 4); abline(h = 1.74)
  
  set.seed(12345)
  f <- fpc::dbscan(t(m), eps = 2, MinPts = 1); f
  d <- dbscan::dbscan(t(m), 2, minPts = 1); d
  
  factoextra::fviz_cluster(d, t(m), geom = "point")

  # 4. K-mediods ------------------------------------------------------------------------------
  
  factoextra::fviz_nbclust(t(m), cluster::pam, method = "wss", k.max = 20)
  
  gap_stat <- cluster::clusGap(x = t(m), FUNcluster = cluster::pam, K.max = 20, B = 100)
  
  factoextra::fviz_gap_stat(gap_stat)
  
  kmed <- cluster::pam(x = t(m), k = 4, metric = "euclidean", stand = FALSE)

  factoextra::fviz_cluster(kmed, data = t(m))
  
