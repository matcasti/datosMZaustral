## función: Generar graficos de redes de términos
## fecha: 25-oct

# Preparar el espacio de trabajo --------------------------------------------------------------

library(tm);
library(igraph);
library(data.table);
.s <- function(x, ...) {
  stopifnot(inherits(x, "data.table"))
  temp <- substitute(x[...])
  eval(temp)
}

# Importar los datos --------------------------------------------------------------------------

data <- readRDS(file = "data/lab_instrumentos/clean/data.RDS") |> 
  # Eliminamos el término macrozona-austral por ser muy influyente en el modelado
  .s(j = normalizacion_causa := gsub(pattern = "macrozona-austral", replacement = "", x = normalizacion_causa));


# Pre-procesamiento ---------------------------------------------------------------------------

## Creación de un corpus para posterior análisis
m <- tm::Corpus(x = tm::VectorSource(x = unique(data$normalizacion_causa) ) ) |> 
  tm::TermDocumentMatrix(control = list(minWordLength = c(1, Inf) ) ) |> 
  as.matrix();

## Obtenemos aquellos términos que aparezcan almenos dos veces entre los problemas únicos
m2 <- m[rowSums(m) > 1, ];

## Y a aquellos problemas les asignamos el valor de 1 (es decir, se encuentra presente o no)
m2[m2 > 1] <- 1;

## Calculamos la co-ocurrencia
termM <- m2 %*% t(m2);

## Creamos los nodos con sus respectivos vertices
g <- igraph::graph.adjacency(termM, weighted = T, mode = 'undirected') |> 
  igraph::simplify();

## Le asignamos etiquetas a los nodos para mejor visualización
igraph::V(g)$label <- igraph::V(g)$name;

## Le asignamos la propiedad degree (número de vertices adjacentes) al grafo
igraph::V(g)$degree <- igraph::degree(g);

# Producto ------------------------------------------------------------------------------------

## Histogram of node degree -------------------------------------------------------------------
local({
  hist(igraph::V(g)$degree,
       col = 'green4',
       main = 'Histogram of Node Degree',
       ylab = 'Frequency',
       xlab = 'Degree of Vertices')
});

## Network diagram ----------------------------------------------------------------------------
local({
  set.seed(222)
  plot(g, vertex.size = 4)
});

## Detacción de comunidades -------------------------------------------------------------------

### Structure detection based on edge betweenness ---------------------------------------------
pdf(file = "output/lab_instrumentos/productos/network_1_causas.pdf", width = 8, height = 8);
local({
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
});
dev.off();

### Structure detection based on propagating labels -------------------------------------------
pdf(file = "output/lab_instrumentos/productos/network_2_causas.pdf", width = 8, height = 8);
local({
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
});
dev.off();

### Structure detection via greedy optimization of modularity ---------------------------------
pdf(file = "output/lab_instrumentos/productos/network_3_causas.pdf", width = 8, height = 8);
local({
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
});
dev.off()
