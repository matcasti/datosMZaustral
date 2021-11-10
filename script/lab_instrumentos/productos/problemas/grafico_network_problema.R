## función: Generar graficos de redes de términos (PROBLEMA)
## fecha: 22-oct

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
  .s(j = clean_problema := gsub(pattern = "macrozona-austral", replacement = "", x = clean_problema));


# Pre-procesamiento ---------------------------------------------------------------------------

message("Iniciando gráfico de redes de términos - PROBLEMAS")

## Creación de un corpus para posterior análisis
m <- tm::Corpus(x = tm::VectorSource(x = unique(data$clean_problema) ) ) |> 
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

plot(g,
     vertex.color = rainbow(22),
     vertex.size = V(g)$degree*2,
     edge.arrow.size = 0.1,
     layout = layout.fruchterman.reingold.grid)


## Detección de comunidades -------------------------------------------------------------------

### Structure detection based on edge betweenness ---------------------------------------------
pdf(file = "output/lab_instrumentos/productos/network_1_problemas.pdf", width = 8, height = 8);
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
  #   lab_instrumentosANID[clean_problema %like% i, clean_problema_net_k1 := as.numeric(k)]
  # }
});
dev.off();

### Structure detection based on propagating labels -------------------------------------------
pdf(file = "output/lab_instrumentos/productos/network_2_problemas.pdf", width = 8, height = 8);
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
  #   lab_instrumentosANID[clean_problema %like% i, clean_problema_net_k2 := as.numeric(k)]
  # }
});
dev.off();

### Structure detection via greedy optimization of modularity ---------------------------------
pdf(file = "output/lab_instrumentos/productos/network_3_problemas.pdf", width = 8, height = 8);
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
  #   lab_instrumentosANID[clean_problema %like% i, clean_problema_net_k3 := as.numeric(k)]
  # }
});
dev.off();

### Structure detection via Spinglass ---------------------------------
pdf(file = "output/lab_instrumentos/productos/network_4_problemas.pdf", width = 8, height = 8);
local({
  set.seed(222)
  SG <- igraph::cluster_spinglass(g)
  plot(SG, g, vertex.size = 8)
  net_k4 <- igraph::membership(SG)
  net_k4_n <- names(net_k4)
});
dev.off()

### Análisis de HUBS - Para matrices indirectas (conectadas o parcialmente conectadas) HUBS y Authorities son iguales ----------------------
hs <- igraph::hub_score(g, weights = NA)$vector
pdf(file = "output/lab_instrumentos/productos/network_hubs_problemas.pdf", width = 8, height = 8);
plot(g, vertex.size = hs * 10, main = 'Hubs',
     vertex.color = rainbow(50))
dev.off();



# Revisar después

# # Hub and authorities
# hs <- igraph::hub_score(g, weights = NA)$vector
# as <- igraph::authority_score(g, weights = NA)$vector
# par(mfrow = c(1,2))
# plot(g, vertex.size = hs * 10, main = 'Hubs',
#    vertex.color = rainbow(50))
# plot(g, vertex.size = as * 9, main = 'Authorities',
#      vertex.color = rainbow(50))
# par(mfrow = c(1,1))
# Highlighting degrees
# igraph::V(g)$label.cex <- 2.2 * igraph::V(g)$degree / max(igraph::V(g)$degree) + 0.3
# igraph::V(g)$label.color <- rgb(0, 0, .2, .8)
# igraph::V(g)$frame.color <- NA
# egam <- (log(igraph::E(g)$weight) + .4) / max(log(igraph::E(g)$weight) + .4)
# igraph::E(g)$color <- rgb(.5, .5, 0, egam)
# igraph::E(g)$width <- egam
# plot(g, vertex.size = igraph::V(g)$degree * .5)

message("✅Tarea completada")
