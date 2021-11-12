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

for (i in list.files("script/lab_instrumentos/funciones/")) {
  source(paste0("script/lab_instrumentos/funciones/",i),echo = TRUE)
}

# Importar los datos --------------------------------------------------------------------------

data <- readRDS(file = "data/lab_instrumentos/clean/data.RDS") |> 
  # Eliminamos el término macrozona-austral por ser muy influyente en el modelado
  .s(j = clean_consecuencias := gsub(pattern = "macrozona-austral", replacement = "", x = clean_consecuencias));


# Pre-procesamiento ---------------------------------------------------------------------------

g <- crear_redes_de_palabras(
  x = data$clean_causa
)

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

## Detección de comunidades -------------------------------------------------------------------

### Structure detection based on edge betweenness ---------------------------------------------
pdf(file = "output/lab_instrumentos/productos/network_1_consecuencia.pdf", width = 8, height = 8);
local({
  comm <- igraph::cluster_edge_betweenness(g)
  plot(comm, g, vertex.size = 4)
  net_k1 <- igraph::membership(comm)
  net_k1_n <- names(net_k1)
});
dev.off();

### Structure detection based on propagating labels -------------------------------------------
pdf(file = "output/lab_instrumentos/productos/network_2_consecuencia.pdf", width = 8, height = 8);
local({
  prop <- igraph::cluster_label_prop(g)
  plot(prop, g, vertex.size = 4)
  net_k2 <- igraph::membership(prop)
  net_k2_n <- names(net_k2)
});
dev.off();

### Structure detection via greedy optimization of modularity ---------------------------------
pdf(file = "output/lab_instrumentos/productos/network_3_consecuencia.pdf", width = 8, height = 8);
local({
  set.seed(222)
  greed <- igraph::cluster_fast_greedy(igraph::as.undirected(g))
  plot(greed, igraph::as.undirected(g), vertex.size = 4)
  net_k3 <- igraph::membership(greed)
  net_k3_n <- names(net_k3)
});
dev.off()


### Structure detection via Spinglass ---------------------------------
pdf(file = "output/lab_instrumentos/productos/network_4_consecuencia.pdf", width = 8, height = 8);
local({
  set.seed(222)
  SG <- igraph::cluster_spinglass(g)
  plot(SG, g, vertex.size = 8)
  net_k4 <- igraph::membership(SG)
  net_k4_n <- names(net_k4)
});
dev.off();

### Análisis de HUBS - Para matrices indirectas (conectadas o parcialmente conectadas) HUBS y Authorities son iguales ----------------------
hs <- igraph::hub_score(g, weights = NA)$vector
pdf(file = "output/lab_instrumentos/productos/network_hubs_consecuencias.pdf", width = 8, height = 8);
plot(g, vertex.size = hs * 10, main = 'Hubs',
     vertex.color = rainbow(50))
dev.off();

message("✅Tarea completada")
