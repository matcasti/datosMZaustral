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
  .s(j = clean_causa := gsub(pattern = "macrozona-austral", replacement = "", x = clean_causa))


# Pre-procesamiento ---------------------------------------------------------------------------

g <- crear_redes_de_palabras(
  x = data$clean_causa
)

# Producto ------------------------------------------------------------------------------------

message("Iniciando graficos de redes de términos - CAUSAS")

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
pdf(file = "output/lab_instrumentos/productos/network_1_causas.pdf", width = 12, height = 12);
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
pdf(file = "output/lab_instrumentos/productos/network_2_causas.pdf", width = 12, height = 12);
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
pdf(file = "output/lab_instrumentos/productos/network_3_causas.pdf", width = 12, height = 12);
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


### Structure detection via Spinglass ---------------------------------

pdf(file = "output/lab_instrumentos/productos/network_4_causas.pdf", width = 12, height = 12);
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
pdf(file = "output/lab_instrumentos/productos/network_hubs_causas.pdf", width = 12, height = 12);
plot(g, vertex.size = hs * 10, main = 'Hubs',
     vertex.color = rainbow(50))
dev.off();

message("✅Tarea completada")

