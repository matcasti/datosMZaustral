
# Datos generales -----------------------------------------------------------------------------

## script: 1-import.R
## función: importar datos de laboratorio de instrumentos
## autor: Matías Castillo
## fecha: viernes 8 octubre 2021

# Cargar paquetes -----------------------------------------------------------------------------
library(readxl)
library(data.table)
library(tm)
library(igraph)

# Funciones auxiliares ------------------------------------------------------------------------
.s <- function(x, ...) {
  stopifnot(inherits(x, "data.table"))
  temp <- substitute(x[...])
  eval(temp)
}

# Importación ---------------------------------------------------------------------------------

## Importamos base de datos
lab_instrumentosANID <- readxl::read_excel("data/lab_instrumentos/raw/Lista de problemas LAB 1.xlsx", sheet = "Lista") |> 
  data.table::as.data.table() |> 
  .s(j = .SD, .SDcols = 1:5) |> 
  `names<-`(c("lab", "instrumento", "problema", "k_problema_manual", "norm_problema"))

## Stop words
stopWords <- readLines(con = "https://raw.githubusercontent.com/Alir3z4/stop-words/master/spanish.txt")

## A minuscula
lab_instrumentosANID <- lab_instrumentosANID[, lapply(.SD, tolower)]

## Eliminamos stopWords
for (i in stopWords) {
  lab_instrumentosANID[, norm_problema := gsub(pattern = paste0(" ", i," "), replacement = " ", x = norm_problema, fixed = TRUE)]
}

## Stopwords con criterios específicos
lab_instrumentosANID[, norm_problema := gsub(pattern = "el ", replacement = "", x = norm_problema, fixed = TRUE)
                     ][, norm_problema := gsub(pattern = "la ", replacement = "", x = norm_problema, fixed = TRUE)
                       ][, norm_problema := gsub(pattern = "las ", replacement = "", x = norm_problema, fixed = TRUE)]

## Creación de un corpus para posterior análisis
m <- Corpus(VectorSource(lab_instrumentosANID$norm_problema)) |> 
  TermDocumentMatrix(control = list(minWordLength = c(1, Inf))) |> 
  as.matrix()

m2 <- m[rowSums(m) > 1, ]

m2[m2 > 1] <- 1
termM <- m2 %*% t(m2)

g <- graph.adjacency(termM, weighted = T, mode = 'undirected')
g
g <- simplify(g)
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)

# Histogram of node degree
hist(V(g)$degree,
     breaks = 100,
     col = 'green',
     main = 'Histogram of Node Degree',
     ylab = 'Frequency',
     xlab = 'Degree of Vertices')

# Network diagram
set.seed(222)
plot(g, vertex.size = 4)
plot(g,
     vertex.color = 'green',
     vertex.size = 4,
     vertex.label.dist = 1.5,
     vertex.label = NA)

# Community detection
comm <- cluster_edge_betweenness(g)
plot(comm, g, vertex.size = 4)

prop <- cluster_label_prop(g)
plot(prop, g, vertex.size = 4)

set.seed(222)
greed <- cluster_fast_greedy(as.undirected(g))
plot(greed, as.undirected(g), vertex.size = 4)

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
tweetM <- t(m2) %*% m2
g <- graph.adjacency(tweetM, weighted = T, mode = 'undirected')
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
g2 <- delete.vertices(g, V(g)[degree(g) < 40])
plot(g2,
     vertex.label.cex = .9,
     vertex.label.color = 'black')

# Delete edges
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam
g3 <- delete.edges(g, E(g)$weight <- 1)
g3 <- delete.vertices(g3, V(g3)[degree(g3) < 20])
plot(g3)















hc <- m |> 
  scale() |> 
  dist() |> 
  hclust(method = "ward.D")

# for (i in 5:20) {
#   pdf(file = paste0("output/dendrogram_lab/dendrogram_lab_instrumentos_k",i,".pdf"), width = 12, height = 8)
#   plot(hc)
#   rect.hclust(hc, k = i)
#   dev.off()
# }

  plot(hc)
  rect.hclust(hc, k = 8)

set.seed(12345)
m |> 
  t() |> 
  kmeans(centers = 8, nstart = 1, iter.max = 1000) |> 
  factoextra::fviz_cluster(data = t(m), 
                           geom = "point",
                           ggtheme = ggplot2::theme_bw())

