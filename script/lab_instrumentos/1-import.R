
# Datos generales -----------------------------------------------------------------------------

## script: 1-import.R
## función: importar datos de laboratorio de instrumentos
## autor: Matías Castillo
## fecha: viernes 8 octubre 2021

# Cargar paquetes -----------------------------------------------------------------------------
library(readxl)
library(data.table)
library(tm)

# Importación ---------------------------------------------------------------------------------

## Importamos base de datos
lab_instrumentosANID <- readxl::read_excel("data/lab_instrumentos/raw/Lista de problemas LAB 1.xlsx", sheet = "Lista") |> 
  data.table::as.data.table() |> 
  dtPipe::dtbl(j = .SD, .SDcols = 1:5) |> 
  `names<-`(c("lab", "instrumento", "problema", "k_problema_manual", "norm_problema"))

## Stop words
# stopWords <- readLines(con = "https://raw.githubusercontent.com/Alir3z4/stop-words/master/spanish.txt")

## A minuscula
lab_instrumentosANID <- lab_instrumentosANID[, lapply(.SD, tolower)]

## Eliminamos stopWords
for (i in stopWords) {
  lab_instrumentosANID[, norm_problema := gsub(pattern = paste0(" ", i," "), replacement = " ", x = norm_problema, fixed = TRUE)]
}

## Stopwords con criterios específicos
lab_instrumentosANID[, norm_problema := gsub(pattern = "el ", replacement = "", x = norm_problema, fixed = TRUE)]
lab_instrumentosANID[, norm_problema := gsub(pattern = "la ", replacement = "", x = norm_problema, fixed = TRUE)]
lab_instrumentosANID[, norm_problema := gsub(pattern = "las ", replacement = "", x = norm_problema, fixed = TRUE)]


m <- Corpus(VectorSource(lab_instrumentosANID$norm_problema)) |> 
  TermDocumentMatrix(control = list(minWordLength = c(1, Inf))) |> 
  removeSparseTerms(sparse = .98) |> 
  as.matrix()

hc <- m |> 
  scale() |> 
  dist() |> 
  hclust(method = "ward.D")

for (i in 5:20) {
  pdf(file = paste0("output/dendrogram_lab/dendrogram_lab_instrumentos_k",i,".pdf"), width = 12, height = 8)
  plot(hc)
  rect.hclust(hc, k = i)
  dev.off()
}

set.seed(12345)
m |> 
  t() |> 
  kmeans(centers = 9, nstart = 1, iter.max = 1000) |> 
  factoextra::fviz_cluster(data = t(m), 
                           geom = "point",
                           ggtheme = ggplot2::theme_bw())
