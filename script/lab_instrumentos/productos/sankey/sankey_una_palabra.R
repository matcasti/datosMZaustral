
# Preparamos espacio de trabajo ---------------------------------------------------------------

## Cargamos los paquetes
library(qdap)
library(highcharter)
library(data.table)

## Cargamos los datos
data <- readRDS(file = "data/lab_instrumentos/clean/data.RDS")

#  Cargamos los stopwords
stopWords <- readLines(con = "https://raw.githubusercontent.com/Alir3z4/stop-words/master/spanish.txt")


# Procesamiento -------------------------------------------------------------------------------


# Causa ---------------------------------------------------------------------------------------

# Obtenemos frecuencias de palabras
terminos <- qdap::freq_terms(data$clean_causa, top = 15, stopwords = stopWords)

db_causa <- NULL
for (i in terminos$WORD) {
  l <- data[, list(word = i, freq = as.numeric(like(clean_causa, i))), grupo][, list(weight = sum(freq)), .(grupo, word)]
  db_causa <- rbind(db_causa, l)
}


# Problema ------------------------------------------------------------------------------------

# Obtenemos frecuencias de palabras
terminos <- qdap::freq_terms(data$clean_problema, top = 15, stopwords = stopWords)

db_problema <- NULL
for (i in terminos$WORD) {
  l <- data[, list(word = i, freq = as.numeric(like(clean_problema, i))), grupo][, list(weight = sum(freq)), .(grupo, word)]
  db_problema <- rbind(db_problema, l)
}

# Consecuencia --------------------------------------------------------------------------------


# Obtenemos frecuencias de palabras
terminos <- qdap::freq_terms(data$clean_consecuencias, top = 15, stopwords = stopWords)

db_consecuencia <- NULL
for (i in terminos$WORD) {
  l <- data[, list(word = i, freq = as.numeric(like(clean_consecuencias, i))), grupo][, list(weight = sum(freq)), .(grupo, word)]
  db_consecuencia <- rbind(db_consecuencia, l)
}


# Unimos todo ---------------------------------------------------------------------------------


db <- rbind(
  cbind(db_problema, categoria = "problema"),
  cbind(db_causa, categoria = "causa"),
  cbind(db_consecuencia, categoria = "consecuencia")
)[weight > 0]

l <- NULL
for (i in 1:nrow(db)) {
  .copy <- db[i]
  for (j in 1:.copy$weight) {
    l <- rbind(l, .copy[,-3])
  }
}

hc <- data_to_sankey(l) |> 
  highcharter::hchart("sankey")

htmlwidgets::saveWidget(hc, file = "output/lab_instrumentos/productos/sankey/una_palabras.html")