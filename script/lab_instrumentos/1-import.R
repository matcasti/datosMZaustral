
# Datos generales -----------------------------------------------------------------------------

## script: 1-import.R
## función: importar datos de laboratorio de instrumentos
## autores: Matías Castillo, Carlos Morales
## fecha: viernes 8 octubre 2021

# Cargar paquetes -----------------------------------------------------------------------------
library(gsheet)
library(data.table)

# Funciones auxiliares ------------------------------------------------------------------------
.s <- function(x, ...) {
  stopifnot(inherits(x, "data.table"))
  temp <- substitute(x[...])
  eval(temp)
}

# Importación desde google----------------------------------------------------------------------

url <- 'https://docs.google.com/spreadsheets/d/1ct6zRIDli4AHjk8QsF4CYSwZzzTVGK0ncCDBXtXhmqY/edit#gid=34243368'

# Importación ---------------------------------------------------------------------------------

## Importamos base de datos
lab_instrumentosANID <- gsheet::gsheet2tbl(url) |>
  data.table::as.data.table() |>
  `names<-`(c("laboratorio", "grupo", "problema", "cluster_problema_manual",                                                                      
              "clean_problema", "cluster_causa_manual", "debido_a", 
              "clean_causa", "conector_3", "consecuencias", "clean_consecuencias", 
              "region", "indicadores_relevantes_como_evidencia")) |> 
  .s(region == "1")

## A minuscula
lab_instrumentosANID <- lab_instrumentosANID[, lapply(.SD, tolower)]

## Archivo local para 
local({
  data_temp <- copy(lab_instrumentosANID)
  saveRDS(object = data_temp, file = "data/lab_instrumentos/raw/tempData.RDS")
})

# Limpieza de texto ---------------------------------------------------------------------------

# Stop words
stopWords <- readLines(con = "https://raw.githubusercontent.com/Alir3z4/stop-words/master/spanish.txt")
stopWords <- c(stopWords, "\\.", "\\,")

# Proceso de eliminar stopwords y puntuación de columnas de interés
for (j in c("clean_problema", "clean_causa", "clean_consecuencias")) {
  
  ## Añadir un espacio antes de cada vector
  lab_instrumentosANID[, c(j) := paste0(" ", get(j))]
  
  ## Removiendo los stopwords
  for (i in stopWords) {
    lab_instrumentosANID[, c(j) := gsub(pattern = paste0(" ", i," "), replacement = " ", x = get(j), fixed = TRUE)]
  }
  
  ## Quitar el espacio añadido previamente al comienzo del vector
  lab_instrumentosANID[, c(j) := gsub(pattern = "^.", replacement = "", x = get(j))]
}

rm(i, j, stopWords, url)

# Guardamos los datos -------------------------------------------------------------------------

saveRDS(object = lab_instrumentosANID, file = "data/lab_instrumentos/clean/data.RDS")
