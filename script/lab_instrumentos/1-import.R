
# Datos generales -----------------------------------------------------------------------------

## script: 1-import.R
## función: importar datos de laboratorio de instrumentos
## autor: Matías Castillo
## fecha: viernes 8 octubre 2021

# Cargar paquetes -----------------------------------------------------------------------------
library(readxl)
library(data.table)

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
  `names<-`(c("laboratorio", "grupo", "problema", "cluster_problema_manual",                                                                      
              "norm_problema", "cluster_causa_manual", "debido_a", 
              "normalizacion_causa", "conector_3", "consecuencias", "region", 
              "indicadores_relevantes_como_evidencia")) |> 
  .s(region == "1")

## Stop words
stopWords <- readLines(con = "https://raw.githubusercontent.com/Alir3z4/stop-words/master/spanish.txt")

## A minuscula
lab_instrumentosANID <- data_temp <- lab_instrumentosANID[, lapply(.SD, tolower)]

saveRDS(object = data_temp, file = "data/lab_instrumentos/raw/tempData.RDS")
rm(data_temp)

## Eliminamos stopWords
for (i in stopWords) {
  lab_instrumentosANID[, norm_problema := gsub(pattern = paste0(" ", i," "), replacement = " ", x = norm_problema, fixed = TRUE)]
}

## Stopwords con criterios específicos
lab_instrumentosANID[, norm_problema := gsub(pattern = "el ", replacement = "", x = norm_problema, fixed = TRUE)
                     ][, norm_problema := gsub(pattern = "la ", replacement = "", x = norm_problema, fixed = TRUE)
                       ][, norm_problema := gsub(pattern = "las ", replacement = "", x = norm_problema, fixed = TRUE)]

rm(stopWords, i)

# Guardamos los datos -------------------------------------------------------------------------

saveRDS(object = lab_instrumentosANID, file = "data/lab_instrumentos/clean/data.RDS")

 