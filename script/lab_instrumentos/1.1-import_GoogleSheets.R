# Datos generales -----------------------------------------------------------------------------

## script: 1-import.R
## función: importar datos de laboratorio de instrumentos
## autor: Carlos Morales
## fecha: martes 26 octubre 2021

# Instalar paquetes -----------------------------------------------------------------------------

#install.packages('gsheet')

# Cargar paquetes -----------------------------------------------------------------------------

library(gsheet)
library(readxl)
library(data.table)

# Funciones auxiliares ------------------------------------------------------------------------
.s <- function(x, ...) {
  stopifnot(inherits(x, "data.table"))
  temp <- substitute(x[...])
  eval(temp)
}

# Importación desde google----------------------------------------------------------------------

url <- 'https://docs.google.com/spreadsheets/d/1zOwKpnzFKfzOX3u_yG4dtvNDK0eB1vLi/edit#gid=34243368'

# Importación ---------------------------------------------------------------------------------

## Importamos base de datos
lab_instrumentosANID <- gsheet2tbl(url) |> 
  data.table::as.data.table() |> 
  `names<-`(c("laboratorio", "grupo", "problema", "cluster_problema_manual",                                                                      
              "norm_problema", "cluster_causa_manual", "debido_a", 
              "normalizacion_causa", "conector_3", "consecuencias", "region", 
              "indicadores_relevantes_como_evidencia")) |> 
  .s(region == "1")
