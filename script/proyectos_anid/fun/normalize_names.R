# Datos generales -----------------------------------------------------------------------------

## script: normalize_names.R
## función: Normalizar nombres de macrozona austral
## autor: Matías Castillo
## fecha: viernes 1 octubre 2021

## Cargamos librarias
library(data.table)

## Cargamos db macrozona austral
if (!exists("mz_austral")) {
  db_proyectos <- readRDS(file = "data/proyectos_anid/raw/db_proyectos.RDS")
}

# Función -------------------------------------------------------------------------------------

names. <- unique(db_proyectos$nombre_responsable)
matchs <- sapply(names., agrep, names., value = TRUE, max.distance = 0)
filtered_matchs <- Filter(f = function(i) length(i) > 1, x = matchs)
