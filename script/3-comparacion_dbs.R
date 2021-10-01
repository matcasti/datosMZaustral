
# ANID historicos proyectos adjudicados (modificado) ----

library(data.table)

source("script/1-import.R")
source("script/2-db_modificada.R")

# Todos los códigos en mz_austral que no estén en dbp_modificado
mz_austral[!mz_austral$codigo_proyecto %in% dbp_modificado$codigo_proyecto]

# Todos los códigos en dbp_modificado que no estén en mz_austral
dbp_modificado[!dbp_modificado$codigo_proyecto %in% mz_austral$codigo_proyecto]
