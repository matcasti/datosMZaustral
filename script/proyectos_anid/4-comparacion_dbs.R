
# ANID historicos proyectos adjudicados (modificado) ----

library(data.table)

source("script/proyectos_anid/1-import.R")
source("script/proyectos_anid/2-normalize.R")
source("script/proyectos_anid/3-db_modificada.R")

# Todos los códigos en mz_austral que no estén en dbp_modificado
mz_austral[!mz_austral$codigo_proyecto %in% dbp_modificado$codigo_proyecto]

# Todos los códigos en dbp_modificado que no estén en mz_austral
dbp_modificado[!dbp_modificado$codigo_proyecto %in% mz_austral$codigo_proyecto]

  ## Debiesen aparecer 4 proyectos 2021, ya que estos al estar agregados manualmente
  ## es esperable que no esten en la base de datos original