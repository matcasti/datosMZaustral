
# ANID historicos proyectos adjudicados ----

## Obtener URL ----
url <- "https://raw.githubusercontent.com/ANID-GITHUB/Historico-de-Proyectos-Adjudicados/master/BDH_Proyectos.csv"

## Descargar los datos ----
db_proyectos <- data.table::fread(url)

## Crear un respaldo local
data.table::fwrite(db_proyectos, file = "data/db_proyectos.csv")
