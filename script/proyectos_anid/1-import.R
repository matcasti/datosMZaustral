
# Datos generales -----------------------------------------------------------------------------

## script: 1-import.R
## función: importar datos de proyectos adjudicados históricos ANID
## autor: Matías Castillo
## fecha: viernes 1 octubre 2021

# ANID historicos proyectos adjudicados -------------------------------------------------------

## Cargamos paquetes
library(data.table)

## Obtener URL
url <- "https://raw.githubusercontent.com/ANID-GITHUB/Historico-de-Proyectos-Adjudicados/master/BDH_Proyectos.csv"

## Descargar los datos
db_proyectos <- data.table::fread(url, encoding = "UTF-8")

## Generamos nombres sintácticamente válidos
names(db_proyectos) <- c("codigo_proyecto", "n", "subdireccion", "programa",
                         "instrumento", "nombre_concurso", "año_curso", "año_fallo",
                         "nombre_proyecto", "area_ocde", "disciplina", "grupo_evaluacion",
                         "duracion_meses", "tipo_beneficiario", "nombre_responsable",
                         "sexo", "institucion_principal", "macrozona",
                         "region_ejecucion", "monto_adjudicado_miles", "sinfo_nosolicita")

## Crear un respaldo local
saveRDS(db_proyectos, file = "data/proyectos_anid/raw/db_proyectos.RDS")