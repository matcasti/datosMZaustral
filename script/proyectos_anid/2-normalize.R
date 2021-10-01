
# Datos generales -----------------------------------------------------------------------------

## script: 2-normalize.R
## función: Normalizar base de datos importada
## autor: Matías Castillo
## fecha: viernes 1 octubre 2021

# ANID historicos proyectos adjudicados -------------------------------------------------------

## Cargamos paquetes
library(data.table)

## Importamos los datos
db_proyectos <- readRDS(file = "data/proyectos_anid/raw/db_proyectos.RDS")

# Normalización -------------------------------------------------------------------------------

#' @description Todos los carácteres de nombre del proyecto, responsable e institución principal las
#' transformamos a mayúsculas
db_proyectos[j = `:=`(
  nombre_proyecto = toupper(nombre_proyecto),
  nombre_responsable = toupper(nombre_responsable),
  institucion_principal = toupper(institucion_principal)
)]

#' @description Modificar la región de ejecución y la macrozona de aquellas personas que su institución
#' principal contenga la palabra 'MAGALLANES' y su región de ejecución no fuera la Región de Magallanes,
#' ni fuera de macrozona 'MULTIREGIONAL'.
db_proyectos[i = institucion_principal %like% "MAGALLANES" & 
               region_ejecucion != "12. MAGALLANES Y ANTARTICA CHILENA" &
               macrozona != "MULTIREGIONAL", 
             j = `:=`(region_ejecucion = "12. MAGALLANES Y ANTARTICA CHILENA",
                      macrozona = "AUSTRAL")]

#' @description Modificar la región de ejecución y la macrozona de aquellas personas que su institución
#' principal contenga la palabra 'AYSEN' y su macrozona no sea 'MULTIREGIONAL'.
db_proyectos[i = institucion_principal %like% "AYSEN" & 
               macrozona != "MULTIREGIONAL", 
             j = `:=`(region_ejecucion = "11. AYSEN",
                      macrozona = "AUSTRAL")]

#' @description Modificar la región de ejecución, la institución principal y la macrozona de aquellas 
#' personas que en entrevistas indicaron que se habían cambiado de institución
db_proyectos[i = codigo_proyecto %in% c(3200226, 3180754),
             j = `:=`(institucion_principal = "UNIVERSIDAD DE AYSEN",
                      region_ejecucion = "11. AYSEN",
                      macrozona = "AUSTRAL")]

#' @description Normalizar CENTRO DE ESTUDIOS DEL CUATERNARIO DE FUEGO-PATAGONIA Y ANTARTICA-CEQUA
db_proyectos[i = institucion_principal %like% "CUATERNARIO", 
             j = institucion_principal := "CEQUA"]


#' @description Normalizar institución CIEP
db_proyectos[i = institucion_principal %like% "ECOSISTEMAS" & 
               institucion_principal %like% "PATAG" | 
               institucion_principal %like% "CIEP",
             j = `:=`(institucion_principal = "CIEP",
                      macrozona = "AUSTRAL",
                      region_ejecucion = "11. AYSEN")]


#' @description Eliminar registros de proyectos a los cuales los responsables renunciaron al financiamiento
db_proyectos <- db_proyectos[!codigo_proyecto %in% c("3170733", "3180280", "PAI77180074")] 

#' @description Eliminar registros duplicado
db_proyectos <- db_proyectos[n != 21894]

#' @description Modificar código de proyecto 'repetido'
db_proyectos <- db_proyectos[n == 12840, codigo_proyecto := "SIN INFORMACION2"]

#' @description Normalizar UNIVERSIDAD DE MAGALLANES
db_proyectos[i = institucion_principal %like% "UNIV" & institucion_principal %like% "MAG",
             j = institucion_principal := "UNIVERSIDAD DE MAGALLANES"]

#' @description Normalizar UNIVERSIDAD DE AYSEN
db_proyectos[i = institucion_principal %like% "UNIV" & institucion_principal %like% "AYSEN",
             j = institucion_principal := "UNIVERSIDAD DE AYSEN"]

#' @description Normalizar INSTITUTO ANTARTICO CHILENO
db_proyectos[i = institucion_principal %like% "INST" & institucion_principal %like% "ANTAR",
             j = institucion_principal := "INSTITUTO ANTARTICO CHILENO"]

#' @description Normalizar ESCUELA ARTURO PRAT
db_proyectos[i = institucion_principal %like% "ARTURO" & institucion_principal %like% "PRAT" &
               !institucion_principal %like% "UNIV" & region_ejecucion == "12. MAGALLANES Y ANTARTICA CHILENA",
             j = institucion_principal := "ESCUELA CAPITAN ARTURO PRAT"]

#' @description Normalizar ESCUELA ARTURO PRAT
db_proyectos[i = institucion_principal %like% "LICEO SAN JOSE ",
             j = institucion_principal := "LICEO SAN JOSE U.R."]

#' @description Normalizar COLEGIO SANTA TERESA DE LOS ANDES
db_proyectos[i = institucion_principal %like% "SANTA TERESA" & 
               institucion_principal %like% "ANDES",
             j = institucion_principal := "COLEGIO SANTA TERESA DE LOS ANDES"]

#' @description Generamos un subset sólo con los proyectos adjudicados con región de ejecición Aysén 
#' y Magallanes
mz_austral <- db_proyectos[i = region_ejecucion %in% c("11. AYSEN", "12. MAGALLANES Y ANTARTICA CHILENA") | 
                             institucion_principal %like% "MAGALLANES"]

# Guardamos los datos -------------------------------------------------------------------------

## Como objetos binarios (rápida carga)
saveRDS(db_proyectos, file = "data/proyectos_anid/clean/db_proyectos_normalized.RDS")
saveRDS(mz_austral, file = "data/proyectos_anid/clean/mz_austral_normalized.RDS")

## Y como archivos csv
data.table::fwrite(db_proyectos, file = "data/proyectos_anid/clean/db_proyectos_normalized.csv")
data.table::fwrite(mz_austral, file = "data/proyectos_anid/clean/mz_austral_normalized.csv")