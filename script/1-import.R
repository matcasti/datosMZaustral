
# ANID historicos proyectos adjudicados ----

## Obtener URL ----
url <- "https://raw.githubusercontent.com/ANID-GITHUB/Historico-de-Proyectos-Adjudicados/master/BDH_Proyectos.csv"

## Descargar los datos ----
db_proyectos <- data.table::fread(url, encoding = "UTF-8")

## Crear un respaldo local
data.table::fwrite(db_proyectos, file = "data/db_proyectos.csv")

## Generamos nombres sintácticamente válidos ----
# names(db_proyectos) <- c("codigo_proyecto", "n", "subdireccion", "programa_conicyt", 
#                          "instrumento", "nombre_curso", "año_curso", "año_fallo", 
#                          "nombre_proyecto", "area_ocde", "disciplina", "grupo_evaluacion", 
#                          "duracion_meses", "tipo_beneficiario", "nombre_responsable", 
#                          "sexo", "institucion_principal", "macrozona", 
#                          "region_ejecucion", "monto_adjudicado_miles", "sinfo_nosolicita")

# Ver error
unique(db_proyectos$INSTITUCION_PRINCIPAL)