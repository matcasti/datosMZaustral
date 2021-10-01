
# ANID historicos proyectos adjudicados (modificado) ----

library(data.table)

## Descargar los datos ----
dbp_modificado <- data.table::fread(input = "data/proyectos_anid/raw/db_proyectos_modificada.csv", encoding = "UTF-8")

## Generamos nombres sintácticamente válidos ----
names(dbp_modificado) <- c("codigo_proyecto", "area_estrategica", "programa", "instrumento", 
                         "nombre_concurso", "ano_concurso", "ano_fallo", "nombre_proyecto", 
                         "area_ocde", "disciplina_detalle", "grupo_estudio", "duracion_meses", 
                         "ano_finalizacion", "tipo_beneficiario", "nombre_responsable", 
                         "sexo", "institucion_principal", "ubicacion_institucion", "macrozona", 
                         "region_ejecucion", "monto_adjudicado_miles")