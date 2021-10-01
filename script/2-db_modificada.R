
# ANID historicos proyectos adjudicados (modificado) ----

library(data.table)

## Descargar los datos ----
dbp_modificado <- data.table::fread(input = "data/db_proyectos_modificada.csv", encoding = "UTF-8")

## Generamos nombres sintácticamente válidos ----
names(dbp_modificado) <- c("codigo_proyecto", "area_estrategica", "programa", "instrumento", 
                         "nombre_concurso", "ano_concurso", "ano_fallo", "nombre_proyecto", 
                         "area_ocde", "disciplina_detalle", "grupo_estudio", "duracion_meses", 
                         "ano_finalizacion", "tipo_beneficiario", "nombre_responsable", 
                         "sexo", "institucion_principal", "ubicacion_institucion", "macrozona", 
                         "region_ejecucion", "monto_adjudicado_miles")

# Exploración ---------------------------------------------------------------------------------

dbp_modificado$programa |> unique()

# [1] "FONDECYT"                                      "FONDEF"                                       
# [3] "EXPLORA"                                       "REGIONAL"                                     
# [5] "PCI"                                           "PROGRAMA BICENTENARIO DE CIENCIA Y TECNOLOGIA"
# [7] "INFORMACION CIENTIFICA"                        "PAI"                                          
# [9] "PIA"                                           "FONDEQUIP"                                    
# [11] "FONIS"                                        

dbp_modificado$region_ejecucion |> unique()

  # [1] "12. MAGALLANES Y ANTARTICA CHILENA" "11. AYSEN" "MULTIREGIONAL"          

dbp_modificado[i = programa != "EXPLORA", 
             j = .N, 
             by = region_ejecucion]

  #                      region_ejecucion   N
  # 1: 12. MAGALLANES Y ANTARTICA CHILENA 236
  # 2:                          11. AYSEN  52
  