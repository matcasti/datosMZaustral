
# Datos generales -----------------------------------------------------------------------------

## script: 3-diagnostics.R
## función: evaluar el ajuste de los modelos a los clusteres manuales
## autor: Matías Castillo
## fecha: viernes 15 octubre 2021

# Cargar paquetes -----------------------------------------------------------------------------

  library(data.table)

# Cargamos modelos generados anteriormente ----------------------------------------------------

  source("script/lab_instrumentos/2-clustering.R", echo = F) # Aquí también se cargan los datos trabajados
                                                             # en el script anterior
  
  ## Con esto convertimos cada problema único a un número                                                           
  lab_instrumentosANID[, norm_problema_num := as.numeric(as.factor(norm_problema))]
  
  ## Funciones auxiliares
  .xuniq <- function(i) {
    x <- unique(i)
    if (length(x) > 1) {
      paste0(x, collapse = "+")
    } else if (length(x) == 1) {
      paste0(x)
    } else " - "
  }

# Evaluación diagnóstica de los modelos creados -----------------------------------------------

  ## 1. Clusterización jerárquica -------------------------------------------------------------
  
    hc <- dcast(
      data = lab_instrumentosANID, 
      formula = norm_problema_num ~ norm_problema_hclust, 
      fun.aggregate = .xuniq,
      value.var = "norm_problema_num",
      margins = T
    )
    
  ## 2. Clusterización no jerárquica mediante K-means -----------------------------------------
  
    km <- dcast(
      data = lab_instrumentosANID, 
      formula = norm_problema_num ~ norm_problema_kmeans, 
      fun.aggregate = .xuniq,
      value.var = "norm_problema_num"
    )
  
  ## 3. Density based clustering --------------------------------------------------------------

    d <- dcast(
      data = lab_instrumentosANID, 
      formula = norm_problema_num ~ norm_problema_dbscan, 
      fun.aggregate = .xuniq,
      value.var = "norm_problema_num"
    )

  ## 4. K-mediods -----------------------------------------------------------------------------
  
    kmed <- dcast(
      data = lab_instrumentosANID, 
      formula = norm_problema_num ~ norm_problema_kmed, 
      fun.aggregate = .xuniq,
      value.var = "norm_problema_num"
    )

  ## Resumen ----------------------------------------------------------------------------------

  knitr::kable(
    x = cbind(
      `colnames<-`(hc, c("Problemas normalizados", 1:10)), 
      `colnames<-`(km[,-1], paste(1:4)), 
      `colnames<-`(d[,-1], paste(1:13)), 
      `colnames<-`(kmed[,-1], paste(1:4))
        ),
  ) |> 
    kableExtra::add_header_above(
      header = c(" " = 1, "Ward-D" = 10, "K-means" = 4, "Density-based" = 13, "K-medians" = 4)
    ) |> 
    kableExtra::column_spec(column = 1:32, width = "3cm") |> 
    kableExtra::kable_styling(full_width = F, )

  