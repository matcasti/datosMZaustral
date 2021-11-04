
# Datos generales -----------------------------------------------------------------------------

## script: 4-diagnostics.R
## función: evaluar el ajuste de los modelos a los clusteres manuales
## autor: Matías Castillo
## fecha: viernes 15 octubre 2021

# Cargar paquetes -----------------------------------------------------------------------------

  library(data.table)

# Cargamos modelos generados anteriormente ----------------------------------------------------

  ## Con esto convertimos cada problema único a un número                                                           
  lab_instrumentosANID[, clean_problema_num := as.numeric(as.factor(clean_problema))]
  
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
      formula = clean_problema_num ~ clean_problema_hclust, 
      fun.aggregate = .xuniq,
      value.var = "clean_problema_num",
      margins = T
    )
    
  ## 2. Clusterización no jerárquica mediante K-means -----------------------------------------
  
    km <- dcast(
      data = lab_instrumentosANID, 
      formula = clean_problema_num ~ clean_problema_kmeans, 
      fun.aggregate = .xuniq,
      value.var = "clean_problema_num"
    )
  
  ## 3. Density based clustering --------------------------------------------------------------

    d <- dcast(
      data = lab_instrumentosANID, 
      formula = clean_problema_num ~ clean_problema_dbscan, 
      fun.aggregate = .xuniq,
      value.var = "clean_problema_num"
    )

  ## 4. K-mediods -----------------------------------------------------------------------------
  
    kmed <- dcast(
      data = lab_instrumentosANID, 
      formula = clean_problema_num ~ clean_problema_kmed, 
      fun.aggregate = .xuniq,
      value.var = "clean_problema_num"
    )

  ## Resumen ----------------------------------------------------------------------------------

  knitr::kable(
    x = cbind(
      `colnames<-`(hc, c("Problemas normalizados", 1:3)), 
      `colnames<-`(km[,-1], paste(1:4)), 
      `colnames<-`(d[,-1], paste(1:10)), 
      `colnames<-`(kmed[,-1], paste(1:4))
        ),
  ) |> 
    kableExtra::add_header_above(
      header = c(" " = 1, "Ward-D" = 3, "K-means" = 4, "Density-based" = 10, "K-medians" = 4)
    ) |> 
    kableExtra::column_spec(column = 1:22, width = "3cm") |> 
    kableExtra::kable_styling(full_width = F, )


# Problemas normalizados por instrumento ------------------------------------------------------

  data_temp <- readRDS("data/lab_instrumentos/raw/tempData.RDS")
  
  temp2 <- merge(
    x = lab_instrumentosANID[region == 1,], 
    y = data_temp[region == 1, .(clean_problema2 = clean_problema, problema)] |> unique(), 
    all.x = TRUE,
    by = "problema"
  )
  
  tabla_1 <- temp2[j = .(clean_problema2, clean_problema_num), by = instrumento] |> 
    unique() |> 
    dcast(clean_problema2 + clean_problema_num ~ instrumento, value.var = "clean_problema_num")
  
  total <- tabla_1[, 3:8][, lapply(.SD, \(i) length(i[!is.na(i)])) |> 
                            append(x = list(clean_problema2 = "Total", clean_problema_num = NA))]
  
  tabla_1 <- rbind(tabla_1, total)

  kableExtra::his