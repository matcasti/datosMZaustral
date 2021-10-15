## Importamos base de datos
source("script/lab_instrumentos/1-import.R")

## Eliminamos el término macrozona-austral, debido a su alta repetición y ser un término
## muy influyente sin importar información adicional
lab_instrumentosANID[j = norm_problema := gsub(pattern = "macrozona-austral", 
                                               replacement = "", 
                                               x = norm_problema)]


# Con todas las observaciones -----------------------------------------------------------------

source("script/lab_instrumentos/2-clustering.R")

  ## Batería diagnóstica ----------------------------------------------------------------------
  
  ### Ward-D
  pdf(file = "output/lab_instrumentos/comparacion/hclust1-con.pdf")
  plot(hc); rect.hclust(hc, k = 5)
  dev.off()
  
  pdf(file = "output/lab_instrumentos/comparacion/hclust2-con.pdf")
  cmdscale(
    d = scale(m) |> 
      dist()
  ) |> 
    plot(
      col = cutree(hc, k = 5) |> 
        as.factor(), 
      pch = 17, 
      cex = 2
    )
  dev.off()
  
  
  ### K-Means
  pdf(file = "output/lab_instrumentos/comparacion/kmeans-con.pdf")
  km |> 
    factoextra::fviz_cluster(data = t(m), 
                             geom = "point",
                             ggtheme = ggplot2::theme_bw())
  dev.off()
  
  ### DBC
  pdf(file = "output/lab_instrumentos/comparacion/dbc-con.pdf")
  factoextra::fviz_cluster(d, t(m), geom = "point")
  dev.off()
  
  ### K-Medians
  pdf(file = "output/lab_instrumentos/comparacion/kmod-con.pdf")
  factoextra::fviz_cluster(kmed, data = t(m))
  dev.off()

# Solo region == 1 ----------------------------------------------------------------------------

  lab_instrumentosANID <- copy(lab_instrumentosANID)[region == "1"]
  source("script/lab_instrumentos/2-clustering.R")
  
  ## Batería diagnóstica ----------------------------------------------------------------------
  
  ### Ward-D
  pdf(file = "output/lab_instrumentos/comparacion/hclust1-sin.pdf")
  plot(hc); rect.hclust(hc, k = 5)
  dev.off()
  
  pdf(file = "output/lab_instrumentos/comparacion/hclust2-sin.pdf")
  cmdscale(
    d = scale(m) |> 
      dist()
  ) |> 
    plot(
      col = cutree(hc, k = 5) |> 
        as.factor(), 
      pch = 17, 
      cex = 2
    )
  dev.off()
  
  
  ### K-Means
  pdf(file = "output/lab_instrumentos/comparacion/kmeans-sin.pdf")
  km |> 
    factoextra::fviz_cluster(data = t(m), 
                             geom = "point",
                             ggtheme = ggplot2::theme_bw())
  dev.off()
  
  ### DBC
  pdf(file = "output/lab_instrumentos/comparacion/dbc-sin.pdf")
  factoextra::fviz_cluster(d, t(m), geom = "point")
  dev.off()
  
  ### K-Medians
  pdf(file = "output/lab_instrumentos/comparacion/kmod-sin.pdf")
  factoextra::fviz_cluster(kmed, data = t(m))
  dev.off()
  