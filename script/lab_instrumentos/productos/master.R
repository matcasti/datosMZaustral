
# Master script - Productos -------------------------------------------------------------------

paths <- paste0("script/lab_instrumentos/productos/", c("problemas/", "causas/"))

for (i in paths) {
  archivos <- paste0(i, list.files(i))
  for (i in archivos) {
    source(file = i)
  }
}
