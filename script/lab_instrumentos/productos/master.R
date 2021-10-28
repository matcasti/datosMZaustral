
# Master script - Productos -------------------------------------------------------------------

source(file = "script/lab_instrumentos/1-import.R", echo = TRUE)

carpeta <- paste0("script/lab_instrumentos/productos/", c("problemas/", "causas/", "consecuencias/"))

for (i in carpeta) {
  archivos <- paste0(i, list.files(i))
  for (i in archivos) {
    source(file = i) 
  }
}
