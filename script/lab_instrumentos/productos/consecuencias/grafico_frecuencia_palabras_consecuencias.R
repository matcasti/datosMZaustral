## función: Generar grafico de frecuencia de palabras (CONSECUENCIAS)
## fecha: 25-oct

# Preparar el espacio de trabajo --------------------------------------------------------------

library(qdap)
library(data.table)

# Importar los datos --------------------------------------------------------------------------

data <- readRDS(file = "data/lab_instrumentos/clean/data.RDS")

# Producto ------------------------------------------------------------------------------------

message("Iniciando gráfico de frecuencia de palabras - CONSECUENCIAS")

## Generamos los términos frecuentes
terminos <- qdap::freq_terms(data$clean_consecuencia, top = 30)

## Graficamos los términos frecuentes
pdf(file = "output/lab_instrumentos/productos/freq_palabras_consecuencias.pdf", width = 8, height = 6);
plot(terminos);
dev.off()

message("✅Tarea completada")