## función: Generar grafico de frecuencia de palabras (CAUSAS)
## fecha: 25-oct

# Preparar el espacio de trabajo --------------------------------------------------------------

library(qdap)
library(data.table)

# Importar los datos --------------------------------------------------------------------------

data <- readRDS(file = "data/lab_instrumentos/clean/data.RDS")

# Producto ------------------------------------------------------------------------------------

## Generamos los términos frecuentes
terminos <- qdap::freq_terms(data$normalizacion_causa, top = 30)

## Graficamos los términos frecuentes
pdf(file = "output/lab_instrumentos/productos/freq_palabras_causas.pdf", width = 8, height = 6);
plot(terminos);
dev.off()
