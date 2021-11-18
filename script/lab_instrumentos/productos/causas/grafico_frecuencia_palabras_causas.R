## función: Generar grafico de frecuencia de palabras (CAUSAS)
## fecha: 25-oct

# Preparar el espacio de trabajo --------------------------------------------------------------

# Producto ------------------------------------------------------------------------------------

message("Iniciando gráfico de frecuencia de palabras - CAUSAS")

## Generamos los términos frecuentes
terminos <- labinstrumentos::get_terms(data$clean_causa, n_words = 1)

## Graficamos los términos frecuentes
pdf(file = "output/lab_instrumentos/productos/freq_palabras_causas.pdf", width = 8, height = 6);
plot(terminos);
dev.off()

message("✅Tarea completada")