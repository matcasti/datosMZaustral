## función: Generar nube de palabras (CAUSAS)
## fecha: 21-oct

# Preparar el espacio de trabajo --------------------------------------------------------------

library(qdap)
library(wordcloud)
library(data.table)

# Importar los datos --------------------------------------------------------------------------

data <- readRDS(file = "data/lab_instrumentos/clean/data.RDS") |> 
  # Eliminamos el término macrozona-austral por ser muy influyente en el modelado
  .s(j = norm_consecuencias := gsub(pattern = "macrozona-austral", replacement = "", x = norm_consecuencias)) |> 
  .s(j = norm_consecuencias := gsub(pattern = "anid", replacement = "", x = norm_consecuencias))

## Importamos también stopwords
stopWords <- readLines(con = "https://raw.githubusercontent.com/Alir3z4/stop-words/master/spanish.txt")

# Producto ------------------------------------------------------------------------------------

message("Iniciando gráfico de nube de palabras - CONSECUENCIAS")

## Generamos los términos frecuentes
terminos <- qdap::freq_terms(data$normalizacion_causa, top = 40, stopwords = stopWords)

## Generamos nube de palabras
pdf(file = "output/lab_instrumentos/productos/nube_palabras_consecuencia.pdf", width = 12, height = 12)
set.seed(12345)
wordcloud(words = terminos$WORD, freq = terminos$FREQ, 
          rot.per = 0, 
          scale = c(5,1), 
          random.order = F)
dev.off()

message("✅Tarea completada")