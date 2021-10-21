## función: Generar nube de palabras
## fecha: 21-oct

# Preparar el espacio de trabajo --------------------------------------------------------------

library(qdap)
library(wordcloud)
library(data.table)

# Importar los datos --------------------------------------------------------------------------

data <- readRDS(file = "data/lab_instrumentos/clean/data.RDS")

## Importamos también stopwords
stopWords <- readLines(con = "https://raw.githubusercontent.com/Alir3z4/stop-words/master/spanish.txt")

# Producto ------------------------------------------------------------------------------------

## Generamos los términos frecuentes
terminos <- qdap::freq_terms(data$norm_problema, top = 40, stopwords = stopWords)

## Generamos nube de palabras
pdf(file = "output/lab_instrumentos/productos/nube_palabras.pdf", width = 8, height = 8)
set.seed(12345)
wordcloud(words = terminos$WORD, freq = terminos$FREQ, 
          rot.per = 0, 
          scale = c(5,1), 
          random.order = F)
dev.off()
