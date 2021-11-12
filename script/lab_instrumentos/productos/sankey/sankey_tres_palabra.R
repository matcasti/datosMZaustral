
# Preparamos espacio de trabajo ---------------------------------------------------------------

## Cargamos los paquetes
library(qdap)
library(highcharter)
library(data.table)

## Funciones auxiliares
### Tokenizar grupos de dos palabras
twogramTokenizer <- function(x) {
  RWeka::NGramTokenizer(
    x = x, 
    control = RWeka::Weka_control(
      min = 3, 
      max = 3
    )
  )
}

## Cargamos los datos
data <- readRDS(file = "data/lab_instrumentos/clean/data.RDS")

#  Cargamos los stopwords
stopWords <- readLines(con = "https://raw.githubusercontent.com/Alir3z4/stop-words/master/spanish.txt")

# Causa ---------------------------------------------------------------------------------------

db_causa <- local({
  # Creamos un corpus
  m <- tm::VCorpus(
    x = tm::VectorSource(
      x = unique(data[["clean_causa"]])
    )
  )
  
  ## Matriz de documentos con dos términos
  m <- tm::DocumentTermMatrix(
    x = m, 
    ### Aplicamos función tokenizadora
    control = list(tokenize = twogramTokenizer)
  )
  
  ## Creamos el conteo de palabras por dos términos
  m <- m |> 
    as.matrix() |> 
    colSums() |> 
    sort(decreasing = TRUE)
  
  terminos <- data.table(WORD = names(m), freq = m)
  
  db_causa <- NULL
  for (i in terminos$WORD) {
    l <- data[j = list(word = i, freq = sum(as.numeric(unique(clean_causa) %like% i))), 
              by = grupo
              ][j = list(weight = sum(freq)), 
                by = .(grupo, word)]
    
    db_causa <- rbind(db_causa, l)
  }
  
  db_causa[weight > 0]
})

# Problema ------------------------------------------------------------------------------------

db_problema <- local({
  # Creamos un corpus
  m <- tm::VCorpus(
    x = tm::VectorSource(
      x = unique(data[["clean_problema"]])
    )
  )
  
  ## Matriz de documentos con dos términos
  m <- tm::DocumentTermMatrix(
    x = m, 
    ### Aplicamos función tokenizadora
    control = list(tokenize = twogramTokenizer)
  )
  
  ## Creamos el conteo de palabras por dos términos
  m <- m |> 
    as.matrix() |> 
    colSums() |> 
    sort(decreasing = TRUE)
  
  terminos <- data.table(WORD = names(m), freq = m)
  
  db_problema <- NULL
  for (i in terminos$WORD) {
    l <- data[j = list(word = i, freq = sum(as.numeric(unique(clean_problema) %like% i))), 
              by = grupo
    ][j = list(weight = sum(freq)), 
      by = .(grupo, word)]
    
    db_problema <- rbind(db_problema, l)
  }
  
  db_problema[weight > 0]
})

# Consecuencia --------------------------------------------------------------------------------

db_consecuencia <- local({
  # Creamos un corpus
  m <- tm::VCorpus(
    x = tm::VectorSource(
      x = unique(data[["clean_consecuencia"]])
    )
  )
  
  ## Matriz de documentos con dos términos
  m <- tm::DocumentTermMatrix(
    x = m, 
    ### Aplicamos función tokenizadora
    control = list(tokenize = twogramTokenizer)
  )
  
  ## Creamos el conteo de palabras por dos términos
  m <- m |> 
    as.matrix() |> 
    colSums() |> 
    sort(decreasing = TRUE)
  
  terminos <- data.table(WORD = names(m), freq = m)
  
  db_consecuencia <- NULL
  for (i in terminos$WORD) {
    l <- data[j = list(word = i, freq = sum(as.numeric(unique(clean_consecuencia) %like% i))), 
              by = grupo
    ][j = list(weight = sum(freq)), 
      by = .(grupo, word)]
    
    db_consecuencia <- rbind(db_consecuencia, l)
  }
  
  db_consecuencia[weight > 0]
})


# Unimos todo ---------------------------------------------------------------------------------


db <- rbind(
  cbind(db_problema, categoria = "problema"),
  cbind(db_causa, categoria = "causa"),
  cbind(db_consecuencia, categoria = "consecuencia")
)[weight > 1]

l <- NULL
for (i in 1:nrow(db)) {
  .copy <- db[i]
  for (j in 1:.copy$weight) {
    l <- rbind(l, .copy[,-3])
  }
}

hc <- data_to_sankey(l) |> 
  highcharter::hchart("sankey")

htmlwidgets::saveWidget(hc, file = "output/lab_instrumentos/productos/sankey/tres_palabras.html")
