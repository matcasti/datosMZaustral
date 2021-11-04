
# Master script - Productos -------------------------------------------------------------------

source(file = "script/lab_instrumentos/1-import.R", echo = TRUE)

carpeta <- paste0("script/lab_instrumentos/productos/", c("problemas/", "causas/", "consecuencias/"))

for (i in carpeta) {
  archivos <- paste0(i, list.files(i))
  for (i in archivos) {
    source(file = i) 
  }
}

# Un palabra ----------------------------------------------------------------------------------

wordcloud_data <- rbind.data.frame(
  cbind(problema, `P-C-C` = "problema"),
  cbind(causas, `P-C-C` = "causa"),
  cbind(consecuencias, `P-C-C` = "consecuencia"),
  make.row.names = FALSE
)

## Guardamos los archivos
googlesheets4::write_sheet(data = wordcloud_data, ss = "16IGJFT63uVC0uerATaEOS6NK4IJ7iOLQ6xoBmIowz58", sheet = "ccp")


# Dos palabras --------------------------------------------------------------------------------

twoword_data <- rbind.data.frame(
  data.frame(WORD = names(twogram_freq_problema), FREQ = as.numeric(twogram_freq_problema), `P-C-C` = "problema", check.names = FALSE),
  data.frame(WORD = names(twogram_freq_causas), FREQ = as.numeric(twogram_freq_causas), `P-C-C` = "causas", check.names = FALSE),
  data.frame(WORD = names(twogram_freq_consecuencias), FREQ = as.numeric(twogram_freq_consecuencias), `P-C-C` = "consecuencia", check.names = FALSE),
  make.row.names = FALSE
)

## Guardamos los archivos
googlesheets4::write_sheet(data = twoword_data, ss = "16IGJFT63uVC0uerATaEOS6NK4IJ7iOLQ6xoBmIowz58", sheet = "two-words")

# Tres palabras --------------------------------------------------------------------------------

threeword_data <- rbind.data.frame(
  data.frame(WORD = names(threegram_freq_problema), FREQ = as.numeric(threegram_freq_problema), `P-C-C` = "problema", check.names = FALSE),
  data.frame(WORD = names(threegram_freq_causas), FREQ = as.numeric(threegram_freq_causas), `P-C-C` = "causas", check.names = FALSE),
  data.frame(WORD = names(threegram_freq_consecuencias), FREQ = as.numeric(threegram_freq_consecuencias), `P-C-C` = "consecuencia", check.names = FALSE),
  make.row.names = FALSE
)

## Guardamos los archivos
googlesheets4::write_sheet(data = threeword_data, ss = "16IGJFT63uVC0uerATaEOS6NK4IJ7iOLQ6xoBmIowz58", sheet = "three-words")

