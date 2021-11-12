
# Datos generales -----------------------------------------------------------------------------

## script: 1-import.R
## función: importar datos de laboratorio de instrumentos
## autores: Matías Castillo, Carlos Morales
## fecha: viernes 8 octubre 2021

# Cargamos paquetes ---------------------------------------------------------------------------

library(data.table)
library(googlesheets4)

# Importamos los datos ------------------------------------------------------------------------

lab_instrumentos <- googlesheets4::read_sheet(
  ss = "1ct6zRIDli4AHjk8QsF4CYSwZzzTVGK0ncCDBXtXhmqY",
  sheet = "Lista P-C-E",
)

stopWords <- readLines("data-raw/stopwords.txt")

# Tratamiento previo --------------------------------------------------------------------------

lab_instrumentos <- data.table::as.data.table(lab_instrumentos)

names(lab_instrumentos) <- c("laboratorio", "grupo", "problema", "cluster_problema_manual",
                             "clean_problema", "cluster_causa_manual", "causa",
                             "clean_causa", "consecuencia", "clean_consecuencia",
                             "region")

# Tratamiento principal -----------------------------------------------------------------------

lab_instrumentos[, names(lab_instrumentos) := lapply(.SD, tolower)]

lab_instrumentos[, names(lab_instrumentos) := lapply(.SD, gsub, pattern = "\\.", replacement = "")]

lab_instrumentos[, names(lab_instrumentos) := lapply(.SD, gsub, pattern = "\\,", replacement = "")]

vars <- c("clean_problema", "clean_causa", "clean_consecuencia")

for (i in stopWords) {
  i_bound <- paste0("(?<!\\S)\\b", i, "\\b(?!\\S)")
  lab_instrumentos[, (vars) := lapply(.SD, gsub, pattern = i_bound, replacement = "", perl = T), .SDcols = vars]
}

lab_instrumentos[, names(lab_instrumentos) := lapply(.SD, gsub, pattern = "\\s+", replacement = " ")]

lab_instrumentos[, names(lab_instrumentos) := lapply(.SD, trimws)][]

# Tratamiento final (detalles) ----------------------------------------------------------------

## No necesario

# Exportamos los datos ------------------------------------------------------------------------

data.table::fwrite(lab_instrumentos, file = "data/lab_instrumentos/clean/data.csv")
saveRDS(object = lab_instrumentos, file = "data/lab_instrumentos/clean/data.RDS")
