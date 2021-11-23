
# Master script - Productos -------------------------------------------------------------------

library(data.table)
library(googlesheets4)
library(labinstrumentos)


lab_instrumentos <- obtener_datos()[region == 1]

# Variables de interés
vars <- c("clean_problema", "clean_causa", "clean_consecuencia", "clean_soluciones")

# Eliminamos el término macrozona-austral por se muy influyente
lab_instrumentos[j = (vars) := lapply(.SD, gsub, pattern = "macrozona-austral", replacement = ""), 
                 .SDcols = vars]

lab_instrumentos_long <- melt.data.table(
  data = lab_instrumentos,
  measure.vars = c("clean_problema", "clean_causa", "clean_consecuencia", "clean_soluciones")
)

# dcast.data.table(
#   data = lab_instrumentos_long[, get_terms(value), .(variable)],
#   formula = WORD ~ variable, value.var = "FREQ"
# ) |> View()


# Una palabra

one <- lab_instrumentos_long[, obtener_terminos(value, n_words = 1), .(variable)
][, .(WORD, FREQ, `P-C-C` = `levels<-`(variable, c("Problema", "Causa", "Consecuencia", "Soluciones")))]

one[WORD == "anid", sum(FREQ)];

write_sheet(data = one, ss = "16IGJFT63uVC0uerATaEOS6NK4IJ7iOLQ6xoBmIowz58", sheet = "ccp")


# Dos palabra

two <- lab_instrumentos_long[, obtener_terminos(value, n_words = 2), .(variable)
][, .(WORD, FREQ, `P-C-C` = `levels<-`(variable, c("Problema", "Causa", "Consecuencia", "Soluciones")))]
write_sheet(data = two, ss = "16IGJFT63uVC0uerATaEOS6NK4IJ7iOLQ6xoBmIowz58", sheet = "two-words")


# tres palabra

three <- lab_instrumentos_long[, obtener_terminos(value, n_words = 3), .(variable)
][, .(WORD, FREQ, `P-C-C` = `levels<-`(variable, c("Problema", "Causa", "Consecuencia", "Soluciones")))]
write_sheet(data = three, ss = "16IGJFT63uVC0uerATaEOS6NK4IJ7iOLQ6xoBmIowz58", sheet = "three-words")


