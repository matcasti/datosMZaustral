## función: Crear diagrama Sankey
## fecha: 22-oct

# Preparar espacio ----------------------------------------------------------------------------

library(glue);
library(highcharter);
library(data.table);
.s <- function(x, ...) {
  stopifnot(inherits(x, "data.table"))
  temp <- substitute(x[...])
  eval(temp)
}

# Importar datos ------------------------------------------------------------------------------

data <- readRDS(file = "data/lab_instrumentos/clean/data.RDS")

# Pre-procesamiento ---------------------------------------------------------------------------

.var <- c("laboratorio", "grupo", "norm_problema")
sankey_data <- highcharter::data_to_sankey(data = copy(data)[, .SD, .SDcols = .var])

# Producto ------------------------------------------------------------------------------------

highcharter::data_to_sankey(data = copy(data)[, .SD, .SDcols = .var]) |>
  highcharter::hchart("sankey")



# Under construction

names <- readLines(con = "script/lab_instrumentos/productos/sankey/assets/names.txt")
  
for(i in nrow(sankey_data)) {
  
}