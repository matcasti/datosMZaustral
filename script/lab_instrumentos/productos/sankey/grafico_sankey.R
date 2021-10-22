## función: Crear diagrama Sankey
## fecha: 22-oct

# Preparar espacio ----------------------------------------------------------------------------

library(networkD3);
library(data.table);
.s <- function(x, ...) {
  stopifnot(inherits(x, "data.table"))
  temp <- substitute(x[...])
  eval(temp)
}

# Importar datos ------------------------------------------------------------------------------

data <- readRDS(file = "data/lab_instrumentos/clean/data.RDS") |> 
  

# Pre-procesamiento ---------------------------------------------------------------------------

.var <- c("laboratorio", "grupo", "norm_problema")

data[, .SD, .SDcols = .var]

# Producto ------------------------------------------------------------------------------------

