#' Grafica una red con un layout circular
#'
#' Utilizando un objeto resultante de la función `crear_redes_de_palabras()`,
#' crea un gráfico de redes con una distribución espacial circular. Para esto,
#' se usan bajo la capa la librería `igraph`.
#'
#' @param red Un objeto de clase `igraph` resultante de la función `crear_redes_de_palabras()` o
#' de procesos similares usando el paquete `igraph`.
#'
#' @importFrom igraph V layout.circle
#' @export

grafico_red_circular <- function(red) {

  if (!"igraph" %in% class(red)) stop("`red` DEBE ser un objeto de clase `igraph`")

  plot(x = red,
       vertex.size = igraph::V(red)$degree * 2L,
       edge.arrow.size = 0.1,
       layout = igraph::layout.circle)
}

#' Grafica una red usando score de Hubs
#'
#' Utilizando un objeto resultante de la función `crear_redes_de_palabras()`,
#' crea un gráfico de redes usando el score de Hubs. Para esto,
#' se usan bajo la capa la librería `igraph`.
#'
#' @param red Un objeto de clase `igraph` resultante de la función `crear_redes_de_palabras()` o
#' de procesos similares usando el paquete `igraph`.
#' @param main Carácter. Título para el gráfico.
#'
#' @importFrom igraph hub_score
#' @importFrom grDevices rainbow
#' @export

grafico_red_hubs <- function(red, main = "Hubs") {

  if (!"igraph" %in% class(red)) stop("`red` DEBE ser un objeto de clase `igraph`")

  hs <- igraph::hub_score(red, weights = NA)$vector

  plot(red,
       vertex.size = hs * 20L,
       main = main,
       vertex.color = grDevices::rainbow(50))
}
