#' Spider monkey subgroup locations
#'
#' Spatial locations of spider monkey subgroups recorded at approximately
#' 30-minute intervals from 2005 to 2008 in a regenerating tropical forest.
#' Coordinates are provided in UTM zone 16N using the WGS84 datum
#' (EPSG:32616).
#'
#' The dataset was used to investigate the effects of roads on spider
#' monkey home-range size and mobility in a heterogeneous regenerating
#' forest.
#'
#' @format An `sf` object containing spider monkey subgroup locations.
#'
#' @references
#' Asensio, N., Murillo-Chacon, E., Schaffner, C. M. & Aureli, F. (2017).
#' The effect of roads on spider monkeys' home range and mobility in a
#' heterogeneous regenerating forest. *Biotropica*.
#' \doi{10.1111/btp.12441}
#'
#' @source
#' Asensio, N., Murillo-Chacon, E., Schaffner, C. M. & Aureli, F. (2017).
#' \doi{10.1111/btp.12441}
#'
#' @examples
#' data(spider_monkeys)
#'
#' grid_hr(
#'   spider_monkeys,
#'   cell_area = 10000,
#'   fill_by = "radial",
#'   title = "Radial space-use intensity of a spider monkey group (1 ha hexagons)"
#' )
"spider_monkeys"
