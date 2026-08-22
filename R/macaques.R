#' Macaque group locations
#'
#' Spatial locations of a macaque group (*Macaca leonina*) recorded at
#' Khao Yai National Park, Thailand, from July 2012 to June 2013.
#' The group was followed for five to eight full days per month,
#' from sleeping site to sleeping site, using a handheld GPS.
#' Locations were recorded at approximately 30-minute intervals.
#'
#' Coordinates are provided in UTM zone 47N using the WGS84 datum.
#'
#' @format An `sf` object containing 16,478 macaque group locations.
#'
#' @source
#' José-Domínguez, J. M., Huynen, M.-C., García, C. J.,
#' Albert-Daviaud, A., Savini, T., & Asensio, N. (2015).
#' Non-territorial Macaques Can Range Like Territorial Gibbons
#' When Partially Provisioned With Food. *Biotropica*, 47, 733–744.
#' doi:10.1111/btp.12256
#'
#' @examples
#' grid_hr(
#'   macaques,
#'   cell_area = 25000,
#'   fill_by = "locations",
#'   title = "Macaque home range with radial space-use pattern",
#'   low_colour = "grey90",
#'   high_colour = "grey10"
#' )
#'
#' @name macaques
NULL
