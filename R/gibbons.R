#' Gibbon movement locations from Khao Yai National Park
#'
#' Movement locations recorded at approximately 5-m intervals
#' throughout 2003 for a group of white-handed gibbons
#' (*Hylobates lar*) in Khao Yai National Park, Thailand.
#'
#' The dataset contains 18,098 spatial locations represented
#' as an `sf` point object. Coordinates are provided in UTM
#' zone 47N (EPSG:32647).
#'
#' The dataset was originally used to examine short-term
#' core-area use in white-handed gibbons.
#'
#' @format An `sf` object containing 18,098 point locations.
#'
#' @references
#' Asensio, N., Brockelman, W. Y., Malaivijitnond, S. &
#' Reichard, U. H. (2014).
#' White-handed Gibbon (*Hylobates lar*) Core Area Use Over
#' a Short-Time Scale. *Biotropica*, 46, 461-469.
#'
#' @examples
#' data(gibbons)
#'
#' grid_hr(
#'   gibbons,
#'   cell_area = 5000,
#'   fill_by = "radial",
#'   show_legend = FALSE,
#'   title = "Radial space-use intensity of a gibbon group
#'   (1 hex = 0.5 ha)"
#' )
"gibbons"
