# ==========================================================
# HOME-RANGE SIZE ACROSS CELL SIZES
# ==========================================================

#' Home-range size across grid cell sizes
#'
#' Calculates grid-based home-range size across a sequence of cell areas.
#' For each cell size, the function overlays the animal locations with a
#' regular grid, identifies occupied cells, and calculates home-range size
#' as the number of occupied cells multiplied by the area of each cell.
#'
#' This function can be used to examine how sensitive grid-based
#' home-range estimates are to the spatial resolution of the grid.
#' Both hexagonal and square cells are supported.
#'
#' @param points An `sf` object containing animal locations as point
#'   geometries. The coordinate reference system must be projected
#'   and use metric units.
#' @param min Numeric. Minimum cell area to evaluate, in square metres.
#'   Default is 100 m2.
#' @param max Numeric. Maximum cell area to evaluate, in square metres.
#'   Default is 1000 m2.
#' @param interval Numeric. Increment between successive cell areas,
#'   in square metres. Default is 100 m2.
#' @param cell_shape Character. Shape of the grid cells. Either
#'   `"hex"` or `"square"`. Default is `"hex"`.
#' @param show_plot Logical. Should the relationship between cell area
#'   and estimated home-range size be displayed? Default is `TRUE`.
#' @param line_colour Colour of the line connecting home-range estimates.
#' @param point_colour Colour of the points representing individual
#'   cell-area estimates.
#' @param linewidth Width of the line.
#' @param point_size Size of the points.
#' @param text_size Base font size for the plot.
#' @param title Character. Title of the plot. Default is an empty
#'   character string.
#'
#' @return A list containing:
#' \describe{
#'   \item{results}{A data frame containing the cell area, number of
#'     occupied cells, and estimated home-range size in square metres
#'     and hectares for each cell size.}
#'   \item{plot}{The resulting `ggplot2` plot.}
#'   \item{cell_shape}{The shape of the grid cells used.}
#'   \item{min_cell}{The minimum cell area evaluated.}
#'   \item{max_cell}{The maximum cell area evaluated.}
#'   \item{interval}{The increment between successive cell areas.}
#' }
#'
#' @examples
#' data(gibbons)
#'
#' hr_cell_size(
#'   gibbons,
#'   min = 100,
#'   max = 5000,
#'   interval = 100,
#'   cell_shape = "hex",
#'   title = "Grid-based home-range size across hexagonal cell areas"
#' )
#'
#' @export
hr_cell_size <- function(
    points,
    min = 100,
    max = 1000,
    interval = 100,
    cell_shape = "hex",
    show_plot = TRUE,
    line_colour = "#08519C",
    point_colour = "#08519C",
    linewidth = 1.2,
    point_size = 3,
    text_size = 13,
    title = ""
) {

  # ==========================================================
  # 1. CHECK INPUT
  # ==========================================================

  if (!inherits(points, "sf")) {
    stop("points must be an sf object.")
  }

  if (nrow(points) == 0) {
    stop("No locations supplied.")
  }

  if (is.na(sf::st_crs(points))) {
    stop("The sf object must have a defined CRS.")
  }

  if (sf::st_is_longlat(points)) {
    stop(
      "points must use a projected CRS with metric units ",
      "(e.g. UTM)."
    )
  }

  if (min <= 0) {
    stop("min must be greater than zero.")
  }

  if (max <= min) {
    stop("max must be greater than min.")
  }

  if (interval <= 0) {
    stop("interval must be greater than zero.")
  }

  if (!cell_shape %in% c("hex", "square")) {
    stop(
      "cell_shape must be either 'hex' or 'square'."
    )
  }


  # ==========================================================
  # 2. CELL SIZES TO TEST
  # ==========================================================

  cell_sizes <- seq(
    min,
    max,
    by = interval
  )

  # Always include max

  if (max(cell_sizes) < max) {
    cell_sizes <- c(
      cell_sizes,
      max
    )
  }


  # ==========================================================
  # 3. FUNCTION TO CREATE GRID
  # ==========================================================

  make_grid <- function(cell_area) {

    if (cell_shape == "hex") {

      # --------------------------------------------------------
      # Regular hexagon
      #
      # A = (3 * sqrt(3) / 2) * side_length^2
      # --------------------------------------------------------

      side_length <- sqrt(
        (2 * cell_area) /
          (3 * sqrt(3))
      )

      # Distance between opposite sides

      cellsize <- side_length * sqrt(3)

      grid <- sf::st_make_grid(
        points,
        cellsize = cellsize,
        what = "polygons",
        square = FALSE
      )

    } else {

      # --------------------------------------------------------
      # Square
      #
      # A = side_length^2
      # --------------------------------------------------------

      cellsize <- sqrt(cell_area)

      grid <- sf::st_make_grid(
        points,
        cellsize = cellsize,
        what = "polygons",
        square = TRUE
      )
    }

    sf::st_sf(
      geometry = grid
    )
  }


  # ==========================================================
  # 4. CALCULATE HOME-RANGE SIZE FOR EACH CELL SIZE
  # ==========================================================

  results <- vector(
    "list",
    length(cell_sizes)
  )


  for (i in seq_along(cell_sizes)) {

    cell_area <- cell_sizes[i]


    # --------------------------------------------------------
    # Create grid
    # --------------------------------------------------------

    grid_sf <- make_grid(
      cell_area
    )


    # --------------------------------------------------------
    # Count locations in each cell
    # --------------------------------------------------------

    grid_sf$n <- lengths(
      sf::st_intersects(
        grid_sf,
        points
      )
    )


    # --------------------------------------------------------
    # Keep occupied cells only
    # --------------------------------------------------------

    hr_cells <- dplyr::filter(
      grid_sf,
      n > 0
    )


    # --------------------------------------------------------
    # Home-range size
    # --------------------------------------------------------

    n_cells <- nrow(hr_cells)

    hr_area_m2 <- n_cells * cell_area

    hr_area_ha <- hr_area_m2 / 10000


    # --------------------------------------------------------
    # Store results
    # --------------------------------------------------------

    results[[i]] <- data.frame(
      cell_area = cell_area,
      n_cells = n_cells,
      hr_area_m2 = hr_area_m2,
      hr_area_ha = hr_area_ha
    )
  }


  # ==========================================================
  # 5. COMBINE RESULTS
  # ==========================================================

  results_df <- do.call(
    rbind,
    results
  )

  rownames(results_df) <- NULL


  # ==========================================================
  # 6. PLOT
  # ==========================================================

  hr_plot <- ggplot2::ggplot(
    results_df,
    ggplot2::aes(
      x = cell_area,
      y = hr_area_ha
    )
  ) +

    ggplot2::geom_line(
      colour = line_colour,
      linewidth = linewidth
    ) +

    ggplot2::geom_point(
      colour = point_colour,
      size = point_size
    ) +

    ggplot2::labs(
      x = "Cell area (square meters)",
      y = "Home-range size (ha)",
      title = title
    ) +

    ggplot2::theme_bw(
      base_size = text_size
    ) +

    ggplot2::theme(
      panel.grid = ggplot2::element_blank()
    )


  # ==========================================================
  # 7. DISPLAY
  # ==========================================================

  if (show_plot) {
    print(hr_plot)
  }


  # ==========================================================
  # 8. RETURN
  # ==========================================================

  return(
    list(

      results = results_df,

      plot = hr_plot,

      cell_shape = cell_shape,

      min_cell = min,

      max_cell = max,

      interval = interval
    )
  )
}
