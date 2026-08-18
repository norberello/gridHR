#' Minimum grid-cell size based on home-range connectivity
#'
#' Evaluates the topological connectivity of a grid-based home range
#' across a sequence of grid-cell sizes. Animal locations are overlaid
#' on grids of increasing cell area, occupied cells are identified, and
#' their connectivity is evaluated based on shared cell edges.
#'
#' Connectivity is expressed as the proportion of occupied cells belonging
#' to the largest topologically connected component. Cells that touch only
#' at a corner are not considered connected. The function also identifies
#' the smallest *tested* cell size at which all occupied cells form a
#' single connected component.
#'
#' Because the grid is recreated independently at each cell size,
#' connectivity is not necessarily monotonic with increasing cell size.
#' The complete connectivity profile is therefore returned rather than
#' assuming that connectivity, once achieved, is maintained at larger
#' cell sizes.
#'
#' @param points An `sf` object containing animal locations as point
#'   geometries. The coordinate reference system must be projected
#'   and use metric units.
#' @param min Numeric. Minimum cell area to test, in square metres.
#'   Default is 100 m2.
#' @param max Numeric. Maximum cell area to test, in square metres.
#'   Default is 1000 m2.
#' @param interval Numeric. Increment between successive cell areas,
#'   in square metres. Default is 100 m2.
#' @param cell_shape Character. Shape of the grid cells. Either
#'   `"hex"` or `"square"`. Default is `"hex"`.
#' @param show_plot Logical. Should the connectivity plot be displayed?
#'   Default is `TRUE`.
#' @param line_colour Character. Colour of the connectivity line.
#' @param point_colour Character. Colour of the connectivity points.
#' @param linewidth Numeric. Width of the connectivity line.
#' @param point_size Numeric. Size of the connectivity points.
#' @param text_size Numeric. Base font size for the plot.
#'
#' @return A list containing:
#' \describe{
#'   \item{results}{A data frame containing the connectivity results
#'   for every tested cell size.}
#'   \item{plot}{The resulting `ggplot2` connectivity plot.}
#'   \item{cell_shape}{The grid-cell shape used in the analysis.}
#'   \item{minimum_connected}{The smallest tested cell area at which
#'   all occupied cells form a single connected component. Returns
#'   `NA` if no tested cell size is fully connected.}
#'   \item{min_cell}{The minimum cell area specified for the analysis.}
#'   \item{max_cell}{The maximum cell area specified for the analysis.}
#'   \item{interval}{The increment between tested cell areas.}
#' }
#'
#' @details
#' For each cell size, the function counts the number of locations in
#' each grid cell and retains occupied cells as the grid-based home
#' range. Topological connectivity is then evaluated using shared
#' cell edges. The resulting `connectivity` value is the proportion of
#' occupied cells belonging to the largest connected component.
#'
#' A connectivity value of 1 indicates that all occupied cells belong
#' to a single connected component. The corresponding `fully_connected`
#' value is `TRUE`.
#'
#' The analysis is useful for evaluating whether a particular grid
#' resolution produces a spatially compact representation of the
#' observed home range and for identifying the smallest tested cell
#' size at which the occupied grid becomes fully connected.
#'
#' @examples
#' data(gibbons)
#'
#' min_cell(
#'   gibbons,
#'   min = 50,
#'   max = 3000,
#'   interval = 200,
#'   cell_shape = "hex",
#'   show_plot = TRUE
#' )
#'
#' data(spider_monkeys)
#'
#' min_cell(
#'   spider_monkeys,
#'   min = 1000,
#'   max = 20000,
#'   interval = 1000,
#'   cell_shape = "square",
#'   show_plot = TRUE
#' )
#'
#' @export

min_cell <- function(
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
    text_size = 13
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

  # Always include max.

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

      # Distance between opposite sides.

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
  # 4. TEST EACH CELL SIZE
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
    # Keep occupied home-range cells
    # --------------------------------------------------------

    hr_cells <- grid_sf[
      grid_sf$n > 0,
      ,
      drop = FALSE
    ]

    n_cells <- nrow(hr_cells)


    # ========================================================
    # 5. DETERMINE TOPOLOGICAL CONNECTIVITY
    # ========================================================

    # If only one cell is occupied, it is trivially
    # fully connected.

    if (n_cells == 1) {

      n_components <- 1L

      largest_component <- 1L

      connectivity <- 1

      fully_connected <- TRUE

    } else {


      # ------------------------------------------------------
      # 5A. FIND SHARED-EDGE NEIGHBOURS
      # ------------------------------------------------------

      # Two cells are neighbours ONLY if they share an EDGE.
      #
      # Cells touching only at a corner are not considered
      # connected.
      #
      # F***1**** identifies polygon pairs whose boundaries
      # intersect along a line.

      edge_neighbours <- sf::st_relate(
        hr_cells,
        hr_cells,
        pattern = "F***1****",
        sparse = TRUE
      )


      # ------------------------------------------------------
      # 5B. BUILD CONNECTIVITY GRAPH
      # ------------------------------------------------------

      adjacency <- matrix(
        FALSE,
        nrow = n_cells,
        ncol = n_cells
      )

      for (j in seq_len(n_cells)) {

        neighbours <- edge_neighbours[[j]]

        if (length(neighbours) > 0) {

          adjacency[
            j,
            neighbours
          ] <- TRUE
        }
      }


      # Make the matrix explicitly symmetric.

      adjacency <- adjacency | t(adjacency)


      # ------------------------------------------------------
      # 5C. FIND CONNECTED COMPONENTS
      # ------------------------------------------------------

      component_id <- rep(
        NA_integer_,
        n_cells
      )

      current_component <- 0L

      for (j in seq_len(n_cells)) {

        if (!is.na(component_id[j])) {
          next
        }

        current_component <-
          current_component + 1L

        component_id[j] <- current_component

        queue <- j

        while (length(queue) > 0) {

          current <- queue[1]

          queue <- queue[-1]

          neighbours <- which(
            adjacency[current, ]
          )

          new_neighbours <- neighbours[
            is.na(
              component_id[neighbours]
            )
          ]

          if (length(new_neighbours) > 0) {

            component_id[
              new_neighbours
            ] <- current_component

            queue <- c(
              queue,
              new_neighbours
            )
          }
        }
      }


      # ------------------------------------------------------
      # 5D. SUMMARISE COMPONENTS
      # ------------------------------------------------------

      component_sizes <- as.numeric(
        table(component_id)
      )

      n_components <- length(
        component_sizes
      )

      largest_component <- max(
        component_sizes
      )

      # Proportion of occupied cells belonging to the
      # largest topologically connected component.

      connectivity <-
        largest_component /
        n_cells

      # TRUE only if ALL occupied cells form one
      # continuous topological component.

      fully_connected <-
        n_components == 1
    }


    # ========================================================
    # 6. STORE RESULTS
    # ========================================================

    results[[i]] <- data.frame(

      cell_area = cell_area,

      n_cells = n_cells,

      n_components = n_components,

      largest_component = largest_component,

      connectivity = connectivity,

      fully_connected = fully_connected
    )
  }


  # ==========================================================
  # 7. COMBINE RESULTS
  # ==========================================================

  results_df <- do.call(
    rbind,
    results
  )

  rownames(results_df) <- NULL


  # ==========================================================
  # 8. MINIMUM FULLY CONNECTED CELL SIZE
  # ==========================================================

  connected_sizes <- results_df$cell_area[
    results_df$fully_connected
  ]

  minimum_connected <- if (
    length(connected_sizes) > 0
  ) {

    min(connected_sizes)

  } else {

    NA_real_
  }


  # ==========================================================
  # 9. PLOT
  # ==========================================================

  connectivity_plot <- ggplot2::ggplot(
    results_df,
    ggplot2::aes(
      x = cell_area,
      y = connectivity
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
      y = "Proportion of largest connected component",
      title = "Home-range connectivity across cell sizes"
    ) +

    ggplot2::theme_bw(
      base_size = text_size
    ) +

    ggplot2::theme(
      panel.grid = ggplot2::element_blank()
    )


  # ==========================================================
  # 10. DISPLAY
  # ==========================================================

  if (show_plot) {

    print(
      connectivity_plot
    )
  }


  # ==========================================================
  # 11. RETURN
  # ==========================================================

  return(
    list(

      results = results_df,

      plot = connectivity_plot,

      cell_shape = cell_shape,

      minimum_connected = minimum_connected,

      min_cell = min,

      max_cell = max,

      interval = interval
    )
  )
}
