#' Grid-based home-range rarefaction
#'
#' Evaluates how grid-based home-range size changes with increasing
#' numbers of animal locations. Random permutations of the locations
#' are used to generate accumulated samples, and home-range size is
#' calculated at successive sample sizes as the number of occupied
#' grid cells multiplied by the area of each cell.
#'
#' Repeated random permutations of the locations are used to estimate
#' the mean and variability of home-range size at each sample size.
#' The resulting rarefaction curve can be used to assess how
#' home-range estimates change with sampling effort and whether they
#' approach an asymptotic value as the number of locations increases.
#'
#' The `step` argument controls the resolution of the rarefaction
#' curve. For example, `step = 25` evaluates home-range size every
#' 25 locations. Smaller values provide a more detailed curve but
#' require more computation, particularly for large datasets.
#'
#' The home range is defined as the set of grid cells containing at
#' least one sampled location. Consequently, estimated home-range
#' area depends on the specified cell size and cell shape.
#'
#' @param points An `sf` object containing animal locations as point
#'   geometries. The coordinate reference system must be projected
#'   and use metric units.
#' @param cell_area Numeric. Area of each grid cell in square metres.
#'   Default is 5000 m2 (0.5 ha).
#' @param cell_shape Character. Shape of the grid cells. Either
#'   `"hex"` or `"square"`. Default is `"hex"`.
#' @param n_reps Integer. Number of random rarefaction replicates.
#'   Default is 1000.
#' @param step Integer. Interval between sample sizes evaluated in
#'   the rarefaction curve. For example, `step = 25` evaluates
#'   home-range size every 25 locations. Default is 1.
#' @param seed Optional integer used to make the random rarefaction
#'   replicates reproducible. Default is `NULL`.
#' @param show_plot Logical. Should the rarefaction plot be displayed?
#'   Default is `TRUE`.
#' @param ribbon_colour Character. Colour used for the variability
#'   ribbon.
#' @param line_colour Character. Colour used for the mean rarefaction
#'   curve.
#' @param linewidth Numeric. Width of the rarefaction curve.
#' @param text_size Numeric. Base font size for the plot.
#'
#' @return A list containing:
#' \describe{
#'   \item{rarefaction_df}{A data frame containing the number of
#'   locations evaluated, mean home-range size, and standard deviation
#'   across rarefaction replicates, expressed in square metres and
#'   hectares.}
#'   \item{rarefaction_plot}{The resulting `ggplot2` rarefaction plot.}
#'   \item{grid}{An `sf` object containing the grid used for the
#'   analysis.}
#'   \item{n_total}{Number of locations included in the analysis.}
#'   \item{cell_area}{Area of each grid cell in square metres.}
#'   \item{cell_shape}{Shape of the grid cells used in the analysis.}
#' }
#'
#' @details
#' For each rarefaction replicate, the locations are randomly
#' reordered and progressively accumulated. At each selected sample
#' size, the number of unique occupied grid cells is recorded and
#' multiplied by the cell area to obtain the grid-based home-range
#' estimate.
#'
#' Sampling is performed without replacement within each replicate.
#' The variability among replicates therefore represents the effect
#' of the order in which locations are accumulated rather than
#' uncertainty arising from different spatial locations being sampled
#' independently.
#'
#' Because grid-based home-range area is based on occupied cells,
#' rarefaction results depend on both sampling effort and the chosen
#' grid resolution. The curve can therefore be used to examine
#' whether additional locations continue to increase the estimated
#' home-range area under a specified grid configuration.
#'
#' @examples
#' data(spider_monkeys)
#'
#' # A reduced number of replicates is used here to keep the example
#' # computationally light. Larger values are recommended for analysis.
#' hr_rare(
#'   spider_monkeys,
#'   cell_area = 10000,
#'   cell_shape = "hex",
#'   n_reps = 99,
#'   step = 25,
#'   seed = 123
#' )
#'
#' @export
# ==========================================================
# HOME-RANGE RAREFACTION FUNCTION
# ==========================================================

hr_rare <- function(
    points,
    cell_area = 5000,
    cell_shape = "hex",
    n_reps = 1000,
    step = 1,
    seed = NULL,
    show_plot = TRUE,
    ribbon_colour = "#08519C",
    line_colour = "#08519C",
    linewidth = 1.1,
    text_size = 13
) {

  # ==========================================================
  # 1. CHECK INPUT
  # ==========================================================

  if (!inherits(points, "sf")) {
    stop("points must be an sf object.")
  }

  if (nrow(points) < 2) {
    stop("At least two locations are required.")
  }

  if (is.na(sf::st_crs(points))) {
    stop("The sf object must have a defined CRS.")
  }

  if (sf::st_is_longlat(points)) {
    stop(
      "points must use a projected CRS with metric units."
    )
  }

  if (cell_area <= 0) {
    stop("cell_area must be greater than zero.")
  }

  if (!cell_shape %in% c("hex", "square")) {
    stop(
      "cell_shape must be either 'hex' or 'square'."
    )
  }

  if (n_reps < 1 || n_reps != round(n_reps)) {
    stop(
      "n_reps must be a positive integer."
    )
  }

  if (step < 1 || step != round(step)) {
    stop(
      "step must be a positive integer."
    )
  }


  # ==========================================================
  # 2. RANDOM SEED
  # ==========================================================

  if (!is.null(seed)) {
    set.seed(seed)
  }


  # ==========================================================
  # 3. CREATE FIXED GRID
  # ==========================================================

  if (cell_shape == "hex") {

    side_length <- sqrt(
      (2 * cell_area) /
        (3 * sqrt(3))
    )

    cellsize <- side_length * sqrt(3)

    grid <- sf::st_make_grid(
      points,
      cellsize = cellsize,
      what = "polygons",
      square = FALSE
    )

  } else {

    cellsize <- sqrt(cell_area)

    grid <- sf::st_make_grid(
      points,
      cellsize = cellsize,
      what = "polygons",
      square = TRUE
    )
  }

  grid_sf <- sf::st_sf(
    geometry = grid
  )


  # ==========================================================
  # 4. ASSIGN LOCATIONS TO GRID CELLS
  # ==========================================================

  point_cells <- sf::st_intersects(
    points,
    grid_sf
  )

  point_cell <- integer(
    length(point_cells)
  )

  for (j in seq_along(point_cells)) {

    if (length(point_cells[[j]]) == 0) {

      point_cell[j] <- NA_integer_

    } else {

      point_cell[j] <- point_cells[[j]][1]
    }
  }


  # ==========================================================
  # 5. REMOVE UNASSIGNED LOCATIONS
  # ==========================================================

  if (anyNA(point_cell)) {

    n_missing <- sum(
      is.na(point_cell)
    )

    warning(
      n_missing,
      " location(s) could not be assigned to a grid cell ",
      "and will be excluded."
    )

    point_cell <- point_cell[
      !is.na(point_cell)
    ]
  }

  n_total <- length(
    point_cell
  )

  if (n_total < 2) {

    stop(
      "Fewer than two locations could be assigned to grid cells."
    )
  }


  # ==========================================================
  # 6. SAMPLE SIZES
  # ==========================================================

  sample_sizes <- seq(
    1,
    n_total,
    by = step
  )

  if (utils::tail(sample_sizes, 1) != n_total) {

    sample_sizes <- c(
      sample_sizes,
      n_total
    )
  }


  # ==========================================================
  # 7. RAREFACTION
  # ==========================================================

  # The rarefaction curve is generated by progressively
  # adding locations from a random permutation.
  #
  # Each replicate therefore requires only ONE random
  # permutation of the locations rather than independently
  # sampling every sample size.

  n_sizes <- length(sample_sizes)


  # ----------------------------------------------------------
  # Store results for each replicate.
  #
  # Rows = sample sizes
  # Columns = random replicates.
  # ----------------------------------------------------------

  replicate_hr <- matrix(
    NA_real_,
    nrow = n_sizes,
    ncol = n_reps
  )


  # ==========================================================
  # 8. RANDOM RAREFACTION REPLICATES
  # ==========================================================

  for (r in seq_len(n_reps)) {

    # --------------------------------------------------------
    # Randomly order all locations once.
    # --------------------------------------------------------

    shuffled_cells <- sample(
      point_cell,
      size = n_total,
      replace = FALSE
    )


    # --------------------------------------------------------
    # Track which cells have been occupied.
    # --------------------------------------------------------

    occupied <- logical(
      max(point_cell)
    )

    n_cells_used <- 0L

    size_index <- 1L


    # --------------------------------------------------------
    # Add locations sequentially.
    # --------------------------------------------------------

    for (i in seq_len(n_total)) {

      cell <- shuffled_cells[i]

      if (!occupied[cell]) {

        occupied[cell] <- TRUE

        n_cells_used <-
          n_cells_used + 1L
      }


      # ------------------------------------------------------
      # Store result whenever a requested sample size
      # is reached.
      # ------------------------------------------------------

      if (
        size_index <= n_sizes &&
        i == sample_sizes[size_index]
      ) {

        replicate_hr[
          size_index,
          r
        ] <-
          n_cells_used * cell_area

        size_index <-
          size_index + 1L
      }
    }
  }


  # ==========================================================
  # 9. SUMMARISE RAREFACTION CURVES
  # ==========================================================

  mean_hr_m2 <- apply(
    replicate_hr,
    1,
    mean
  )

  sd_hr_m2 <- apply(
    replicate_hr,
    1,
    stats::sd
  )


  rarefaction_df <- data.frame(

    n_locations = sample_sizes,

    mean_hr_m2 = mean_hr_m2,

    sd_hr_m2 = sd_hr_m2
  )


  # Convert to hectares

  rarefaction_df$mean_hr_ha <-
    rarefaction_df$mean_hr_m2 / 10000

  rarefaction_df$sd_hr_ha <-
    rarefaction_df$sd_hr_m2 / 10000


  # ==========================================================
  # 10. CREATE RAREFACTION PLOT
  # ==========================================================

  rarefaction_plot <- ggplot2::ggplot(
    rarefaction_df,
    ggplot2::aes(
      x = n_locations,
      y = mean_hr_ha
    )
  ) +

    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = pmax(
          0,
          mean_hr_ha - sd_hr_ha
        ),
        ymax = mean_hr_ha + sd_hr_ha
      ),
      fill = ribbon_colour,
      alpha = 0.2
    ) +

    ggplot2::geom_line(
      colour = line_colour,
      linewidth = linewidth
    ) +

    ggplot2::labs(
      x = "Number of locations",
      y = "Home-range size (ha)",
      title = "Home-range rarefaction"
    ) +

    ggplot2::theme_bw(
      base_size = text_size
    ) +

    ggplot2::theme(
      panel.grid = ggplot2::element_blank()
    )


  # ==========================================================
  # 11. DISPLAY
  # ==========================================================

  if (show_plot) {

    print(
      rarefaction_plot
    )
  }


  # ==========================================================
  # 12. RETURN
  # ==========================================================

  invisible(
    list(

      rarefaction_df =
        rarefaction_df,

      rarefaction_plot =
        rarefaction_plot,

      grid =
        grid_sf,

      n_total =
        n_total,

      cell_area =
        cell_area,

      cell_shape =
        cell_shape
    )
  )
}
