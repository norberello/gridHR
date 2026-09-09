#' Grid-based home-range estimation
#'
#' Estimates home-range size by overlaying animal locations with a
#' regular spatial grid and identifying the occupied cells. The function
#' supports both hexagonal and square cells and can display spatial
#' variation in space-use intensity using either the number of locations
#' recorded in each cell or a radial space-use profile.
#'
#' When `fill_by = "locations"`, each occupied cell is coloured according
#' to the number of recorded locations it contains. This represents the
#' conventional grid-based spatial distribution of location density.
#'
#' When `fill_by = "radial"`, the occupied cells are grouped into radial
#' rings according to their distance from the centre of the home range.
#' Cells belonging to the same ring are assigned the same colour,
#' representing the normalized space-use intensity of that ring. Radial
#' intensity is calculated as the number of locations recorded in a ring
#' divided by the number of occupied cells in that ring. This provides a
#' spatial representation of the radial space-use profile produced by
#' [radial_plot()].
#'
#' The centre used for the radial representation is the centre of the
#' grid cell containing the geometric centroid of all observed locations.
#'
#' @param points An `sf` object containing animal locations as point
#'   geometries. The coordinate reference system must be projected
#'   and use metric units.
#' @param cell_area Numeric. Area of each grid cell in square metres.
#'   Default is 5000 m2 (0.5 ha).
#' @param title Character. Title of the home-range plot.
#' @param cell_shape Character. Shape of the grid cells. Either
#'   `"hex"` or `"square"`. Default is `"hex"`.
#' @param show_locations Logical. Should the individual locations
#'   be displayed on top of the home-range cells? Default is `FALSE`.
#' @param location_fill Colour used to fill individual locations.
#' @param location_outline Colour used for the outline of individual
#'   locations.
#' @param location_size Size of individual location points.
#' @param location_alpha Transparency of individual location points.
#' @param show_gradient Logical. Should a colour gradient be used
#'   to represent space-use intensity? Default is `TRUE`.
#' @param fill_by Character. Determines how space-use intensity is
#'   represented. `"locations"` colours each cell according to its
#'   number of recorded locations. `"radial"` colours cells according
#'   to the normalized space-use intensity of their radial ring.
#'   Default is `"locations"`.
#' @param low_colour Colour representing low space-use intensity.
#' @param high_colour Colour representing high space-use intensity.
#' @param cell_fill Fill colour when `show_gradient = FALSE`.
#' @param cell_outline Colour of cell outlines. Default is `NA`,
#'   producing no cell outlines.
#' @param linewidth Line width of cell outlines.
#' @param show_centroid Logical. Should the centre of the central
#'   home-range cell be displayed? Default is `TRUE`.
#' @param centroid_colour Colour of the central-cell marker.
#' @param centroid_size Size of the central-cell marker.
#' @param show_legend Logical. Should the gradient legend be displayed?
#'   Default is `FALSE`.
#' @param text_size Base font size for the plot.
#'
#' @return A list containing:
#' \describe{
#'   \item{hr_size_m2}{Home-range area in square metres.}
#'   \item{hr_size_ha}{Home-range area in hectares.}
#'   \item{n_cells}{Number of occupied grid cells.}
#'   \item{grid}{An `sf` object containing the occupied home-range cells.}
#'   \item{hr_centroid}{The geometric centroid of all locations.}
#'   \item{central_cell_center}{The centre of the grid cell containing
#'   the location centroid.}
#'   \item{hr_plot}{The resulting `ggplot2` home-range map.}
#' }
#'
#' @examples
#' data(gibbons)
#'
#' grid_hr(
#'   gibbons,
#'   cell_area = 5000,
#'   fill_by = "locations",
#'   show_legend = TRUE,
#'   title = "Grid-based space-use intensity of a gibbon group"
#' )
#'
#' grid_hr(
#'   gibbons,
#'   cell_area = 5000,
#'   fill_by = "radial",
#'   show_legend = TRUE,
#'   title = "Radial space-use intensity of a gibbon group (0.5-ha hexagons)"
#' )
#'
#' @export
# ==========================================================
# HOME-RANGE GRID FUNCTION
# ==========================================================

grid_hr <- function(
    points,
    cell_area = 5000,
    title = "",
    cell_shape = "hex",

    # --------------------------------------------------------
    # Locations
    # --------------------------------------------------------

    show_locations = FALSE,
    location_fill = "black",
    location_outline = "white",
    location_size = 1.2,
    location_alpha = 0.6,

    # --------------------------------------------------------
    # Home-range cells
    # --------------------------------------------------------

    show_gradient = TRUE,
    fill_by = "locations",
    low_colour = "lightblue",
    high_colour = "darkblue",
    cell_fill = "lightblue",
    cell_outline = NA,
    linewidth = 0.4,

    # --------------------------------------------------------
    # Centroid
    # --------------------------------------------------------

    show_centroid = TRUE,
    centroid_colour = "red",
    centroid_size = 5,

    # --------------------------------------------------------
    # Plot
    # --------------------------------------------------------

    show_legend = FALSE,
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

  if (cell_area <= 0) {
    stop("cell_area must be greater than zero.")
  }

  if (!cell_shape %in% c("hex", "square")) {
    stop(
      "cell_shape must be either 'hex' or 'square'."
    )
  }

  if (!fill_by %in% c("locations", "radial")) {
    stop(
      "fill_by must be either 'locations' or 'radial'."
    )
  }


  # ==========================================================
  # 2. CENTRE OF THE LOCATIONS
  # ==========================================================

  hr_centroid <- sf::st_centroid(
    sf::st_union(
      sf::st_geometry(points)
    )
  )


  # ==========================================================
  # 3. CREATE GRID
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
  # 4. COUNT LOCATIONS PER CELL
  # ==========================================================

  grid_sf$n <- lengths(
    sf::st_intersects(
      grid_sf,
      points
    )
  )


  # ==========================================================
  # 5. KEEP USED HOME-RANGE CELLS
  # ==========================================================

  grid_used <- dplyr::filter(
    grid_sf,
    n > 0
  )


  # ==========================================================
  # 6. HOME-RANGE SIZE
  # ==========================================================

  n_cells <- nrow(grid_used)

  hr_size_m2 <- n_cells * cell_area

  hr_size_ha <- hr_size_m2 / 10000


  # ==========================================================
  # 7. DEFINE CENTRAL CELL
  # ==========================================================

  central_cell_idx <- which(
    sf::st_contains(
      grid_sf,
      hr_centroid,
      sparse = FALSE
    )
  )


  # ----------------------------------------------------------
  # If centroid lies exactly on a cell boundary,
  # use nearest cell.
  # ----------------------------------------------------------

  if (length(central_cell_idx) == 0) {

    grid_centroids <- sf::st_centroid(
      sf::st_geometry(grid_sf)
    )

    distances_to_centroid <- sf::st_distance(
      hr_centroid,
      grid_centroids
    )

    central_cell_idx <- which.min(
      as.numeric(
        distances_to_centroid
      )
    )
  }


  central_cell_idx <- central_cell_idx[1]


  # ==========================================================
  # 8. CENTRAL CELL CENTRE
  # ==========================================================

  central_cell_center <- sf::st_centroid(
    sf::st_geometry(
      grid_sf[central_cell_idx, ]
    )
  )


  # ==========================================================
  # 9. RADIAL SPACE-USE VALUES
  # ==========================================================

  if (fill_by == "radial") {

    all_centroids <- sf::st_centroid(
      sf::st_geometry(grid_sf)
    )

    grid_sf$dist <- as.numeric(
      sf::st_distance(
        central_cell_center,
        all_centroids
      )
    )


    ring_width <- cellsize

    grid_sf$ring <- floor(
      grid_sf$dist / ring_width + 0.5
    )

    grid_sf$ring[
      central_cell_idx
    ] <- 0L


    grid_used <- dplyr::filter(
      grid_sf,
      n > 0
    )


    # Use the base R pipe to avoid an implicit
    # magrittr dependency.

    ring_summary <- grid_used |>
      sf::st_set_geometry(NULL) |>
      dplyr::group_by(ring) |>
      dplyr::summarise(
        n_total = sum(n),
        n_cells = dplyr::n(),
        intensity = n_total / n_cells,
        .groups = "drop"
      )


    ring_summary$norm_intensity <-
      ring_summary$intensity /
      max(ring_summary$intensity)


    grid_used <- dplyr::left_join(
      grid_used,

      dplyr::select(
        ring_summary,
        ring,
        norm_intensity
      ),

      by = "ring"
    )
  }


  # ==========================================================
  # 10. CREATE HOME-RANGE MAP
  # ==========================================================

  hr_plot <- ggplot2::ggplot(
    grid_used
  )


  if (show_gradient) {

    if (fill_by == "locations") {

      hr_plot <- hr_plot +

        ggplot2::geom_sf(
          ggplot2::aes(fill = n),
          colour = cell_outline,
          linewidth = linewidth
        ) +

        ggplot2::scale_fill_gradient(
          low = low_colour,
          high = high_colour,
          name = "Locations"
        )

    } else {

      hr_plot <- hr_plot +

        ggplot2::geom_sf(
          ggplot2::aes(fill = norm_intensity),
          colour = cell_outline,
          linewidth = linewidth
        ) +

        ggplot2::scale_fill_gradient(
          low = low_colour,
          high = high_colour,
          name = "Normalized\nspace-use"
        )
    }

  } else {

    hr_plot <- hr_plot +

      ggplot2::geom_sf(
        fill = cell_fill,
        colour = cell_outline,
        linewidth = linewidth
      )
  }


  # ==========================================================
  # 11. OPTIONAL LOCATION POINTS
  # ==========================================================

  if (show_locations) {

    hr_plot <- hr_plot +

      ggplot2::geom_sf(
        data = points,
        shape = 21,
        fill = location_fill,
        colour = location_outline,
        size = location_size,
        alpha = location_alpha,
        stroke = 0.3
      )
  }


  # ==========================================================
  # 12. CENTRAL CELL MARKER
  # ==========================================================

  if (show_centroid) {

    hr_plot <- hr_plot +

      ggplot2::geom_sf(
        data = central_cell_center,
        colour = centroid_colour,
        size = centroid_size,
        shape = 3
      )
  }


  # ==========================================================
  # 13. PLOT FORMATTING
  # ==========================================================

  hr_plot <- hr_plot +

    ggplot2::labs(
      title = title
    ) +

    ggplot2::theme_bw(
      base_size = text_size
    ) +

    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      legend.position =
        if (show_legend && show_gradient) {
          "right"
        } else {
          "none"
        }
    ) +

    ggplot2::coord_sf(
      datum = NA
    )


  # ==========================================================
  # 14. DISPLAY
  # ==========================================================

  print(hr_plot)


  # ==========================================================
  # 15. SUMMARY
  # ==========================================================

  n_locations <- nrow(points)

  cat("\n")
  cat("----------------------------------------\n")
  cat(
    "Grid-based home-range analysis:",
    title,
    "\n"
  )
  cat("----------------------------------------\n")

  cat(
    "Locations:              ",
    n_locations,
    "\n"
  )

  cat(
    "Cell shape:             ",
    cell_shape,
    "\n"
  )

  cat(
    "Cell area:              ",
    cell_area,
    "square meters\n"
  )

  cat(
    "Fill method:            ",
    fill_by,
    "\n"
  )

  cat(
    "Used cells:             ",
    n_cells,
    "\n"
  )

  cat(
    "Home-range area:        ",
    hr_size_m2,
    "square meters\n"
  )

  cat(
    "Home-range area:        ",
    round(
      hr_size_ha,
      2
    ),
    "ha\n"
  )

  cat("----------------------------------------\n\n")


  # ==========================================================
  # 16. RETURN
  # ==========================================================

  invisible(
    list(
      hr_size_m2 = hr_size_m2,
      hr_size_ha = hr_size_ha,
      n_cells = n_cells,
      grid = grid_used,
      hr_centroid = hr_centroid,
      central_cell_center = central_cell_center,
      hr_plot = hr_plot
    )
  )
}
