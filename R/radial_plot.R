#' Radial space-use profile of a grid-based home range
#'
#' Calculates and plots the relationship between space-use intensity
#' and distance from the centre to the periphery of a grid-based
#' home range.
#'
#' The function divides the occupied home-range cells into radial
#' rings according to their distance from the centre. The centre is
#' defined as the centre of the grid cell containing the geometric
#' centroid of all observed locations. For each radial ring,
#' space-use intensity is calculated as the total number of recorded
#' locations divided by the number of occupied cells in that ring.
#'
#' This approach provides a cell-based description of how space-use
#' intensity is organized spatially from the centre toward the
#' periphery, while accounting for differences in the number of
#' occupied cells represented at different distances.
#'
#' Both distance and space-use intensity are normalized from 0 to 1.
#' Normalized distance represents the position of each radial ring
#' between the centre and the outermost occupied ring, whereas
#' normalized space-use intensity represents relative intensity,
#' with the most intensively used ring assigned a value of 1.
#'
#' The resulting profile describes how the intensity of space use
#' changes with increasing distance from the centre of the home range.
#' A decreasing profile indicates relatively greater use toward the
#' centre, whereas an increasing profile indicates relatively greater
#' use toward the periphery. The function can display either the
#' observed radial profile or a LOESS-smoothed curve.
#'
#' @param points An `sf` object containing the observed animal
#'   locations as point geometries. The coordinate reference system
#'   must be projected and use metric units.
#' @param cell_area Numeric. Area of each grid cell in square metres.
#'   Default is 5000 m2 (0.5 ha).
#' @param title Character. Title of the plot. Default is `""`.
#' @param cell_shape Character. Shape of the grid cells. Either
#'   `"hex"` or `"square"`. Default is `"hex"`.
#' @param smooth Logical. If `TRUE`, fits a LOESS-smoothed curve to
#'   the radial space-use profile. Default is `FALSE`.
#' @param loess_span Numeric. Span parameter used for LOESS smoothing.
#'   Must be greater than 0 and less than or equal to 1.
#'   Default is `0.8`.
#' @param show_points Logical. If `TRUE`, displays the observed
#'   radial values as points. Default is `FALSE`.
#' @param point_colour Character. Colour of the observed radial
#'   points. Default is `"#08519C"`.
#' @param point_size Numeric. Size of the observed radial points.
#'   Default is `2`.
#' @param point_alpha Numeric. Transparency of the observed radial
#'   points. Default is `0.8`.
#' @param line_colour Character. Colour of the radial profile curve.
#'   Default is `"#08519C"`.
#' @param linewidth Numeric. Width of the radial profile curve.
#'   Default is `1.3`.
#' @param text_size Numeric. Base text size of the plot.
#'   Default is `13`.
#'
#' @return A list containing:
#' \describe{
#'   \item{ring_summary}{A data frame containing the radial rings,
#'   number of locations, number of occupied cells, space-use
#'   intensity, distance from the centre, and normalized distance
#'   and intensity.}
#'   \item{central_cell_center}{The centre of the grid cell containing
#'   the geometric centroid of the observed locations.}
#'   \item{radial_plot}{A `ggplot` object showing the radial
#'   space-use profile.}
#' }
#'
#' @details
#' Space-use intensity is calculated separately for each radial ring
#' as the total number of locations recorded in the ring divided by
#' the number of occupied cells comprising that ring. This prevents
#' rings containing more occupied cells from automatically having
#' higher intensity simply because they cover a larger number of
#' cells.
#'
#' The grid is constructed using the specified cell area and shape.
#' Only cells containing at least one recorded location are considered
#' part of the home range. Radial distance is measured from the centre
#' of the central grid cell to the centres of the occupied grid cells.
#'
#' The radial profile is therefore based on the spatial organization
#' of observed locations within the grid rather than on a kernel
#' density estimate. No spatial smoothing is applied unless
#' `smooth = TRUE` is specified.
#'
#' @examples
#' data(gibbons)
#'
#' radial_plot(
#'   gibbons,
#'   cell_area = 5000,
#'   cell_shape = "hex",
#'   title = "Radial space-use profile of a gibbon group (0.5-ha hexagons)"
#' )
#' Radial space-use profile of a grid-based home range
#'
#' Calculates and plots the relationship between space-use intensity
#' and distance from the centre to the periphery of a grid-based
#' home range.
#'
#' The function divides the occupied home-range cells into radial
#' rings according to their distance from the centre. The centre is
#' defined as the centre of the grid cell containing the geometric
#' centroid of all observed locations. For each radial ring,
#' space-use intensity is calculated as the total number of recorded
#' locations divided by the number of occupied cells in that ring.
#'
#' This approach provides a cell-based description of how space-use
#' intensity is organized spatially from the centre toward the
#' periphery, while accounting for differences in the number of
#' occupied cells represented at different distances.
#'
#' Both distance and space-use intensity are normalized from 0 to 1.
#' Normalized distance represents the position of each radial ring
#' between the centre and the outermost occupied ring, whereas
#' normalized space-use intensity represents relative intensity,
#' with the most intensively used ring assigned a value of 1.
#'
#' The resulting profile describes how the intensity of space use
#' changes with increasing distance from the centre of the home range.
#' A decreasing profile indicates relatively greater use toward the
#' centre, whereas an increasing profile indicates relatively greater
#' use toward the periphery. The function can display either the
#' observed radial profile or a LOESS-smoothed curve.
#'
#' @param points An `sf` object containing the observed animal
#'   locations as point geometries. The coordinate reference system
#'   must be projected and use metric units.
#' @param cell_area Numeric. Area of each grid cell in square metres.
#'   Default is 5000 m2 (0.5 ha).
#' @param title Character. Title of the plot. Default is `""`.
#' @param cell_shape Character. Shape of the grid cells. Either
#'   `"hex"` or `"square"`. Default is `"hex"`.
#' @param smooth Logical. If `TRUE`, fits a LOESS-smoothed curve to
#'   the radial space-use profile. Default is `FALSE`.
#' @param loess_span Numeric. Span parameter used for LOESS smoothing.
#'   Must be greater than 0 and less than or equal to 1.
#'   Default is `0.8`.
#' @param show_points Logical. If `TRUE`, displays the observed
#'   radial values as points. Default is `FALSE`.
#' @param point_colour Character. Colour of the observed radial
#'   points. Default is `"#08519C"`.
#' @param point_size Numeric. Size of the observed radial points.
#'   Default is `2`.
#' @param point_alpha Numeric. Transparency of the observed radial
#'   points. Default is `0.8`.
#' @param line_colour Character. Colour of the radial profile curve.
#'   Default is `"#08519C"`.
#' @param linewidth Numeric. Width of the radial profile curve.
#'   Default is `1.3`.
#' @param text_size Numeric. Base text size of the plot.
#'   Default is `13`.
#'
#' @return A list containing:
#' \describe{
#'   \item{ring_summary}{A data frame containing the radial rings,
#'   number of locations, number of occupied cells, space-use
#'   intensity, distance from the centre, and normalized distance
#'   and intensity.}
#'   \item{central_cell_center}{The centre of the grid cell containing
#'   the geometric centroid of the observed locations.}
#'   \item{radial_plot}{A `ggplot` object showing the radial
#'   space-use profile.}
#' }
#'
#' @details
#' Space-use intensity is calculated separately for each radial ring
#' as the total number of locations recorded in the ring divided by
#' the number of occupied cells comprising that ring. This prevents
#' rings containing more occupied cells from automatically having
#' higher intensity simply because they cover a larger number of
#' cells.
#'
#' The grid is constructed using the specified cell area and shape.
#' Only cells containing at least one recorded location are considered
#' part of the home range. Radial distance is measured from the centre
#' of the central grid cell to the centres of the occupied grid cells.
#'
#' The radial profile is therefore based on the spatial organization
#' of observed locations within the grid rather than on a kernel
#' density estimate. No spatial smoothing is applied unless
#' `smooth = TRUE` is specified.
#'
#' @examples
#' data(gibbons)
#' radial_plot(
#'   gibbons,
#'   cell_area = 5000,
#'   cell_shape = "hex",
#'   title = "Radial space-use profile of a gibbon group (0.5-ha hexagons)"
#' )
#'
#' @export
radial_plot <- function(
    points,
    cell_area = 5000,
    title = "",
    cell_shape = "hex",
    smooth = FALSE,
    loess_span = 0.8,
    show_points = FALSE,
    point_colour = "#08519C",
    point_size = 2,
    point_alpha = 0.8,
    line_colour = "#08519C",
    linewidth = 1.3,
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

    if (loess_span <= 0 || loess_span > 1) {
      stop(
        "loess_span must be > 0 and <= 1."
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
    # 5. KEEP USED CELLS
    # ==========================================================

    grid_used <- dplyr::filter(
      grid_sf,
      n > 0
    )


    # ==========================================================
    # 6. DEFINE CENTRAL CELL
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
    # 7. CENTRAL CELL CENTRE
    # ==========================================================

    central_cell_center <- sf::st_centroid(
      sf::st_geometry(
        grid_sf[central_cell_idx, ]
      )
    )


    # ==========================================================
    # 8. DISTANCE FROM CENTRAL CELL
    # ==========================================================

    all_centroids <- sf::st_centroid(
      sf::st_geometry(grid_sf)
    )

    grid_sf$dist <- as.numeric(
      sf::st_distance(
        central_cell_center,
        all_centroids
      )
    )


    # ==========================================================
    # 9. ASSIGN RADIAL RINGS
    # ==========================================================

    ring_width <- cellsize

    grid_sf$ring <- floor(
      grid_sf$dist / ring_width + 0.5
    )

    grid_sf$ring[
      central_cell_idx
    ] <- 0L


    # ==========================================================
    # 10. RADIAL SUMMARY
    # ==========================================================

    ring_summary <- grid_sf |>
      dplyr::filter(n > 0) |>
      sf::st_set_geometry(NULL) |>
      dplyr::group_by(ring) |>
      dplyr::summarise(
        n_total = sum(n),
        n_cells = dplyr::n(),
        intensity = n_total / n_cells,
        mean_dist = mean(dist),
        .groups = "drop"
      ) |>
      dplyr::arrange(ring)


    # ==========================================================
    # 11. NORMALIZE DISTANCE
    # ==========================================================

    if (max(ring_summary$ring) == 0) {

      ring_summary$norm_dist <- 0

    } else {

      ring_summary$norm_dist <-
        ring_summary$ring /
        max(ring_summary$ring)
    }


    # ==========================================================
    # 12. NORMALIZE SPACE USE
    # ==========================================================

    ring_summary$norm_intensity <-
      ring_summary$intensity /
      max(ring_summary$intensity)


    # ==========================================================
    # 13. CREATE RADIAL PLOT
    # ==========================================================

    radial_plot <- ggplot2::ggplot(
      ring_summary,
      ggplot2::aes(
        x = norm_dist,
        y = norm_intensity
      )
    )


    # ----------------------------------------------------------
    # Optional observed points
    # ----------------------------------------------------------

    if (show_points) {

      radial_plot <- radial_plot +

        ggplot2::geom_point(
          colour = point_colour,
          size = point_size,
          alpha = point_alpha
        )
    }


    # ==========================================================
    # 14. OBSERVED LINE OR LOESS
    # ==========================================================

    if (!smooth) {

      radial_plot <- radial_plot +

        ggplot2::geom_line(
          colour = line_colour,
          linewidth = linewidth
        )

    } else {

      n_rings <- nrow(ring_summary)

      # --------------------------------------------------------
      # Too few rings for useful LOESS
      # --------------------------------------------------------

      if (n_rings < 4) {

        warning(
          paste0(
            "Only ",
            n_rings,
            " radial rings are available. ",
            "LOESS smoothing was not applied; ",
            "the observed radial profile is shown instead."
          )
        )

        radial_plot <- radial_plot +

          ggplot2::geom_line(
            colour = line_colour,
            linewidth = linewidth
          )

      } else {

        minimum_span <- min(
          1,
          4 / n_rings
        )

        if (loess_span < minimum_span) {

          warning(
            paste0(
              "The requested LOESS span (",
              loess_span,
              ") is too small for ",
              n_rings,
              " radial rings. ",
              "LOESS smoothing was not applied; ",
              "the observed radial profile is shown instead. ",
              "Use a span >= ",
              round(
                minimum_span,
                2
              ),
              "."
            )
          )

          radial_plot <- radial_plot +

            ggplot2::geom_line(
              colour = line_colour,
              linewidth = linewidth
            )

        } else {

          radial_plot <- radial_plot +

            ggplot2::geom_smooth(
              method = "loess",
              span = loess_span,
              se = FALSE,
              colour = line_colour,
              linewidth = linewidth,
              na.rm = TRUE
            )
        }
      }
    }


    # ==========================================================
    # 15. PLOT FORMATTING
    # ==========================================================

    radial_plot <- radial_plot +

      ggplot2::scale_x_continuous(
        limits = c(0, 1),
        breaks = seq(
          0,
          1,
          0.2
        )
      ) +

      ggplot2::scale_y_continuous(
        limits = c(0, 1),
        breaks = seq(
          0,
          1,
          0.2
        )
      ) +

      ggplot2::labs(
        title = title,
        x = "Normalized distance",
        y = "Normalized space-use"
      ) +

      ggplot2::theme_bw(
        base_size = text_size
      ) +

      ggplot2::theme(
        panel.grid = ggplot2::element_blank()
      )


    # ==========================================================
    # 16. DISPLAY
    # ==========================================================

    print(radial_plot)


    # ==========================================================
    # 17. RETURN
    # ==========================================================

    invisible(
      list(
        ring_summary = ring_summary,
        central_cell_center = central_cell_center,
        radial_plot = radial_plot
      )
    )
  }
