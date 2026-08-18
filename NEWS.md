# gridHR 1.0.0

## Initial release

* Initial release of `gridHR`, providing grid-based tools for home-range
  estimation and spatial analysis.

* `grid_hr()` estimates home-range size and represents spatial variation
  in space-use intensity using regular hexagonal or square grids.

* `hr_cell_size()` evaluates home-range size across different grid-cell
  areas.

* `min_cell()` evaluates topological connectivity across grid resolutions
  and identifies the smallest tested cell size producing a fully connected
  home range.

* `hr_rare()` evaluates how grid-based home-range estimates change with
  increasing numbers of animal locations through spatial rarefaction.

* `radial_plot()` characterizes the relationship between space-use intensity
  and distance from the centre toward the periphery of the home range.

* Includes example datasets for demonstrating the package functions.

## Improvements

* Working on methods for identifying and plotting core areas;

  the most intensively used cells within a grid-based home range.

* Working on methods for comparing observed radial space-use profiles with

  theoretical profiles to identify the profile that best matches observed

  patterns of space use.
