% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.R
\name{get.field.heymsfield.2015}
\alias{get.field.heymsfield.2015}
\title{Get gridded warm-rain fraction from Field and Heymsfied (2015)}
\usage{
get.field.heymsfield.2015()
}
\description{
(Data courtesy of Paul Field.)
}
\examples{
    col.frac <- rev(RColorBrewer::brewer.pal(9, "RdYlBu"))[c(1:5,7,9)]
    phase.breaks <- c(-1e-8, 0.05, 0.1, 0.2 * 1:5)
get.field.heymsfield.2015() \%>\%
    mutate(fracwr = cut(fracwr, phase.breaks)) \%>\%
    ggplot(aes(x = lon, y = lat, fill = fracwr)) +
    geom_raster() +
    scale_fill_manual(values = col.frac, drop = FALSE) +
    geom_world_polygon() +
    scale_x_geo() +
    scale_y_geo() +
    coord_fixed(1, c(-180, 180), c(-90, 90))
get.field.heymsfield.2015() \%>\%
    mutate(frac = cut(frac, phase.breaks)) \%>\%
    ggplot(aes(x = lon, y = lat, fill = frac)) +
    geom_raster() +
    scale_fill_manual(values = col.frac, drop = FALSE) +
    geom_world_polygon() +
    scale_x_geo() +
    scale_y_geo() +
    coord_fixed(1, c(-180, 180), c(-90, 90))
}
