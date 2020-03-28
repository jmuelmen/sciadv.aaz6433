#' Get cloud summary \code{data.frame}
#'
#' This data frame makes it possible to look at more advanced
#' statistics than just the warm-rain fraction.  For example, one
#' could look at the probability of warm and cold rain (i.e.,
#' normalized by the total number of CloudSat footprints) in addition
#' to the warm-rain fraction (i.e., normalized by the number of
#' raining CloudSat footprints).
#' @export
get.cloud.summaries <- function() {
    ## readRDS("cloud-summaries-2006-2007-2008-2009-2010-2011.rds")
    readRDS("cloud-rain-summaries.rds")
}

#' Get precip rate summary \code{data.frame}
#'
#' @export
get.rainrate.summaries <- function() {
    readRDS("rainrate-summaries-2008.rds")
}

#' DARDAR rain phase data
#'
#' 2006 to 2011, cloud-top phase included, rows with no phase information removed
#' 
#' @export
get.rain.summaries <- function() {
    readRDS("rain-summaries-v8-phase.rds")
}

#' Get gridded warm-rain properties (2x2 degree lat-lon grid)
#' 
#' @export
#'
#' @examples
#' col.frac <- rev(RColorBrewer::brewer.pal(9, "RdYlBu"))[c(1:5,7,9)]
#' phase.breaks <- c(0, 0.05, 0.1, 0.2 * 1:5)
#' get.grid.warm() %>%
#'     mutate(sum = ice + liq + mixed) %>%
#'     gather(phase, frac, c(ice:mixed, sum)) %>%
#'     discretize(frac, phase.breaks, TRUE) %>%
#'     ## mutate(frac = cut(frac, phase.breaks)) %>%
#'     ggplot(aes(x = lon, y = lat, fill = frac)) +
#'     geom_raster() +
#'     scale_fill_manual(values = col.frac, drop = FALSE) +
#'     ## scale_fill_distiller(palette = "Spectral") +
#'     facet_grid(phase ~ denom) +
#'     scale_x_geo(facet = TRUE) +
#'     scale_y_geo() +
#'     geom_world_polygon() +
#'     theme_bw(24) + theme(legend.key.size = unit(2, "lines"))
get.grid.warm <- function() {
    ## this is now part of the package data
    data(warmrain)
    warmrain
    ## readRDS("cloud-warm-grid.rds")
}

#' Get gridded warm-rain fraction (2x2 degree lat-lon grid)
#' 
#' @export
get.warmfrac <- function() {
    data(warmrain)
    warmrain %>%
        filter(denom == "rain")
}

#' Get gridded "rain-certain" warm-rain fraction (2x2 degree lat-lon grid)
#' 
#' @export
get.warmfrac_certain <- function() {
    data(warmrain_certain)
    warmrain_certain %>%
        filter(denom == "rain")
}

#' Get gridded warm-rain fraction from Field and Heymsfied (2015)
#'
#' (Data courtesy of Paul Field.)
#'
#' @export
#'
#' @examples
#'     col.frac <- rev(RColorBrewer::brewer.pal(9, "RdYlBu"))[c(1:5,7,9)]
#'     phase.breaks <- c(-1e-8, 0.05, 0.1, 0.2 * 1:5)
#' get.field.heymsfield.2015() %>%
#'     mutate(fracwr = cut(fracwr, phase.breaks)) %>%
#'     ggplot(aes(x = lon, y = lat, fill = fracwr)) +
#'     geom_raster() +
#'     scale_fill_manual(values = col.frac, drop = FALSE) +
#'     geom_world_polygon() +
#'     scale_x_geo() +
#'     scale_y_geo() +
#'     coord_fixed(1, c(-180, 180), c(-90, 90))
#' get.field.heymsfield.2015() %>%
#'     mutate(frac = cut(frac, phase.breaks)) %>%
#'     ggplot(aes(x = lon, y = lat, fill = frac)) +
#'     geom_raster() +
#'     scale_fill_manual(values = col.frac, drop = FALSE) +
#'     geom_world_polygon() +
#'     scale_x_geo() +
#'     scale_y_geo() +
#'     coord_fixed(1, c(-180, 180), c(-90, 90))
get.field.heymsfield.2015 <- function() {
    nc <- ncdf4::nc_open("ann.nc")
    frac <- ncdf4::ncvar_get(nc, "frac")
    fracwr <- ncdf4::ncvar_get(nc, "fracwr")
    lon <- ncdf4::ncvar_get(nc, "Longitude")[,1]
    lat <- ncdf4::ncvar_get(nc, "Latitude")[1,]
    ncdf4::nc_close(nc)
    expand.grid(lon = lon, lat = lat) %>%
        mutate(frac = as.vector(replace(frac, frac == -999, NA)),
               fracwr = as.vector(replace(fracwr, fracwr == -999, NA)))
}

