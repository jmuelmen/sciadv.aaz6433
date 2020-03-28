#' Combine summary CloudSat cloud/precip flag information summary
#' files into a long data.frame
#'
#' @param path Path to cloud summary files
#' @param years Years to process
#' @param out.fname.prefix Prefix for output file name (<prefix><years>.rds)
#' @param ... Optional list of variables to select (lazily evaluated) 
#' @return \code{NULL}
#' @export
combine.cloud <- function(path = "cloud-summaries",
                          years = 2006:2011,
                          out.fname.prefix = "cloud-summaries-",
                          vars = c(lon, lat, time,
                                   daynight, landsea,
                                   conv.strat, surface.elev,
                                   num.cloud.layers, cloud.flag,
                                   precip.flag, dardar.precip.flag)) {
    vars. <- lazyeval::lazy(vars)
    ll <- plyr::adply(list.files(path, ".*rds", full.names = TRUE), 1, function(fname) {
        if (any(sapply(sprintf("DARDAR-MASK_v1.1.4_%d", years), grepl, fname))) {
            print(fname)
            res <- readRDS(fname) %>%
                dplyr::select_(vars.)
        }
    }, .parallel = TRUE)
    ll %>%
        select(-X1) %>%
        saveRDS(sprintf("%s%s.rds", out.fname.prefix, paste(years, collapse = "-")))
}

#' Add cloud-top phase information to  summary warm/cold rain mask files into long data frames
#'
#' @param path Path to cloud summary files
#' @param years Years to process
#' @param out.fname.prefix Prefix for output file name (<prefix><years>.rds)
#' @param ... Optional list of variables to select (lazily evaluated) 
#' @return \code{NULL}
#' @export
add.rain.info.to.cloud <- function() {
    df.rain <- get.rain.summaries() %>%
        dplyr::select(lon, lat, time, phase)
    df.cloud <- get.cloud.summaries() 
    gc()
    
    left_join(df.cloud,
              df.rain,
              by = c("time", "lon", "lat")) %>%
        saveRDS("cloud-rain-summaries.rds")
}

#' Grid warm rain statistics
#'
#' @param df Data frame to grid, \code{\link{get.cloud.summaries}()} by default
#' @param bins_lon Number of bins or vector of bin boundaries (longitude)
#' @param bins_lat Number of bins or vector of bin boundaries (latitude)
#' @param out.fname File name for output
#'
#' @export
#'
#' @examples
#' ## make the \code{warmrain.rda} file used by \code{data(\link{warmrain})}
#' library(devtools)
#' warmrain <- grid.warm(get.cloud.summaries(),
#'                       bins_lon = 180, bins_lat = 90,
#'                       NULL) 
#' str(warmrain)
#' devtools::use_data(warmrain, pkg = "/home/jmuelmen/lib64/R/library/preciptools")
#' 
#' 
#' ## look at the Great Lakes region, if that's your thing
#' df <- grid.warm(get.cloud.summaries() %>%
#'                filter(lon > -95, lon < -75, lat > 40, lat < 50),
#'                seq(-95, -75, 0.1), seq(40, 50, 0.1),
#'                "great-lakes-grid.rds")
#' col.frac <- rev(RColorBrewer::brewer.pal(9, "RdYlBu"))[c(1:5,7,9)]
#' phase.breaks <- c(0, 0.05, 0.1, 0.2 * 1:5)
#' df %>%
#'     mutate(sum = ice + liq + mixed) %>%
#'     gather(phase, frac, c(ice:mixed, sum)) %>%
#'     discretize(frac, phase.breaks, TRUE) %>%
#'     ggplot(aes(x = lon, y = lat)) +
#'     geom_raster(aes(fill = frac)) +
#'     geom_world_polygon(highres = TRUE, lwd = 0.25) +
#'     xlim(c(-95, -75)) + ylim(c(40,50)) +
#'     scale_fill_manual(values = col.frac, drop = FALSE) +
#'     facet_grid(phase ~ denom) +
#'     coord_fixed(sqrt(2)) +
#'     theme_bw()
grid.warm <- function(df = get.cloud.summaries(),
                      bins_lon = 180, bins_lat = 90,
                      out.fname = "cloud-warm-grid.rds",
                      precip.classes = 1:3) {
    df %>%
        plotutils::discretize(lon, bins_lon) %>%
        plotutils::discretize(lat, bins_lat) %>%
        plyr::ddply(~ lon + lat, function(x) {
            tbl <- table(filter(x, precip.flag %in% precip.classes)$phase)
            df <- data.frame(ice = numeric(0),
                             liq = numeric(0),
                             mixed = numeric(0))
            df[nrow(df) + 1, ] <- tbl / sum(tbl)
            df[nrow(df) + 1, ] <- tbl / sum(x$cloud.flag)
            df[nrow(df) + 1, ] <- tbl / nrow(x)
            df %>%
                mutate(denom = c("rain", "cloud", "all"))
        }) %>%
        filter(is.finite(ice), is.finite(liq), is.finite(mixed)) -> df.out

    if (!is.null(out.fname)) {
        df.out %>%
            saveRDS(out.fname)
    }

    df.out
}
