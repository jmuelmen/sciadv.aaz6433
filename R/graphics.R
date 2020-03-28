using.tizk <- function() any(grepl("tikz", names(dev.cur())))

#' Color scale for warm-rain fraction: color names
#'
#' @param projector Logical.  Use bright color scale appropriate for
#'     projectors (default)
#' @export
warmfrac.col <- function(projector = TRUE) {
    if (projector) {
        rev(RColorBrewer::brewer.pal(9, "RdYlBu"))[c(1:5,7,9)]
    } else {
        c("#272840", rev(RColorBrewer::brewer.pal(10, "RdYlBu")))
    }
}

#' Color scale for warm-rain fraction: breaks
#'
#' @param projector Logical.  Use bright color scale appropriate for
#'     projectors (default)
#' @export
warmfrac.breaks <- function(projector = TRUE) {
    if (projector) {
        phase.breaks <- c(0, 0.05, 0.1, 0.2 * 1:5)
    } else {
        phase.breaks <- c(0, 0.05, 0.1 * 1:10)
    }
}

#' Color scale for warm-rain fraction: \code{\link{discrete_scale}} 
#'
#' @param name Expression.  Label for the scale
#' @param projector Logical.  Use bright color scale appropriate for
#'     projectors (default)
#' @export
scale_fill_warmfrac <- function(name = if (!using.tizk()) {
                                           expression(italic(f)[warm])
                                       } else {
                                           "$f_\\text{warm}$"
                                       },
                                labels,
                                projector = TRUE) {
    ggplot2::scale_fill_manual(values = warmfrac.col(projector),
                               labels = if (missing(labels)) {
                                            warmfrac.breaks(projector)[-1]
                                        } else {
                                            labels
                                        },
                               name = name, drop = FALSE) 
}

#' Color scale for warm-rain probability: color names
#'
#' @param projector Logical.  Use bright color scale appropriate for
#'     projectors (default)
#' @export
warmprob.col <- function(projector = TRUE) {
    if (projector) {
        rev(RColorBrewer::brewer.pal(9, "YlGnBu")[1:8])
    } else {
        rev(RColorBrewer::brewer.pal(9, "YlGnBu")[1:8])
    }
}

#' Color scale for warm-rain probability: breaks
#'
#' @param projector Logical.  Use bright color scale appropriate for
#'     projectors (default)
#' @export
warmprob.breaks <- function(projector = TRUE) {
    if (projector) {
        c(0,0.01,0.05,0.1,0.2,0.3,0.4,0.5,1)
    } else {
        c(0,0.01,0.05,0.1,0.2,0.3,0.4,0.5,1)
    }
}

#' Color scale for warm-rain probability: \code{\link{discrete_scale}} 
#'
#' @param name Expression.  Label for the scale
#' @param projector Logical.  Use bright color scale appropriate for
#'     projectors (default)
#' @export
scale_fill_warmprob <- function(name = if (!using.tizk()) {
                                           expression(italic(p)[warm])
                                       } else {
                                           "$p_\\text{warm}$"
                                       },
                                labels,
                                projector = TRUE) {
    ggplot2::scale_fill_manual(values = warmprob.col(projector),
                               labels = if (missing(labels)) {
                                            warmprob.breaks(projector)[-1]
                                        } else {
                                            labels
                                        },
                               name = name, drop = FALSE) 
}
