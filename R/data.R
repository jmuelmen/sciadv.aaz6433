#' Gridded warm-rain fractions ("possible drizzle" to "rain certain")
#'
#' A dataset containing the A-Train warm-rain-fraction climatology
#'
#' @format A data frame with 6 variables:
#' \describe{
#'   \item{lon}{longitude; -180 < lon < 180}
#'   \item{lat}{latitude}
#'   \item{ice}{fraction of raining ice-topped clouds}
#'   \item{liq}{fraction of raining liquid clouds}
#'   \item{mixed}{fraction of raining ice clouds with liquid tops}
#'   \item{denom}{denominator for fractions (character):
#'     \describe{
#'        \item{all}{number of satellite overpasses}
#'        \item{cloud}{number of cloudy satellite overpasses}
#'        \item{rain}{number of raining satellite overpasses}
#'     }}
#' }
#' 
#' @source See the first example in \code{\link{grid.warm}}, meant to
#'     be run in \code{/projekt5/climate/jmuelmen} on monsun
"warmrain"

#' Gridded warm-rain fractions ("rain certain")
#'
#' A dataset containing the A-Train warm-rain-fraction climatology
#'
#' @format A data frame with 6 variables:
#' \describe{
#'   \item{lon}{longitude; -180 < lon < 180}
#'   \item{lat}{latitude}
#'   \item{ice}{fraction of raining ice-topped clouds}
#'   \item{liq}{fraction of raining liquid clouds}
#'   \item{mixed}{fraction of raining ice clouds with liquid tops}
#'   \item{denom}{denominator for fractions (character):
#'     \describe{
#'        \item{all}{number of satellite overpasses}
#'        \item{cloud}{number of cloudy satellite overpasses}
#'        \item{rain}{number of raining satellite overpasses}
#'     }}
#' }
#' 
#' @source See the first example in \code{\link{grid.warm}}, meant to
#'     be run in \code{/projekt5/climate/jmuelmen} on monsun
"warmrain_certain"

#' CERES EBAF climatology
#'
#' A dataset containing the CERES EBAF 2005-2015 climatology
#'
#' @format A data frame with the following variables:
#' \describe{
#'   \item{toa_sw_all_clim            }{}
#'   \item{toa_lw_all_clim            }{}
#'   \item{toa_net_all_clim           }{}
#'   \item{toa_sw_clr_clim            }{}
#'   \item{toa_lw_clr_clim            }{}
#'   \item{toa_net_clr_clim           }{}
#'   \item{toa_cre_sw_clim            }{}
#'   \item{toa_cre_lw_clim            }{}
#'   \item{toa_cre_net_clim           }{}
#'   \item{solar_clim                 }{}
#'   \item{cldtau_total_day_clim      }{}
#'   \item{cldarea_total_daynight_clim}{}
#' }
#' 
#' @source CERES EBAF Version: Edition 4.0; Release Date March 7, 2017;
#' DOI: 10.5067/TERRA+AQUA/CERES/EBAF-TOA_L3B.004.0
"ceres"

