#' Generate summary data frame from CERES EBAF 
#'
#' @param infile NetCDF file containing CERES EBAF data
#' @return \code{data.frame} containing climatological TOA fluxes
#' @export
process.ceres <- function(infile) {
    nc <- ncdf4::nc_open(infile)
    month <- ncdf4::ncvar_get(nc, "ctime")
    lon <- ncdf4::ncvar_get(nc, "lon")
    lat <- ncdf4::ncvar_get(nc, "lat")
    
    df <- expand.grid(lon = as.vector(lon),
                      lat = as.vector(lat),
                      month = as.vector(month)) %>%
        dplyr::mutate(toa_sw_all_clim             = as.vector(ncdf4::ncvar_get(nc, "toa_sw_all_clim"             )),
                      toa_lw_all_clim             = as.vector(ncdf4::ncvar_get(nc, "toa_lw_all_clim"             )),
                      toa_net_all_clim            = as.vector(ncdf4::ncvar_get(nc, "toa_net_all_clim"            )),
                      toa_sw_clr_clim             = as.vector(ncdf4::ncvar_get(nc, "toa_sw_clr_clim"             )),
                      toa_lw_clr_clim             = as.vector(ncdf4::ncvar_get(nc, "toa_lw_clr_clim"             )),
                      toa_net_clr_clim            = as.vector(ncdf4::ncvar_get(nc, "toa_net_clr_clim"            )),
                      toa_cre_sw_clim             = as.vector(ncdf4::ncvar_get(nc, "toa_cre_sw_clim"             )),
                      toa_cre_lw_clim             = as.vector(ncdf4::ncvar_get(nc, "toa_cre_lw_clim"             )),
                      toa_cre_net_clim            = as.vector(ncdf4::ncvar_get(nc, "toa_cre_net_clim"            )),
                      solar_clim                  = as.vector(ncdf4::ncvar_get(nc, "solar_clim"                  )),
                      cldtau_total_day_clim       = as.vector(ncdf4::ncvar_get(nc, "cldtau_total_day_clim"       )),
                      cldarea_total_daynight_clim = as.vector(ncdf4::ncvar_get(nc, "cldarea_total_daynight_clim" )))
}
