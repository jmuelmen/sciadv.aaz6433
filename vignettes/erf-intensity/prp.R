## ---- prp-setup ---------------------
prp_base <- function(path.prp, prefix) {
    df.prp.sw <- ldply(c("cdnc", "cf", "lwp"), function(pert) {
        fname <- sprintf("%s/%s_flsh_diff_%s_timmean.nc", path.prp, prefix, pert)
        varname <- sprintf("flsh_diff_%s", pert)
        nc <- nc_open(fname)
        on.exit(nc_close(nc))
        var <- ncvar_get(nc, varname)
        lon <- ncvar_get(nc, "lon")
        lat <- ncvar_get(nc, "lat")
        expand.grid(lon = as.vector(lon), lat = as.vector(lat)) %>%
            mutate(erf = -as.vector(var),
                   pert = pert,
                   spectrum = "SW")
    })

    df.prp.lw <- ldply(c("cdnc", "cf", "lwp"), function(pert) {
        fname <- sprintf("%s/%s_flth_diff_%s_timmean.nc", path.prp, prefix, pert)
        varname <- sprintf("flth_diff_%s", pert)
        nc <- nc_open(fname)
        on.exit(nc_close(nc))
        var <- ncvar_get(nc, varname)
        lon <- ncvar_get(nc, "lon")
        lat <- ncvar_get(nc, "lat")
        expand.grid(lon = as.vector(lon), lat = as.vector(lat)) %>%
            mutate(erf = -as.vector(var),
                   pert = pert,
                   spectrum = "LW")
    })

    bind_rows(df.prp.sw, df.prp.lw)
}

prp <- function(path.prp) {
    prp_base(path.prp, "prp")
}

prp.fw <- function(path.prp) {
    prp_base(path.prp, "prp_fw")
}

prp.bk <- function(path.prp) {
    prp_base(path.prp, "prp_bk")
}

prp.global <- function(path.prp) {
    df.prp.sw <- ldply(c("cdnc", "cf", "lwp"), function(pert) {
        fname <- sprintf("%s/prp_flsh_diff_%s_global.nc", path.prp, pert)
        varname <- sprintf("flsh_diff_%s", pert)
        nc <- nc_open(fname)
        on.exit(nc_close(nc))
        var <- ncvar_get(nc, varname)
        data.frame(erf = -as.vector(var)) %>%
            mutate(pert = pert,
                   spectrum = "SW")
    })
    
    df.prp.lw <- ldply(c("cdnc", "cf", "lwp"), function(pert) {
        fname <- sprintf("%s/prp_flth_diff_%s_global.nc", path.prp, pert)
        varname <- sprintf("flth_diff_%s", pert)
        nc <- nc_open(fname)
        on.exit(nc_close(nc))
        var <- ncvar_get(nc, varname)
        data.frame(erf = -as.vector(var)) %>%
            mutate(pert = pert,
                   spectrum = "LW")
    })
    
    bind_rows(df.prp.sw, df.prp.lw)
}

expname.prp <- function(ccraut, ccauloc, creth,
                        cautalpha, cautbeta, nocosp,
                        nudged, daily, three.hourly) {
    experiment <- sprintf("~/echam-prp/rain_%g%s%s%s%s%s%s%s%s",
                          ccraut,
                          ifelse(is.na(ccauloc), "", sprintf("_%g", ccauloc)),
                          ifelse(is.na(creth), "", sprintf("_%g", creth)),
                          ifelse(is.na(cautalpha), "", sprintf("_cautalpha_%g", cautalpha)),
                          ifelse(is.na(cautbeta), "", sprintf("_cautbeta_%g", cautbeta)),
                          ifelse(nocosp, "_no-cosp", ""),
                          ifelse(nudged, "_nudged", ""),
                          ifelse(daily, "_daily", ""),
                          ifelse(three.hourly, "_3hourly", ""))
}

get.prp.echam <- function(ccraut = 4, ccauloc = 1, creth = -1,
                          cautalpha = NA,
                          cautbeta = NA) {
    plyr::ddply(expand.grid(ccraut = ccraut,
                            ccauloc = ccauloc,
                            creth = creth,
                            cautalpha = cautalpha,
                            cautbeta = cautbeta),
                ~ ccraut + ccauloc + creth + cautalpha + cautbeta,
                function(x) with(x, {
                    nudged <- TRUE
                    daily <- FALSE
                    three.hourly <- TRUE
                    experiment.nocosp <- expname.prp(ccraut, ccauloc, creth,
                                                     cautalpha, cautbeta, nocosp = FALSE, nudged, daily, three.hourly)
                    experiment.cosp <- expname.prp(ccraut, ccauloc, creth,
                                                   cautalpha, cautbeta, nocosp = TRUE, nudged, daily, three.hourly)
                    if (!is.na(file.info(experiment.nocosp)$isdir))
                        df <- prp.global(experiment.nocosp)
                    else
                        df <- prp.global(experiment.cosp)
                    df %>% 
                        dplyr::mutate(ccraut = ccraut,
                                      ccauloc = ccauloc,
                                      creth = creth,
                                      cautalpha = cautalpha,
                                      cautbeta = cautbeta)
                }))
}

prp.combine.lw.and.sw <- function(df) {
    df %>%
        tidyr::spread(spectrum, erf) %>%
        dplyr::mutate(erf = LW + SW) %>%
        dplyr::select(-c(LW, SW))
}

prp.plot <- function(df, range = 5, palette = "RdYlBu", symmetric = TRUE, direction = -1, title = "W~m$^{-2}$") {
    if (length(range) == 1) {
        range <- c(-range,
                   ifelse(symmetric, range, 0))
    }
    df %>%
        mutate(lon = ifelse(lon <= 180, lon, lon - 360)) %>%
        ggplot(aes(lon, lat, fill = pmax(pmin(erf, range[2]), range[1]))) +
        geom_raster() +
        scale_x_geo(facet = FALSE) + scale_y_geo() +
        coord_fixed(xlim = c(-180, 180), ylim = c(-80, 80), expand = FALSE) +
        ## scale_x_continuous("", labels = NULL, breaks = NULL) +
        ## scale_y_continuous("", labels = NULL, breaks = NULL) +
        ## scale_fill_manual(values = col.frac, name = expression(f[liq])) +
        ## scale_fill_warmfrac() +
        ## scale_fill_brewer("$F_\\mathcal{L}~(\\text{W~m}^{-2})$", palette = "RdBu", drop = FALSE, direction = -1) +
        scale_fill_distiller(labels = tikz_sanitize, title, palette = palette, 
                             limits = range, direction = direction) +
        geom_world_polygon(highres = FALSE) +
        theme_bw(12)  +
        facet_grid(as.character(dplyr::groups(df))[1:2] %>%
                   coalesce(".") %>%
                   Reduce(function(x, y) paste(x, y, sep = " ~ "), .)) +
        theme(legend.position = "bottom", legend.box = "horizontal") +
        guides(fill = guide_colorbar(direction = "horizontal", title.vjust = 0.75, barwidth = 8))
}
