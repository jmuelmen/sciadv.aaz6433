#' Generate summary warm/cold rain mask files from DARDAR,
#' 2C-PRECIP-COLUMN, and CALMOD
#'
#' At the end of processing, a summary file containing, for each
#' CloudSat column, a dry/warm/cold/snow flag based on DARDAR and
#' 2C-PRECIP-COLUMN flags is produced.
#' 
#' @param dardar.path Path to DARDAR mask files (used for cloud phase
#'     and DARDAR precip flag)
#' @param 2cprecip.path Path to 2C-PRECIP-COLUMN files (used for
#'     2C-PRECIP-COLUMN rain flag)
#' @return \code{NULL}
#' @export
cloud.summaries <- function(path.dardar = "ftp.icare.univ-lille1.fr/SPACEBORNE/MULTI_SENSOR/DARDAR_MASK",
                            path.2cprecip = "/projekt3/climate/jmuelmen/ftp.icare.univ-lille1.fr/SPACEBORNE/CLOUDSAT/2C-PRECIP-COLUMN/",
                            path.calmod = "/projekt3/climate/CALMOD/CALMOD/") {
    lf <- ## "/tmp/CER-NEWS_CCCM_Aqua-FM3-MODIS-CAL-CS_RelB1_905906.20071226.hdf"
        list.files(path = path.dardar, pattern = "DARDAR-MASK.*hdf", recursive = TRUE, full.names = TRUE)

    lf.2cprecip <- list.files(path = path.2cprecip, pattern = ".*hdf",
                              recursive = TRUE, full.names = TRUE)

    lf.calmod <- list.files(path = path.calmod, pattern = ".*h5",
                            recursive = TRUE, full.names = TRUE)
    lf.calmod <- lf.calmod[order(basename(lf.calmod))]

    res <- adply(lf, 1, .parallel = TRUE, function(fname) {
        ## don't remake files that already exist
        if (length(list.files("cloud-summaries", gsub(".hdf", ".rds", basename(fname)))) != 0)
            return(data.frame(NULL))
        ## fname <- "dardar/ftp.icare.univ-lille1.fr/SPACEBORNE/MULTI_SENSOR/DARDAR_MASK/2010/2010_10_27/DARDAR-MASK_v1.1.4_2010300224841_23935.hdf"
        g.classif <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":68', fname))
        r.classif <- getRasterData(g.classif)
        g.hgt <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":0', fname))
        r.hgt <- getRasterData(g.hgt)
        ## g.lid.time <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":17', fname))
        ## r.lid.time <- getRasterData(g.lid.time)
        g.lat <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":4', fname))
        r.lat <- getRasterData(g.lat)
        g.lon <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":5', fname))
        r.lon <- getRasterData(g.lon)
        g.2bcldclass.precip <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":30', fname))
        r.2bcldclass.precip <- getRasterData(g.2bcldclass.precip)
        g.2bcldclass.cloud <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":9', fname))
        r.2bcldclass.cloud <- getRasterData(g.2bcldclass.cloud) 
        g.daynight <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":24', fname))
        r.daynight <- factor(getRasterData(g.daynight), levels = 0:1, labels = c("day", "night"))
        g.landsea <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":22', fname))
        r.landsea <- getRasterData(g.landsea)

        ## g.temp <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":31', fname))
        ## r.temp <- getRasterData(g.temp)
        ## g.atb <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":18', fname))
        ## r.atb <- getRasterData(g.atb)
        ## g.atb.par <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":19', fname))
        ## r.atb.par <- getRasterData(g.atb.par)
        ## g.atb.per <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":20', fname))
        ## r.atb.per <- getRasterData(g.atb.per)
        g.z <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":7', fname))
        r.z <- getRasterData(g.z) / 1e4
        ## g.dist <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":16', fname))
        ## r.dist <- getRasterData(g.dist)
        g.utc <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":1', fname))
        r.utc <- getRasterData(g.utc)  ## seconds since 00:00:00 on the current day

        dardar.datestring <- strsplit(basename(fname), "_")[[1]][3]
        dardar.date <- as.POSIXlt(dardar.datestring, format = "%Y%j", tz = "UTC") ## strips off the %H%M%S --> 00:00:00 UTC
        dardar.date.begin <- dardar.date + r.utc[1]
        dardar.date.end <- dardar.date + r.utc[length(r.utc)]

        ##################################################################
        # get rain mask from 2C-PRECIP
        ##################################################################
        ## find 2C-PRECIP-COLUMN file corresponding to the DARDAR file (easy)
        fname.2cprecip <- lf.2cprecip[grep(dardar.datestring, lf.2cprecip)]
        if (length(fname.2cprecip) != 1) {
            conv.strat<- rep(NA, length(rain.mask))
            rain.mask <- FALSE
        } else {
            list.sds <- h4list(fname.2cprecip, ignore.vd = FALSE) ## never ignore vd...
            conv.strat <- h4read(fname.2cprecip, list.sds, "Conv_strat_flag")
            precip.flag <- h4read(fname.2cprecip, list.sds, "Precip_flag")
            surface.elevation <- h4read(fname.2cprecip, list.sds, "DEM_elevation")
            cloud.flag <- h4read(fname.2cprecip, list.sds, "Cloud_flag")
        }
        
        dardar.precip.flag <- apply(r.classif, 2, function(vfm) any(vfm == dardar.rain))

        ############################################################
        # determination of phase at cloud top
        ############################################################
        r.classif.and.radar <- abind(vfm = r.classif, Ze = r.z, cldcl = r.2bcldclass.cloud, rev.along = 0)
        res <- adply(r.classif.and.radar, 2, phase.at.top.of.lowest.layer, r.hgt = r.hgt)

        #############################################################
        # Calculation of CPR cloud base
        #############################################################
        cpr.cloud.base.height <- sapply(c(20, 30, 40), function(min.cldclass) { ## increasing thresholds on the 2B-CLDCLASS cloud mask
            apply(r.2bcldclass.cloud, 2, function(cpr, min.cldclass) {
                min(r.hgt[cpr >= min.cldclass])
            }, min.cldclass)
        })
        dimnames(cpr.cloud.base.height)[[2]] <- sprintf("cpr.cloud.base.height.2bcldclass.cldmask.%d", 10 * 2:4)
        
        res <- cbind(res, precip.flag = precip.flag,
                     dardar.precip.flag = dardar.precip.flag,
                     cloud.flag = cloud.flag,
                     lon = r.lon, lat = r.lat,
                     time = dardar.date + r.utc,
                     daynight = r.daynight,
                     landsea = r.landsea,
                     conv.strat = conv.strat,
                     surface.elev = surface.elevation,
                     cpr.cloud.base.height = cpr.cloud.base.height)

        ############################################################
        # Collocation with CALMOD
        ############################################################
        dates.calmod <- sapply(strsplit(basename(lf.calmod), "\\."),
                               function(x) as.POSIXct(sprintf("%s.%s", x[3], x[4]), "UTC", format = "%Y-%m-%d.%H-%M-%S"))
        stopifnot(all(order(dates.calmod) == 1 : length(dates.calmod)))
        dates.calmod.begin <- dates.calmod[-length(dates.calmod)]
        dates.calmod.end <- dates.calmod[-1]
        as.POSIXct(dates.calmod[dates.calmod < dardar.date.end & dates.calmod > dardar.date.begin], origin = "1970-01-01", tz = "UTC")
        
        fname.calmod <- lf.calmod[dates.calmod < dardar.date.end & dates.calmod > dardar.date.begin]
        if (length(fname.calmod) > 0) {
            ## if there is no CALMOD file, adply will automatically fill the columns with NA
            m.lat <- h5read(fname.calmod, "MODIS.C6.MYD06.333m/Latitude")
            m.lon <- h5read(fname.calmod, "MODIS.C6.MYD06.333m/Longitude")
            
            back.ass <- back.associate.calmod(r.lon, r.lat, m.lon, m.lat)
            myd06 <- h5read(fname.calmod, "MODIS.C6.MYD06.333m")
            ## myd06 is a list of variables; for each list element, select the entries associated with rainy DARDAR profiles
            df.myd06 <- transform(t( ## just some basic reshaping
                                    sapply(back.ass[1,], function(i, dim1) {
                                        ## i <- 22850
                                        ## dim1 <- 63450
                                        unlist(sapply(myd06, function(x) { 
                                            if (length(dim(x)) < 2) ## vectors just pass through
                                                x[i] 
                                            else {
                                                if (dim(x)[1] == dim1) ## arrays are unpacked (that's what the unlist() is for)
                                                    x[i,]
                                                else x[,i]
                                            }
                                        }))
                                    }, ## finally, some arrays are [length(track), n], others are [n, length(track)] for
                                       ## n = {2 (source pixels), 9 (QA flags), etc}, so we need to know length(track)
                                           dim1 = median(sapply(myd06, function(x) if (length(dim(x)) < 2) length(x) else NA), na.rm = TRUE))))
            ## this was too simple (would have worked if all the variables were vectors, but some are arrays): df.myd06 <- transform(sapply(myd06, function(x) x[back.ass[1,]])) ## select the MYD06 values along the rainy parts of the track
            res <- cbind(res, replace(df.myd06, df.myd06 == -999, NA)) ## CALMOD uses -999 as NA
        }            

        GDAL.close(g.classif)
        GDAL.close(g.hgt    )
        GDAL.close(g.lat    )
        GDAL.close(g.lon    )
        GDAL.close(g.utc    )
        ## sapply(nc.goccp, nc_close)
        
        saveRDS(res, file = paste("cloud-summaries", gsub(".hdf", ".rds", basename(fname)), sep = "/"))
        res
    })

    res
}

#' Generate summary warm/cold rain mask files from DARDAR,
#' 2C-PRECIP-COLUMN, and CALMOD
#'
#' At the end of processing, a summary file containing, for each
#' CloudSat column, a dry/warm/cold/snow flag based on DARDAR and
#' 2C-PRECIP-COLUMN flags is produced.
#' 
#' @param dardar.path Path to DARDAR mask files (used for cloud phase
#'     and DARDAR precip flag)
#' @param 2cprecip.path Path to 2C-PRECIP-COLUMN files (used for
#'     2C-PRECIP-COLUMN rain flag)
#' @return \code{NULL}
#' @export
make.it.rain <- function(path = "ftp.icare.univ-lille1.fr/SPACEBORNE/MULTI_SENSOR/DARDAR_MASK") {
    lf <- ## "/tmp/CER-NEWS_CCCM_Aqua-FM3-MODIS-CAL-CS_RelB1_905906.20071226.hdf"
        list.files(path = path, pattern = "DARDAR-MASK.*hdf", recursive = TRUE, full.names = TRUE)

    ## list of all GOCCP files (will come in handy when we need to match GOCCP to DARDAR)
    lf.goccp <- list.files(path = "goccp/ftp.climserv.ipsl.polytechnique.fr/cfmip/GOCCP/instant_SR_CR_DR/grid_L40", pattern = ".*nc",
                           recursive = TRUE, full.names = TRUE)
    lf.goccp <- lf.goccp[order(basename(lf.goccp))]
    ## turn the file names into file start times
    dates.goccp <- sapply(strsplit(basename(lf.goccp), "_"),
                          function(x) as.POSIXct(x[5], "UTC", format = "%Y-%m-%dT%H-%M-%SZ"))
    stopifnot(all(order(dates.goccp) == 1 : length(dates.goccp)))
    dates.goccp.begin <- dates.goccp[-length(dates.goccp)]
    dates.goccp.end <- dates.goccp[-1]
    ## dates.goccp is a global variable containing the dates (0000 UTC) of the files for later use (to calculate times)
    dates.goccp <- sapply(strsplit(basename(lf.goccp), "_"),
                          function(x) as.POSIXct(x[5], "UTC", format = "%Y-%m-%dT"))

    lf.2cprecip <- list.files(path = "/projekt3/climate/jmuelmen/ftp.icare.univ-lille1.fr/SPACEBORNE/CLOUDSAT/2C-PRECIP-COLUMN/", pattern = ".*hdf",
                           recursive = TRUE, full.names = TRUE)

    res <- adply(lf, 1, .parallel = TRUE, function(fname) {
        ## don't remake files that already exist
        if (length(list.files("rain-summaries", gsub(".hdf", ".rds", basename(fname)))) != 0)
            return(data.frame(NULL))
        ## fname <- "dardar/ftp.icare.univ-lille1.fr/SPACEBORNE/MULTI_SENSOR/DARDAR_MASK/2010/2010_10_27/DARDAR-MASK_v1.1.4_2010300224841_23935.hdf"
        g.classif <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":68', fname))
        r.classif <- getRasterData(g.classif)
        g.hgt <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":0', fname))
        r.hgt <- getRasterData(g.hgt)
        ## g.lid.time <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":17', fname))
        ## r.lid.time <- getRasterData(g.lid.time)
        g.lat <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":4', fname))
        r.lat <- getRasterData(g.lat)
        g.lon <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":5', fname))
        r.lon <- getRasterData(g.lon)
        g.2bcldclass.precip <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":30', fname))
        r.2bcldclass.precip <- getRasterData(g.2bcldclass.precip)
        g.2bcldclass.cloud <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":9', fname))
        r.2bcldclass.cloud <- getRasterData(g.2bcldclass.cloud) 
        g.daynight <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":24', fname))
        r.daynight <- factor(getRasterData(g.daynight), levels = 0:1, labels = c("day", "night"))
        g.landsea <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":22', fname))
        r.landsea <- getRasterData(g.landsea)

        ## g.temp <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":31', fname))
        ## r.temp <- getRasterData(g.temp)
        ## g.atb <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":18', fname))
        ## r.atb <- getRasterData(g.atb)
        ## g.atb.par <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":19', fname))
        ## r.atb.par <- getRasterData(g.atb.par)
        ## g.atb.per <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":20', fname))
        ## r.atb.per <- getRasterData(g.atb.per)
        g.z <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":7', fname))
        r.z <- getRasterData(g.z) / 1e4
        ## g.dist <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":16', fname))
        ## r.dist <- getRasterData(g.dist)
        g.utc <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":1', fname))
        r.utc <- getRasterData(g.utc)  ## seconds since 00:00:00 on the current day

        dardar.datestring <- strsplit(basename(fname), "_")[[1]][3]
        dardar.date <- as.POSIXlt(dardar.datestring, format = "%Y%j", tz = "UTC") ## strips off the %H%M%S --> 00:00:00 UTC
        dardar.date.begin <- dardar.date + r.utc[1]
        dardar.date.end <- dardar.date + r.utc[length(r.utc)]

        ##################################################################
        # get rain mask from 2C-PRECIP
        ##################################################################
        ## find 2C-PRECIP-COLUMN file corresponding to the DARDAR file (easy)
        fname.2cprecip <- lf.2cprecip[grep(dardar.datestring, lf.2cprecip)]
        if (length(fname.2cprecip) != 1) {
            conv.strat<- rep(NA, length(rain.mask))
            rain.mask <- FALSE
        } else {
            list.sds <- h4list(fname.2cprecip, ignore.vd = FALSE) ## never ignore vd...
            conv.strat <- h4read(fname.2cprecip, list.sds, "Conv_strat_flag")
            precip.flag <- h4read(fname.2cprecip, list.sds, "Precip_flag")
            surface.elevation <- h4read(fname.2cprecip, list.sds, "DEM_elevation")
            rain.mask <- precip.flag %in% 1:7
        }
        
        ## rain.mask.dardar <- apply(r.classif, 2, function(vfm) any(vfm == dardar.rain))
        if (all(rain.mask == FALSE))
            return(data.frame(NULL))
        r.rain <- r.classif[, rain.mask]


        ############################################################
        # Collocation with GOCCP
        ############################################################
        
        ## ## print(dates.goccp[dates.goccp.begin < dardar.date.end & dates.goccp.end > dardar.date.begin])
        ## dates.goccp <- dates.goccp[dates.goccp.begin < dardar.date.end & dates.goccp.end > dardar.date.begin]
        ## nc.goccp <- sapply(lf.goccp[dates.goccp.begin < dardar.date.end & dates.goccp.end > dardar.date.begin], nc_open, simplify = FALSE)
        ## print(sprintf("DARDAR: %s (%d GOCCP files)", fname, length(nc.goccp)))

        ## goccp.get <- function(nc.goccp, varname) {
        ##     ## get the variable from each goccp file, then bind the resulting arrays together
        ##     var <- NULL
        ##     for (i in 1:length(nc.goccp)) {
        ##         var.tmp <- ncvar_get(nc.goccp[[i]], varname)
        ##         if (varname == "time") { ## time is a special case because we need to add the date.  Note that the dates.goccp come from the current scope (and that dates.goccp means different things in different scopes).  This should definitely be fixed.
        ##             var.tmp <- 3600 * var.tmp + dates.goccp[i]
        ##         }
        ##         if (length(dim(var.tmp)) < 2) {
        ##             var <- c(var, var.tmp)
        ##         } else {
        ##             var <- cbind(var, var.tmp)
        ##         }
        ##     }
        ##     var
        ## }

        ## time.goccp <- goccp.get(nc.goccp, "time")
        ## print(paste("GOCCP time range:", as.POSIXct(range(time.goccp), origin = "1970-01-01", tz = "UTC")))
        ## hgt.goccp <- ncvar_get(nc.goccp[[1]], "alt_mid")  ## don't splice three copies of the same altitute together...
        ## hgt.bnd.goccp <- ncvar_get(nc.goccp[[1]], "alt_bound")  ## don't splice three copies of the same altitute together...
        ## hgt.bnd.goccp <- c(hgt.bnd.goccp[1,1], hgt.bnd.goccp[,2])
        ## lat.goccp <- goccp.get(nc.goccp, "latitude")
        ## lon.goccp <- goccp.get(nc.goccp, "longitude")
        ## ## se.goccp <- goccp.get(nc.goccp, "SE")
        ## phase.goccp <- goccp.get(nc.goccp, "instant_Phase")

        ## ## translate DARDAR index to GOCCP index
        ## back.ass <- back.associate.goccp(dardar.date + r.utc[rain.mask], r.lon[rain.mask], r.lat[rain.mask],
        ##                                  time.goccp, lon.goccp, lat.goccp)
        ## ## an array of the same length as the DARDAR height array that gives the
        ## ## index into the GOCCP height array
        ## ass.hgt <- as.integer(cut(r.hgt, hgt.bnd.goccp))

        ## ## is the DARDAR thermodynamic phase correct according to GOCCP?
        ## correct <- sapply(1 : length(r.lon[rain.mask]), function(x) {
        ##     ## for every DARDAR profile, find the corresponding GOCCP profile
        ##     x.goccp <- back.ass[1, x]
        ##     if (is.na(x.goccp))
        ##         return(rep(NA, length(r.hgt)))
        ##     classif <- r.rain[, x]
        ##     res <- sapply(1 : length(classif), function(r.j) {
        ##         dardar <- classif[r.j]
        ##         correct <- TRUE
        ##         j.goccp <- ass.hgt[r.j]
        ##         if (is.na(j.goccp))
        ##             return(correct)
        ##         goccp <- phase.goccp[j.goccp, x.goccp]
        ##         if (dardar == dardar.ice && (goccp == goccp.liq || goccp == goccp.fliq))
        ##             correct <- FALSE
        ##         if (dardar == dardar.liquidwarm && (goccp == goccp.ice || goccp == goccp.fice))
        ##             correct <- FALSE
        ##         if (dardar == dardar.supercooled && (goccp == goccp.ice || goccp == goccp.fice))
        ##             correct <- FALSE
        ##         correct
        ##     })
        ##     stopifnot(length(res) == length(r.hgt))
        ##     res
        ## })

        ## ## The extended VFM is calculated as follows:
        ## ## * if DARDAR and GOCCP agree, the extended VFM is the same as the DARDAR VFM
        ## ## * if DARDAR and GOCCP disagree, the extended VFM is the DARDAR VFM + 10.
        ## extended.vfm <- r.rain + 10 * (!correct)
        ## ## but then we don't use it anymore (after it turned out that the disagreement was at the 5% level)

        ############################################################
        # determination of phase at cloud top
        ############################################################
        res <- adply(r.rain, 2, phase.at.top.of.lowest.layer)

        #############################################################
        # Calculation of CPR cloud base
        #############################################################
        cpr.cloud.base.height <- sapply(c(20, 30, 40), function(min.cldclass) { ## increasing thresholds on the 2B-CLDCLASS cloud mask
            apply(r.2bcldclass.cloud[, rain.mask], 2, function(cpr, min.cldclass) {
                min(r.hgt[cpr >= min.cldclass])
            }, min.cldclass)
        })
        dimnames(cpr.cloud.base.height)[[2]] <- sprintf("cpr.cloud.base.height.2bcldclass.cldmask.%d", 10 * 2:4)
        
        res <- cbind(res, precip.flag = precip.flag[rain.mask],
                     lon = r.lon[rain.mask], lat = r.lat[rain.mask],
                     time = dardar.date + r.utc[rain.mask],
                     daynight = r.daynight[rain.mask],
                     landsea = r.landsea[rain.mask],
                     conv.strat = conv.strat[rain.mask],
                     surface.elev = surface.elevation[rain.mask],
                     cpr.cloud.base.height = cpr.cloud.base.height)

        ############################################################
        # Collocation with CALMOD
        ############################################################
        lf.calmod <- list.files(path = "/projekt3/climate/CALMOD/CALMOD/", pattern = ".*h5",
                                recursive = TRUE, full.names = TRUE)
        lf.calmod <- lf.calmod[order(basename(lf.calmod))]
        dates.calmod <- sapply(strsplit(basename(lf.calmod), "\\."),
                               function(x) as.POSIXct(sprintf("%s.%s", x[3], x[4]), "UTC", format = "%Y-%m-%d.%H-%M-%S"))
        stopifnot(all(order(dates.calmod) == 1 : length(dates.calmod)))
        dates.calmod.begin <- dates.calmod[-length(dates.calmod)]
        dates.calmod.end <- dates.calmod[-1]
        as.POSIXct(dates.calmod[dates.calmod < dardar.date.end & dates.calmod > dardar.date.begin], origin = "1970-01-01", tz = "UTC")
        
        fname.calmod <- lf.calmod[dates.calmod < dardar.date.end & dates.calmod > dardar.date.begin]
        if (length(fname.calmod) > 0) {
            ## if there is no CALMOD file, adply will automatically fill the columns with NA
            m.lat <- h5read(fname.calmod, "MODIS.C6.MYD06.333m/Latitude")
            m.lon <- h5read(fname.calmod, "MODIS.C6.MYD06.333m/Longitude")
            
            back.ass <- back.associate.calmod(r.lon[rain.mask], r.lat[rain.mask], m.lon, m.lat)
            myd06 <- h5read(fname.calmod, "MODIS.C6.MYD06.333m")
            ## myd06 is a list of variables; for each list element, select the entries associated with rainy DARDAR profiles
            df.myd06 <- transform(t( ## just some basic reshaping
                                    sapply(back.ass[1,], function(i, dim1) {
                                        ## i <- 22850
                                        ## dim1 <- 63450
                                        unlist(sapply(myd06, function(x) { 
                                            if (length(dim(x)) < 2) ## vectors just pass through
                                                x[i] 
                                            else {
                                                if (dim(x)[1] == dim1) ## arrays are unpacked (that's what the unlist() is for)
                                                    x[i,]
                                                else x[,i]
                                            }
                                        }))
                                    }, ## finally, some arrays are [length(track), n], others are [n, length(track)] for
                                       ## n = {2 (source pixels), 9 (QA flags), etc}, so we need to know length(track)
                                           dim1 = median(sapply(myd06, function(x) if (length(dim(x)) < 2) length(x) else NA), na.rm = TRUE))))
            ## this was too simple (would have worked if all the variables were vectors, but some are arrays): df.myd06 <- transform(sapply(myd06, function(x) x[back.ass[1,]])) ## select the MYD06 values along the rainy parts of the track
            res <- cbind(res, replace(df.myd06, df.myd06 == -999, NA)) ## CALMOD uses -999 as NA
        }            

        #############################################################
        # Comparison of DARDAR rain mask with 2B-CLDCLASS precip flag
        #############################################################
        ## some summary statistics one could imagine calculating:

        ## here is one to turn you into a pessimist: DARDAR and 2B-CLDCLASS disagree about precip 60% of the time pixel
        ## by pixel:
        
        ## cpr.rain <- r.2bcldclass.precip[, rain.mask]
        ## cpr.dardar.contingency <- sapply(1:ncol(cpr.rain), function(i) {
        ##     cpr <- cpr.rain[, i]
        ##     dardar <- r.rain[, i]
        ##     table(factor(cpr[dardar == dardar.rain], levels = 0:3))
        ## })
        ## cpr.dardar.contingency <- table(r.2bcldclass.precip, r.classif)

        ## here is a more optimistic one: on the question whether there is precip anywhere in the column, the agreement
        ## is 90-something % overall and about 80% discounting columns where neither algorithm sees precip
        
        ## rain.mask.cpr <- sapply(1:3, function(class) apply(r.2bcldclass.precip, 2, function(cpr) any(cpr == class)))
        ## aperm(aaply(rain.mask.cpr, 2, function(x) table(rain.mask, x)), c(2,3,1)) / length(rain.mask) * 100
        ## rain.mask.cpr <- apply(r.2bcldclass.precip, 2, function(cpr) any(cpr %in% c(1,3)))
        ## table(rain.mask, rain.mask.cpr) / length(rain.mask) * 100

        ## the philosophy here is to just write the rain masks into the summary files and let the user calculate the
        ## summary statistic of his or her choice:
        dardar.2bcldclass <- cbind(r.lon, r.lat, r.utc, 
                                   rain.mask,
                                   sapply(1:3, function(class) apply(r.2bcldclass.precip, 2, function(cpr) any(cpr == class))))
        dimnames(dardar.2bcldclass)[[2]] <- c("lon", "lat", "time", "dardar.rain.mask", sprintf("cpr.rain.mask.%d", 1:3))
        ## a suitable summary statistic that counts either liquid precip or "possible drizzle" as rain in the CPR case:
        ## table(dardar = dardar.2bcldclass[,1], cpr = dardar.2bcldclass[,2] | dardar.2bcldclass[,4])

        GDAL.close(g.classif)
        GDAL.close(g.hgt    )
        GDAL.close(g.lat    )
        GDAL.close(g.lon    )
        GDAL.close(g.utc    )
        ## sapply(nc.goccp, nc_close)
        
        saveRDS(list(res = res, dardar.2bcldclass.precip.comp = dardar.2bcldclass), file = paste("rain-summaries", gsub(".hdf", ".rds", basename(fname)), sep = "/"))
        res
    })

    res
}
