#' A supplemental function (supplemental to make.it.rain()) that only
#' grabs the 2C-PRECIP-COLUMN rain rate
rain.rate <- function(path = "ftp.icare.univ-lille1.fr/SPACEBORNE/MULTI_SENSOR/DARDAR_MASK") {
    lf <- ## "/tmp/CER-NEWS_CCCM_Aqua-FM3-MODIS-CAL-CS_RelB1_905906.20071226.hdf"
        list.files(path = path, pattern = "DARDAR-MASK.*hdf", recursive = TRUE, full.names = TRUE)


    ## 2C-PRECIP-COLUMN files
    lf.2cprecip <- list.files(path = "/projekt3/climate/jmuelmen/ftp.icare.univ-lille1.fr/SPACEBORNE/CLOUDSAT/2C-PRECIP-COLUMN/", pattern = ".*hdf",
                              recursive = TRUE, full.names = TRUE)
    
    lf.2crain <- list.files(path = "/projekt3/climate/jmuelmen/ftp.icare.univ-lille1.fr/SPACEBORNE/CLOUDSAT/2C-RAIN-PROFILE/", pattern = ".*hdf",
                           recursive = TRUE, full.names = TRUE)
    
    res <- adply(lf, 1, .parallel = TRUE, function(fname) {
        ## don't remake files that already exist
        if (length(list.files("rainrate-summaries", gsub(".hdf", ".rds", basename(fname)))) != 0)
            return(data.frame(NULL))

        dardar.datestring <- strsplit(basename(fname), "_")[[1]][3]
        dardar.date <- as.POSIXlt(dardar.datestring, format = "%Y%j", tz = "UTC") ## strips off the %H%M%S --> 00:00:00 UTC
        
        ## find 2C-PRECIP-COLUMN file corresponding to the DARDAR file (easy)
        fname.2cprecip <- lf.2cprecip[grep(dardar.datestring, lf.2cprecip)]
        fname.2crain <- lf.2crain[grep(dardar.datestring, lf.2crain)]
        
        if (length(fname.2cprecip) != 1 || length(fname.2crain) != 1) 
            return(data.frame(NULL))
        
        gc()

        g.lat <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":4', fname))
        r.lat <- getRasterData(g.lat)
        g.lon <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":5', fname))
        r.lon <- getRasterData(g.lon)
        g.utc <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":1', fname))
        r.utc <- getRasterData(g.utc)  ## seconds since 00:00:00 on the current day

        ##################################################################
        # get rain properties from 2C-PRECIP and 2C-RAIN
        ##################################################################
        list.sds.2pc <- h4list(fname.2cprecip, ignore.vd = FALSE) ## never ignore vd...
        list.sds.2rp <- h4list(fname.2crain, ignore.vd = FALSE)

        res <- data_frame(## 2C-PRECIP-COLUMN
                          precip.flag.2pc             = h4read(fname.2cprecip, list.sds.2pc, "Precip_flag"),
                          precip.rate.2pc             = h4read(fname.2cprecip, list.sds.2pc, "Precip_rate"),
                          precip.rate.min.2pc         = h4read(fname.2cprecip, list.sds.2pc, "Precip_rate_min"),
                          precip.rate.max.2pc         = h4read(fname.2cprecip, list.sds.2pc, "Precip_rate_max"),
                          clwp.2pc                    = h4read(fname.2cprecip, list.sds.2pc, "CLWP"),
                          rlwp.2pc                    = h4read(fname.2cprecip, list.sds.2pc, "RLWP"),
                          ## 2C-RAIN-PROFILE
                          precip.flag.2rp             = h4read(fname.2crain, list.sds.2rp, "precip_flag"),          
                          rain.quality.flag.2rp       = h4read(fname.2crain, list.sds.2rp, "rain_quality_flag"),    
                          rain.status.flag.2rp        = h4read(fname.2crain, list.sds.2rp, "rain_status_flag"),     
                          rain.rate.2rp               = h4read(fname.2crain, list.sds.2rp, "rain_rate"),            
                          ## rain.rate.uncertainty.2rp   = h4read(fname.2crain, list.sds.2rp, "rain_rate_uncertainty"), ## not the same length as the otherse, for some reason
                          lon = r.lon, lat = r.lat, time = dardar.date + r.utc)
        
        rain.mask <- res$precip.flag.2pc %in% 1:7

        res <- res[rain.mask, ]
        res <- replace(res, res == -1000 | res == -9999, NA)

        saveRDS(res, paste("rainrate-summaries", gsub(".hdf", ".rds", basename(fname)), sep = "/"))
        res
    })

    res
}

## a supplemental function (supplemental to make.it.rain()) that only
## grabs the 2B-CWC-RO LWP
cloudsat.2b.cwc.ro <- function(path = "/DATA/LIENS/MULTI_SENSOR/DARDAR_MASK") {
    lf <- ## "/tmp/CER-NEWS_CCCM_Aqua-FM3-MODIS-CAL-CS_RelB1_905906.20071226.hdf"
        list.files(path = path, pattern = "DARDAR-MASK.*hdf", recursive = TRUE, full.names = TRUE)

    ## 2C-PRECIP-COLUMN files
    lf.2bcwc <- list.files(path = "/DATA/LIENS/CLOUDSAT/2B-CWC-RO", pattern = ".*hdf",
                           recursive = TRUE, full.names = TRUE)
    
    res <- adply(lf, 1, .parallel = TRUE, function(fname) {
        ## don't remake files that already exist
        if (length(list.files("rainrate-summaries", gsub(".hdf", ".rds", basename(fname)))) != 0)
            return(data.frame(NULL))

        dardar.datestring <- strsplit(basename(fname), "_")[[1]][3]
        dardar.date <- as.POSIXlt(dardar.datestring, format = "%Y%j", tz = "UTC") ## strips off the %H%M%S --> 00:00:00 UTC
        
        ## find 2C-PRECIP-COLUMN file corresponding to the DARDAR file (easy)
        fname.2cprecip <- lf.2cprecip[grep(dardar.datestring, lf.2cprecip)]
        fname.2crain <- lf.2crain[grep(dardar.datestring, lf.2crain)]
        
        if (length(fname.2cprecip) != 1 || length(fname.2crain) != 1) 
            return(data.frame(NULL))
        
        gc()

        g.lat <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":4', fname))
        r.lat <- getRasterData(g.lat)
        g.lon <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":5', fname))
        r.lon <- getRasterData(g.lon)
        g.utc <- GDAL.open(sprintf('HDF4_SDS:UNKNOWN:"%s":1', fname))
        r.utc <- getRasterData(g.utc)  ## seconds since 00:00:00 on the current day

        ##################################################################
        # get rain properties from 2C-PRECIP and 2C-RAIN
        ##################################################################
        list.sds.2pc <- h4list(fname.2cprecip, ignore.vd = FALSE) ## never ignore vd...
        list.sds.2rp <- h4list(fname.2crain, ignore.vd = FALSE)

        res <- data_frame(## 2C-PRECIP-COLUMN
                          precip.flag.2pc             = h4read(fname.2cprecip, list.sds.2pc, "Precip_flag"),
                          precip.rate.2pc             = h4read(fname.2cprecip, list.sds.2pc, "Precip_rate"),
                          precip.rate.min.2pc         = h4read(fname.2cprecip, list.sds.2pc, "Precip_rate_min"),
                          precip.rate.max.2pc         = h4read(fname.2cprecip, list.sds.2pc, "Precip_rate_max"),
                          clwp.2pc                    = h4read(fname.2cprecip, list.sds.2pc, "CLWP"),
                          rlwp.2pc                    = h4read(fname.2cprecip, list.sds.2pc, "RLWP"),
                          ## 2C-RAIN-PROFILE
                          precip.flag.2rp             = h4read(fname.2crain, list.sds.2rp, "precip_flag"),          
                          rain.quality.flag.2rp       = h4read(fname.2crain, list.sds.2rp, "rain_quality_flag"),    
                          rain.status.flag.2rp        = h4read(fname.2crain, list.sds.2rp, "rain_status_flag"),     
                          rain.rate.2rp               = h4read(fname.2crain, list.sds.2rp, "rain_rate"),            
                          ## rain.rate.uncertainty.2rp   = h4read(fname.2crain, list.sds.2rp, "rain_rate_uncertainty"), ## not the same length as the otherse, for some reason
                          lon = r.lon, lat = r.lat, time = dardar.date + r.utc)
        
        rain.mask <- res$precip.flag.2pc %in% 1:7

        res <- res[rain.mask, ]
        res <- replace(res, res == -1000 | res == -9999, NA)

        saveRDS(res, paste("rainrate-summaries", gsub(".hdf", ".rds", basename(fname)), sep = "/"))
        res
    })

    res
}
