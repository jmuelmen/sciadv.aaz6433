#' A function to label vertically contiguous features
#' @export
label.vertical.features <- function(vfm) {
    x <- vfm
    if (length(x) == 0)
        return(x)
    diff.x <- diff(c(0, x)) ## guarantee that the first group of 1's is preceded by a transition
    labels <- cumsum(diff.x != 0) ## count up the edges
    labels
}

phase.at.top.of.lowest.layer <- function(vfm, r.hgt) {
    ## look for the lowest cloud layer in the column (using ground and clear to define the base of the lowest cloud
    ## layer); return an octal mask of the features in the layer, with MSB at the top
    ## 
    ## if Ze and 2B-GEOPROF cloud class are not missing, then the maximum reflectivity within the lowest layer is also
    ## returned
    Ze <- NULL
    cldcl <- NULL
    if (length(dim(vfm)) == 2) {
        cldcl <- vfm[, "cldcl"]
        Ze <- vfm[, "Ze"]
        vfm <- vfm[, "vfm"]
    }
    
    vfm <- rev(vfm) ## profile now begins at (inside...) the ground
    vfm.labels <- label.vertical.features(vfm)

    ## working up from the ground, list the DARDAR type of each layer
    label.to.mask <- function(label) {
        mask <- vfm[vfm.labels == label]
        ret <- median(mask)
        stopifnot(all(ret == mask))
        ret
    }
    stack <- sapply(as.integer(levels(as.factor(vfm.labels))), label.to.mask)

    ## isolate the lowest cloud layer
    lowest.cloud <- min(which(!(stack %in% c(dardar.clear, dardar.ground))))
    if (!is.finite(lowest.cloud))
        return(data.frame(cloud.stack.size = NA))
    cloud.stack <- stack[-(1 : (lowest.cloud - 1))]
    lowest.clear <- min(which(cloud.stack == dardar.clear))
    if (!is.finite(lowest.clear))
        return(data.frame(cloud.stack.size = NA))
    cloud.stack <- cloud.stack[1 : (lowest.clear - 1)]

    ## turn the cloud stack into a base-8 (smallest that
    ## fits...) bitmask, with the top as MSB; this way,
    ## truncation affects the deeper layers, which are less
    ## important
    features <- sapply(cloud.stack, function(feature) {
        if (feature %in% dardar.ice : dardar.rain)
            feature 
        else if (feature %in% dardar.aerosol : dardar.stratosphericfeature)
            6
        else if (feature == dardar.dontknow)
            7
        else NA
    })
    features <- sum(features * 8 ^ ((1 : length(features)) - 1))
    ## sprintf("%o", features)

    ## determine the top height of the lowest cloud layer
    cloud.base <- lowest.cloud
    cloud.top <- length(cloud.stack) + lowest.cloud - 1
    cloud.top.height <- rev(r.hgt)[max(which(vfm.labels == cloud.top))]

    if (!is.null(Ze) && !is.null(cldcl)) {
        ## record maximum reflectivity within the lowest cloud layer, for all range gates that are 2B-GEOPROF cloud
        ## class 40
        Ze <- rev(Ze)
        cldcl <- rev(cldcl)
        Zmax <- max(Ze[vfm.labels >= cloud.base & vfm.labels <= cloud.top & cldcl == 40])
    } else {
        Zmax <- NA
    }

    ## count cloud layers (including the lowest cloud layer), record the number
    num.cloud.layers <- max(label(stack[-(1 : (lowest.cloud - 1))] != dardar.clear))
    
    ret <- data.frame(cloud.stack.size = length(cloud.stack),
                      features = features,
                      num.cloud.layers = num.cloud.layers,
                      cloud.top.height = cloud.top.height,
                      Zmax = Zmax)
    return(ret)
}

dardar.phase.at.top <- function(lll) {
    top.two <- floor(lll$features / 8^floor(log(lll$features) / log(8) - 1)) ## lll$features / (lll$features %/% 8)) ## 
    feature.at.top <- top.two %/% 8
    ## feature.1 <- as.integer(sapply(strsplit(sprintf("%o", lll$features), ""), function(x) x[1]))
    feature.below.top <- top.two %% 8
    ## feature.2 <- as.integer(sapply(strsplit(sprintf("%o", lll$features), ""), function(x) x[2]))
    
    phase <- mapply(function(top, below) {
        supercooled <- function(below)
            switch(below,
                   "1" =, "2" = "mixed", ## supercooled liquid top with ice or ice + supercooled underneath
                   "0" = , "3" =, "5" =, "7" = "liquid", ## supercooled liquid top with warm liquid or nothing underneath 
                   NA)
        switch(top,
               "1" = "ice",
               "2" = "mixed",
               "3" = "liquid",
               "4" = supercooled(below),
               NA)
    }, as.character(as.factor(feature.at.top)), as.character(as.factor(feature.below.top)))
    factor(phase)
}

dardar.v7.phase.at.top <- function(lll) {
    feature.at.top <- (lll$features.above.rain %% 16) %% 10
    ## feature.1 <- as.integer(sapply(strsplit(sprintf("%o", lll$features), ""), function(x) x[1]))
    feature.below.top <- ((lll$features.above.rain %/% 16) %% 16) %% 10
    ## feature.2 <- as.integer(sapply(strsplit(sprintf("%o", lll$features), ""), function(x) x[2]))
    
    phase <- mapply(function(top, below) {
        supercooled <- function(below)
            switch(below,
                   "1" =, "2" = "mixed", ## supercooled liquid top with ice or ice + supercooled underneath
                   "0" = , "3" =, "5" =, "7" = "liquid", ## supercooled liquid top with warm liquid or nothing underneath 
                   NA)
        switch(top,
               "1" = "ice",
               "2" = "mixed",
               "3" = "liquid",
               "4" = supercooled(below),
               NA)
    }, as.character(as.factor(feature.at.top)), as.character(as.factor(feature.below.top)))
    factor(phase)
}

