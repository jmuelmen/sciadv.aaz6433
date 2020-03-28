i <- -1
dardar.clear <- i <- i + 1
dardar.ice <- i <- i + 1
dardar.iceplussupercooled <- i <- i + 1
dardar.liquidwarm <- i <- i + 1
dardar.supercooled <- i <- i + 1
dardar.rain <- i <- i + 1
dardar.aerosol <- i <- i + 1
dardar.maybeinsects <- i <- i + 1
dardar.stratosphericfeature <- i <- i + 1
dardar.ground <- 247
dardar.dontknow <- 255

i <- -1
goccp.clear <- i <- i + 1
goccp.liq <- i <- i + 1
goccp.ice <- i <- i + 1
goccp.undef <- i <- i + 1
goccp.fliq <- i <- i + 1
goccp.fice <- i <- i + 1
goccp.ho <- i <- i + 1
goccp.noise <- i <- i + 1

levs.names <- c("clear", "ice", "ice + supercooled", "liquid warm",
                "supercooled", "rain", "aerosol", "maybe insects",
                "stratospheric feature", "ground", "don't know")
col.scheme <- c("darkblue", (RColorBrewer::brewer.pal(8, "Set1")), "brown", "white")
col.scheme.phase <- c("darkblue", (RColorBrewer::brewer.pal(7, "Paired")))
col.scheme.clear 	<- "darkblue"
col.scheme.ice		<- RColorBrewer::brewer.pal(3, "Blues")[3:2]
col.scheme.iceliq 	<- RColorBrewer::brewer.pal(3, "Purples")[3:2]
col.scheme.sc 		<- RColorBrewer::brewer.pal(3, "RdPu")[3:2]
col.scheme.liqwarm 	<- RColorBrewer::brewer.pal(3, "Reds")[3:2]
col.scheme.rain 	<- RColorBrewer::brewer.pal(8, "Set1")[3]
col.scheme.other 	<- RColorBrewer::brewer.pal(8, "Set1")[6]
col.scheme.ground 	<- "brown"
col.scheme.att	 	<- "white"

levs.cols <- matrix(NA, 256, 2, dimnames = list(1:256, c("agree", "disagree")))
levs.cols[c(dardar.clear:dardar.stratosphericfeature, dardar.ground, dardar.dontknow) + 1, 1:2] <-
    matrix(c(col.scheme.clear, col.scheme.clear,
             col.scheme.ice          ,
             col.scheme.iceliq       ,
             col.scheme.sc           ,
             col.scheme.liqwarm      ,
             col.scheme.rain, col.scheme.rain,
             col.scheme.other, col.scheme.other,
             col.scheme.other, col.scheme.other,
             col.scheme.other, col.scheme.other,
             col.scheme.ground, col.scheme.ground,
             col.scheme.att, col.scheme.att),
           11, 2, byrow = TRUE)

