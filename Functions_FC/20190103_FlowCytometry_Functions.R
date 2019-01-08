# --
################
# pal #
################
# just to illustrate colors
pal <- function(col, border = "light gray", ...){
        n <- length(col)
        plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
             axes = FALSE, xlab = "", ylab = "", ...)
        rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}
# --




# --
################
# extract_parameter_keywords_from_flowFrame
################
# extracts the keyword values associated with the parameters and puts them in a data frame
# you can find the keyword meanings here: http://docs.flowjo.com/vx/workspaces-and-samples/keywords-and-annotation/ws-fcskeys/
# I now about:
# N = Short name for parameter n
# R = Range for parameter number n
# B = Number of bits reserved for parameter number n.
# E = Amplification type for parameter n
# V = Detector voltage for parameter n
# G = Amplifier gain used for acquisition of parameter n
# DISPLAY = gains
 
extract_parameter_keywords_from_flowFrame <- function(flowFrame, suffixVector = NULL) { # suffixVector = c("N", "R", "B", "E", "V", "G", "DISPLAY", "BS", "MS")
        
        if(! class(flowFrame) == "flowFrame") {
                stop("flowFrame is actually not a flow frame.")
        }
        
        # unfortunately this easy way does not work when some parameters/columns have not been loaded in read.FCS (i.e. use of column pattern)
        # NoParameters <- dim(flowFrame)[2]
        # instead use
        ParNumbers <- gsub(pattern = "\\$P", "", names(featureNames(flowFrame)))
        ParNumbers <- as.numeric(gsub(pattern = "N$", "", ParNumbers))
        
        if (is.null(suffixVector)){
                
                allKeywords <- names(keyword(flowFrame))
                ParameterKeywords <- allKeywords[grepl(pattern = "P[1-9]", allKeywords)]
                KL <- strsplit(x = ParameterKeywords, "[0-9]")
                KL <- sapply(KL, function(listi){listi[[length(listi)]]})
                suffixVector <- unique(KL)
                
        }
        
        
        ValueList <- sapply(suffixVector, function(suffix){
                values <- keyword(flowFrame, paste0("$P", ParNumbers, suffix))
                if (all(vapply(values, is.null, logical(1)))){
                        values <- keyword(flowFrame, paste0("P", ParNumbers, suffix))
                }
                values <- lapply(values, function(entry){
                        if (is.null(entry)){
                                "NULL"
                        } else {
                                entry
                        }
                })
                unlist(values)
        })
        
        ValueDF <- as.data.frame(ValueList)
        
        ValueDF[] <- lapply(ValueDF[], as.character) # to not have factors!
        
        # colnames(ValueDF) <- c("N_name", "R_range", "B_bits", "E_amplification", "V_voltage", "G_gain", "DISPLAY", "BS", "MS")
        
        ValueDF
}
# --




# --
################
# extract_all_non_parameter_keywords_from_flowFrame
################
extract_all_non_parameter_keywords_from_flowFrame <- function(flowFrame) {
        
        if(! class(flowFrame) == "flowFrame") {
                stop("flowFrame is actually not a flow frame.")
        }
        
        
        allKeywords <- names(keyword(flowFrame))
        nonParameterKeywords <- allKeywords[!grepl(pattern = "P[1-9]", allKeywords)]
        
        values <- keyword(flowFrame, nonParameterKeywords)
        
        # change matrix into one long character vector
        values <- lapply(values, as.character, length = 1)
        values <- lapply(values, function(entry){
                if (length(entry) > 1){
                        entry <- paste(entry, collapse = "")
                } else {
                        entry
                }
        }) 
        
        DF <- data.frame(Keyword = names(values), Value = unlist(values))
        
        DF[] <- lapply(DF[], as.character)
        
        DF
}
# --




# --
################
#### gs.Stats ##
################
# uses getPopStats to get an overview of the number of events after the different gates
# the user can use the gates argument to restrict to certain gates
# references for the frequencies is always the first gate given with gates

gs.Stats <- function(gs, gates = getNodes(gs), gatenames = NULL) {
        
        
        if(!(all(gates %in% getNodes(gs)))) {
                stop("not all gates correspond to gates in gs")
        }
        
        # # - since getPopStats removes / from gate names, do so too -
        # gates <- gsub(pattern = "^/", replacement = "", gates)
        # # --
        
        if(!is.null(gatenames) && (length(gatenames) != length(gates))) {
                stop("gatenames not same length as gates")
        }
        
        Count <- getPopStats(gs, format = "wide", statistic = "count")
        Count <- as.data.frame(t(Count))
        
        # - find the columns that match to the gates user wants to see -
        Indexes <- match(gates, colnames(Count))
        if(any(is.na(Indexes))){
                Indexes[is.na(Indexes)] <- match(substr(gates[is.na(Indexes)], 2, 10000), colnames(Count))
        }
        if(any(is.na(Indexes))){
                Indexes[is.na(Indexes)] <- match(basename(gates[is.na(Indexes)]), colnames(Count))
        }
        # --
        
        # - restrict to desired columns and reorder -
        Count <- dplyr::select(Count, Indexes)
        # --
        
        
        if(!is.null(gatenames)) {
                
                colnames(Count) <- gatenames
        }
        
        Freq <- Count/Count[,1]
        
        CountFreq <- list(Count, Freq)
        
        
        return(CountFreq)
}
# --




# --
########################
#### FC.geom_hex
########################
## Input:
# fs: a flowSet object
# name: the sampleNames to be plotted (by default all)
# x and y: what to plot
# co.fix: default TRUE, if the x and y axis ratio should be 1:1
# xmax, ymax: default = NULL using the default ggplot axis ranges, if you want to 
# override you need to give numbers for both, eg. 2^18, the min values are used 
# then for the lower axis 
# trans: if not null, scale_x/y_continuous will be added with the given trans, e.g. log10
## Output: list with the Trellis objects 

FC.geom_hex <- function(fs, x = "FSC-A", y = "SSC-A", co.fix = TRUE,
                        xlimits = NULL, ylimits = NULL, trans = NULL, bins = 150, name = NULL, colPalette = NULL,
                        ncolors = 10){
        
        if (is.null(name)) {
                name <- flowWorkspace::sampleNames(fs)
        }
        
        if (is.numeric(name)) {
                
                name <- flowWorkspace::sampleNames(fs)[name]
        }
        
        if (is.null(colPalette)) {
                colPalette <- topo.colors
        }
        
        MatrixList <- lapply(name, function(current_name){exprs(fs[[current_name]])})
        names(MatrixList) <- name
        
        TrList <- lapply(seq_along(MatrixList), FUN = function(i){
                
                cDF <- as.data.frame(MatrixList[[i]])
                colnames(cDF) <- make.names(colnames(cDF))
                xnew <- make.names(x)
                ynew <- make.names(y)
                
                # - decided to remove infinite values in case of transformation -
                if(!is.null(trans)){
                        
                        cDF$transX <- match.fun(trans)(cDF[,xnew])
                        cDF$transY <- match.fun(trans)(cDF[,ynew])
                        # -- remove NA or infinite data --
                        cDF <- dplyr::filter(cDF, is.finite(transX), is.finite(transY))
                        # ----
                }
                
                # --
                
                
                Tr <- ggplot(cDF, aes_string(x = xnew, y = ynew)) +
                        geom_hex(bins = bins) +
                        xlab(x) +
                        ylab(y) +
                        scale_fill_gradientn(colours = colPalette(n = ncolors)) +
                        theme_bw() +
                        ggtitle(paste("Sample:", names(MatrixList)[i], "; Events", nrow(cDF)))
                
                
                if(!is.null(trans)){
                        
                        Tr <- Tr +
                                scale_x_continuous(trans = trans, limits = xlimits) +
                                scale_y_continuous(trans = trans, limits = ylimits)
                        
                } else {
                        
                        Tr <- Tr +
                                scale_x_continuous(limits = xlimits) +
                                scale_y_continuous(limits = ylimits)
                }
                
                
                if(co.fix){Tr <- Tr + coord_fixed()}
                
                
                
        })
        
        names(TrList) <- flowWorkspace::sampleNames(fs)
        TrList
        
}
# --




########################
#### plotPolygon   ######
########################
## Input:
# TrL: a list with the Trellis object where you want to put a polygon on
# polyDF: a df specifying the polygon, first column x values, second column y values
# name: the sampleNames to be plotted (by default all)
# col: color of the polygon
# size = point size of the points marking the polygon
# Label: usually the values of the events in the polygon
# LabelValues: c(x, y, size) to position the text
## Output: list with the new Trellis objects 
#########################

plotPolygon <- function(TrL, polyDF, Label = NULL, name = NULL, col = "red",
                       size = 2, LabelValues = NULL){
        
        if (!is.null(Label) & length(Label)!=length(TrL)) {
                
                stop("length Label does not agree with TrL")
        }
        
        if (!is.null(Label) & length(LabelValues)!=3) {
                
                stop("If Label is given, Label Values with c(x, y, size) must be given too")
        }
        
        if (is.null(name)) {
                
                name <- names(TrL)
        }
        
        TrList <- list()
        
        for(i in seq_along(name)){
                
                
                Tr <- TrL[[name[i]]] + 
                        geom_polygon(data = polyDF, aes_string(x = names(polyDF)[1], y = names(polyDF)[2]), fill = NA, color = col) + 
                        geom_point(data = polyDF, aes_string(x = names(polyDF)[1], y = names(polyDF)[2]), size = size, color = col)
                
                if(!is.null(Label)) {
                        Tr <- Tr + annotate("text", label=Label[i], x = LabelValues[1], y = LabelValues[2], colour=col, size = LabelValues[3])
                }
                
                TrList[[i]] <- Tr
                
        }
        
        names(TrList) <- name
        return(TrList)
        
}
# --




# --
########################
#### FC.geom_point
########################
# uses densCols function for the coloring, which unfortunately does not really allow a legend but is fast
# see <http://r.789695.n4.nabble.com/Re-densCols-what-are-the-computed-densities-and-how-to-create-a-legend-td2020672.html>
# or google densCols legend

FC.geom_point <- function(fs, name = NULL, x = "FSC-A", y = "SSC-A", co.fix = TRUE,
                          xlimits = NULL, ylimits = NULL, trans = NULL, pch = ".", colPalette = NULL, nbin = 200){
        
        
        if (is.null(name)) {
                
                name <- flowWorkspace::sampleNames(fs)
        }
        
        
        if (is.numeric(name)) {
                
                name <- flowWorkspace::sampleNames(fs)[name]
        }
        
        
        if (is.null(colPalette)) {
                colPalette <- topo.colors
        }
        
        MatrixList <- lapply(name, function(current_name){exprs(fs[[current_name]])})
        names(MatrixList) <- name
        
        
        TrList <- lapply(seq_along(MatrixList), FUN = function(i){
                
                cDF <- as.data.frame(MatrixList[[i]])
                colnames(cDF) <- make.names(colnames(cDF))
                xnew <- make.names(x)
                ynew <- make.names(y)
                
                if(!is.null(trans)){
                        
                        cDF$transX <- match.fun(trans)(cDF[,xnew])
                        cDF$transY <- match.fun(trans)(cDF[,ynew])
                        # - remove NA or infinite data -
                        cDF <- dplyr::filter(cDF, is.finite(transX), is.finite(transY))
                        # --
                        col <- densCols(cDF[c("transX","transY")], colramp = colPalette, nbin = nbin)
                        
                } else {
                        col <- densCols(cDF[c(xnew,ynew)], colramp = colPalette, nbin = nbin)
                        #col <- densCols(f.exprs[, channels.new], colramp = colPalette)
                }
                
                
                Tr <- ggplot(cDF, aes_string(x = xnew, y = ynew))
                Tr <- Tr +
                        geom_point(pch = pch, colour = col) +
                        xlab(x) +
                        ylab(y) +
                        theme_bw() +
                        ggtitle(paste("Sample:", names(MatrixList)[i], "; Events", nrow(cDF)))
                
                if(!is.null(trans)){
                        
                        Tr <- Tr +
                                scale_x_continuous(trans = trans, limits = xlimits) +
                                scale_y_continuous(trans = trans, limits = ylimits)
                        
                } else {
                        
                        Tr <- Tr +
                                scale_x_continuous(limits = xlimits) +
                                scale_y_continuous(limits = ylimits)
                }
                
                
                if(co.fix){Tr <- Tr + coord_fixed()}
                
                
                Tr
                
                
                
        })
        
}

# --



# --
###############
# get density #
###############
# from <https://slowkow.com/notes/ggplot2-color-by-density/>

# Get density of points in 2 dimensions.
# @param x A numeric vector.
# @param y A numeric vector.
# @param n Create a square n by n grid to compute density.
# @return The density within each square.
get_density <- function(x, y, ...) {
        dens <- MASS::kde2d(x, y, ...)
        ix <- findInterval(x, dens$x)
        iy <- findInterval(y, dens$y)
        ii <- cbind(ix, iy)
        return(dens$z[ii])
}
# --




# --
########################
#### FC.geom_point_kernel
########################
# https://slowkow.com/notes/ggplot2-color-by-density/
# we use the 2D kernel density estimation function from the MASS R package to to color points by density in a plot created with ggplot2.
# unfortunately a bit slow, but gives a legend! so therefore nicer than FC.geom_point


FC.geom_point_kernel <- function(fs, name = NULL, x = "FSC-A", y = "SSC-A", co.fix = TRUE,
                          xlimits = NULL, ylimits = NULL, trans = NULL, pch = ".", colPalette = NULL, nbin = 70, ncolors = 10){
        
        
        if (is.null(name)) {
                
                name <- flowWorkspace::sampleNames(fs)
        }
        
        
        if (is.numeric(name)) {
                
                name <- flowWorkspace::sampleNames(fs)[name]
        }
        
        
        if (is.null(colPalette)) {
                colPalette <- topo.colors
        }
        
        MatrixList <- lapply(name, function(current_name){exprs(fs[[current_name]])})
        names(MatrixList) <- name
        
        TrList <- lapply(seq_along(MatrixList), FUN = function(i){
                
                cDF <- as.data.frame(MatrixList[[i]])
                colnames(cDF) <- make.names(colnames(cDF))
                xnew <- make.names(x)
                ynew <- make.names(y)
                
                if(!is.null(trans)){
                        
                        cDF$transX <- match.fun(trans)(cDF[,xnew])
                        cDF$transY <- match.fun(trans)(cDF[,ynew])
                        # - remove missing or infinite data -
                        cDF <- dplyr::filter(cDF, is.finite(transX), is.finite(transY))
                        # --
                        cDF$Color <- get_density(x = cDF$transX, y = cDF$transY, n = nbin)
                        
                } else {
                        cDF$Color <- get_density(x = cDF[[xnew]], y = cDF[[ynew]], n = nbin)
                        #col <- densCols(f.exprs[, channels.new], colramp = colPalette)
                }
                
                
                Tr <- ggplot(cDF, aes_string(x = xnew, y = ynew, col = "Color"))
                Tr <- Tr +
                        geom_point(pch = pch) +
                        xlab(x) +
                        ylab(y) +
                        # scale_color_viridis() +
                        scale_color_gradientn("", colours = colPalette(n = ncolors)) + 
                        theme_bw() +
                        ggtitle(paste("Sample:", names(MatrixList)[i], "; Events", nrow(cDF)))
                
                if(!is.null(trans)){
                        
                        Tr <- Tr +
                                scale_x_continuous(trans = trans, limits = xlimits) +
                                scale_y_continuous(trans = trans, limits = ylimits)
                        
                } else {
                        
                        Tr <- Tr +
                                scale_x_continuous(limits = xlimits) +
                                scale_y_continuous(limits = ylimits)
                }
                
                
                if(co.fix){Tr <- Tr + coord_fixed()}
                
                
                Tr
                
                
                
        })
        
}

# --













