# Auxiliary functions

rgb0 <- function(r, g, b){
    return(rgb(1-g-b, 1-r-b, 1-r-g))
}

scatter3D_fancy <- function(x, y, z, bg, ...)
{
    panelfirst <- function(pmat, back = bg) {
        XY <- trans3D(x, y, z = rep(0, length(z)), pmat = pmat)
        scatter2D(XY$x, XY$y, xlim = c(0, 1), ylim = c(0, 1), pch = 20, col = adjustcolor(back, alpha.f = 0.5), cex = .7, add = TRUE, colkey = FALSE)
        
        XY <- trans3D(x = rep(0, length(x)), y, z, pmat = pmat)
        scatter2D(XY$x, XY$y, xlim = c(0, 1), ylim = c(0, 1), pch = 20, col = adjustcolor(back, alpha.f = 0.5), cex = .7, add = TRUE, colkey = FALSE)
        
        XY <- trans3D(x, y = rep(1, length(y)), z, pmat = pmat)
        scatter2D(XY$x, XY$y, xlim = c(0, 1), ylim = c(0, 1), pch = 20, col = adjustcolor(back, alpha.f = 0.5), cex = .7, add = TRUE, colkey = FALSE)
    }
    scatter3D(x, y, z, ..., bg = bg, panel.first = panelfirst, colkey = F) 
}

heatmapplus <-
    function (x,
              Rowv = NULL,
              Colv = if (symm)
                  "Rowv"
              else
                  NULL,
              distfun = dist,
              hclustfun = hclust,
              reorderfun = function(d,
                                    w)
                  reorder(d, w),
              add.expr,
              symm = FALSE,
              revC = identical(Colv,
                               "Rowv"),
              scale = c("row", "column", "none"),
              na.rm = TRUE,
              margins = c(5, 5),
              ColSideColors,
              RowSideColors,
              printFactorsNames = F,
              cexRow = 0.2 +
                  1 / log10(nr),
              cexCol = 0.2 + 1 / log10(nc),
              labRow = NULL,
              labCol = NULL,
              main = NULL,
              xlab = NULL,
              ylab = NULL,
              keep.dendro = FALSE,
              verbose = getOption("verbose"),
              ...) {
        scale <- if (symm && missing(scale))
            "none"
        else
            match.arg(scale)
        if (length(di <- dim(x)) != 2 || !is.numeric(x))
            stop("'x' must be a numeric matrix")
        nr <- di[1]
        nc <- di[2]
        if (nr <= 1 || nc <= 1)
            stop("'x' must have at least 2 rows and 2 columns")
        if (!is.numeric(margins) || length(margins) != 2)
            stop("'margins' must be a numeric vector of length 2")
        doRdend <- !identical(Rowv, NA)
        doCdend <- !identical(Colv, NA)
        if (is.null(Rowv))
            Rowv <- rowMeans(x, na.rm = na.rm)
        if (is.null(Colv))
            Colv <- colMeans(x, na.rm = na.rm)
        if (doRdend) {
            if (inherits(Rowv, "dendrogram"))
                ddr <- Rowv
            else {
                hcr <- hclustfun(distfun(x))
                ddr <- as.dendrogram(hcr)
                if (!is.logical(Rowv) || Rowv)
                    ddr <- reorderfun(ddr, Rowv)
            }
            if (nr != length(rowInd <- order.dendrogram(ddr)))
                stop("row dendrogram ordering gave index of wrong length")
        }
        else
            rowInd <- 1:nr
        if (doCdend) {
            if (inherits(Colv, "dendrogram"))
                ddc <- Colv
            else if (identical(Colv, "Rowv")) {
                if (nr != nc)
                    stop("Colv = \"Rowv\" but nrow(x) != ncol(x)")
                ddc <- ddr
            }
            else {
                hcc <- hclustfun(distfun(if (symm)
                    x
                    else
                        t(x)))
                ddc <- as.dendrogram(hcc)
                if (!is.logical(Colv) || Colv)
                    ddc <- reorderfun(ddc, Colv)
            }
            if (nc != length(colInd <- order.dendrogram(ddc)))
                stop("column dendrogram ordering gave index of wrong length")
        }
        else
            colInd <- 1:nc
        x <- x[rowInd, colInd]
        labRow <- if (is.null(labRow))
            if (is.null(rownames(x)))
                (1:nr)[rowInd]
        else
            rownames(x)
        else
            labRow[rowInd]
        labCol <- if (is.null(labCol))
            if (is.null(colnames(x)))
                (1:nc)[colInd]
        else
            colnames(x)
        else
            labCol[colInd]
        if (scale == "row") {
            x <- sweep(x, 1, rowMeans(x, na.rm = na.rm))
            sx <- apply(x, 1, sd, na.rm = na.rm)
            x <- sweep(x, 1, sx, "/")
        }
        else if (scale == "column") {
            x <- sweep(x, 2, colMeans(x, na.rm = na.rm))
            sx <- apply(x, 2, sd, na.rm = na.rm)
            x <- sweep(x, 2, sx, "/")
        }
        lmat <- rbind(c(NA, 3), 2:1)
        lwid <- c(if (doRdend)
            1
            else
                0.05, 4)
        lhei <-
            c((if (doCdend)
                1
               else
                   0.05) + if (!is.null(main))
                       0.2
              else
                  0,
              4)
        if (!missing(ColSideColors)) {
            if (!is.matrix(ColSideColors))
                stop("'ColSideColors' must be a matrix")
            if (!is.character(ColSideColors) ||
                dim(ColSideColors)[1] !=
                nc)
                stop("'ColSideColors' dim()[2] must be of length ncol(x)")
            lmat <- rbind(lmat[1,] + 1, c(NA, 1), lmat[2,] + 1)
            lhei <- c(lhei[1], 0.2, lhei[2])
        }
        if (!missing(RowSideColors)) {
            if (!is.matrix(RowSideColors))
                stop("'RowSideColors' must be a matrix")
            if (!is.character(RowSideColors) ||
                dim(RowSideColors)[1] !=
                nr)
                stop("'RowSideColors' must be a character vector of length nrow(x)")
            lmat <- cbind(lmat[, 1] + 1, c(rep(NA, nrow(lmat) -
                                                   1), 1), lmat[, 2] + 1)
            lwid <- c(lwid[1], 0.2, lwid[2])
        }
        lmat[is.na(lmat)] <- 0
        if (verbose) {
            cat("layout: widths = ", lwid, ", heights = ", lhei,
                "; lmat=\n")
            print(lmat)
        }
        op <- par(no.readonly = TRUE)
        on.exit(par(op))
        layout(lmat,
               widths = lwid,
               heights = lhei,
               respect = FALSE)
        if (!missing(RowSideColors)) {
            par(mar = c(margins[1], 0, 0, 0.5))
            rsc = RowSideColors[rowInd,]
            rsc.colors = matrix()
            rsc.names = names(table(rsc))
            rsc.i = 1
            for (rsc.name in rsc.names) {
                rsc.colors[rsc.i] = rsc.name
                rsc[rsc == rsc.name] = rsc.i
                rsc.i = rsc.i + 1
            }
            rsc = matrix(as.numeric(rsc), nrow = dim(rsc)[1])
            image(t(rsc), col = as.vector(rsc.colors), axes = FALSE)
            if (length(colnames(RowSideColors)) > 0) {
                axis(
                    1,
                    0:(dim(rsc)[2] - 1) / (dim(rsc)[2] - 1),
                    colnames(RowSideColors),
                    las = 2,
                    tick = FALSE
                )
            }
        }
        if (!missing(ColSideColors)) {
            par(mar = c(0.5, 0, 0, margins[2]))
            csc = ColSideColors[colInd,]
            csc.colors = matrix()
            csc.names = names(table(csc))
            csc.i = 1
            for (csc.name in csc.names) {
                csc.colors[csc.i] = csc.name
                csc[csc == csc.name] = csc.i
                csc.i = csc.i + 1
            }
            csc.dim1 = ifelse(is.null(dim(csc)), length(csc), dim(csc)[1])
            csc.dim2 = ifelse(is.null(dim(csc)), 1, dim(csc)[2])
            csc = matrix(as.numeric(csc), nrow = csc.dim1)
            image(csc, col = as.vector(csc.colors), axes = FALSE)
            if (printFactorsNames & (length(colnames(ColSideColors)) > 0)) {
                axis(
                    2,
                    0:(csc.dim2 - 1) / (csc.dim2 - 1),
                    colnames(ColSideColors),
                    las = 2,
                    tick = FALSE
                )
            }
        }
        par(mar = c(margins[1], 0, 0, margins[2]))
        if (!symm || scale != "none") {
            x <- t(x)
        }
        if (revC) {
            iy <- nr:1
            ddr <- rev(ddr)
            x <- x[, iy]
        }
        else
            iy <- 1:nr
        image(
            1:nc,
            1:nr,
            x,
            xlim = 0.5 + c(0, nc),
            ylim = 0.5 +
                c(0, nr),
            axes = FALSE,
            xlab = "",
            ylab = "",
            ...
        )
        axis(
            1,
            1:nc,
            labels = labCol,
            las = 2,
            line = -0.5,
            tick = 0,
            cex.axis = cexCol
        )
        if (!is.null(xlab))
            mtext(xlab, side = 1, line = margins[1] - 1.25)
        axis(
            4,
            iy,
            labels = labRow,
            las = 2,
            line = -0.5,
            tick = 0,
            cex.axis = cexRow
        )
        if (!is.null(ylab))
            mtext(ylab, side = 4, line = margins[2] - 1.25)
        if (!missing(add.expr))
            eval(substitute(add.expr))
        par(mar = c(margins[1], 0, 0, 0))
        if (doRdend)
            plot(
                ddr,
                horiz = TRUE,
                axes = FALSE,
                yaxs = "i",
                leaflab = "none"
            )
        else
            frame()
        par(mar = c(0, 0, if (!is.null(main))
            1
            else
                0, margins[2]))
        if (doCdend)
            plot(ddc,
                 axes = FALSE,
                 xaxs = "i",
                 leaflab = "none")
        else if (!is.null(main))
            frame()
        if (!is.null(main))
            title(main, cex.main = 1.5 * op[["cex.main"]])
        invisible(list(
            rowInd = rowInd,
            colInd = colInd,
            Rowv = if (keep.dendro &&
                       doRdend)
                ddr,
            Colv = if (keep.dendro && doCdend)
                ddc
        ))
    }


olm_two_axis <- function(x, y){
    df = data.frame('x' = x, 'y' = y)
    
    pca = prcomp(df)
    rot = pca$rotation
    
    xy_beta <- -rot[-ncol(rot), ncol(rot)] / rot[ncol(rot), ncol(rot)]
    xy_beta0 <- mean(y) - mean(x)*xy_beta
    
    yx_beta <- -rot[ncol(rot), -ncol(rot)] / rot[-ncol(rot), -ncol(rot)]
    yx_beta0 <- mean(x) - mean(y)*yx_beta
    
    return(list('xy_beta' = xy_beta, 'xy_beta0' = xy_beta0, 'yx_beta' = yx_beta, 'yx_beta0' = yx_beta0))
}

lm_two_axis <- function(x, y, df_xy){
    lm = lm(x ~ y, df_xy)
    coef = lm$coefficients
    
    xy_beta <- coef[2]
    xy_beta0 <- coef[1]
    
    lm = lm(y ~ x, df_xy)
    coef = lm$coefficients
    yx_beta <- coef[2]
    yx_beta0 <- coef[1]
    
    return(list('xy_beta' = xy_beta, 'xy_beta0' = xy_beta0, 'yx_beta' = yx_beta, 'yx_beta0' = yx_beta0))
}

lines_two_axis <- function(coefs, x = T, y = T, ...){
    if(x){
        abline(a = coefs[['xy_beta0']], b = coefs[['xy_beta']], ...)
    }
    
    if(y){
        y1 = -1000
        y2 = 1000
        x1 = coefs[['yx_beta0']] + coefs[['yx_beta']] * y1
        x2 = coefs[['yx_beta0']] + coefs[['yx_beta']] * y2
        
        lines(c(x1, x2), c(y1, y2), ...)
    }
}

dist_bolas <- function(bolas_var){
    bolas_var[, 'vermelho'] = 0
    return(dist(bolas_var))
}

dist_futebol <- function(futebol_var){
    cooperation <- c('Crossing', 'ShortPassing', 'LongPassing')
    ability <- c('HeadingAccuracy', 'Volleys', 'Curve', 'BallControl', 'Balance')
    speed <- c('Acceleration', 'SprintSpeed', 'Agility', 'Stamina')
    movement <- c('Dribbling', 'Jumping', 'Interceptions', 'Positioning', 'Marking')
    kicking <- c('Finishing', 'Volleys', 'ShotPower', 'Strength', 'Penalties')
    futebol_var[, cooperation] <- futebol_var[, cooperation] * (exp(futebol_var[, cooperation]/100) - 1)
    futebol_var[, ability] <- 75 * (futebol_var[, ability]/100)**3
    futebol_var[, speed] <- 32 * sqrt(futebol_var[, speed]/8)
    futebol_var[, movement] <- abs((futebol_var[, movement]/3)**2 - 5 * futebol_var[, movement])/5
    futebol_var[, kicking] <- (log2(futebol_var[, kicking]/5)+2)*100/6
    
    return(dist(futebol_var))
}

postMDS2 <- function(X, dist, pc = TRUE, center = TRUE, halfchange, threshold = 0.8, 
          nthreshold = 10, plot = FALSE, ...) 
{
    Size <- attr(dist, "Size")
    
    if (any(names(X) == "points")) 
        x <- X$points
    else x <- as.matrix(X)
    if (center) 
        x <- scale(x, scale = FALSE)
    if (pc) {
        dn <- dimnames(x)
        x <- prcomp(x, center = center)
        sdev <- x$sdev
        x <- x$x
        dimnames(x) <- dn
    }
    if (missing(halfchange)) {
        maxdis <- attr(dist, "maxdis")
        if (!is.null(maxdis) && maxdis) 
            halfchange <- TRUE
        else halfchange <- FALSE
    }
    if (halfchange) {
        dist <- as.vector(dist)
        ordi <- as.vector(vegdist(x, "euclidean"))
        take <- dist < threshold
        if (sum(take) < nthreshold) {
            warning("skipping half-change scaling: too few points below threshold")
            halfchange <- FALSE
        }
        else {
            k <- coef(lm(dist[take] ~ ordi[take]))
            names(k) <- NULL
            hc <- (1 - k[1])/2/k[2]
            x <- x/hc
        }
    }
    if (!halfchange) {
        hc <- max(dist(x, "euclidean"))/max(dist, na.rm = TRUE)
        x <- x/hc
    }
    if (plot && halfchange) {
        cross.lim <- 45
        if (Size > cross.lim) 
            pch <- "."
        else pch <- "+"
        orange <- range(c(ordi, 0, 1))
        drange <- range(c(dist, 0, 1))
        plot(orange, drange, type = "n", xlab = "Ordination distance", 
             ylab = "Community dissimilarity")
        points(ordi[take], dist[take], pch = pch, col = 4)
        points(ordi[!take], dist[!take], pch = pch, col = 8)
        abline(h = threshold)
        abline(h = k[1])
        hclevel <- (1 - k[1])/2 + k[1]
        segments(0, hclevel, hc, hclevel, col = 2, lwd = 2)
        arrows(hc, hclevel, hc, 0, col = 2, lwd = 2)
        arrows(0, k[1], 0, hclevel, col = 2, code = 3)
        arrows(0, hclevel, 0, 1, col = 2, code = 3)
        j <- 0.02
        text(0 + j, threshold + j, "Threshold", adj = c(0, 0))
        text(0 + j, k[1] + j, "Replicate dissimilarity", adj = c(0, 
                                                                 0))
        text(0 + j, hclevel + j, "Half-change", adj = c(0, 0))
        abline(k, col = 4, lwd = 2)
    }
    attr(x, "centre") <- center
    attr(x, "pc") <- pc
    attr(x, 'sdev') <- sdev
    attr(x, "halfchange") <- halfchange
    attr(x, "internalscaling") <- hc
    if (any(names(X) == "points")) 
        X$points <- x
    else X <- x
    X
}