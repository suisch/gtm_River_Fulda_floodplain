# this code relies heavily on parcoord.R
parcoord_gtm <- 
function (data, lower = NULL, upper = NULL, log = NULL, col = 1, 
    lty = 1, lwd = 1, plotorder = NULL, var.label = FALSE, col.axis = "darkgrey",
    at = 1L:p)
{
    X <- as.matrix(data)
    stopifnot(is.numeric(X))
    if (is.null(colnames(X)))
        colnames(X) <- paste0("X", 1L:ncol(X))
    col <- rep(col, length.out = nrow(X))
    lty <- rep(lty, length.out = nrow(X))
    lwd <- rep(lwd, length.out = nrow(X))
    if (!is.null(plotorder)) {
        oo <- order(rep(plotorder, length.out = nrow(X)))
        X <- X[oo, ]
        col <- col[oo]
        lty <- lty[oo]
        lwd <- lwd[oo]
    }
    lty <- rep(lty, length.out = nrow(X))
    if (!is.null(lower)) {
        X <- rbind(X, lower)
        lty <- c(lty, 0)
    }
    if (!is.null(upper)) {
        X <- rbind(X, upper)
        lty <- c(lty, 0)
    }
    xrange <- apply(X, 2, range, na.rm = TRUE)
    keep <- apply(xrange, 2, diff) > 0
    if (any(!keep)) {
        warning(sprintf("deleted columns: %s", paste(colnames(X)[!keep], 
            collapse = ", ")))
        X <- X[, keep, drop = FALSE]
        xrange <- xrange[, keep, drop = FALSE]
    }
    p <- ncol(X)
    nms <- colnames(X)
    yrange <- xrange
    if (!is.null(log)) {
        mm <- match(log, nms)
        if (any(is.na(mm)))
            stop(sprintf("unmatched 'log' values: %s", paste(nms[mm],
                collapse = ", ")))
        X[, mm] <- log(X[, mm])
        xrange[, mm] <- log(xrange[, mm])
        nms[mm] <- paste(nms[mm], "(log)")
    }
    Y <- (t(X) - xrange[1, ])/(xrange[2, ] - xrange[1, ])
    plot.new()
    cex <- par("cex.lab")
    mgp <- par("mgp")
    mar <- par("mar")
    #mai <- par("mai")
    #oma <- par("oma")
    mar[1] <- 8
    #mai[1] <- 8
    #oma[1] <- 8
    par("mar" = mar) 
    #par("oma" = oma) 
    #par("mai" = mai) 

#sis does not change anything
#    op <- par(mfrow = c(2, 2), # 2 x 2 pictures on one plot
#          pty = "s",
#          par(mar = c(9,2,2,2)))
#    par(op)
    #error windows(par(mar = c(9,2,2,2)))


    if (var.label)
        mgp[2] <- mgp[2] + 1
    xlim <- range(at)
    plot.window(xlim = xlim, ylim = c(0, 1)#, mar = mar#, oma = oma
    )
    axis(1, at = at, labels = nms, pos = 0, las = 2, tick = FALSE,
        cex.axis = cex, mgp = mgp
        )
    for (j in at) lines(c(j, j), c(0, 1), col = col.axis)
    matplot(at, Y, type = "l", col = col, lty = lty, lwd = lwd,
        add = TRUE#, oma = oma
        )
    if (var.label) {
        text(at, 0, formatC(yrange[1, ], digits = 3), pos = 1,
            cex = cex, xpd = NA)
        text(at, 1, formatC(yrange[2, ], digits = 3), pos = 3, 
            cex = cex, xpd = NA)
    }
    invisible()
}