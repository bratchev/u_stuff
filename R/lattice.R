## lattice.R - helper functions for lattice plots

library("lattice")
library("latticeExtra")
library("hexbin")

diag.panel.density <- function (x, ...) {
    yrng <- current.panel.limits()$ylim
    d <- density(x, na.rm=TRUE)
    d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y))
    panel.lines(d)
    diag.panel.splom(x, ...)
}
diag.panel.hist <- function(x, breaks = "FD", ...) {
    yrng <- current.panel.limits()$ylim
    h <- hist(x, breaks = breaks, plot = FALSE)
    breaks <- h$breaks + median(diff(h$breaks))
    nB <- length(breaks)
    y <- h$counts
    y <- yrng[1] + 0.95 * diff(yrng) * y / max(y)
    panel.rect(breaks[-nB], yrng[1], breaks[-1], y,  
               col=rgb(0.0, 0.5, 1.0, 0.5), ...)
    diag.panel.splom(x, ...)
}
diag.panel <- function (x, ...) {
    if (is.factor(x)) {
        diag.panel.splom(x, ...)
    } else if (length(unique(x)) < 10) {
        diag.panel.hist(x, ...)
    } else {
        diag.panel.density(x, ...)
    }
}
dm.panel <- function (x, y, ...) {
    if (is.factor(y) || length(unique(y)) < 10) {
        panel.bwplot(x, y, ...)
    } else if (is.factor(x) || length(unique(x)) < 10) {
        panel.bwplot(x, y, horizontal = FALSE)
    } else {
        panel.hexbinplot(x, y, ...)
    }
}
panel.correlation <- function (x, y, ...) {
    lims <- current.panel.limits()
    xpos <- mean(lims$xlim)
    ypos <- mean(lims$ylim)
    correlation <- cor(as.numeric(x), as.numeric(y))
    col <- if (correlation < 0) rgb(1,0,0, -correlation) else rgb(0,1,0, correlation)
    grid.rect(gp = gpar(col = NA, fill = col))
    panel.text(x = xpos, y = ypos, labels = sprintf("%.2f", correlation), ...)
}       

panel.histnormdens <- function (x, ...) {
    panel.histogram(x, ...)
    panel.mathdensity(dmath = dnorm,
                      args = list(mean = mean(x), sd = sd(x)),
                      ...)
}
panel.histgammadens <- function (x, ...) {
    panel.histogram(x, ...)
    panel.mathdensity(dmath = dgamma,
                      args = list(scale = var(x)/mean(x),
                          shape = mean(x)^2/var(x)),
                      ...)
}
panel.mlines <- function (mlines, mlines.col = "red", ...) {
    mlines <- as.matrix(mlines)
    apply(mlines, 1,
          function (x) panel.lines(x = c(x[1], x[2]), y = c(x[3], x[4]),
                                   col = mlines.col))
}
