
limitToRange <- function(x, lim)
{
    x[x >= lim[1] & x <= lim[2]]
}


ypoints <- function(...) { tpoints(...) }
ylines <- function(...) { tlines(...) }
ysegments <- function(...) tsegments(...)
ypolygon <- function(...) { tpolygon(...) }
ytext <- function(...) { ttext(...) }

yplot.xy <- function(...) { tessella.plot.xy(...) }

yrect <- function(x, y, width, height,
                  xleft = x - width/2, ybottom = y - height/2,
                  xright = x + width/2, ytop = y + height/2,
                  ...)
{
    trect(xleft = xleft, ybottom = ybottom, xright = xright, ytop = ytop, ...)
}



yabline <-
    function(a = NULL, b = NULL,
             h = NULL, v = NULL,
             reg = NULL, coef = NULL,
             ...,
             reference = FALSE,
             col = if (reference) .yagpenv$theme$reference$col
                   else .yagpenv$theme$add.line$col,
             vp)
{
    if (!is.null(h) || !is.null(v))
    {
        h <- limitToRange(unique(h), vp$ylim)
        v <- limitToRange(unique(v), vp$xlim)
        nh <- length(h)
        nv <- length(v)
        x0 <- c(numeric(0), rep(vp$xlim[1], nh), v)
        x1 <- c(numeric(0), rep(vp$xlim[2], nh), v)
        y0 <- c(numeric(0), h, rep(vp$ylim[1], nv))
        y1 <- c(numeric(0), h, rep(vp$ylim[2], nv))
        ysegments(x0, y0, x1, y1, ..., col = col, vp = vp)
    }
    if (!is.null(reg))
    {
        if (is.null(coef)) warning("'coef' overridden by 'reg'")
        coef <- coef(reg)
    }
    if (!is.null(coef))
    {
        if (!(is.null(a) && is.null(b))) warning("'a' and 'b' overridden by 'coef'")
        a <- coef[1]
        b <- coef[2]
    }
    if (!is.null(a))
    {
        if (is.null(b))
        {
            if (length(a) == 2)
            {
                b <- a[2]
                a <- a[1]
            }
            else stop("'a' must have length 2 if 'b' is NULL")
        }
        if (any(!is.finite(b))) stop("all elements of 'b' must be finite; use 'v' instead.")
        fabline <- function(x) { a + b * x }
        fbaline <- function(y) { (y - a) / b } ## b shouldn't be 0 then
        y0 <- fabline(vp$xlim[1])
        y1 <- fabline(vp$xlim[2])
        if (any(c(y0,y1) < vp$ylim[1] |
                c(y0,y1) > vp$ylim[2])) warning("FIXME: potential clipping issues")
        if (FALSE) ## check b != 0, and what about lines completely outside?
        {
            x0 <- fbaline(vp$ylim[1])
            x1 <- fbaline(vp$ylim[2])
            ## do something with these
        }
        ysegments(vp$xlim[1], y0, vp$xlim[2], y1, ..., col = col, vp = vp)
    }
}


ygrid <-
    function(h = 3, v = 3, col = yagp.theme()$reference$col,
             xlim = vp$xlim, ylim = vp$ylim, # explicit limits, for (pretty) dates etc
             ..., vp)
{
    h <- h[1]; v <- v[1]
    if (h == -1) h <- -5
    if (v == -1) v <- -5
    if (h < 0)
        yabline(h = pretty(ylim, n = -h), col = col, ..., vp = vp)
    if (v < 0)
        yabline(v = pretty(xlim, n = -v), col = col, ..., vp = vp)
    if (h > 0)
        yabline(h = do.breaks(ylim, h + 1)[-c(1, h + 2)], col = col, ..., vp = vp)
    if (v > 0)
        yabline(v = do.breaks(xlim, v + 1)[-c(1, v + 2)], col = col, ..., vp = vp)
}


yrect <- function(x, y, width, height,
                  xleft = x - width/2, ybottom = y - height/2,
                  xright = x + width/2, ytop = y + height/2,
                  ...)
{
    trect(xleft = xleft, ybottom = ybottom, xright = xright, ytop = ytop, ...)
}
