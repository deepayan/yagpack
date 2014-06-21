
## Renderings take arguments and renders them.  The render_* functions
## are constructors of such functions, with the option to change
## defaults.

render_null <- function(...)
{
    yrender(function(...) { })
}

render_points <- function(...)
{
    f <- function(...) { ypoints(...) }
    yrender(changeDefaults(f, ...), c(x="y", y="x"))
}

render_grid <- function(...)
{
    f <- function(...) { ygrid(...) }
    yrender(changeDefaults(f, ...), c(h="v", v="h", xlim="ylim", ylim="xlim"))
}

render_abline <- function(...)
{
    f <- function(...) { yabline(...) }
    yrender(changeDefaults(f, ...), c(h="v", v="h"))
}

render_lines <- function(...)
{
    f <- function(...) { ylines(...) }
    yrender(changeDefaults(f, ...), c(x="y", y="x"))
}

render_rect <- function(...)
{
    f <- function(...) { yrect(...) }
    yrender(changeDefaults(f, ...), c(x = "y", width = "height", y = "x", height = "width",
                                      xleft="ybottom", ybottom="xleft", xright="ytop", ytop="xright"))
}

render_segments <- function(...)
{
    f <- function(...) { ysegments(...) }
    yrender(changeDefaults(f, ...), c(x0="y0", y0="x0", x1="y1", y1="x1"))
}


render_polygon <- function(...)
{
    f <- function(...) { ypolygon(...) }
    yrender(changeDefaults(f, ...), c(x0="y0", y0="x0", x1="y1", y1="x1"))
}

render_xy <- function(...)
{
    f <- function(...) { yplot.xy(...) }
    yrender(changeDefaults(f, ...), c(x="y", y="x"))
}

render_text <- function(...)
{
    f <- function(...) { ytext(...) }
    yrender(changeDefaults(f, ...), c(x="y", y="x"))
}


## render_table <- function(...)
## {
##     f <- function(x, width, ...)  { yrect(xleft = x - width/2, xright = x + width/2, ...) }
##     yrender(changeDefaults(f, ...), ???)
## }



## Mappings take data (display/panel variables) and convert them into
## quantitative values that can be used by data-agnostic plotting
## functions.  To allow grouping, mapping functions return a list,
## each element a list of arguments to be passed on to be rendered.

map_null <- function(...)
{
    f <- function(...)
    {
        list(list())
    }
    f
}

map_id <- function(...)
{
    f <- function(...)
    {
        list(list(...))
    }
    changeDefaults(f, ...)
}


points_constructor <-
    function(x, y, jitter.x = FALSE, jitter.y = FALSE, ..., 
             col = NULL, fill = NULL, size=NULL, color=NULL, limits = TRUE)
{
    l <- list(col = col, fill = fill)
    if (limits) l$limits <- default.limits(x, y)
    if (!is.null(size)) l$cex <- mapSize(size, ...)
    if (!is.null(color)) l$col <- mapColor(color, ...)
    l$x  <- if (jitter.x) jitter(as.numeric(x)) else as.numeric(x)
    l$y  <-  if (jitter.y) jitter(as.numeric(y)) else as.numeric(y)
    l[!sapply(l, is.null)]
}


map_points <- function(..., jitter.x = FALSE, jitter.y = FALSE)
{
    pargs <- list(jitter.x = jitter.x, jitter.y = jitter.y)
    f <- function(x, y, ..., groups = NULL,
                  mapcolor = !is.null(groups) && is.null(color),
                  col = NULL, size = NULL, color = NULL, limits = TRUE)
    {
        mapByGroups(x = x, y = y, groups = groups, size = size, color = color,
                    constructor = points_constructor,
                    args = pargs, col = col, mapcolor = mapcolor, limits = limits)
    }
    changeDefaults(f, ...)
}



compute_qprobs <- function(n, f.value = NULL, tails.n = 0)
{
    pp <- if (is.numeric(f.value)) f.value else f.value(n)
    if (tails.n > 0)
    {
        ## use exact data for tails of distribution
        tails.n <- min(tails.n, n %/% 2)
        ppd <- ppoints(n)
        ## omit probabilities within the exact tails
        pp <- pp[(pp > ppd[tails.n] &
                  pp < ppd[n + 1 - tails.n])]
        ## add on probs corresponding to exact tails
        pp <- c(head(ppd, tails.n), pp, tail(ppd, tails.n))
        ## must use a quantile type that recovers exact values:
    }
    pp
}


qqmath_constructor <-
    function(x,
             f.value = NULL,
             distribution = qnorm,
             qtype = if (tails.n >0) 1 else 7,
             tails.n = 0)
{
    x <- as.numeric(x)
    distribution <- getFunctionOrName(distribution)
    nobs <- sum(!is.na(x))
    if (nobs)
    {
        if (is.null(f.value)) # exact data instead of quantiles
        {
            list(x = distribution(ppoints(nobs)),
                 y = sort(x))
        }
        else
        {
            pp <- compute_qprobs(nobs, f.value = f.value, tails.n = tails.n)
            xx <- distribution(pp)
            yy <- quantile(x, pp, names = FALSE, type = qtype, na.rm = TRUE)
            list(x = xx, y = yy)
        }
    }
    else list(x = numeric(0), y = numeric(0))
}

map_qqmath <- function(f.value = NULL,
                       distribution = qnorm,
                       qtype = 7,
                       tails.n = 0, ...)
{
    qargs <- list(f.value = f.value, distribution = distribution,
                  qtype = qtype, tails.n = tails.n)
    f <- function(x, ..., col = NULL, groups = NULL, mapcolor = TRUE, limits = TRUE)
    {
        mapByGroups(x = x, constructor = qqmath_constructor, args = qargs,
                    col = col, groups = groups, mapcolor = mapcolor,
                    limits = limits)
    }
    changeDefaults(f, ...)
}

map_density <- function(bw = "nrd0", adjust = 1, kernel = "gaussian",
                        n = 50, ...)
{
    dargs <- list(bw = bw, adjust = adjust, kernel = kernel, n = n)
    f <- function(x, weights = NULL, ..., col = NULL,
                  groups = NULL, mapcolor = TRUE, limits = TRUE)
    {
        mapByGroups(x = x, weights = weights, constructor = density, args = dargs,
                    col = col, groups = groups, mapcolor = mapcolor,
                    limits = limits)
    }
    changeDefaults(f, ...)
}

default_hist_breaks <-
    function(x, breaks = NULL, equal.widths = TRUE,
             nint = round(log2(length(x)) + 1)) 
{
    if (is.factor(x)) seq_len(1 + nlevels(x)) - 0.5
    else if (equal.widths) do.breaks(range(x, finite = TRUE), nint)
    else quantile(x, 0:nint/nint, na.rm = TRUE)
}

## to be used for barcharts (and maybe histograms)

## The idea here is that for every unique level of 'x' (must be
## factor), we add up 'y'-s, grouping by 'groups'.  For barchart,
## there will usually be just one y value for every x, but if we allow
## multiple things, we can then pass in the original data without
## tabulating it first.  Also, when using this for histograms, we can
## make 'y' the weights.

## Return value: matrix, with one row per group.


## FIXME: spine (mosaic) plots are sort of similar (barcharts but like
## with variable width bins).  Think about how to support (needs
## separate limit calculations).


table_constructor <-
    function(x = NULL, y = NULL, groups = NULL, tab, stack = TRUE, area = FALSE,
             xmid = seq_len(ncol(tab)), width = 2/3, origin = 0,
             mapfill = TRUE, col = NULL, fill = NULL, limits = TRUE)
{
    if (missing(tab))
    {
        tab <-
            if (is.null(groups)) structure(xtabs(y ~ x), dim = c(1, nlevels(x)))
            else as.matrix(xtabs(y ~ groups + x))
    }
    xorig <- x # FIXME: update names to avoid this unnecessary renaming
    x <- xmid

    ## origin is only honored is stack=FALSE (or no groups). origin=NA
    ## could indicate panel limits.

    ## area=TRUE|FALSE is used when stack=FALSE. If TRUE, basically
    ## stack horizontally (single bar with area proportionally filled
    ## according to group).

    ## FIXME: Need better argument names for this.
    
    ## Need to decide what to return.  The trect() primitive needs
    ## xleft, ybottom, etc.  But is is possible that we may use this
    ## constructor for other things, like do dotplots instead of
    ## barcharts.  So let's return a special format: x, width,
    ## ybottom, ytop.
    
    cumsum0 <- function(x) c(0, cumsum(x))

    ## Straightforward if single row:
    nx <- ncol(tab)
    ngroups <- nrow(tab)
    if (ngroups == 1)
    {
        l <- 
            lapply(list(x = x, width = width, ybottom = origin, ytop = as.numeric(tab)),
                   rep, length.out = nx)
        if (!is.null(col)) l$col <- col
        if (!is.null(fill)) l$fill <- fill
    }
    else if (stack) # relatively easy, just stack up values by column. Ignores 'origin'
    {
        st <- t(apply(tab, 2, cumsum0))
        ## 'st' must be matrix (with 'dim' because nrow(t) > 1). ncol()==1 doesn't seem to matter
        l <-
            lapply(list(x = x, width = width,
                        ybottom = as.numeric(st[, 1:ngroups]),
                        ytop = as.numeric(st[,1L + 1:ngroups])),
                   rep, length.out = nx * ngroups)
        if (!is.null(col)) l$col <- col
        if (mapfill) l$fill <- rep(mapFill(gl(ngroups, 1)), each = nx) # FIXME: use mapFill() instead
        else if (!is.null(fill)) l$fill <- fill
    }
    else if (!area)
    {
        ## y-s are simply the table values.  widths become width/ngroups, and x values need shifting.
        shiftDelta <- function(w, n) # this is assumed to be for one row.  'w' is a scalar
        {
            ## n=1 ==> delta=0, n=2 ==> delta=c(-w/4, w/4), etc.
            d <- seq(-w/2, w/2, length.out = n+1)
            0.5 * (d[-1] + d[-(n+1)])
        }
        delta <- lapply(rep(width, length.out = nx),
                        shiftDelta, n = ngroups)
        delta <- as.numeric(do.call(rbind, delta)) # delta's are in wrong order, so fix.
        l <- 
            lapply(list(x = x + delta, width = width / ngroups,
                        ybottom = origin, ytop = as.numeric(t(tab))),
                   rep, length.out = nx * ngroups)
        if (!is.null(col)) l$col <- col
        if (mapfill) l$fill <- rep(mapFill(gl(ngroups, 1)), each = nx)
        else if (!is.null(fill)) l$fill <- fill
    }
    else # area=TRUE - stack horizontally (like spineplot)
    {
        ## heights are now column-totals.  x and width are a bit more
        ## complicated: for each column, take total width and divide
        ## proportionally by group values.
        bmat <- apply(tab, 2, function(p) cumsum0(p/sum(p)) - 0.5)          # breaks
        mmat <- apply(bmat, 2, function(d) 0.5 * (d[-1] + d[-(length(d))])) # midpoints
        wmat <- apply(bmat, 2, diff)                                        # widths
        l <- 
            lapply(list(ybottom = origin, ytop = colSums(tab)),
                   rep, length.out = nx * ngroups)
        l$x <- x + (t(mmat) * width)
        l$width <- (t(wmat) * width)
        if (!is.null(col)) l$col <- col
        if (mapfill) l$fill <- rep(mapFill(gl(ngroups, 1)), each = nx)
        else if (!is.null(fill)) l$fill <- fill
    }
    if (limits)
    {
        l$limits <-
            if (is.null(xorig)) default.limits(x = c(l$x-l$width, l$x+l$width),
                                               y = c(l$ybottom, l$ytop))
            else default.limits(x = xorig, y = c(l$ybottom, l$ytop))
    }
    ## ## Remap to argument recognized by render_rect()
    ## l$xleft <- l$x - l$width / 2
    ## l$xright <- l$x + l$width / 2
    ## l$x <- NULL
    ## l$width <- NULL
    l
}

map_table <- function(...)
{
    f <- function(x, y, groups = NULL, ...,
                  stack = TRUE, area = FALSE, width = 2/3, origin = 0,
                  col = NULL, fill = NULL, mapfill = TRUE, limits = TRUE)
    {
        list(table_constructor(x = x, y = y, groups = groups,
                               stack = stack, area = area,
                               width = width, origin = origin,
                               mapfill = mapfill, col = col, fill = fill,
                               limits = limits))
    }
    changeDefaults(f, ...)
}


hist_constructor <- 
    function(x, weights = NULL, groups = NULL, type = c("density", "count", "percent"),
             stack = TRUE, area = FALSE, #FIXME: better name
             breaks, include.lowest = TRUE, right = TRUE, origin = 0, ...,
             mapfill = TRUE, col = NULL, fill = NULL, limits = TRUE)
{
    ## Assume breaks is already vector
    if (length(breaks) == 1) stop("'breaks' must be a numeric vector of breakpoints.")
    type <- match.arg(type)
    ## We'll think of weights as something we sum up for each
    ## observation.  type=count means heights add up to n=length(x),
    ## so sum(weights) = n. type=percent means sum(weights)=100.
    ## type=density means area adds up to 1, which needs to look at
    ## breaks as well.
    n <- length(x)
    if (is.null(weights)) weights <- rep(1, n) # then sum(weights)=n
    if (type == "percent") weights <- 100 * weights / n
    ## Will deal with type=density later
    xf <- cut(x, breaks = breaks, include.lowest = include.lowest, right = right)
    tab <-
        if (is.null(groups)) structure(xtabs(weights ~ xf), dim = c(1, nlevels(xf)))
        else as.matrix(xtabs(weights ~ groups + xf))
    if (type == "density")
    {
        ## column totals = weight for each bin. Suppose f is desired column multiplier.
        ## The total area: colSums(tab) * f * diff(breaks) == colSums(tab)/n  ==>
        f <- 1 / (n * diff(breaks))
        for (i in seq_along(f)) tab[,i] <- tab[,i] * f[i]
    }
    table_constructor(tab = tab, groups = groups,
                      stack = stack, area = area,
                      xmid = 0.5 * (breaks[-1] + breaks[-length(breaks)]),
                      width = diff(breaks), origin = origin,
                      mapfill = mapfill, col = col, fill = fill,
                      limits = limits)
}


map_histogram <- function(...)
{
    f <- function(x, weights = NULL, ..., include.lowest = TRUE, right = TRUE, 
                  col = NULL, fill = NULL, groups = NULL, stack = TRUE, #area = TRUE,
                  breaks = shared.env$setup$breaks, equal.widths = TRUE,
                  nint = round(log2(length(x)) + 1),
                  type = "count",
                  shared.env = NULL, origin = 0,
                  limits = TRUE)
    {
        if (is.null(x)) stop("'x' cannot be NULL")
        xorig <- x
        x <- as.numeric(x)
        if (any(is.finite(x)))
        {
            if (is.null(breaks)) breaks <- default_hist_breaks(x, breaks = breaks, equal.widths = equal.widths, nint = nint)
            l <- hist_constructor(x = x, weights = weights,
                                  include.lowest = include.lowest, right = right,
                                  col = col, fill = fill, groups = groups, stack = stack, area = TRUE,
                                  breaks = breaks, type = type, origin = 0, limits = limits)
            if (limits) l$limits <- modifyList(l$limits, default.limits(x = xorig)[c("xlim", "xat", "xlabels")])
            list(l[!sapply(l, is.null)])
        }
        else list(list(x = numeric(0), y = numeric(0)))
    }
    changeDefaults(f, ...)
}


map_boxplot <- function(...)
{
    ## As usual, x is the factor, and y is the continuous variable.
    ## How do we want to handle grouping (for color)?  One option is
    ## to have like side-by-side barcharts.  That basically means the
    ## center lines of the boxes will have to be shifted.  However, if
    ## y is nested within groups (which is possible), we would like
    ## only one location, not others for 0-length levels.  Let's try
    ## something. and see how it goes.

    f <- function(x, y, ..., groups = NULL,
                  stats = boxplot.stats,
                  coef = 1.5, do.out = TRUE,
                  varwidth = FALSE,
                  ## notch = FALSE,   # Maybe later
                  ## notch.frac = 0.5,
                  box.width = 2/3,
                  col = NULL, fill = NULL, 
                  mapfill = !is.null(groups), mapcolor = TRUE,
                  limits = TRUE)
    {
        olimits <- default.limits(x = x, y = y)
        flevels <- sort(unique(as.numeric(x)))
        byvar <- if (is.factor(x)) droplevels(x) else factor(x, levels = flevels)
        if (!is.null(groups)) 
        {
            groups <- as.factor(groups)
            olev <- expand.grid(byvar = seq_len(nlevels(byvar)),
                                groups = seq_len(nlevels(groups))) # order of levels
            byvar <- interaction(byvar, groups)
            ## want to drop levels, but keep track of original
            ## 'groups' value
            olev <- olev[sort(unique(as.numeric(byvar))), ]
            byvar <- droplevels(byvar)
            flevels <- seq_len(nlevels(byvar))
            olimits <- default.limits(x = byvar, y = y)
        }
        blist <- tapply(y, byvar, stats, coef = coef, do.out = do.out)
        blist.stats <- t(sapply(blist, "[[", "stats"))
        blist.out <- lapply(blist, "[[", "out")
        blist.thickness <- box.width
        if (varwidth)
        {
            blist.n <- sapply(blist, "[[", "n")
            maxn <- max(blist.n)
            blist.thickness <- sqrt(blist.n / maxn) * blist.thickness
        }
        nout <- sapply(blist.out, length) # no of outliers in each, needed later
        l <- list(xleft = flevels - 0.5 * blist.thickness,
                  xright = flevels + 0.5 * blist.thickness,
                  ybottom = blist.stats[, 2],
                  ytop = blist.stats[, 4],
                  ## whisker
                  x0whisker = rep(flevels, 2),
                  y0whisker = c(blist.stats[, 2], blist.stats[, 4]),
                  x1whisker = rep(flevels, 2),
                  y1whisker = c(blist.stats[, 1], blist.stats[, 5]),
                  x0umbrella = flevels - blist.thickness / 2,
                  y0umbrella = c(blist.stats[, 1], blist.stats[, 5]),
                  x1umbrella = flevels + blist.thickness / 2,
                  y1umbrella = c(blist.stats[, 1], blist.stats[, 5]),
                  ## median
                  x0median = flevels - blist.thickness / 2,
                  y0median = blist.stats[, 3],
                  x1median = flevels + blist.thickness / 2,
                  y1median = blist.stats[, 3],
                  ## outliers
                  x = rep(flevels, nout),
                  y = unlist(blist.out))
        if (mapcolor) 
        {
            if (is.null(groups)) 
            {
                l$col <- mapColor(gl(1, 1))
                l$pointsCol <- l$col
            }
            else
            {
                l$col <- mapColor(gl(nlevels(groups), 1))[olev[["groups"]]]
                l$pointsCol <- rep(l$col, nout)
            }
        }
        else if (!is.null(col)) l$col <- col
        if (mapfill) 
        {
            l$fill <-
                if (is.null(groups)) mapFill(gl(1, 1))
                else mapFill(gl(nlevels(groups), 1))[olev[["groups"]]]
        }
        else if (!is.null(fill)) l$fill <- fill
        if (limits) l$limits <- olimits
        list(l[!sapply(l, is.null)])
    }
    changeDefaults(f, ...)
}


render_boxplot <- function(...)
{
    f <- function(xleft, xright, ybottom, ytop,
                  x0whisker, y0whisker, x1whisker, y1whisker,
                  x0umbrella, y0umbrella, x1umbrella, y1umbrella,
                  x0median, y0median, x1median, y1median,
                  x, y, col = 1, fill = "transparent", pointsCol = col, ...)
    {
        yrect(xleft = xleft, xright = xright, ybottom = ybottom, ytop = ytop,
              col = col, fill = fill, ...)
        ysegments(x0 = x0whisker, y0 = y0whisker, x1 = x1whisker, y1 = y1whisker,
                  col = col, fill = fill, ...)
        ysegments(x0 = x0umbrella, y0 = y0umbrella, x1 = x1umbrella, y1 = y1umbrella,
                  col = col, fill = fill, ...)
        ysegments(x0 = x0median, y0 = y0median, x1 = x1median, y1 = y1median,
                  col = col, fill = fill, ...)
        ypoints(x = x, y = y, col = pointsCol, ...)
    }
    yrender(changeDefaults(f, ...),
            c(x="y", y="x",
              xleft="ybottom", ybottom="xleft", xright="ytop", ytop="xright",
              x0whisker="y0whisker", y0whisker="x0whisker", x1whisker="y1whisker", y1whisker="x1whisker",
              x0umbrella="y0umbrella", y0umbrella="x0umbrella", x1umbrella="y1umbrella", y1umbrella="x1umbrella",
              x0median="y0median", y0median="x0median", x1median="y1median", y1median="x1median"))
}


map_map <- function(...)
{
    f <- function(x = NULL, y = NULL, ..., map,
                  cuts = 30, breaks,
                  col = NULL, fill = NULL, 
                  colramp = yagp.theme()$regions$col,
                  limits = TRUE)
    {
        l <- list(x = map$x, y = map$y)
        if (limits) l$limits <- default.limits(map$x, map$y)
        if (!is.null(x))
        {
            names(x) <- as.character(y)
            x <- x[map$names] # match order with map polygons
            if (missing(breaks))
                breaks <-
                    if (is.factor(x)) seq_len(1 + nlevels(x)) - 0.5
                    else do.breaks(range(x, finite = TRUE), cuts)
            l$fill <- level.colors(x, at = breaks, colramp = colramp)
        }
        else if (!is.null(fill)) l$fill <- fill
        list(l[!sapply(l, is.null)])
    }
    changeDefaults(f, ...)
}


parallel_constructor <- function(xfrom, xto, i, col = NULL, limits = NULL)
{
    l <- list(x0 = i, x1 = i+1, y0 = xfrom, y1 = xto, limits = limits, col = col)
    l[!sapply(l, is.null)]
}


map_parallel <- function(...)
{
    f <- function(x = NULL, groups = NULL, ...,
                  vorder = shared.env$setup$vorder,
                  shared.env = NULL, 
                  color = NULL, col = NULL, limits = TRUE)
    {
        ## x should be a matrix
        ## Maybe we can return a bunch of segments() calls
        ## Y-axis: numeric data
        ## X-axis: columns of x
        if (!is.null(vorder)) x <- x[, vorder, drop = FALSE]
        cnames <- colnames(x)
        if (is.null(cnames)) cnames <- as.character(seq_len(ncol(x)))
        lim <-
            if (limits) default.limits(x = factor(cnames, levels = cnames),
                                       y = as.numeric(x))
            else NULL
        col <- if (is.null(color)) col else mapColor(color)
        l <- lapply(seq_len(ncol(x) - 1),
                    function(i) {
                        parallel_constructor(xfrom = x[,i],
                                             xto = x[,i+1],
                                             i = i, col = col, limits = lim)
                    })
    }
    changeDefaults(f, ...)
}


map_parallel <- function(...)
{
    f <- function(x = NULL, groups = NULL, ...,
                  vorder = shared.env$setup$vorder,
                  shared.env = NULL, 
                  color = NULL, col = NULL, limits = TRUE)
    {
        ## x should be a matrix
        ## Maybe we can return a bunch of segments() calls
        ## Y-axis: numeric data
        ## X-axis: columns of x
        if (!is.null(vorder)) x <- x[, vorder, drop = FALSE]
        cnames <- colnames(x)
        if (is.null(cnames)) cnames <- as.character(seq_len(ncol(x)))
        lim <-
            if (limits) default.limits(x = factor(cnames, levels = cnames),
                                       y = as.numeric(x))
            else NULL
        col <- if (is.null(color)) col else mapColor(color)
        l <- lapply(seq_len(ncol(x) - 1),
                    function(i) {
                        parallel_constructor(xfrom = x[,i],
                                             xto = x[,i+1],
                                             i = i, col = col, limits = lim)
                    })
    }
    changeDefaults(f, ...)
}


map_projection <- function(..., jitter.x = FALSE, jitter.y = FALSE)
{
    pargs <- list(jitter.x = jitter.x, jitter.y = jitter.y)
    f <- function(x = NULL, groups = NULL, ...,
                  xproj = shared.env$setup$xproj,
                  yproj = shared.env$setup$yproj,
                  shared.env = NULL, 
                  mapcolor = !is.null(groups) && is.null(color),
                  col = NULL, size = NULL, color = NULL, limits = TRUE)
    {
        ## x should be a matrix
        ## center and scale? Not here.
        cnames <- colnames(x)
        if (is.null(cnames)) cnames <- as.character(seq_len(ncol(x)))
        x2d <- x %*% cbind(xproj, yproj)
        mapByGroups(x = x2d[,1], y = x2d[,2],
                    groups = groups, size = size, color = color,
                    constructor = points_constructor,
                    args = pargs, col = col, mapcolor = mapcolor, limits = limits)
    }
    changeDefaults(f, ...)
}

map_projection_axes <- function(..., names = TRUE)
{
    f <- function(x = NULL, ...,
                  xproj = shared.env$setup$xproj,
                  yproj = shared.env$setup$yproj,
                  shared.env = NULL, 
                  limits = TRUE)
    {
        ## x should be a matrix
        nvars <- ncol(x)
        cnames <- colnames(x)
        if (is.null(cnames)) cnames <- as.character(seq_len(ncol(x)))
        ## Create a new matrix with length(cnames) rows, each row
        ## representing the position of the corresponding variable's axis 
        r <- apply(x, 2, function(x) max(abs(x)))
        r2d <- diag(r) %*% cbind(xproj, yproj)
        l <- list(x0 = rep(0, nvars), y0 = rep(0, nvars),
                  x1 = r2d[,1], y1 = r2d[,2])
        if (limits) l$limits <- default.limits(c(l$x0, l$x1), c(l$y0, l$y1))
        list(l)
    }
    changeDefaults(f, ...)
}


ytransform3dMatrix <- function(screen, R.mat = diag(4))
{
    ## can simplify to have at most one x|y|z
    rot.mat <- diag(3)
    screen.names <- names(screen)
    screen <- lapply(screen, "*", pi/180)
    for(i in seq_along(screen.names))
    {
        th <- screen[[i]]
        cth <- cos(th)
        sth <- sin(th)
        tmp.mat <-
            (if (screen.names[i]=="x")
             matrix(c(1, 0, 0, 0, cth, sth, 0, -sth, cth), 3, 3)
            else if (screen.names[i]=="y")
             matrix(c(cth, 0, -sth, 0, 1, 0, sth, 0, cth), 3, 3)
            else if (screen.names[i]=="z")
             matrix(c(cth, sth, 0, -sth, cth, 0, 0, 0, 1), 3, 3))
        rot.mat <- tmp.mat %*% rot.mat
    }
    rot.mat <- cbind(rot.mat, c(0,0,0))
    rot.mat <- rbind(rot.mat, c(0,0,0,1))
    if (!missing(R.mat)) rot.mat <- rot.mat %*% R.mat
    rot.mat
}

ytransform3dto3d <- function(x, R.mat, dist = 0)
{
    if (length(x) == 0) return(x)
    tdata <- R.mat %*% rbind(x, 1)

    ## back to 3d
    tdata[1,] <- tdata[1,]/tdata[4,]
    tdata[2,] <- tdata[2,]/tdata[4,]
    tdata[3,] <- tdata[3,]/tdata[4,]

    ## now 'perspective' x,y coordinates. z remains unmodified
    if (dist != 0)  ## 1/dist = distance of eye from center
    {
        tdata[1,] <- tdata[1,] / (1/dist - tdata[3,])
        tdata[2,] <- tdata[2,] / (1/dist - tdata[3,])
    }

    tdata[1:3, , drop = FALSE]
}




map_cloud <- function(..., jitter.x = FALSE, jitter.y = FALSE)
{
    pargs <- list(jitter.x = jitter.x, jitter.y = jitter.y)
    f <- function(x = NULL, y = NULL, z = NULL, groups = NULL, ...,
                  ## screen = shared.env$setup$screen,
                  rot.mat = shared.env$setup$rot.mat,
                  distance = 0,
                  shared.env = NULL,
                  mapcolor = !is.null(groups) && is.null(color),
                  col = NULL, size = NULL, color = NULL, limits = TRUE)
    {
        x2d <- ytransform3dto3d(rbind(x, y, z),
                                rot.mat,
                                dist = distance)
        mapByGroups(x = x2d[1,], y = x2d[2,],
                    groups = groups, size = size, color = color,
                    constructor = points_constructor,
                    args = pargs, col = col, mapcolor = mapcolor, limits = limits)
    }
    changeDefaults(f, ...)
}


map_cloud_axes <- function(..., names = TRUE)
{
    f <- function(x = NULL, y = NULL, z = NULL, ...,
                  ## screen = shared.env$setup$screen,
                  rot.mat = shared.env$setup$rot.mat,
                  distance = 0,
                  shared.env = NULL, limits = TRUE)
    {
        fmin <- function(x) min(x, na.rm = TRUE)
        fmax <- function(x) max(x, na.rm = TRUE)
        rmin <- c(fmin(x), fmin(y), fmin(z))
        rmax <- c(fmax(x), fmax(y), fmax(z))
        x0 <- rep(rmin[1], 3)
        y0 <- rep(rmin[2], 3)
        z0 <- rep(rmin[3], 3)
        x1 <- c(rmax[1], rmin[1], rmin[1])
        y1 <- c(rmin[2], rmax[2], rmin[2])
        z1 <- c(rmin[3], rmin[3], rmax[3])
        x2d0 <- ytransform3dto3d(rbind(x0, y0, z0), rot.mat, dist = distance)
        x2d1 <- ytransform3dto3d(rbind(x1, y1, z1), rot.mat, dist = distance)
        l <- list(x0 = x2d0[1,], y0 = x2d0[2,],
                  x1 = x2d1[1,], y1 = x2d1[2,],
                  x = x2d1[1,], y = x2d1[2,], labels = c("x", "y", "z"))
        if (limits) l$limits <- default.limits(c(l$x0, l$x1), c(l$y0, l$y1))
        list(l)
    }
    changeDefaults(f, ...)
}
