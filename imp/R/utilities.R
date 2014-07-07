
## simple helper function to generate new functions from old ones by
## overriding certain arguments.
customize <- function(fun, ...)
{
    args <- list(...)
    function(...) {
        dots <- list(...)
        do.call(fun, modifyList(dots, args))
    }
}

## similar one where we only change the defaults
changeDefaults <- function(fun, ...)
{
    args <- list(...)
    function(...) {
        dots <- list(...)
        do.call(fun, modifyList(args, dots))
    }
}

## changes the names of list elements (e.g. x->y, y->x, etc)
changeListNames <- function(x, nmmap) 
{
    if (is.null(nmmap)) return(x)
    ## e.g., x = list(xlim=, ylim=, dx=, dy=, xat=, ...)
    m <- which(names(x) %in% names(nmmap)) # others remain unchanged
    names(x)[m] <- nmmap[names(x)[m]]
    x
}


getFunctionOrName <- base::match.fun

## ## Get a function, possibly from its name
## getFunctionOrName <- function(FUN)
##      ## Try namespace first? Does that happen automatically?
## {
##     if (is.function(FUN)) FUN
##     else if (is.character(FUN)) get(FUN)
##     else eval(FUN)
## }


## Like do.call(), but only with arguments that will be accepted by FUN
checkArgsAndCall <- function(FUN, args) ## unnamed arguments not allowed
{
    FUN <- getFunctionOrName(FUN)
    if (!("..." %in% names(formals(FUN))))
        args <- args[intersect(names(args), names(formals(FUN)))]
    do.call(FUN, args)
}


splitByGroup <- function(..., groups)
{
    ## FIXME: groups could also be shingles. Why not? Useful for
    ## boxplot type things
    vars <- list(...)
    d <- do.call(data.frame, vars[!sapply(vars, is.null)])
    vals <-
        if (is.null(groups)) NULL
        else if (is.factor(groups)) levels(groups)
        else sort(unique(groups))
    nvals <- if (is.null(vals)) 1L else length(vals)
    structure(lapply(seq_len(nvals),
                     function(i) {
                         if (is.null(vals)) d
                         else d[groups == vals[i], ,drop = FALSE] # handles NA
                     }),
              nvals = nvals,
              names = as.character(vals))
}


mapSize <- function(x, r = range(x), min = 0.5, max = 2, ...)
{
    min + (max - min) * (x - r[1]) / diff(r)
}

level.colors <- 
    function (x, at = do.breaks(range(x), n), n = length(colramp),
              colramp, indices = FALSE, ...) 
{
    if (missing(at) && missing(n)) n <- if (is.function(colramp)) 100 else length(colramp)
    ind.col <- cut(x, at, include.lowest = TRUE, labels = FALSE)
    if (indices) ind.col
    else {
        if (is.function(colramp)) colramp <- colramp(nregions)
        nregions <- length(at) - 1
        ncolor <- length(colramp)
        colramp <-
            if (ncolor <= nregions) rep(colramp, length.out = nregions)
            else colramp[round(seq(1, ncolor, length.out = nregions))]
        colramp[ind.col]
    }
}


mapColor <- function(x, r = if (is.numeric(x)) range(x) else levels(as.factor(x)),
                     pallette = theme$default$col, regions = theme$regions$col, ..., theme = imp.theme())
{
    if (is.numeric(x)) level.colors(x, colramp = regions)
    else
    {
        i <- 1L + (as.numeric(as.factor(x))-1L) %% length(pallette)
        pallette[i]
    }
}

mapFill <- function(x, r = if (is.numeric(x)) range(x) else levels(as.factor(x)),
                    pallette = theme$polygon$fill, regions = theme$regions$col, ..., theme = imp.theme())
{
    if (is.numeric(x)) level.colors(x, colramp = regions)
    else
    {
        i <- 1L + (as.numeric(as.factor(x))-1L) %% length(pallette)
        pallette[i]
    }
}


mapByColor <- function(..., constructor, args, col = NULL, color = NULL, limits = FALSE)
{
    .Deprecated("mapByGroups")
    dlist <- splitByGroup(..., groups = color)
    nvals <- attr(dlist, "nvals")
    l <- lapply(dlist,
                function(d) {
                    checkArgsAndCall(constructor, c(d, args))[c("x", "y")]
                })
    if (limits)
    {
        for (i in seq_along(l))
            l[[i]]$limits <- default.limits(l[[i]]$x, l[[i]]$y)
    }
    if (!is.null(color)) {
        col <- mapColor(gl(nvals, 1))
        for (i in seq_along(l)) l[[i]]$col <- col[i]
    }
    else if (!is.null(col))
    {
        for (i in seq_along(l)) l[[i]]$col <- col
    }
    l
}


## Want to group according to groups variable, and then use a color
## variable to map to color.  Except that there are two different
## cases.  For points, we may want a vector of colors (possibly a
## gradient for a continuous color variable) for each group,
## determined by the color variable.  For almost everything else, we
## want a scalar color, determined by the groups variable.  How can we
## unify?

## How about this: we always group by 'groups', and
## mapcolor=TRUE/FALSE tells us whether to color by 'groups' as well.
## The vector coloring, if necessary, is handled by the constructor,
## and mapByGroups doesn't know anything about it.  The only thing is
## that is mapcolor=TRUE (or col != NULL), then that overrides
## anything the constructor does to assign colors.

mapByGroups <- function(..., groups = NULL, constructor, args,
                        col = NULL, mapcolor = FALSE, limits = FALSE)
{
    dlist <- splitByGroup(..., groups = groups)
    nvals <- attr(dlist, "nvals")
    l <- lapply(dlist,
                function(d) {
                    checkArgsAndCall(constructor, c(d, args)) #[c("x", "y")]
                })
    if (limits)
    {
        ## FIXME: should mapByGroups handle limits? Or should it be
        ## done by the constructor?
        for (i in seq_along(l))
            if (is.null(l[[i]]$limits))
                l[[i]]$limits <- default.limits(l[[i]]$x, l[[i]]$y)
            else
                l[[i]]$limits <-
                    modifyList(default.limits(l[[i]]$x, l[[i]]$y),
                               l[[i]]$limits)
        ## If already present, still filling in those missing. Think about this. 
    }
    else
    {
        for (i in seq_along(l))
            l[[i]]$limits <- NULL
    }
    if (mapcolor) {
        col <- mapColor(gl(nvals, 1))
        for (i in seq_along(l)) l[[i]]$col <- col[i]
    }
    else if (!is.null(col))
    {
        for (i in seq_along(l)) l[[i]]$col <- col
    }
    ## debug.str(l)
    l
}

debug.str <- function(x, give.attr = FALSE, max.level = 3,
                      src = as.character(sys.call(sys.parent()))[1L])
{
    s <- getOption("debug.str")
    nm <- deparse(substitute(x))
    if (isTRUE(s))
    {
        cat(sprintf("DEBUG: %s (%s)\n", src, nm))
        showCalls()
        str(x)
    }
}

showCalls <- function() # adapted from recover(), but remove trace-related stuff
{
    ## find an interesting environment to start from
    calls <- sys.calls()
    n <- length(calls)
    if (identical(sys.function(n), showCalls))
        n <- n - 1L
    from <- n
    if (from > 0L)
    {
        .GlobalEnv$calls <- calls
        calls <- sapply(calls[1L:from],
                        function(x) {
                            o <- x[[1]]
                            if (is.name(o)) as.character(o)
                            else sQuote(class(o))
                        })
        cat(calls, sep = " > ")
        cat("\n")
    }
}









