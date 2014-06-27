
## This is how we are going to deal with limits.  The 'mapping'
## component of each layer should also optionally return a component
## (or attribute?) limits=list(xlim=, ylim=, dx=, dy=)

## compute.limits <-
##     function(packets, panel.vars, prepanel,
##              data = .GlobalEnv, enclos = .GlobalEnv, ...)
## {
##     ans <- packets
##     for (i in seq_len(length(ans)))
##     {
##         packet <- packets[[i]]
##         packet_data <-
##             lapply(panel.vars,
##                    function(x) evaluate(x, data = data, subset = packet,
##                                         enclos = enclos))
##         ans[[i]] <- checkArgsAndCall(prepanel, c(packet_data, list(...)))
##     }
##     ans
## }



compute.limits <- function(panel, packets, ...)
{
    ans <- packets
    panel.args <- list(..., packets = packets)
    for (i in seq_len(length(ans)))
    {
        ans[[i]] <-
            lapply(panel, function(layer)
               {
                   ## FIXME: Should allow for empty data (e.g., for reference grids), but not sure how.
                   pargs <- panel.args # copy for this layer, different only if 'data' changes
                   if (!is.null(layer$data))
                   {
                       stopifnot(identical(dim(layer$packets), dim(panel.args$packets)))
                       pargs$data <- layer$data
                       pargs$packets <- layer$packets
                   }
                   do.call(yprepanel.super,
                           c(pargs, list(mapping = layer$mapping, which.packet = i)))
               })
    }
    ## debug.str(ans) #, compute.limits = ans))
    ans
}

## utility function to take a list of 'limit' lists and make one (that encloses all)

enclose_limits <- function(l)
{
    ## Combine results from multiple layers, each possibly lists
    ## corresponding to groups.
    l <- do.call(c, l)
    ## str(l)
    ## Then compute bounding box.
    ## FIXME: Need to think what do you with [xy]at,labels
    ans <- c(default.limits(x = unlist(lapply(l, "[[", "xlim")), 
                            y = unlist(lapply(l, "[[", "ylim")))[c("xlim", "ylim")],
             list(dx = unlist(lapply(l, "[[", "dx"), use.names = FALSE),
                  dy = unlist(lapply(l, "[[", "dy"), use.names = FALSE),
                  xat = unique(unlist(lapply(l, "[[", "xat"), use.names = FALSE)),
                  xlabels = unique(unlist(lapply(l, "[[", "xlabels"), use.names = FALSE)),
                  yat = unique(unlist(lapply(l, "[[", "yat"), use.names = FALSE)),
                  ylabels = unique(unlist(lapply(l, "[[", "ylabels"), use.names = FALSE))))
    ## str(ans)
    ans
}


combine.limits <-
    function(limits,
             relation = relation(),
             tick.number = 5,
             xlim = NULL, ylim = NULL)
{
    debug.str(limits) #, src = "combine.limits")
    limits <- lapply(limits, enclose_limits)
    ## needs thought. 'margin' is intended to control combination
    ## across margins; e.g., may want a common scale for rows only.

    ## For now, just combine xlim and ylim.

    ## for factor annotation, [xy]at and [xy]labels should also be
    ## combined.

    xat <- unlist(lapply(limits, "[[", "xat"))
    if (is.null(xat)) ## numeric: leave alone for auto-generation later
    {
        if (is.null(xlim)) xlim <- extendrange(unlist(lapply(limits, "[[", "xlim")))
        ## xat <- pretty(xlim, tick.number)
        ## xlabels <- format(xat)
        xlabels <- NULL
    }
    else # factor? (not necessarily; e.g., date-time, explicit specification)
    {
        if (is.null(xlim)) xlim <- range(unlist(lapply(limits, "[[", "xlim")), na.rm = TRUE) + c(-0.6, 0.6)
        xlabels <- unlist(lapply(limits, "[[", "xlabels"))
        xid <- !duplicated(xat)
        xat <- xat[xid]
        xlabels <- xlabels[xid] ## should check that xlabels[!xid] are all duplicated
    }
    yat <- unlist(lapply(limits, "[[", "yat"))
    if (is.null(yat))
    {
        if (is.null(ylim)) ylim <- extendrange(unlist(lapply(limits, "[[", "ylim")))
        ## yat <- pretty(ylim, tick.number)
        ## ylabels <- format(yat)
        ylabels <- NULL
    }
    else 
    {
        if (is.null(ylim)) ylim <- range(unlist(lapply(limits, "[[", "ylim")), na.rm = TRUE) + c(-0.6, 0.6)
        ylabels <- unlist(lapply(limits, "[[", "ylabels"))
        yid <- !duplicated(yat)
        yat <- yat[yid]
        ylabels <- ylabels[yid]
    }
    for (i in seq_len(length(limits)))
    {
        if (relation$x == "same") 
        {
            limits[[i]][["xlim"]] <- xlim
            ## if (!is.null(xat))
            ## {
            limits[[i]][["xat"]] <- xat
            limits[[i]][["xlabels"]] <- xlabels
            ## }
        }
        else 
        {
            if (is.null(limits[[i]][["xat"]]))
            {
                limits[[i]][["xlim"]] <- extendrange(limits[[i]][["xlim"]])
                ## limits[[i]][["xat"]] <- pretty(limits[[i]][["xlim"]], tick.number)
                ## limits[[i]][["xlabels"]] <- format(limits[[i]][["xat"]])
            }
            else 
            {
                limits[[i]][["xlim"]] <- range(limits[[i]][["xlim"]]) + c(-0.6, 0.6)
            }
        }
    }
    for (i in seq_len(length(limits)))
    {
        if (relation$y == "same") 
        {
            limits[[i]][["ylim"]] <- ylim
            ## if (!is.null(yat))
            ## {
            limits[[i]][["yat"]] <- yat
            limits[[i]][["ylabels"]] <- ylabels
            ## }
        }
        else 
        {
            if (is.null(limits[[i]][["yat"]]))
            {
                limits[[i]][["ylim"]] <- extendrange(limits[[i]][["ylim"]])
                ## limits[[i]][["yat"]] <- pretty(limits[[i]][["ylim"]], tick.number)
                ## limits[[i]][["ylabels"]] <- format(limits[[i]][["yat"]])
            }
            else 
            {
                limits[[i]][["ylim"]] <- range(limits[[i]][["ylim"]]) + c(-0.6, 0.6)
            }
        }
    }
    ## FIXME: There is one hitch: if some panels are empty, some
    ## components may have xlim or ylim = c(NA, NA) or (-Inf, Inf).
    ## These cause problems.  For now, just change them to c(0, 1).
    limits <-
        lapply(limits,
               function(l) {
                   if (any(!is.finite(l$xlim))) l$xlim <- c(0, 1)
                   if (any(!is.finite(l$ylim))) l$ylim <- c(0, 1)
                   l
               })
    limits
}


banking <- function(dx, dy = 1)
{
    if (is.null(dx) || is.null(dy)) return(NULL)
    if (length(dx) != length(dy)) stop("Non matching lengths")
    id <- dx!=0 & dy!=0 & !is.na(dx) & !is.na(dy)
    if (any(id)) median(abs(dx[id] / dy[id]))
    else 1
}


compute.aspect <- function(aspect, limits)
{
    ## limits is a list, one component per panel, each list with
    ## components xlim, ylim, dx, dy
    if (is.numeric(aspect)) return(aspect)
    xdiff <- unlist(lapply(limits, function(x) diff(range(x[["xlim"]], na.rm = TRUE))))
    ydiff <- unlist(lapply(limits, function(x) diff(range(x[["ylim"]], na.rm = TRUE))))
    if (aspect == "xy")
    {
        b <- sapply(limits, function(x) banking(x[["dx"]], x[["dy"]]))
        median(b * ydiff / xdiff, na.rm = TRUE)
    }
    else if (aspect == "iso")
    {
        median(ydiff / xdiff, na.rm = TRUE)
    }
    else stop("Invalid aspect: ", aspect)
}

