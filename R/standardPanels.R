
null.limits <- function()
{
    list(xlim = c(NA_real_, NA_real_),
         ylim = c(NA_real_, NA_real_))
}

default.limits <- function(x, y)
{
    ans <- null.limits()
    if (!missing(x) && any(is.finite(as.numeric(x))))
    {
        ans$xlim <- range(as.numeric(x), na.rm = TRUE, finite = TRUE)
        ans$dx <- diff(as.numeric(x))
        if (is.factor(x))
        {
            ans$xat <- as.numeric(unique(x))
            ans$xlabels <- as.character(unique(x))
        }
    }
    if (!missing(y) && any(is.finite(as.numeric(y))))
    {
        ans$ylim <- range(as.numeric(y), na.rm = TRUE, finite = TRUE)
        ans$dy <- diff(as.numeric(y))
        if (is.factor(y))
        {
            ans$yat <- as.numeric(unique(y))
            ans$ylabels <- as.character(unique(y))
        }
    }
    ans
}

## FIXME: have prepanels use generalized 'range' function (for
## numeric, factors, time) when computing limits.


## `super'-panel function that deals with a layer by computing packet,
## mapping, and rendering.

ypanel.super <-
    function(panel.vars = list(x = NULL, y = NULL, groups = NULL),
             which.packet, packets,
             data, enclos,
             give.limits = FALSE,
             mapping = NULL,
             render = NULL,
             ...,
             switch.axes = FALSE,
             shared.env)
{
    packet <- packets[[which.packet]]
    packet_data <-
        lapply(panel.vars,
               function(x) evaluate(x, data = data, subset = packet,
                                    enclos = enclos))
    args <- list(shared.env = shared.env, ...) # Sent to both mapping and render.
    ## For each layer in render, draw layer using each component of mapping.
    mlist <- checkArgsAndCall(mapping, c(packet_data, args))
    lapply(render,
           function(r) {
               lapply(mlist, function(m) {
                   if (switch.axes) 
                       checkArgsAndCall(r, c(changeListNames(m, attr(r, "switch_names")), args))
                   else
                       checkArgsAndCall(r, c(m, args))
               })
           })
}

## `super'-prepanel function that deals with a layer by computing
## packet, mapping, and reporting limits.

yprepanel.super <-
    function(panel.vars = list(x = NULL, y = NULL, groups = NULL),
             which.packet, packets,
             data, enclos,
             mapping = NULL,
             ...,
             switch.axes = FALSE)
{
    packet <- packets[[which.packet]]
    packet_data <- lapply(panel.vars, evaluate, data = data, subset = packet, enclos = enclos)
    args <- list(...)
    mlist <- checkArgsAndCall(mapping, c(packet_data, args))
    debug.str(mlist, src = "yprepanel.super")
    l <- lapply(mlist, "[[", "limits")
    if (switch.axes)
        l <- lapply(l, changeListNames,
                    nmmap = c(xlim = "ylim", dx = "dy", xat = "yat", xlabels = "ylabels",
                              ylim = "xlim", dy = "dx", yat = "xat", ylabels = "xlabels"))
    l
}


do.breaks <- function (endpoints, nint)
{
    stopifnot(length(endpoints) == 2)
    endpoints[1] + diff(endpoints) * 0:nint/nint
}




