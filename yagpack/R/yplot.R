
## This is the main high-level function in the package that produces a
## "yagp" object.  This needs to be careful to avoid making
## assumptions about the rendering backend, but include enough
## information to make rendering possible.

## 'Panel functions' should be written using a graphics API (that
## needs to be defined) that provides primitive functions.  The plan
## is to call the panel function after setting its containing
## environment to a backend-specific environment that contains the
## suitable primitives.

yplot <-
    function(data = environment(), enclos = .GlobalEnv,
             margin.vars = list(),
             layout = NULL, skip = FALSE,

             ## perhaps useful for predefined packets; multiple data sources
             packets = compute.packets(margin.vars,
                                       data = data,
                                       enclos = enclos),
             
             panel.vars, panel, ## prepanel,

             ## specify how limits are combined
             relation = list(),

             ## specify how to alternate labels
             alternating = list(),

             ## scales.  FIXME: do some combining _a la_ lattice

             scales = list(x = list(tick.number = 5, font = 1, tck = 1, rot = 0),
                           y = list(tick.number = 5, font = 1, tck = 1, rot = 0)),
             switch.axes = FALSE,
             aspect = NULL,

             xlim = NULL, ylim = NULL,
             xlab = shared.env$setup$xlab,
             ylab = shared.env$setup$ylab,
             main = NULL, sub = NULL,

             theme = yagp.theme(),
             legend = NULL,

             ...)
{

    ## an environment to store shared information, notably axis
    ## limits.  All panel, axis functions, etc. have access to this.
    ## Panel functions (but not prepanel functions) also have access
    ## to a panel-specific environment 'panel.env'

    shared.env <- new.env(parent = emptyenv())
    shared.env$layer.envs <- list()
    shared.env$viewports <- list()
    shared.env$setup <- list() # for setup functions to store things

    relation <- do.call(yagp.relation, relation)
    alternating <- do.call(yagp.alternating, alternating)
    if (inherits(margin.vars, "formula")) margin.vars <- margin.terms(margin.vars)
    if (is.null(layout))
    {
        layout <-
            if (length(dim(packets)) == 1)
                c(0, dim(packets))
            else dim(packets)[1:2]
    }

    ## Collect all arguments that are relevant to data-driven
    ## computations (except margins, which have already been
    ## processed)
    xargs <- list(panel = panel,
                  packets = packets,
                  panel.vars = panel.vars,
                  ## prepanel = prepanel,
                  data = data,
                  enclos = enclos,
                  shared.env = shared.env,
                  switch.axes = switch.axes, ...)

    ## Perform necessary setup.  This should not need any primitives.
    ## It may define default xlab, ylab, etc.
    for (layer in panel)
    {
        if (!is.null(layer$setup)) checkArgsAndCall(layer$setup, xargs)
    }
    x <- list(layout = layout, skip = skip,
              xlim = xlim, ylim = ylim,
              relation = relation,
              alternating = alternating,
              switch.axes = switch.axes,
              aspect = aspect,
              xlab = xlab,
              ylab = ylab,
              main = main,
              sub = sub,
              legend = legend,
              theme = theme,
              xargs = xargs,
              shared.env = shared.env, # also in xargs
              ...)
    class(x) <- "yagp"
    x
}

## plot method



plot.yagp <-
    function(x, page = 1,
             primitives = NULL,
             ...,
             draw_labels = TRUE,
             draw_panels = TRUE,
             draw_strips = TRUE,
             draw_axes = TRUE)
{
    x$panel.layout <- compute.layout(x$layout, dim(x$xargs$packets), skip = x$skip)
    packet.limits <- checkArgsAndCall(compute.limits, x$xargs)
    x$shared.env$limits <- combine.limits(limits = packet.limits, relation = x$relation, xlim = x$xlim, ylim = x$ylim)
    if (!is.null(x$aspect)) x$aspect <- with(x, compute.aspect(aspect, shared.env$limits))

    ## FIXME: make theme handling simpler
    if (is.null(.yagpenv$theme)) .yagpenv$theme <- yagp.custom.theme()
    if (!is.null(x$theme)) 
    {
        .yagpenv$theme <- modifyList(.yagpenv$theme, x$theme)
    }
    if (is.null(primitives)) primitives <- .yagpenv$backend
    if (is.function(primitives)) primitives <- primitives()
    a <- attr(attach(primitives), "name")
    on.exit(detach(a, character.only = TRUE))
    context <- tget_context()
    for (p in page)
    {
        yagp_page(x, page = p, ...,
                  draw_labels = draw_labels,
                  draw_panels = draw_panels,
                  draw_strips = draw_strips,
                  draw_axes = draw_axes,
                  vp = tviewport(context))
    }
}




print.yagp <- function(x, ...)
{
    plot(x, ...)
    invisible(x)
}


summary.yagp <-
    function(object, ...)
{
    ans <- 
        with(object, 
             structure(sapply(packets, length),
                       dim = dim(packets),
                       dimnames = dimnames(packets)))
    class(ans) <- "summary.yagp"
    ans
}


print.summary.yagp <- function(x, ...)
{
    cat(gettext("\nNumber of observations:\n"))
    x <- unclass(x)
    NextMethod("print")
    invisible(x)
}


dim.yagp <- function(x)
{
    dim(x$packets)
}


dimnames.yagp <- function(x)
{
    dimnames(x$packets)
}


"dimnames<-.yagp" <- 
    function (x, value)
{
    dimnames(x$packets) <- value
    x
}

## "[.yagp" <- function(x, i, j, ..., drop = FALSE)
## {
##     if (!missing(drop) && drop)
##         warning("'drop=TRUE' ignored")
##     indices <- rep(list(TRUE), length.out = length(dim(x)))
##     if (!missing(i)) {
##         indices[[1]] <- i
##     }
##     if (!missing(j)) {
##         indices[[2]] <- j
##     }
##     x$packets <- do.call("[", c(list(x$packets), indices, list(drop = drop)))
##     ## x$packets <- x$packets[i, j, ..., drop = FALSE]
##     x
## }





## high-level convenience functions

xyplot <- yplot_xy <- 
    function(x, y = NULL, data, enclos, groups = NULL, color = NULL, size = NULL,
             legend = auto.legend("p", panel.vars, data = data, enclos = enclos, 
                 more.args = legend.args),
             legend.args = list(),
             panel = ypanel.xyplot(), 
             ...)
{
    panel.vars <- panel.terms(list(x = substitute(x), y = substitute(y), 
            groups = substitute(groups), color = substitute(color), 
            size = substitute(size)))
    if (missing(data)) data <- parent.frame()
    if (missing(enclos)) enclos <- parent.frame()
    yplot(data = data, enclos = enclos, panel.vars = panel.vars, 
        panel = panel, legend = legend, ...)
}

densityplot <- yplot_density <- 
    function(x, data, enclos, weights = NULL, groups = NULL, 
             legend = auto.legend("l", panel.vars, data = data, enclos = enclos, 
                 more.args = legend.args),
             legend.args = list(),
             panel = ypanel.density(), ...)
{
    panel.vars <- panel.terms(list(x = substitute(x), 
            weights = substitute(weights), groups = substitute(groups)))
    if (missing(data)) data <- parent.frame()
    if (missing(enclos)) enclos <- parent.frame()
    yplot(data = data, enclos = enclos, panel.vars = panel.vars, panel = panel, 
        legend = legend, ...)
}

qqmath <- yplot_qqmath <- 
    function(x, data, enclos, groups = NULL, 
             legend = auto.legend("p", panel.vars, data = data, enclos = enclos,
                 more.args = legend.args),
             legend.args = list(),
             panel = ypanel.qqmath(), ...) {
    panel.vars <- panel.terms(list(x = substitute(x), 
            groups = substitute(groups)))
    if (missing(data)) data <- parent.frame()
    if (missing(enclos)) enclos <- parent.frame()
    yplot(data = data, enclos = enclos, panel.vars = panel.vars, 
        panel = panel, legend = legend, ...)
}

histogram <- yplot_hist <- 
    function(x, data, enclos, weights = NULL, groups = NULL,
             legend = auto.legend("r", panel.vars, data = data, enclos = enclos, more.args = legend.args),
             legend.args = list(),
             panel = ypanel.histogram(), ...)
{
    panel.vars <- panel.terms(list(x = substitute(x), weights = substitute(weights), groups = substitute(groups)))
    if (missing(data)) data <- parent.frame()
    if (missing(enclos)) enclos <- parent.frame()
    yplot(data = data, enclos = enclos, panel.vars = panel.vars, panel = panel, legend = legend, ...)
}

dotplot <- yplot_dot <- yplot_xy


bwplot <- yplot_bw <- 
    function(x, y = NULL, data, enclos, groups = NULL, 
             legend = auto.legend("b", panel.vars, data = data, enclos = enclos, more.args = legend.args),
             legend.args = list(),
             panel = ypanel.boxplot(), ...)
{
    panel.vars <- panel.terms(list(x = substitute(x), y = substitute(y), groups = substitute(groups)))
    if (missing(data)) data <- parent.frame()
    if (missing(enclos)) enclos <- parent.frame()
    yplot(data = data, enclos = enclos, panel.vars = panel.vars, panel = panel, legend = legend, ...)
}

barchart <- yplot_bar <- 
    function(x, y = NULL, data, enclos, groups = NULL, 
             legend = auto.legend("r", panel.vars, data = data, enclos = enclos, more.args = legend.args),
             legend.args = list(),
             panel = ypanel.barchart(), ...)
{
    panel.vars <- panel.terms(list(x = substitute(x), y = substitute(y), groups = substitute(groups)))
    if (missing(data)) data <- parent.frame()
    if (missing(enclos)) enclos <- parent.frame()
    yplot(data = data, enclos = enclos, panel.vars = panel.vars, panel = panel, legend = legend, ...)
}



## maps


## yagp.mapplot <- yplot_map <- 
##     function(x, y = NULL, data, enclos, 
##              panel = ypanel.mapplot, ...)
## {
##     panel.vars <- panel.terms(list(x = substitute(x), y = substitute(y)))
##     if (missing(data)) data <- parent.frame()
##     if (missing(enclos)) enclos <- parent.frame()
##     yplot(data = data, enclos = enclos, panel.vars = panel.vars, panel = panel, ...)
## }
