
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
    class(x) <- c("yagp", "ylayer")
    x
}

## plot method


##' Plot a yagp object
##'
##' .. content for \details{} ..
##' @title 
##' @param x An object of class \code{"yagp"}.
##' @param page A vector specifying which page(s) to draw, for
##' multi-page plots.
##' @param primitives An environment containing the primitives to use,
##' or a function that returns such an environment.  By default,
##' \code{.yagpenv$backend} is used.
##' @param ... Further arguments, passed on to \code{\link{yagp_page}}.
##' @param new Logical flag to control whether the plot should start a new page.
##' @param draw_labels Logical flag to control whether labels are to be drawn.
##' @param draw_panels Logical flag to control whether panels are to be drawn.
##' @param draw_strips Logical flag to control whether strips are to be drawn.
##' @param draw_axes Logical flag to control whether axes are to be drawn.
##' @param position A list specifying the position of a viewport
##' inside the main figure region (root viewport) where the object is
##' to be drawn.  The viewport is most generally specified in terms of
##' the location of the top-left corner (\code{x}, \code{y}) and width
##' (\code{w}) and height (\code{h}) assuming a coordinate system with
##' range [0, 1] on both axes.  Alternatively, \code{split} specified
##' in the form \code{c(row, column, nrow, ncol)} can be used to
##' specify the position as a cell in a  matrix.
##' @return NULL
##' @author Deepayan Sarkar
plot.yagp <-
    function(x, page = 1,
             primitives = NULL,
             ...,
             new = TRUE,
             draw_labels = TRUE,
             draw_panels = TRUE,
             draw_strips = TRUE,
             draw_axes = TRUE,
             position = list(x = 0, y = 0, h = 1, w = 1, split = NULL))
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
    computePosition <- function(x = 0, y = 0, h = 1, w = 1, split = NULL)
    {
        if (is.null(split)) {
            list(x = x, y = y, w = w, h = h)
        }
        else {
            ## split = (i, j, m, n) : put in (i,j) cell of mxn matrix
            if (length(split) != 4) stop("'split' must have the form c(row, column, nrow, ncol).")
            if (split[1] < 1 || split[1] > split[3]) stop("Invalid row position in 'split'.")
            if (split[2] < 1 || split[2] > split[4]) stop("Invalid column position in 'split'.")
            list(x = (split[2] - 1) / split[4],
                 y = (split[1] - 1) / split[3],
                 w = 1 / split[4], h = 1 / split[3])
        }
    }
    position <- do.call(computePosition, position)
    for (p in page)
    {
        yagp_page(x, page = p, ..., new = new,
                  draw_labels = draw_labels,
                  draw_panels = draw_panels,
                  draw_strips = draw_strips,
                  draw_axes = draw_axes,
                  vp = tviewport(tviewport(context),
                                 x = position$x, y = position$y,
                                 w = position$w, h = position$h))
    }
    invisible()
}




print.yagp <- function(x, ...)
{
    plot(x, ...)
    invisible(x)
}


summary.yagp <-
    function(object, ...)
{
    ## FIXME: This sort of assumes a data.frame (at least nrow())
    ans <- 
        with(object$xargs,
             if (identical(packets, array(list(TRUE), dim = c(1)))) nrow(data)
             else structure(sapply(packets, length),
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
             legend = auto.legend("p", panel.vars, data = data, enclos = enclos, more.args = legend.args),
             legend.args = list(),
             panel = ypanel.xyplot(), 
             ...)
{
    panel.vars <- panel.terms(list(x = substitute(x), y = substitute(y), groups = substitute(groups), color = substitute(color), size = substitute(size)))
    if (missing(data)) data <- parent.frame()
    if (missing(enclos)) enclos <- parent.frame()
    yplot(data = data, enclos = enclos, panel.vars = panel.vars, panel = panel, legend = legend, ...)
}

densityplot <- yplot_density <- 
    function(x, data, enclos, weights = NULL, groups = NULL, 
             legend = auto.legend("l", panel.vars, data = data, enclos = enclos, more.args = legend.args),
             legend.args = list(),
             panel = ypanel.density(), ...)
{
    panel.vars <- panel.terms(list(x = substitute(x), weights = substitute(weights), groups = substitute(groups)))
    if (missing(data)) data <- parent.frame()
    if (missing(enclos)) enclos <- parent.frame()
    yplot(data = data, enclos = enclos, panel.vars = panel.vars, panel = panel, legend = legend, ...)
}

qqmath <- yplot_qqmath <- 
    function(x, data, enclos, groups = NULL, 
             legend = auto.legend("p", panel.vars, data = data, enclos = enclos, more.args = legend.args),
             legend.args = list(),
             panel = ypanel.qqmath(), ...)
{
    panel.vars <- panel.terms(list(x = substitute(x), groups = substitute(groups)))
    if (missing(data)) data <- parent.frame()
    if (missing(enclos)) enclos <- parent.frame()
    yplot(data = data, enclos = enclos, panel.vars = panel.vars, panel = panel, legend = legend, ...)
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
