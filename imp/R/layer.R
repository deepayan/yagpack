
## A "yrender" object is a list of rendering functions that all work
## with the same input data.  The list has class "yrender" and such
## objects can be combined using "+.yrender".

yrender <- function(f, switch_names = NULL)
{
    structure(list(f = structure(f, switch_names = switch_names)),
              class = "yrender")
}

"+.yrender" <- function(e1, e2)
{
    structure(c(e1, e2), class = "yrender")
}

## A 'layer' consists of a mapping function, and a 'yrender' object.
## A "ylayer" object is a list of one or more such layers, that can be
## combined using "+.ylayer".

## As a convenience, a "yplot" plot object and a layer can also be `added'.

ylayer <- function(setup = NULL, mapping, render, data = NULL, margin.vars = NULL, panel.vars = NULL, enclos = NULL)
{
    packets  <-
        if (is.null(data)) NULL
        else compute.packets(margin.vars,
                             data = data,
                             enclos = enclos)
    structure(list(list(setup = setup, mapping = mapping, render = render,
                        panel.vars = panel.vars, data = data, packets = packets)),
              class = "ylayer")
}

"+.ylayer" <- function(e1, e2)
{
    ## FIXME: doesn't check if both are yplot objects
    if (inherits(e1, "yplot")) 
    {
        e1$xargs$panel <- e1$xargs$panel + e2
        e1
    }
    else if (inherits(e2, "yplot")) 
    {
        e2$xargs$panel <- e1 + e2$xargs$panel
        e2
    }
    else structure(c(e1, e2), class = "ylayer")
}



## lattice-like 'panel functions' that actually produce layers

## Common data-driven layers


ypanel.lines <- function(jitter.x = FALSE, jitter.y = FALSE, ...)
{
    ylayer(mapping = map_points(jitter.x = jitter.x, jitter.y = jitter.y, ...),
           render = render_lines())
}

ypanel.points <- function(jitter.x = FALSE, jitter.y = FALSE, ..., render = render_points())
{
    ylayer(mapping = map_points(jitter.x = jitter.x, jitter.y = jitter.y, ...),
           render = render)
}

ypanel.xyplot <- function(jitter.x = FALSE, jitter.y = FALSE, ..., render = render_xy())
{
    xy_setup <- function(panel.vars, data, enclos, shared.env, ...)
    {
        if (!is.null(panel.vars$x)) shared.env$setup$xlab <- as.character(panel.vars$x)
        if (!is.null(panel.vars$y)) shared.env$setup$ylab <- as.character(panel.vars$y)
    }
    ylayer(setup = xy_setup,
           mapping = map_points(jitter.x = jitter.x, jitter.y = jitter.y, ...),
           render = render)
}


ypanel.loess <- function(..., render = render_lines())
{
    ylayer(mapping = map_loess(...), render = render)
}

ypanel.lm <- function(..., render = render_lines())
{
    ylayer(mapping = map_lm(...), render = render)
}

ypanel.qqmath <- function(..., render = render_points())
{
    qq_setup <- function(panel.vars, data, enclos, shared.env, ...)
    {
        if (!is.null(panel.vars$x)) shared.env$setup$ylab <- as.character(panel.vars$x)
        shared.env$setup$xlab <- "theoretical quantiles"
    }
    ylayer(mapping = map_qqmath(...), render = render)
}

ypanel.density <- function(..., render = render_lines())
{
    dens_setup <- function(panel.vars, data, enclos, shared.env, ...)
    {
        if (!is.null(panel.vars$x)) shared.env$setup$xlab <- as.character(panel.vars$x)
        shared.env$setup$ylab <- "density"
    }
    ylayer(setup = dens_setup, mapping = map_density(...), render = render)
}

ypanel.histogram <- function(..., render = render_rect())
{
    hist_setup <- function(panel.vars, data, enclos, shared.env,
                           breaks = NULL, equal.widths = TRUE,
                           nint = round(log2(length(x)) + 1),
                           type = "density", ...)
    {
        if (!is.null(panel.vars$x)) shared.env$setup$xlab <- as.character(panel.vars$x)
        shared.env$setup$ylab <- type
        packet_data <- lapply(panel.vars, evaluate, data = data, enclos = enclos)
        x <- packet_data$x
        if (missing(breaks)) # explicit NULL, or function, or character is fine
            breaks <-
                default_hist_breaks(x, equal.widths = equal.widths, nint = nint)
        shared.env$setup$breaks <- breaks
    }
    ## ylayer(setup = changeDefaults(hist_setup, ...),
    ##        mapping = map_histogram(...), render = render_rect())
    ylayer(setup = changeDefaults(hist_setup, ...),
           mapping = map_histogram(...), render = render)
}

ypanel.barchart <- function(..., render = render_rect())
{
    bar_setup <- function(panel.vars, data, enclos, shared.env, ...)
    {
        if (!is.null(panel.vars$y)) shared.env$setup$ylab <- as.character(panel.vars$y)
    }
    ylayer(setup = bar_setup, mapping = map_table(...), render = render)
}

ypanel.boxplot <- function(..., render = render_boxplot())
{
    box_setup <- function(panel.vars, data, enclos, shared.env, ...)
    {
        if (!is.null(panel.vars$y)) shared.env$setup$ylab <- as.character(panel.vars$y)
    }
    ylayer(setup = box_setup,
           mapping = map_boxplot(...), render = render)
}

## 'reference' layers, usually not data driven.


ypanel.grid <- function(..., mapping = map_null(), data = NULL)
{
    ylayer(mapping = map_null(), render = render_grid(...), data = data)
}

ypanel.abline <- function(..., mapping = map_null(), data = NULL)
{
    ylayer(mapping = map_null(), render = render_abline(...), data = data)
}

## ypanel.refline <- function(horizontal = TRUE, ...) # for dotplot
## {
##     if (horizontal)
##         yabline(h = unique(as.numeric(y)), col = col.ref, ...)
##     else 
##         yabline(v = unique(as.numeric(x)), col = col.ref, ...)
##     ylayer(mapping = map_points(), render = render_abline(...), data = NULL)
## }


ypanel.map <- function(..., render = render_polygon())
{
    ylayer(mapping = map_map(...),
           render = render)
}

ypanel.parallel <- function(..., render = render_segments())
{
    parallel_setup <- function(panel.vars, data, enclos, shared.env, ...)
    {
        packet_data <- lapply(panel.vars, evaluate, data = data, enclos = enclos)
        shared.env$setup$vorder <- seq_len(ncol(packet_data$x))
    }
    ylayer(setup = parallel_setup,
           mapping = map_parallel(...),
           render = render)
}

ypanel.projection <- function(..., render = render_xy())
{
    projection_setup <- function(panel.vars, data, enclos, shared.env, ...)
    {
        packet_data <- lapply(panel.vars, evaluate, data = data, enclos = enclos)
        nvars <- ncol(packet_data$x)
        p1 <- rnorm(nvars)
        p2 <- rnorm(nvars)
        shared.env$setup$xproj <- p1 / sqrt(sum(p1*p1))
        shared.env$setup$yproj <- p2 / sqrt(sum(p2*p2))
    }
    ylayer(setup = projection_setup,
           mapping = map_projection(...),
           render = render)
}

ypanel.projection.axes <- function(..., render = render_segments())
{
    ylayer(mapping = map_projection_axes(...),
           render = render)
}


ypanel.cloud <- function(screen = list(z = 40, x = -60, y = 0), ..., render = render_xy())
{
    cloud_setup <- function(shared.env, ...)
    {
        ## shared.env$setup$screen <- screen
        shared.env$setup$rot.mat <- ytransform3dMatrix(screen = screen)
    }
    ylayer(setup = cloud_setup,
           mapping = map_cloud(...),
           render = render)
}


ypanel.cloud.axes <- function(..., render = render_segments() + render_text())
{
    ylayer(mapping = map_cloud_axes(...),
           render = render)
}


