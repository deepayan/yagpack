
## give.extent = TRUE means required space (in pixels) should be returned (no actual drawing)


yagp.strip.horizontal <-
    function(which.packet, 
             packets,
             which.margins = seq_along(dim(packets)),
             shared.env, 
             ...,
             theme = yagp.theme(),
             col = theme$strip$col,
             fill = theme$strip$fill,
             give.extent = FALSE,
             vp)
{
    which.margins <- seq_along(dim(packets))
    nmargin <- length(which.margins)
    col <- rep(col, length = nmargin)
    fill <- rep(fill, length = nmargin)
    margin.combs <- do.call(expand.grid, dimnames(packets))
    labs <-
        sapply(which.margins,
               function(w) {
                   as.character(margin.combs[which.packet, w])
               })
    strip_label <- paste(labs, collapse = "\n")
    if (give.extent) return(list(w = -1, h = tstrheight(strip_label)))
    tclip(vp)
    on.exit(tunclip(vp))
    tessella.fill(col = col, fill = fill, vp = vp)
    ttext(mean(vp$xlim), mean(vp$ylim), strip_label, vp = vp)
}




yagp_page_compute_viewports <- function(x, page = 1, vp = NULL, ...)
{
    ## Compute all viewports in x$sharev.env for later access

    viewports <- list()
    storevp <- function(vp, name = deparse(substitute(vp)),
                        warn.conflict = TRUE)
    {
        if (name %in% names(viewports))
            warning("viewport ", name, " already exists")
        viewports[[name]] <<- vp
    }
    if (is.null(vp))
    {
        context <- tget_context()
        vp <- tviewport(context)
    }
    else context <- vp$context

    ## tinitialize(context, newpage = new)
    ## on.exit(tfinalize())

    storevp(vp, name = "plot_vp")
    
    panel.layout <- x$panel.layout[,, page, drop = FALSE] # array of packets
    dim(panel.layout) <- dim(panel.layout)[1:2] # matrix for current page
    ldim <- dim(panel.layout)
    packets <- x$xargs$packets
    panel.vars <- x$xargs$panel.vars
    panel <- x$xargs$panel
    if (!is.list(panel)) panel <- list(panel)
    limits <- x$shared.env$limits

    panel.args <- x$xargs[setdiff(names(x$xargs), c("panel"))]

    ## panel.args <- ## everything in x except those that are not
    ##     x[setdiff(names(x), c("panel.layout", "panel"))]

    ## c(panel.layout, packets, panel.vars, panel, data, enclos,
    ##   shared.env, relation, alternating, xlab, ylab, main, sub)

    ## whether conditioning variables are present
    show.strips <- !is.null(dimnames(packets))
    
    ## The "plot layout" consists of main,sub,xlab,ylab, legends,
    ## (common) axes, and figure area (containing panels etc, see
    ## below). Also legend

    sheight <- function(s) if (is.null(s)) 0 else unname(tstrheight(s))

    plot_layout.x <- c(margin.left = 5,
                       legend.left = 0,
                       ylab = sheight(x$ylab),
                       axis.left = 0,
                       figure = -1,
                       axis.right = 0,
                       legend.right = 0,
                       margin.right = 5)
    plot_layout.y <- c(margin.top = 5,
                       main = sheight(x$main),
                       legend.top = 0,
                       axis.top = 0,
                       figure = -1,
                       axis.bottom = 0,
                       xlab = sheight(x$xlab),
                       legend.bottom = 0,
                       sub = sheight(x$sub),
                       margin.bottom = 5)
    ## Helper function to find indices by name
    findx <- function(s) which(names(plot_layout.x) == s)
    findy <- function(s) which(names(plot_layout.y) == s)

    if (!is.null(x$legend))
    {
        plot_layout.x[findx("legend.right")] <- 
            do.call(yagp.key,
                    c(x$legend, list(give.extent = TRUE)))$w
    }

    ## Space needed for axis labeling.

    ## This is a bit complicated, because there are several
    ## possibilities.  The simplest is that each panel will have its
    ## own axis (relation="free").  Even then, questions are: (1)
    ## Whether we allow axes only on left/bottom (as lattice does) or
    ## also (possibly different) on right/top. (2) Whether all
    ## rows/columns should use the same space (enough for all labels)
    ## or different (just enough for that particular row/column).  In
    ## both we will for the moment follow lattice.  This means we need
    ## to find out the space needed for each (xy)axis, and take the
    ## maximum as the space for each axis row/column.

    ## The other case is where we have common axes on the boundary.
    ## Here an additional complication is that depending on the value
    ## of alternating, one or both boundaries will be used.  To keep
    ## the logic simple, we will for now compute each axis with and
    ## without labels and then choose the appropriate one later.

    ## In either case, we loop through panels and compute the space
    ## required.  WARNING/FIXME: The current system assumes that
    ## ticks/labels will be the same for top/bottom and left/right.

    x.withlabel <- y.withlabel <- x.nolabel <- y.nolabel <- rep(0, length(panel.layout))
    for (p in seq_len(length(panel.layout)))
    {
        i <- panel.layout[p]
        if (i > 0)
        {
            ## X-axis
            xargs <-
                list(lim = limits[[i]]$xlim, 
                     at = limits[[i]]$xat,
                     labels = limits[[i]]$xlabels, 
                     side = "bottom", 
                     give.extent = TRUE)
            yargs <-
                list(lim = limits[[i]]$ylim, 
                     at = limits[[i]]$yat, labels = limits[[i]]$ylabels, 
                     side = "left",
                     give.extent = TRUE)
            ## FIXME: allow custom functions
            x.withlabel[p] <- do.call(yagp.xaxis, c(xargs, x$scales$x, list(do.labels = TRUE)))$h
            x.nolabel[p] <- do.call(yagp.xaxis, c(xargs, x$scales$x, list(do.labels = FALSE)))$h
            y.withlabel[p] <- do.call(yagp.yaxis, c(yargs, x$scales$y, list(do.labels = TRUE)))$w
            y.nolabel[p] <- do.call(yagp.yaxis, c(yargs, x$scales$y, list(do.labels = FALSE)))$w
        }
    }
    x.withlabel <- 5 + max(x.withlabel)
    y.withlabel <- 5 + max(y.withlabel)
    x.nolabel <- 5 + max(x.nolabel)
    y.nolabel <- 5 + max(x.nolabel)

    if (x$relation$x == "same")
    {
        altx <- rep(x$alternating$x, length.out = nrow(panel.layout))
        plot_layout.y[findy("axis.bottom")] <- 
            if (any(altx %in% c(1, 3))) x.withlabel else x.nolabel
        plot_layout.y[findy("axis.top")] <- 
            if (any(altx %in% c(2, 3))) x.withlabel else x.nolabel
    }
    if (x$relation$y == "same")
    {
        alty <- rep(x$alternating$y, length.out = ncol(panel.layout))
        plot_layout.x[findx("axis.left")] <-
            if (any(alty %in% c(1, 3))) y.withlabel else y.nolabel
        plot_layout.x[findx("axis.right")] <- 
            if (any(alty %in% c(2, 3))) y.withlabel else y.nolabel
    }

    plot_layout <- tlayout(widths = plot_layout.x, heights = plot_layout.y, parent = vp)
    storevp(tviewport(plot_layout, seq_along(plot_layout.x), findy("main")), "main_vp")
    storevp(tviewport(plot_layout, seq_along(plot_layout.x), findy("sub")), "sub_vp")
    storevp(tviewport(plot_layout, findx("figure"), findy("xlab")), "xlab_vp")
    storevp(tviewport(plot_layout, findx("ylab"), findy("figure")), "ylab_vp")

    ## Figure area.  We don't know here how many row/columns we will
    ## need in the layout, but we assume that it won't more than 10
    ## per panel.

    figure_vp <- tviewport(plot_layout, findx("figure"), findy("figure"))
    storevp(figure_vp)
    
    ## Initialize everything to 0, and modify as needed.
    figure_layout.x <- rep(0, 10L * ldim[1])
    figure_layout.y <- rep(0, 10L * ldim[2])

    ## First figure out the layout for panels, strips, and per-panel axes.
    for (p in seq_len(length(panel.layout)))
    {
        i <- panel.layout[p]
        if (i > 0)
        {
            ## rows/columns in plot are columns/rows in panel.layout
            pos <- compute.position(row(panel.layout)[p], col(panel.layout)[p], what = "panel")
            figure_layout.x[pos["column"]] <- -1
            figure_layout.y[pos["row"]] <- if (is.null(x$aspect)) -1 else -x$aspect
            if (show.strips)
            {
                pos <- compute.position(row(panel.layout)[p], col(panel.layout)[p], what = "strip.top")
                figure_layout.y[pos["row"]] <-
                    max(figure_layout.y[pos["row"]],
                        1.2 * yagp.strip.horizontal(which.packet = i,
                                                    packets = packets,
                                                    give.extent = TRUE)$h)
            }
            if (x$relation$x != "same")
            {
                pos <- compute.position(row(panel.layout)[p], col(panel.layout)[p], what = "xaxis.bottom")
                figure_layout.y[pos["row"]] <- x.withlabel
            }
            if (x$relation$y != "same")
            {
                pos <- compute.position(row(panel.layout)[p], col(panel.layout)[p], what = "yaxis.left")
                figure_layout.x[pos["column"]] <- y.withlabel
            }
        }
    }

    storevp(tviewport(plot_layout, findx("legend.right"), findy("figure")), "legend_vp")

    figure_layout <- # figure layout
        tlayout(widths = figure_layout.x, # rep(-1, dim(panel.layout)[1]),
                heights = figure_layout.y, # rep(-1, dim(panel.layout)[2]),
                parent = figure_vp,
                respect.aspect = !is.null(x$aspect))

    ## Now create and store the corresponding viewports
    for (p in seq_len(length(panel.layout)))
    {
        i <- panel.layout[p]
        if (i > 0)
        {
            fcolumn <- col(panel.layout)[p]
            frow <- row(panel.layout)[p]
            ## rows/columns in plot are columns/rows in panel.layout
            pos <- compute.position(frow, fcolumn, what = "panel")
            panel_vp <-
                tviewport(figure_layout, pos["column"], pos["row"],
                          xlim = limits[[i]]$xlim,
                          ylim = limits[[i]]$ylim)
            storevp(panel_vp, sprintf("panel_vp_%g", p))
            pos <- compute.position(frow, fcolumn, what = "strip.top")
            if (show.strips)
            {
                strip_vp <-
                    tviewport(figure_layout, pos["column"], pos["row"],
                              xlim = limits[[i]]$xlim)
            }
            else strip_vp <- panel_vp # for axes on top
            storevp(strip_vp, sprintf("strip_vp_%g", p))
        }
    }
    viewports
}


yagp_page_render_labels <- function(x, page = 1, viewports = x$shared.env$viewports, ...)
{
    getvp <- function(name) viewports[[name]]
    if (!is.null(x$main)) ttext(0.5, 0.5, x$main, vp = getvp("main_vp"))
    if (!is.null(x$sub)) ttext(0.5, 0.5, x$sub, vp = getvp("sub_vp"))
    if (!is.null(x$xlab)) ttext(0.5, 0.5, x$xlab, vp = getvp("xlab_vp"))
    if (!is.null(x$ylab)) ttext(0.5, 0.5, x$ylab, vp = getvp("ylab_vp"), rot = 90)
    if (!is.null(x$legend)) do.call(yagp.key, c(x$legend, list(vp = getvp("legend_vp"))))
    invisible()
}


yagp_page_render_panels <- function(x, page = 1, viewports = x$shared.env$viewports, ..., panel = x$xargs$panel)
{
    getvp <- function(name) viewports[[name]]
    panel.layout <- x$panel.layout[,, page, drop = FALSE] # array of packets
    dim(panel.layout) <- dim(panel.layout)[1:2] # matrix for current page
    ldim <- dim(panel.layout)
    packets <- x$xargs$packets
    panel.vars <- x$xargs$panel.vars
    if (!is.list(panel)) panel <- list(panel)
    limits <- x$shared.env$limits
    panel.args <- x$xargs
    ## panel.args <- ## everything in x except those that are for something else. FIXME: review
    ##     x[setdiff(names(x), c("panel.layout", "panel", "main", "sub", "xlab", "ylab", "relation", "alternating"))]

    ## whether conditioning variables are present
    show.strips <- !is.null(dimnames(packets))
    
    for (p in seq_len(length(panel.layout)))
    {
        i <- panel.layout[p]
        if (i > 0)
        {
            fcolumn <- col(panel.layout)[p]
            frow <- row(panel.layout)[p]
            panel_vp <- getvp(sprintf("panel_vp_%g", p))
            tessella.fill(col = x$theme$axis$col, fill = x$theme$background$col,  vp = panel_vp)
            tclip(panel_vp)

            if (inherits(panel, "ylayer"))
                lapply(panel, function(layer)
                   {
                       ## 'layer' can have its own 'data' and
                       ## 'packets'.  In that case, we should use
                       ## them, but make sure they are compatible.
                       ## FIXME: Should allow for empty data (e.g., for reference grids), but not sure how.
                       pargs <- panel.args # copy for this layer, different only if 'data' or 'panel.vars' changes
                       if (!is.null(layer$data))
                       {
                           stopifnot(identical(dim(layer$packets), dim(panel.args$packets)))
                           pargs$data <- layer$data
                           pargs$packets <- layer$packets
                       }
                       if (!is.null(layer$panel.vars))
                       {
                           pargs$panel.vars <- layer$panel.vars
                       }
                       do.call(ypanel.super,
                               c(list(mapping = layer$mapping, render = layer$render),
                                 pargs,
                                 list(which.packet = i, vp = panel_vp)))
                   })

            tunclip(panel_vp)
        }
    }
    invisible()
}


yagp_page_render_strips <- function(x, page = 1, viewports = x$shared.env$viewports, ...)
{
    getvp <- function(name) viewports[[name]]
    panel.layout <- x$panel.layout[,, page, drop = FALSE] # array of packets
    packets <- x$xargs$packets
    ## whether conditioning variables are present
    show.strips <- !is.null(dimnames(packets))
    if (!show.strips) return(invisible())    
    for (p in seq_len(length(panel.layout)))
    {
        i <- panel.layout[p]
        if (i > 0)
        {
            strip_vp <- getvp(sprintf("strip_vp_%g", p))
            yagp.strip.horizontal(which.packet = i,
                                  packets = packets,
                                  shared.env = x$shared.env,
                                  theme = x$theme,
                                  vp = strip_vp)
        }
    }
    invisible()
}


yagp_page_render_axes <- function(x, page = 1, viewports = x$shared.env$viewports, ...)
{
    getvp <- function(name) viewports[[name]]
    panel.layout <- x$panel.layout[,, page, drop = FALSE] # array of packets
    dim(panel.layout) <- dim(panel.layout)[1:2] # matrix for current page
    ldim <- dim(panel.layout)
    packets <- x$xargs$packets
    panel.vars <- x$xargs$panel.vars
    panel <- x$xargs$panel
    if (!is.list(panel)) panel <- list(panel)
    limits <- x$xargs$shared.env$limits
    panel.args <- x$xargs[setdiff(names(x$xargs), c("panel"))]
    ## panel.args <- ## everything in x except those that are not
    ##     x[setdiff(names(x), c("panel.layout", "panel"))]

    ## whether conditioning variables are present
    show.strips <- !is.null(dimnames(packets))
    
    for (p in seq_len(length(panel.layout)))
    {
        i <- panel.layout[p]
        if (i > 0)
        {
            fcolumn <- col(panel.layout)[p]
            frow <- row(panel.layout)[p]
            panel_vp <- getvp(sprintf("panel_vp_%g", p))
            strip_vp <- getvp(sprintf("strip_vp_%g", p))

            ## IF PER-PANEL AXIS NEEDED 

            xargs <-
                list(lim = limits[[i]]$xlim, at = limits[[i]]$xat, labels = limits[[i]]$xlabels, theme = x$theme)
            yargs <-
                list(lim = limits[[i]]$ylim, at = limits[[i]]$yat, labels = limits[[i]]$ylabels, theme = x$theme)

            if (x$relation$x == "same")
            {
                if (fcolumn == 1 && rep(x$alternating$x, length = frow)[frow] %in% c(2, 3))
                    do.call(yagp.xaxis, c(xargs, x$scales$x, list(side = "top", vp = strip_vp)))
                if (fcolumn == ldim[2] && rep(x$alternating$x, length = frow)[frow] %in% c(1, 3))
                    do.call(yagp.xaxis, c(xargs, x$scales$x, list(side = "bottom", vp = panel_vp)))
            }
            else
            {
                do.call(yagp.xaxis, c(xargs, x$scales$x, list(side = "bottom", vp = panel_vp)))
            }
            if (x$relation$y == "same")
            {
                if (frow == 1 && rep(x$alternating$y, length = fcolumn)[fcolumn] %in% c(1, 3))
                    do.call(yagp.yaxis, c(yargs, x$scales$y, list(side = "left", vp = panel_vp)))
                if (frow == ldim[1] && rep(x$alternating$y, length = fcolumn)[fcolumn] %in% c(2, 3))
                    do.call(yagp.yaxis, c(yargs, x$scales$y, list(side = "right", vp = panel_vp)))
            }
            else
            {
                do.call(yagp.yaxis, c(yargs, x$scales$y, list(side = "left", vp = panel_vp)))
            }
        }
    }
    invisible()
}


yagp_page <- function(x, page = 1, vp = NULL, ...,
                      draw_labels = TRUE,
                      draw_panels = TRUE,
                      draw_strips = TRUE,
                      draw_axes = TRUE,
                      new = TRUE)
{
    ## Compute all viewports in x$sharev.env for later access
    
    if (is.null(vp))
    {
        context <- tget_context()
        vp <- tviewport(context)
    }
    else context <- vp$context
    tinitialize(context, newpage = new)
    on.exit(tfinalize())

    x$shared.env$viewports <- yagp_page_compute_viewports(x, page = page, vp = vp, ...)
    if (draw_labels) yagp_page_render_labels(x, page = page, ...) # includes legend
    if (draw_panels) yagp_page_render_panels(x, page = page, ...)
    if (draw_strips) yagp_page_render_strips(x, page = page, ...)
    if (draw_axes) yagp_page_render_axes(x, page = page, ...)
}



