
yagp.key <-
    function(text,
             type = c("p", "l", "b", "n"), # points/lines/both/none
             bg = "transparent", border = "transparent", # for rectangles
             title = NULL,
             lwd = 1, lty = 1,
             col = "black", col.symbol = col, col.line = col, pch = 1, cex = 1,
             theme = yagp.theme(),
             give.extent = FALSE,
             vp)
{
    type <- match.arg(type)
    width <- max(tstrwidth(text)) + 50
    height <- 1.5 * length(text) * max(tstrheight(text))
    if (!is.null(title)) 
    {
        width <- max(width, tstrwidth(title))
        height <- height + tstrheight(title)
    }
    if (give.extent)
    {
        return(list(w = width, h = height))
    }
    full_layout <- # figure layout
        tlayout(widths = -1,
                heights = c(-1, height, -1),
                parent = vp)
    ## make viewport with native=pixel coordinates
    middle_vp <- tviewport(full_layout, 1, 2, 
                           ## vp$xlim[1], vp$ylim[1], diff(vp$xlim), diff(vp$ylim),
                           xlim = c(0, vp$w)) # ylimits = [0, 1]
    ## tessella.fill(fill = "yellow", col = "transparent", vp = middle_vp)
    n <- length(text)
    N <- n + !is.null(title) # FIXME: allow title to be bigger etc
    YY <- (1:N)/N - 0.5 / N
    yy <- rev(head(YY, n)) ## without title, if any
    ttext(50, yy, text, adj = c(0, 0.5), vp = middle_vp)
    trect(rep(0, n), yy - 0.5/N, rep(50, n), yy + 0.5/N,
          col = border, fill = bg, vp = middle_vp)
    switch(type,
           l = {
               tsegments(rep(10, n), yy, rep(40, n), yy,
                         col = col.line, lwd = lwd, lty = lty, vp = middle_vp)
           },
           p = {
               tpoints(rep(25, n), yy, col = col.symbol, pch = pch, cex = cex, vp = middle_vp)
           },
           b = {
               tsegments(rep(10, n), yy, rep(40, n), yy,
                         col = col.line, lwd = lwd, lty = lty,
                         vp = middle_vp)
               tpoints(rep(25, n), yy, col = col.symbol, pch = pch, cex = cex, vp = middle_vp)
           })
    if (!is.null(title))
        ttext(mean(middle_vp$xlim), YY[N], title, font = 2, vp = middle_vp)
}


auto.legend <- function(type = c("p", "l", "b", "r"),
                        vars = panel.vars, data, enclos = .GlobalEnv, ...,
                        theme = yagp.theme(),
                        more.args = list(), space = "top")
{
    ## type="r" means rectangles.  TODO|FIXME: All this needs to be refined.
    groups <- evaluate(vars$groups, data = data, enclos = enclos)
    if (is.null(groups)) NULL
    else
    {
        largs <-
            if (type == "r")
                list(text = levels(as.factor(groups)),
                     type = "n",
                     bg = theme$polygon$fill, border = theme$polygon$col,
                     theme = theme)
            else
                list(text = levels(as.factor(groups)),
                     type = type,
                     lwd = theme$default$lwd, lty = theme$default$lty,
                     col = theme$default$col, pch = theme$default$pch, cex = theme$default$cex, 
                     theme = theme)
        modifyList(largs, more.args)
    }
}


