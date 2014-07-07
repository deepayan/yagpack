

imp.xaxis <-
    function(lim, tick.number = 5,
             at = NULL,
             labels = NULL,
             do.labels = TRUE,
             font = 1,
             side = c("bottom", "top"),
             ## y = switch(side, bottom = axis_vp$ylim[1], top = axis_vp$ylim[2]),
             tck = 1,
             rot = 0,
             hadj = 0.5,
             vadj = switch(side, bottom = 1, top = 0),
             ...,
             tick.length = 8,
             theme = imp.theme(),
             give.extent = FALSE,
             vp)
{
    if (is.null(at)) at <- pretty(lim, tick.number)
    if (is.null(labels)) labels <- as.character(at)
    side <- match.arg(side)
    id <- at >= lim[1] & at <= lim[2]
    at <- at[id]
    labels <- labels[id]
    if (give.extent) # FIXME: make height of rotated bounding box
        return(list(h = 1.2 * tick.length * max(tck) + max(tstrheight(labels)), w = -1))
    axis_vp <- tviewport(vp, vp$xlim[1], vp$ylim[1], diff(vp$xlim), diff(vp$ylim),
                         xlim = vp$xlim, ylim = c(0, vp$h)) # y-axis is now pixel
    switch(side,
           bottom = {
               tsegments(at, 0, at, -tick.length * tck, col = theme$axis$col, vp = axis_vp)
               ttext(at, -1.2 * tick.length * tck, labels, rot = rot, adj = c(hadj, vadj), col = theme$axis$col, vp = axis_vp)
           },
           top = {
               tsegments(at, vp$h, at, vp$h + tick.length * tck, col = theme$axis$col, vp = axis_vp)
               ttext(at, vp$h + 1.2 * tick.length * tck, labels, rot = rot, adj = c(hadj, vadj), col = theme$axis$col, vp = axis_vp)
           })
}


imp.yaxis <-
    function(lim, tick.number = 5,
             at = NULL,
             labels = NULL,
             do.labels = TRUE,
             font = 1,
             side = c("left", "right"),
             ## x = switch(side, left = vp$xlim[1], right = vp$xlim[2]),
             tck = 1,
             rot = 0,
             hadj = switch(side, left = 1, right = 0),
             vadj = 0.5,
             ...,
             tick.length = 8,
             theme = imp.theme(),
             give.extent = FALSE,
             vp)
{
    if (is.null(at)) at <- pretty(lim, tick.number)
    if (is.null(labels)) labels <- as.character(at)
    side <- match.arg(side)
    id <- at >= lim[1] & at <= lim[2]
    at <- at[id]
    labels <- labels[id]
    if (give.extent) # FIXME: make width of rotated bounding box
        return(list(h = -1, w = 1.5 * tick.length * max(tck) + max(tstrwidth(labels))))
    axis_vp <- tviewport(vp, vp$xlim[1], vp$ylim[1], diff(vp$xlim), diff(vp$ylim),
                         xlim = c(0, vp$w), ylim = vp$ylim) # x-axis is now pixel
    switch(side,
           left = {
               tsegments(0, at, -tick.length * tck, at, col = theme$axis$col, vp = axis_vp)
               ttext(-1.5 * tick.length * tck, at, labels, rot = rot, adj = c(hadj, vadj), col = theme$axis$col, vp = axis_vp)
           },
           right = {
               tsegments(vp$w, at, vp$w + tick.length * tck, at, col = theme$axis$col, vp = axis_vp)
               ttext(vp$w + 1.5 * tick.length * tck, at, labels, rot = rot, adj = c(hadj, vadj), col = theme$axis$col, vp = axis_vp)
           })
}


