

dstates <-
    cbind(as.data.frame(state.x77),
          Region = state.region,
          State = I(rownames(state.x77)),
          Area = state.area)

pstates <- 
    yplot(data = dstates,
          margin.vars = elist(Region), layout = c(2,2),
          panel.vars = elist(x = Illiteracy,
                             y = Murder,
                             size = Area),
          panel = ypanel.grid() + ypanel.xyplot())




identify_points_qtbase <-
    function(object, rscene, 
             tooltips = rownames(object$xargs$data))
{
    getvp <- function(name) object$shared.env$viewports[[name]]
    plot_vp <- getvp("plot_vp")
    plot_context <- plot_vp$context
    panel_vp <- NULL
    xp <- NULL
    yp <- NULL
    
    isInside <- function(vp, xpixel, ypixel)
    {
        ## Given viewport vp, return TRUE if (xpixel, ypixel) is inside.
        (xpixel > vp$x) && (xpixel < vp$x + vp$w) &&
            (ypixel > vp$y) && (ypixel < vp$y + vp$h)
    }

    ## foo <- function(layer, painter)
    ## {
    ##     str(as.matrix(layer$geometry))
    ## }
    
    tooltipPainter <- function(layer, painter)
    {
        ## str(as.matrix(layer$geometry))
        if (!is.null(xp) && !is.null(yp)) 
        {
            primitives <- qtpaint_primitives(layer, painter, plot_context) # or NULL for context
            a <- attr(attach(primitives), "name")
            on.exit(detach(a, character.only = TRUE))
            ## tinitialize(tget_context(), newpage = TRUE)

            invp <- sapply(object$shared.env$viewports, isInside, xp, yp)
            invp <- invp[invp]
            which_panel <- which(substring(names(invp)[invp], 1, 8) == "panel_vp")
            if (length(which_panel) == 1)
            {
                panel_vp <<- getvp(names(invp)[which_panel])
                xnative <- pixel2x(xp, panel_vp)
                ynative <- pixel2y(yp, panel_vp)
                ## find nearby point
                panel_number <- as.numeric(substring(names(invp)[which_panel], 10, 20))

                panel.id <- object$xargs$packets[[panel_number]]
                data.xp <-
                    x2pixel(evaluate(object$xargs$panel.vars$x,
                                     data = object$xargs$data, 
                                     subset = panel.id),
                            panel_vp)
                data.yp <-
                    y2pixel(evaluate(object$xargs$panel.vars$y,
                                     data = object$xargs$data, 
                                     subset = panel.id),
                            panel_vp)
                dists <- sqrt((data.xp - xp)^2 + (data.yp - yp)^2)
                dmin <- which.min(dists)
                if (dists[dmin] < 30)
                {
                    w <- panel.id[dmin]
                    ttext(xnative, ynative,
                          tooltips[w],
                          vp = panel_vp)
                }
            }
            ## tfinalize()

        }
    }
    
    tooltipHandler <- function(layer, event)
    {
        e <- parseQtEvent(event)
        xp <<- e$scenePos[1]
        yp <<- e$scenePos[2]
        qupdate(tooltipPainterLayer)
    }

    tooltipPainterLayer <- qlayer(rscene, paintFun = tooltipPainter, cache = FALSE)
    ## fooLayer <- qlayer(rscene, paintFun = foo, cache = TRUE)
    tooltipHandlerLayer <- qlayer(rscene, hoverMoveFun = tooltipHandler)

    environment()
}


identify_points_qtpaint <-
    function(object, rscene, 
             tooltips = rownames(object$xargs$data))
{
    getvp <- function(name) object$shared.env$viewports[[name]]
    plot_vp <- NULL
    plot_context <- NULL
    panel_vp <- NULL
    xp <- NULL
    yp <- NULL
    
    isInside <- function(vp, xpixel, ypixel)
    {
        ## Given viewport vp, return TRUE if (xpixel, ypixel) is inside.
        (xpixel > vp$x) && (xpixel < vp$x + vp$w) &&
            (ypixel > vp$y) && (ypixel < vp$y + vp$h)
    }

    mainplotPainter <- function(layer, painter)
    {
        primitives <- qtpaint_primitives(layer, painter, NULL) # or NULL for context
        ## a <- attr(attach(primitives), "name")
        ## on.exit(detach(a, character.only = TRUE))
        plot(object, primitives = primitives)

        plot_vp <<- getvp("plot_vp")
        plot_context <<- plot_vp$context
    }

    tooltipPainter <- function(layer, painter)
    {
        ## str(as.matrix(layer$geometry))
        if (!is.null(xp) && !is.null(yp)) 
        {
            primitives <- qtpaint_primitives(layer, painter, plot_context) # or NULL for context
            a <- attr(attach(primitives), "name")
            on.exit(detach(a, character.only = TRUE))
            ## tinitialize(tget_context(), newpage = TRUE)

            invp <- sapply(object$shared.env$viewports, isInside, xp, yp)
            invp <- invp[invp]
            which_panel <- which(substring(names(invp)[invp], 1, 8) == "panel_vp")
            if (length(which_panel) == 1)
            {
                panel_vp <<- getvp(names(invp)[which_panel])
                xnative <- pixel2x(xp, panel_vp)
                ynative <- pixel2y(yp, panel_vp)
                ## find nearby point
                panel_number <- as.numeric(substring(names(invp)[which_panel], 10, 20))

                panel.id <- object$xargs$packets[[panel_number]]
                data.xp <-
                    x2pixel(evaluate(object$xargs$panel.vars$x,
                                     data = object$xargs$data, 
                                     subset = panel.id),
                            panel_vp)
                data.yp <-
                    y2pixel(evaluate(object$xargs$panel.vars$y,
                                     data = object$xargs$data, 
                                     subset = panel.id),
                            panel_vp)
                dists <- sqrt((data.xp - xp)^2 + (data.yp - yp)^2)
                dmin <- which.min(dists)
                if (dists[dmin] < 30)
                {
                    w <- panel.id[dmin]
                    ttext(xnative, ynative,
                          tooltips[w], adj = c(-0.2, 1),
                          vp = panel_vp)
                }
            }
            ## tfinalize()

        }
    }
    
    tooltipHandler <- function(layer, event)
    {
        e <- parseQtEvent(event)
        xp <<- e$scenePos[1]
        yp <<- e$scenePos[2]
        qupdate(tooltipPainterLayer)
    }

    mainplotPainterLayer <- qlayer(rscene, mainplotPainter, cache = TRUE)
    tooltipPainterLayer <- qlayer(rscene, tooltipPainter, cache = TRUE)
    tooltipHandlerLayer <- qlayer(rscene, hoverMoveFun = tooltipHandler)

    environment()
}

scale1 <- function(x)
{
    if (is.numeric(x)) 2 * (x - min(x, na.rm=TRUE)) / diff(range(x, na.rm = TRUE)) - 1
    else x
}

rotate_cloud <- function(object, primitives = NULL)
{
    startx <- NULL
    starty <- NULL

    devset <- function()
        if (dev.cur() != eventEnv$which) dev.set(eventEnv$which)
    
    mousedown <- function(buttons, x, y)
    {
        ## cat("Down: ", buttons, x, y, "\n")
        startx <<- x
        starty <<- y
        eventEnv$onMouseMove <- dragmousemove
        NULL
    }

    mouseup <- function(buttons, x, y)
    {
        eventEnv$onMouseMove <- NULL
        NULL
    }
    
    dragmousemove <- function(buttons, x, y)
    {
        ## object$shared.env$setup$screen$x <-
        ##     object$shared.env$setup$screen$x + 50 * (y - starty) 
        ## object$shared.env$setup$screen$z <-
        ##     object$shared.env$setup$screen$z - 50 * (x - startx) 
        screen <- list(y = 50 * (x - startx), x = -50 * (y - starty))
        object$shared.env$setup$rot.mat <-
            ytransform3dMatrix(screen, R.mat = object$shared.env$setup$rot.mat)
        startx <<- x
        starty <<- y
        plot(object)
    }
    
    keydown <- function(key) {
        if (key == "q") invisible(1)
        else NULL
    }
    
    setGraphicsEventHandlers(prompt = "Click and drag, q will quit",
                             onMouseDown = mousedown,
                             onMouseMove = NULL,
                             onMouseUp = mouseup,
                             onKeybd = keydown)
    eventEnv <- getGraphicsEventEnv()
    getGraphicsEvent()
}


## Pan and zoom time series plot

pan_zoom_ts <- function(object, primitives = NULL)
{
    plot_vp <- object$shared.env$viewports[["plot_vp"]]
    panel_vp <- object$shared.env$viewports[["panel_vp_1"]]
    ## R returns coordinates as (0,1) x (0,1).  Need to convert to
    ## pixel coords, and then to panel viewport
    startx <- NULL
    starty <- NULL
    xlim <- object$xlim <- object$shared.env$limits[[1]]$xlim
    refresh <- function() 
    {
        object$xlim <- xlim
        plot(object)
        plot_vp <<- object$shared.env$viewports[["plot_vp"]]
        panel_vp <<- object$shared.env$viewports[["panel_vp_1"]]
    }

    devset <- function()
        if (dev.cur() != eventEnv$which) dev.set(eventEnv$which)
    
    mousedown <- function(buttons, x, y)
    {
        cat("Down: ", buttons, x, y, "\n")
        startx <<- pixel2x(x2pixel(x, vp = plot_vp), panel_vp)
        print(startx)
        eventEnv$onMouseMove <- dragmousemove
        NULL
    }

    mouseup <- function(buttons, x, y)
    {
        eventEnv$onMouseMove <- NULL
        NULL
    }
    
    dragmousemove <- function(buttons, x, y)
    {
        ## cat("Move: ", buttons, x, y, "\n")
        newx <- pixel2x(x2pixel(x, vp = plot_vp), panel_vp)
        xlim <<- xlim + (startx - newx)
        refresh()
        ## startx <<- newx
        ## print(startx)
        NULL
    }

    keydown <- function(key) {
        cat(key)
        if (key == "q") return(invisible(1))
        else
        {
            if (key == "a")
            {
                xlim <<- extendrange(r = xlim, f = 0.05)
                refresh()
            }
            else if (key == "z") 
            {
                xlim <<- extendrange(r = xlim, f = -0.05)
                refresh()
            }
        }
        return (NULL)
    }
    
    setGraphicsEventHandlers(prompt = "Click and drag, q will quit",
                             onMouseDown = mousedown,
                             onMouseMove = NULL,
                             onMouseUp = mouseup,
                             onKeybd = keydown)
    eventEnv <- getGraphicsEventEnv()
    getGraphicsEvent()
}

reorder_vars <- function(object, primitives = NULL,
                         dev.main, dev.feedback)
{
    if (is.null(primitives)) primitives <- .impenv$backend
    if (is.function(primitives)) primitives <- primitives()
    dev.set(dev.main)
    plot(object, primitives = primitives)
    getvp <- function(name) object$shared.env$viewports[[name]]
    plot_vp <- getvp("plot_vp")
    plot_context <- plot_vp$context
    ## R returns coordinates as (0,1) x (0,1), and so is xlim and ylim for plot_vp

    panel_vp <- NULL
    startx <- NULL
    starty <- NULL
    from_var <- NULL
    valid <- FALSE # used to record if release happens in a valid location
    nvars <- length(object$shared.env$setup$vorder)
    
    isInside <- function(vp, xpixel, ypixel)
    {
        ## Given viewport vp, return TRUE if (xpixel, ypixel) is inside.
        (xpixel > vp$x) && (xpixel < vp$x + vp$w) &&
            (ypixel > vp$y) && (ypixel < vp$y + vp$h)
    }

    make_permutation <- function(from, to, n = nvars)
    {
        ## from comes between floor(to) and ceiling(to)
        pre <- as.integer(floor(to))
        if (pre < 0) pre <- 0 else if (pre > n) pre  <- n
        if (pre >= from)
        {
            i1 <- seq_len(from-1)
            i2 <- seq_len(pre-from) + from
            i3 <- seq_len(n-pre) + pre
            c(i1, i2, from, i3)
        }
        else if (pre < from)
        {
            i1 <- seq_len(pre)
            i2 <- seq_len(from - pre - 1) + pre
            i3 <- seq_len(n - from) + from
            c(i1, from, i2, i3)
        }
    }

    ## devset <- function()
    ##     if (dev.cur() != eventEnv$which) dev.set(eventEnv$which)
    
    dragmousedown <- function(buttons, x, y)
    {
        ## cat("Down: ", buttons, x, y, "\n")
        ## devset()
        a <- attr(attach(primitives), "name")
        on.exit(detach(a, character.only = TRUE))
        tinitialize(plot_context, newpage = FALSE)
        ## Location in pixel coordinates
        xp <- x2pixel(x, vp = plot_vp)
        yp <- y2pixel(y, vp = plot_vp)
        ## check if the click is inside any panel viewport (which are
        ## names like panel_vp_n).
        invp <- sapply(object$shared.env$viewports, isInside, xp, yp)
        invp <- invp[invp]
        which_panel <- which(substring(names(invp)[invp], 1, 8) == "panel_vp")
        if (length(which_panel) > 1) stop("Bug: click cannot be in multiple panels")
        if (length(which_panel) == 1)
        {
            panel_vp <<- getvp(names(invp)[which_panel])
            startx <<- pixel2x(xp, panel_vp)
            starty <<- pixel2y(yp, panel_vp)
            from_var <<- if (object$switch.axes) round(starty) else round(startx)
            if (from_var >= 1 && from_var <= nvars)
            {
                eventEnv$onMouseMove <- dragmousemove
            }
            else 
            {
                panel_vp <<- NULL
                eventEnv$onMouseMove <- NULL
            }
        }
        else
        {
            panel_vp <<- NULL
            eventEnv$onMouseMove <- NULL
        }
        tfinalize()
        NULL
    }
    
    dragmousemove <- function(buttons, x, y) {
        ## cat("Move: ", buttons, x, y, "\n")
        a <- attr(attach(primitives), "name")
        on.exit(detach(a, character.only = TRUE))
        ##dev.set(dev.feedback)
        ##devset()
        tinitialize(plot_context, newpage = TRUE)
        xp <- x2pixel(x, vp = plot_vp)
        yp <- y2pixel(y, vp = plot_vp)
        if (isInside(panel_vp, xp, yp))
        {
            px <<- pixel2x(xp, panel_vp)
            py <<- pixel2y(yp, panel_vp)
            ## tsegments(startx, starty, px, py, vp = panel_vp)
            ttext(px, py,
                  object$shared.env$limits[[1]]$ylabels[from_var],
                  vp = panel_vp)
            startx <<- px
            starty <<- py
            valid <<- TRUE
        }
        else
            valid <<- FALSE
        tfinalize()
        ## devset()
        NULL
    }
    
    mouseup <- function(buttons, x, y) {
        ## cat("Up: ", buttons, x, y, "\n")
        ## devset()
        if (valid)
        {
            to_var <- if (object$switch.axes) starty else startx
            ## print(unname(c(from_var, to_var)))
            ## Now need to find suitable permutation.  
            newperm <- make_permutation(from_var, to_var)
            object$shared.env$setup$vorder <- object$shared.env$setup$vorder[newperm]
        }
        eventEnv$onMouseMove <- NULL
        panel_vp <<- NULL
        startx <<- NULL
        starty <<- NULL
        from_var <<- NULL
        valid <<- FALSE

        dev.set(dev.main)
        plot(object, primitives = primitives)

        dev.set(dev.feedback)
        a <- attr(attach(primitives), "name")
        on.exit(detach(a, character.only = TRUE))
        tinitialize(plot_context, newpage = TRUE)
        tfinalize()
        NULL
    }
    
    keydown <- function(key) {
        cat("Key: ", key, "\n")
        if (key == "q") {
            dev.set(dev.main)
            return(invisible(1))
        }
        eventEnv$onMouseMove <- NULL
        NULL
    }

    dev.set(dev.feedback)
    
    setGraphicsEventHandlers(prompt = "Click and drag, q will quit",
                             onMouseDown = dragmousedown,
                             onMouseMove = NULL,
                             onMouseUp = mouseup,
                             onKeybd = keydown)
    eventEnv <- getGraphicsEventEnv()
    getGraphicsEvent()
}



identify_points_graphics <-
    function(object, primitives = NULL,
             dev.main, dev.feedback,
             tooltips = rownames(object$xargs$data))
{
    if (is.null(primitives)) primitives <- .impenv$backend
    if (is.function(primitives)) primitives <- primitives()
    dev.set(dev.main)
    plot(object, primitives = primitives)
    getvp <- function(name) object$shared.env$viewports[[name]]
    plot_vp <- getvp("plot_vp")
    plot_context <- plot_vp$context
    ## R returns coordinates as (0,1) x (0,1), and so is xlim and ylim for plot_vp

    panel_vp <- NULL
    
    isInside <- function(vp, xpixel, ypixel)
    {
        ## Given viewport vp, return TRUE if (xpixel, ypixel) is inside.
        (xpixel > vp$x) && (xpixel < vp$x + vp$w) &&
            (ypixel > vp$y) && (ypixel < vp$y + vp$h)
    }

    mousedown <- function(buttons, x, y)
    {
        ## cat("Down: ", buttons, x, y, "\n")
        ## devset()
        a <- attr(attach(primitives), "name")
        on.exit(detach(a, character.only = TRUE))
        tinitialize(plot_context, newpage = TRUE)
        ## Location in pixel coordinates
        xp <- x2pixel(x, vp = plot_vp)
        yp <- y2pixel(y, vp = plot_vp)
        ## check if the click is inside any panel viewport (which are
        ## names like panel_vp_n).
        invp <- sapply(object$shared.env$viewports, isInside, xp, yp)
        invp <- invp[invp]
        which_panel <- which(substring(names(invp)[invp], 1, 8) == "panel_vp")
        if (length(which_panel) > 1) stop("Bug: click cannot be in multiple panels")
        if (length(which_panel) == 1)
        {
            panel_vp <<- getvp(names(invp)[which_panel])
            xnative <- pixel2x(xp, panel_vp)
            ynative <- pixel2y(yp, panel_vp)
            ## find nearby point
            ## print(unname(c(which_panel, xp, yp, xnative, ynative)))
            panel_number <- as.numeric(substring(names(invp)[which_panel], 10, 20))

            panel.id <- object$xargs$packets[[panel_number]]
            data.xp <-
                x2pixel(evaluate(object$xargs$panel.vars$x,
                                 data = object$xargs$data, 
                                 subset = panel.id),
                        panel_vp)
            data.yp <-
                y2pixel(evaluate(object$xargs$panel.vars$y,
                                 data = object$xargs$data, 
                                 subset = panel.id),
                        panel_vp)
            dists <- sqrt((data.xp - xp)^2 + (data.yp - yp)^2)
            dmin <- which.min(dists)
            if (dists[dmin] < 30)
            {
                w <- panel.id[dmin]
                ttext(xnative, ynative,
                      tooltips[w],
                      vp = panel_vp)
            }
        }
        tfinalize()
        NULL
    }
    
    keydown <- function(key) {
        cat("Key: ", key, "\n")
        if (key == "q") {
            a <- attr(attach(primitives), "name")
            on.exit(detach(a, character.only = TRUE))
            tinitialize(plot_context, newpage = TRUE)
            tfinalize()
            dev.set(dev.main)
            return(invisible(1))
        }
        eventEnv$onMouseMove <- NULL
        NULL
    }

    dev.set(dev.feedback)
    
    setGraphicsEventHandlers(prompt = "Click and drag, q will quit",
                             onMouseDown = NULL,
                             onMouseMove = NULL,
                             onMouseUp = mousedown,
                             onKeybd = keydown)
    eventEnv <- getGraphicsEventEnv()
    getGraphicsEvent()
}


