

library(RColorBrewer)
library(tessella)
library(yagpack)
## source.pkg("yagpack")
## if (interactive()) x11(type = "Xlib") else pdf()

library(qtutils)
QT()

.yagpenv$backend <- graphics_primitives


## There doesn't seem to be any obvious way to design a cross-backend
## interaction API.  So, let's try to do some use-cases with
## backend-specific code, and then rethink the possibility.  This file
## tries with the graphics backend.


## Use-cases:

## 0. Highlight selected rows (modifying selection comes later)

## 1. Roerder variables in parallel coordinate plot

## 2. Multivariable plot - like a tour, supporting animation.  Once
## setup, we can use it to animate from one pair of variables to
## another pair (like Gapminder).  One question is: how to choose?

## 3. Pan and zoom. Special time series plot?


### Implementation

## General setup:

## Some setup done by setup components of layers.  One special type of
## setup always done is axis limits, etc.

## For interaction, p$handlers can be list of "handler sets".  Each
## can be initiated by calling them (p$handlers$reorder(),
## p$handlers[[1]], etc.)

## 0. Highlight selected rows (modifying selection comes later)

NULL

## 1. Roerder variables in parallel coordinate plot

p <- 
    yplot(data = iris,
          ## margin.vars = elist(Species),
          panel.vars = elist(x = cbind(Sepal.Length, Sepal.Width,
                                       Petal.Length, Petal.Width),
                             color = Species),
          panel = ypanel.parallel(),
          switch.axes = TRUE)

## p$shared.env$setup$vorder <- sample(p$shared.env$setup$vorder); p



reorder_vars <- function(object, primitives = NULL)
{
    if (is.null(primitives)) primitives <- .yagpenv$backend
    if (is.function(primitives)) primitives <- primitives()
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

    devset <- function()
        if (dev.cur() != eventEnv$which) dev.set(eventEnv$which)
    
    dragmousedown <- function(buttons, x, y)
    {
        ## cat("Down: ", buttons, x, y, "\n")
        devset()
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
        devset()
        tinitialize(plot_context, newpage = FALSE)
        xp <- x2pixel(x, vp = plot_vp)
        yp <- y2pixel(y, vp = plot_vp)
        if (isInside(panel_vp, xp, yp))
        {
            px <<- pixel2x(xp, panel_vp)
            py <<- pixel2y(yp, panel_vp)
            tsegments(startx, starty, px, py, vp = panel_vp) 
            startx <<- px
            starty <<- py
            valid <<- TRUE
        }
        else
            valid <<- FALSE
        tfinalize()
        NULL
    }
    
    mouseup <- function(buttons, x, y) {
        ## cat("Up: ", buttons, x, y, "\n")
        devset()
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
        plot(object, primitives = primitives)
        NULL
    }
    
    keydown <- function(key) {
        cat("Key: ", key, "\n")
        if (key == "q") return(invisible(1))
        eventEnv$onMouseMove <- NULL
        NULL
    }
    
    setGraphicsEventHandlers(prompt = "Click and drag, q will quit",
                             onMouseDown = dragmousedown,
                             onMouseMove = NULL,
                             onMouseUp = mouseup,
                             onKeybd = keydown)
    eventEnv <- getGraphicsEventEnv()
    getGraphicsEvent()
}

reorder_vars(p)


## 2. Multivariable plot - like a tour, supporting animation.  Once
## setup, we can use it to animate from one pair of variables to
## another pair (like Gapminder).  One question is: how to choose?


p <- 
    yplot(data = iris,
          ## margin.vars = elist(Species),
          panel.vars = elist(x = cbind(Sepal.Length, Sepal.Width,
                                       Petal.Length, Petal.Width),
                             groups = Species),
          panel = ypanel.projection())

norm1 <- function(x) x / sqrt(sum(x * x))
p$shared.env$setup$xproj <- norm1(runif(4))
p$shared.env$setup$yproj <- norm1(runif(4))
p


scale1 <- function(x)
{
    if (is.numeric(x)) 2 * (x - min(x, na.rm=TRUE)) / diff(range(x, na.rm = TRUE)) - 1
    else x
}

p <- 
    yplot(data = do.call(data.frame, lapply(iris, scale1)),
          ## margin.vars = elist(Species),
          panel.vars = elist(x = cbind(Sepal.Length, Sepal.Width,
                                       Petal.Length, Petal.Width),
                             groups = Species),
          panel = ypanel.projection())


make_tour <- function(n, steps = 50, xfrom = NULL, yfrom = NULL)
{
    ## n = number of variables
    ## 'xfrom' and 'yfrom' are initial projections.  We pick a random new
    ## direction and go there in 'steps' increments.
    norm1 <- function(x) x / sqrt(sum(x * x))
    xfrom <- if (is.null(xfrom)) norm1(rnorm(n)) else norm1(xfrom)
    yfrom <- if (is.null(yfrom)) norm1(rnorm(n)) else norm1(yfrom)
    counter <- -1
    xto <- norm1(rnorm(n))
    yto <- norm1(rnorm(n))
    function()
    {
        counter <<- counter + 1
        if (counter == steps)
        {
            counter <<- 0
            xfrom <<- xto
            yfrom <<- yto
            xto <<- norm1(rnorm(n))
            yto <<- norm1(rnorm(n))
        }
        w <- counter / (steps-1)
        xproj <- norm1((1-w) * xfrom + w * xto)
        yproj <- norm1((1-w) * yfrom + w * yto)
        list(xproj = xproj, yproj = yproj)
    }
}

tgrand <- make_tour(length(p$shared.env$setup$xproj))

while (TRUE)
{
    nproj <- tgrand()
    p$shared.env$setup$xproj <- nproj$xproj
    p$shared.env$setup$yproj <- nproj$yproj
    dev.hold()
    plot(p)
    dev.flush()
}


p <- 
    yplot(data = mtcars,
          panel.vars = elist(x = data.matrix(mtcars)),
          panel = ypanel.projection())


xmat <- matrix(rnorm(500), 100, 5)

p <- 
    yplot(data = mtcars,
          panel.vars = elist(x = xmat),
          panel = ypanel.projection() + ypanel.projection.axes(),
          xlim = c(-3, 3), ylim = c(-3, 3))




grand_tour <- function(object, primitives = NULL)
{
    tgrand <- make_tour(length(object$shared.env$setup$xproj))

    devset <- function()
        if (dev.cur() != eventEnv$which) dev.set(eventEnv$which)
    
    mousemove <- function(buttons, x, y) {
        devset()
        nproj <- tgrand()
        object$shared.env$setup$xproj <- nproj$xproj
        object$shared.env$setup$yproj <- nproj$yproj
        plot(object)
        NULL
    }
    
    keydown <- function(key) {
        cat("Key: ", key, "\n")
        if (key == "q")
        {
            eventEnv$onMouseMove <- NULL
            return(invisible(1))
        }        
        NULL
    }
    
    setGraphicsEventHandlers(prompt = "Click and drag, q will quit",
                             onMouseDown = NULL,
                             onMouseMove = mousemove,
                             onMouseUp = NULL,
                             onKeybd = keydown)
    eventEnv <- getGraphicsEventEnv()
    getGraphicsEvent()
}

## FIXME: buggy with qtutils (no mouse effect after using keyboard) 

grand_tour(yplot(data = do.call(data.frame, lapply(iris, scale1)),
                 ## margin.vars = elist(Species),
                 panel.vars = elist(x = cbind(Sepal.Length, Sepal.Width,
                                              Petal.Length, Petal.Width),
                                    groups = Species),
                 panel = ypanel.projection() + ypanel.projection.axes()))


grand_tour(yplot(data = do.call(data.frame, lapply(quakes, scale1)),
                 ## margin.vars = elist(Species),
                 panel.vars = elist(x = cbind(long, lat, -depth), size = mag),
                 panel = ypanel.projection.axes() + ypanel.projection()))



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


p <- 
    yplot(data = do.call(data.frame, lapply(quakes, scale1)),
          ## margin.vars = elist(Species),
          panel.vars = elist(x = long, y = lat, z = -depth, size = mag),
          panel = ypanel.cloud.axes() + ypanel.cloud(screen = list()),
          distance = 0.2,
          xlim = c(-0.5, 0.5), ylim = c(-0.5, 0.5))


rotate_cloud(p)


## vanilla animation

while (TRUE)
{
    p$shared.env$setup$rot.mat <-
            ytransform3dMatrix(list(y = 17/10, x = 19/10), R.mat = p$shared.env$setup$rot.mat)
    plot(p)
}





## 3. Pan and zoom. Special time series plot?


pan_zoom_ts <- function(object, primitives = NULL)
{
    plot_vp <- object$shared.env$viewports[["plot_vp"]]
    panel_vp <- object$shared.env$viewports[["panel_vp_1"]]
    ## R returns coordinates as (0,1) x (0,1).  Need to convert to
    ## pixel coords, and then to panel viewport
    startx <- NULL
    starty <- NULL
    xlim <- object$xlim
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


p <- 
    yplot(data = NULL,
          panel.vars = elist(x = time(sunspot.year), y = sunspot.year),
          panel = ypanel.lines(), xlim = range(time(sunspot.year)),
          ylab = "Number of sunspots")

p
pan_zoom_ts(p)



