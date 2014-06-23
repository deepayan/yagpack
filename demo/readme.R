
### FIXME: rejig everything to have stroke and fill instead of
### colors. Explicitly reject notion of pch.  Instead, allow shapes
### which are path-based.

## Will try to keep this up-to-date.

library(RColorBrewer)
library(tessella)
## source.pkg("yagpack") # or
library(yagpack)
if (interactive()) x11() else pdf()
.yagpenv$backend <- graphics_primitives()


## We distinguish between two kinds of variables: margin
## (conditioning) variables, and panel (display) variables.  Both are
## generally specified as a list of expressions.  Such lists can be
## generated using

elist(x = a, groups = b)

## A convenience function used for margin variables is

margin.terms(~a + b)

## and another one for some common display variables is

panel.terms(list(x = b ~ a, groups = expression(g)))

## The basic high-level plot function is yplot.

p <- yplot(data = mtcars, # aspect = "xy",
           margin.vars = elist(gear = factor(gear)),
           panel.vars = elist(x = disp, y = mpg, size = wt),
           panel = ypanel.xyplot(col = "red"))

p


## We want both of the following to work

## yplot(data = mtcars,
##       margin.vars = elist(gear = factor(gear)),
##       panel.vars = elist(x = disp, y = mpg, size = wt),
##       panel = ypanel.grid(h = -1, v = -1) + ypanel.points())


## yplot(data = mtcars,
##       margin.vars = elist(gear = factor(gear)),
##       panel.vars = elist(x = disp, y = mpg, size = wt),
##       mapping = map_points,
##       panel = function(x, y, groups, ...) {
##           ypanel.grid(h = -1, v = -1)
##           ypanel.points(x, y)
##       },
##       prepanel = function(x, y, ...) default.limits(x, y))


## But what if we want points + smooth?  These are technically different operations:

## 1. data -> map_points -> plot points
## 2. data -> map_smooth -> plot lines, maybe with error bars

## We may even want a fitted model from a different data source

## One possible abstraction is to have multiple (mapping,render)
## pairs, each of which we can think of as a layer in the ggplot2
## sense.

## But how about different data source?  We assume the same
## margin.vars and panel.vars, but is that enough?  In any case, the
## 'superpanel' function has to change then.  In the most general
## case, we may want to combine two separate "yagp" objects (e.g.,
## scales lasrge enough to enclose both).  But often just use the
## first one's details, and the second one provides a layer with new
## data.

## OK, so let's make the following layers:

lpoints <- 
    ylayer(mapping = map_points(col = "red"),
           render = render_points(),
           data = NULL, margin.vars = NULL)

lsmooth <- 
    ylayer(mapping = map_lm(degree = 2),
           render = render_lines(lwd = 2),
           data = NULL, margin.vars = NULL)


(p <- yplot(data = mtcars,
            margin.vars = elist(gear = factor(gear)),
            panel.vars = elist(x = disp, y = mpg, size = wt),
            panel = lpoints + lsmooth)
 )

(p <- yplot(data = mtcars,
            margin.vars = elist(gear = factor(gear)),
            panel.vars = elist(x = disp, y = mpg, color = hp, size = wt),
            panel = lpoints,
            xlab= "disp", ylab = "mpg")
)

## ypanel.xyplot <- ylayer(mapping = map_points(), render = render_points())
## ypanel.grid <- ylayer(mapping = map_null(), render = render_grid(h = -1, v = -1))

## ypanel.xyplot() defines automatic xlab and ylab

yplot(data = mtcars,
      margin.vars = elist(gear = factor(gear)),
      relation = list(x = "free", y = "free"),
      panel.vars = elist(x = disp, y = mpg, color = hp),
      panel = ypanel.grid(v = -1) + ypanel.xyplot())

## Can be overridden by xlab, ylab

yplot(data = mtcars,
      margin.vars = elist(gear = factor(gear)),
      relation = list(x = "free", y = "free"),
      panel.vars = elist(x = disp, y = mpg, color = hp),
      panel = ypanel.grid(v = -1) + ypanel.xyplot(),
      xlab= "Displacement", ylab = "Miles per gallon")

##?? Some empty panels

xtabs(~ gear + cyl, mtcars)

(p <- 
 yplot(data = mtcars,
       margin.vars = elist(gear = factor(gear), cyl = factor(cyl)),
       panel.vars = elist(x = disp, y = mpg),
       panel = ypanel.grid() + ypanel.xyplot())
)

## FIXME: this doesn't work yet

if (FALSE)
(p <- 

 yplot(data = mtcars, #subset(mtcars, gear != 4),
      margin.vars = elist(gear = factor(gear), cyl = factor(cyl)),
      panel.vars = elist(x = disp, y = mpg),
      panel = ypanel.grid() + ypanel.xyplot(),
      relation = list(x = "free", y = "free"),
      xlab= "disp", ylab = "mpg")
)

yplot(data = mtcars,
      margin.vars = elist(cyl = factor(cyl)),
      panel.vars = elist(x = disp, y = mpg),
      panel = ypanel.grid() + ypanel.xyplot() + ypanel.loess(degree = 0))


yplot(data = mtcars,
      panel.vars = elist(x = disp, y = mpg, color = factor(gear)),
      panel = ypanel.grid() + ypanel.xyplot() + ypanel.loess(evaluation = 200),
      prepanel = function(x, y, ...) default.limits(x, y))

yplot(data = mtcars,
      panel.vars = elist(x = disp, y = mpg, groups = factor(gear), color = hp),
      panel = ypanel.grid() + ypanel.xyplot() + ypanel.loess(evaluation = 200),
      prepanel = function(x, y, ...) default.limits(x, y))

yplot(data = mtcars,
      panel.vars = elist(x = disp, y = mpg, groups = factor(gear)),
      panel = (ypanel.grid(h = -1, v = -1) +
               ylayer(mapping = map_points(mapcolor = TRUE),
                      render = render_xy(type = "S") + render_xy(type = "p"))))

yplot(data = mtcars,
      panel.vars = elist(x = disp, y = mpg, groups = factor(gear)),
      panel = ypanel.grid() + ypanel.xyplot(mapcolor = TRUE) + ypanel.lm(degree = 2),
      xlab= "disp", ylab = "mpg")

yplot(data = iris,
      panel.vars = elist(x = Petal.Length, y = Petal.Width, groups = Species),
      panel = ypanel.grid() + ypanel.xyplot(jitter.y = TRUE) + ypanel.lm(degree = 1),
      xlab= "disp", ylab = "mpg",
      prepanel = function(x, y, ...) default.limits(x, y))

yplot(data = iris,
      margin.vars = ~Species,
      panel.vars = elist(x = Petal.Length, y = Petal.Width),
      panel = ypanel.grid() + ypanel.xyplot(jitter.y = TRUE) + ypanel.lm(degree = 1),
      xlab= "disp", ylab = "mpg",
      prepanel = function(x, y, ...) default.limits(x, y))

yplot(data = iris,
      panel.vars = elist(x = Petal.Length, groups = Species),
      panel = ypanel.density())


## FIXME: add a separate background layer argument that works without
## data (grid and other reference objects)


## FIXME: should have grouping and weights in histogram

(p <- 
 yplot(data = iris,
       panel.vars = elist(x = Petal.Length, color = Species),
       panel = ypanel.histogram(fill = "seagreen", nint = 15))
)

## For common breakpoints etc., need to do some preprocessing that
## looks at whole data, not just packets (and store things in
## shared.env).  The natural place to specify this is in a layer, say,
## in a 'setup' component.

yplot(data = iris,
      margin.vars = elist(Species),
      panel.vars = elist(x = Petal.Length, color = Species),
      panel = ypanel.histogram(fill = "seagreen", nint = 15))

yplot(data = iris,
      panel.vars = elist(x = Petal.Length, groups = Species),
      panel = ypanel.histogram(col = "transparent"))

yplot(data = iris,
      panel.vars = elist(x = Petal.Length, groups = Species),
      panel = ypanel.histogram(stack = FALSE))

## FIXME: list(x=...) should work, but need to fix evaluate 

yplot(data = data.frame(x = factor(rpois(1000, lambda = 5))),
      panel.vars = elist(x = x),
      panel = ypanel.histogram(stack = FALSE))

(p <- 
 yplot(data = iris,
       margin.vars = elist(Species),
       panel.vars = elist(x = Petal.Length, groups = Species),
       panel = ypanel.qqmath())
)

yplot(data = iris,
      panel.vars = elist(x = Petal.Length, groups = Species),
      panel = ypanel.qqmath(distribution = qunif))

d <- data.frame(x = rexp(1000), g = gl(4, 250))

yplot(data = d,
      panel.vars = elist(x = x, groups = g),
      panel = ypanel.qqmath(distribution = qexp,
                            f.value = ppoints(100), tails.n = 20))


## stripchart

yplot(data = iris,
      panel.vars = elist(x = Petal.Length, y = Species),
      panel = ypanel.xyplot(jitter.y = TRUE))

yplot(data = iris,
      panel.vars = elist(x = Petal.Length, y = Species, groups = Species),
      panel = ypanel.xyplot(jitter.y = F))

## FIXME: dotplot

data(barley, package = "lattice") 

yplot(data = barley,
      margin.vars = elist(site),
      panel.vars = elist(x = yield, y = variety,
                         groups = year),
      panel = ypanel.xyplot(pch = 16),
      alternating = list(y = 1))

yplot(data = barley,
      margin.vars = elist(site),
      panel.vars = elist(x = yield, y = variety,
                         groups = year),
      panel = ypanel.xyplot(pch = 16),
      layout = c(1, 6), aspect = 0.5,
      legend = auto.legend(type = "p",
                           vars = elist(groups = year),
                           data = barley),
      alternating = list(y = 1))

yplot(data = data.frame(a = gl(5, 1), b = 1:5),
      panel.vars = elist(x = a, y = b),
      panel = ypanel.barchart(fill = "seagreen"))


yplot(data = data.frame(a = gl(5, 1), b = 1:5),
      panel.vars = elist(x = a, y = b, groups = a),
      panel = ypanel.barchart(fill = "seagreen") + ypanel.points())


yplot(data = barley,
      margin.vars = elist(site),
      panel.vars = elist(x = variety, y = yield,
                         groups = year),
      panel = ypanel.barchart())

yplot(data = barley,
      margin.vars = elist(site),
      panel.vars = elist(x = variety, y = yield,
                         groups = year),
      panel = ypanel.barchart(stack = FALSE))

lbar <- ypanel.barchart(stack = FALSE)
lbar[[1]]$render <- yrender(function(x = 0.5 * (xleft + xright),
                                     y = ytop,
                                     xleft, xright, ybottom, ytop,
                                     col = fill, fill = "transparent",
                                     ...)
                            ypoints(x, y, col = col, ...))

yplot(data = barley,
      margin.vars = elist(site),
      panel.vars = elist(x = variety, y = yield,
                         groups = year),
      panel = lbar)


yplot(data = barley,
      margin.vars = elist(site),
      panel.vars = elist(x = variety, y = yield,
                         groups = year),
      panel = ypanel.barchart(stack = FALSE, area = TRUE))


data(Oxboys, package = "mlmRev") 

## FIXME: Need a better way to do this 
l <- ypanel.xyplot()
l[[1]]$render <- render_xy(type = "l")

yplot(data = Oxboys,
      margin.vars = elist(Subject),
      panel.vars = elist(x = age, y = height),
      panel = l)

## won't work
yplot(data = Oxboys,
      panel.vars = elist(x = age, y = height, color = Subject),
      panel = l)

## should group, but same color
yplot(data = Oxboys,
      panel.vars = elist(x = age, y = height, groups = Subject, mapcolor = FALSE),
      panel = l)

## Both should group with different colors (lines)
yplot(data = Oxboys,
      panel.vars = elist(x = age, y = height, groups = Subject, mapcolor = TRUE),
      panel = l)

yplot(data = Oxboys,
      panel.vars = elist(x = age, y = height, groups = Subject, color = Subject),
      panel = l)


## Both should group with different colors (points)
yplot(data = Oxboys,
      panel.vars = elist(x = age, y = height, groups = Subject, 
                         mapcolor = TRUE, size = age),
      panel = ypanel.xyplot())

yplot(data = Oxboys,
      panel.vars = elist(x = age, y = height, groups = Subject,
                         size = age, color = Subject),
      panel = ypanel.xyplot())


## To avoid branching code as much as possible, we would like some
## generic way to switch the roles of x-and y-axes.  We don't want to
## rotate the plots, just switch the coordinate mapping.  Here are
## some examples:

## densityplots/histograms are oriented to have data on x-axis by convention.

## loess/lm smooths usually have predicted variable on y-axis.

## barcharts/boxplots can have factor variable on both sides.  But it
## would be simpler if we could code for one, and rely on switching for other.

## This would mean the `intuitive' specification of 'x' and 'y' for
## boxplots, barcharts, etc. will not work.  'x' would always be the
## factor variable, and 'y' the `response'.  Switching would have to
## be done using the switch flag.

## To work, this switching will have to be honored by both the mapping
## (but only for limits computations) and rendering.  The limit
## elements are fairly standard, so shouldn't be too problematic.  But
## the different rendering functions will need different types of
## rearrangements.  Where do we specify this?  Maybe an attribute of
## the render function.  Are we always calling the render function
## using do.call?  Then it would be easiest to rename the argument
## list.

## Note that this does not solve everything.  If we want a scatterplot
## with 2 regression lines - y on x and x on y, this won't help.


yplot(data = as.data.frame.table(Titanic),
      margin.vars = elist(Sex, Age),
      panel.vars = elist(x = Class, y = Freq, groups = Survived),
      panel = ypanel.barchart(stack = TRUE))


yplot(data = as.data.frame.table(Titanic),
      margin.vars = elist(Sex, Age),
      panel.vars = elist(x = Class, y = Freq, groups = Survived),
      panel = ypanel.barchart(stack = TRUE),
      switch.axes = TRUE)

yplot(data = iris,
      panel.vars = elist(x = Petal.Length, groups = Species),
      panel = ypanel.histogram(col = "transparent"),
      relation = list(y = "free"),
      switch.axes = T)


data(singer, package = "lattice")

yplot(data = singer,
      panel.vars = elist(x = factor(voice.part), y = height),
      panel = ypanel.grid(v = -1, h = 0) + ypanel.boxplot(),
      switch.axes = TRUE)

yplot(data = singer,
      panel.vars = elist(x = factor(voice.part), y = height),
      panel = ypanel.grid(v = -1, h = 0) + ypanel.boxplot(),
      relation = list(y = "free"),
      switch.axes = TRUE)

singer$v2 <-
    factor(sapply(strsplit(as.character(singer$voice.part), " "), "[[", 1))

yplot(data = singer,
      panel.vars = elist(x = factor(voice.part),
                         y = height, groups = v2),
      panel = ypanel.grid(v = -1, h = 0) + ypanel.boxplot(),
      switch.axes = TRUE)

yplot(data = singer,
      panel.vars = elist(x = v2,
                         y = height, groups = voice.part),
      panel = ypanel.grid(v = -1, h = 0) + ypanel.boxplot(),
      switch.axes = TRUE)


library("maps")

state.info <-
    data.frame(name = state.name,
               long = state.center$x,
               lat = state.center$y,
               area = state.x77[, "Area"],
               population = 1000 * state.x77[, "Population"])

state.info$density <- with(state.info, population / area)
state.info$name <- tolower(state.info$name)

state.map <- map("state", plot = FALSE, fill = TRUE)

yplot(data = subset(state.info, !(name %in% c("alaska", "hawaii"))),
      panel.vars = elist(x = log(density), y = name),
      map = state.map,
      aspect = "iso",
      panel = ypanel.map())

yplot(data = iris,
      panel.vars = elist(x = cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
                         color = NULL),
      panel = ypanel.parallel())


scale01 <- function(x, jitter.data = FALSE)
{
    if (is.numeric(x))
        (if (jitter.data) jitter else I)((x - min(x, na.rm = TRUE)) /
                                         diff(range(x, na.rm = TRUE, finite = TRUE)))
    else x
}

## FIXME: shouldn't need extra data.frame wrapper
yplot(data = do.call(data.frame, lapply(iris, scale01, jitter = TRUE)),
      panel.vars = elist(x = cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
                         color = Species),
      panel = ypanel.parallel(),
      switch.axes = TRUE)



1



## Still left: levelplots, contourplots.



## What's the idea of themes?  A default theme can be saved in
## .yagpenv$theme.  yagp.theme() will return this if set.  We don't
## want to pass around the theme everywhere as it will be difficult to
## keep track.  So everyone should take their defaults from
## yagp.theme().

## To allow per-plot customization, yplot() has a theme argument. This
## will be temporarily made the default in plot.yagp.


