
## NOT READY

library(imp.lattice)
.impenv$backend <- graphics_primitives()

.impenv$theme <- imp.theme("lattice")

## code from lattice book

## Examples from code/Chapter01.R
data(Chem97, package = "mlmRev")
xtabs( ~ score, data = Chem97)

histogram(~ gcsescore | factor(score), data = Chem97, type = "percent")

densityplot(~ gcsescore | factor(score), data = Chem97,
            ref = TRUE, plot.points = FALSE)

densityplot(~ gcsescore, data = Chem97, groups = score,
            plot.points = FALSE, ref = TRUE)
            ## auto.key = list(columns = 3))

tp1 <- histogram(~ gcsescore | factor(score), data = Chem97)

tp2 <- 
    densityplot(~ gcsescore, data = Chem97, groups = score,
                plot.points = FALSE,
                auto.key = list(space = "right", title = "score"))

class(tp2)
summary(tp1)
plot(tp1, position = list(split = c(1, 1, 1, 2)))
plot(tp2, position = list(split = c(1, 2, 1, 2)), new = FALSE)

## Examples from code/Chapter02.R
data(Oats, package = "MEMSS")
tp1.oats <- 
    xyplot(yield ~ nitro | Variety + Block, data = Oats, type = 'o')
print(tp1.oats)
dim(tp1.oats)
dimnames(tp1.oats)
xtabs(~Variety + Block, data = Oats)
summary(tp1.oats)
summary(tp1.oats[, 1])
print(tp1.oats[, 1])
update(tp1.oats, aspect="xy")
update(tp1.oats, aspect = "xy",
       layout = c(0, 18))
update(tp1.oats, aspect = "xy", layout = c(0, 18), 
       between = list(x = c(0, 0, 0.5), y = 0.5)) # no between yet
dotplot(variety ~ yield | site, data = barley, 
        layout = c(1, 6), aspect = c(0.7),
        groups = year) ## , auto.key = list(space = 'right'))
## key.variety <- 
##     list(space = "right", text = list(levels(Oats$Variety)),
##          points = list(pch = 1:3, col = "black"))
## xyplot(yield ~ nitro | Block, Oats, aspect = "xy", type = "o", 
##        groups = Variety, key = key.variety, lty = 1, pch = 1:3, 
##        col.line = "darkgrey", col.symbol = "black",
##        xlab = "Nitrogen concentration (cwt/acre)",
##        ylab = "Yield (bushels/acre)", 
##        main = "Yield of three varieties of oats",
##        sub = "A 3 x 4 split plot experiment with 6 blocks")

barchart(Freq ~ Class | Sex + Age, data = as.data.frame(Titanic),
         groups = Survived, stack = TRUE, layout = c(4, 1),
         legend.args = list(title = "Survived"),
         switch.axes = TRUE)

barchart(Freq ~ Class | Sex + Age, data = as.data.frame(Titanic), 
         groups = Survived, stack = TRUE, layout = c(4, 1), 
         legend.args = list(title = "Survived"),
         relation = list(x = "free"),
         switch.axes = TRUE)

bc.titanic <- 
    barchart(Freq ~ Class | Sex + Age, data = as.data.frame(Titanic), 
             groups = Survived, stack = TRUE, layout = c(4, 1), 
             legend.args = list(title = "Survived"),
             relation = list(x = "free"),
             switch.axes = TRUE)

ypanel.grid(h = 0, v = -1) + bc.titanic

## update(bc.titanic, 
##        panel = function(...) {
##            panel.grid(h = 0, v = -1)
##            panel.barchart(...)
##        })

## update(bc.titanic, 
##        panel = function(..., border) {
##            panel.barchart(..., border = "transparent")
##        })


## Examples from code/Chapter03.R
densityplot(~ eruptions, data = faithful)

densityplot(~ eruptions, data = faithful, 
            kernel = "rect", bw = 0.2, plot.points = "rug", n = 200)

## library("latticeExtra")

data(gvhd10, package = "latticeExtra")

densityplot(~log(FSC.H) | Days, data = gvhd10, 
            plot.points = FALSE, ref = TRUE, layout = c(2, 4))

histogram(~log2(FSC.H) | Days, gvhd10, xlab = "log Forward Scatter",
          type = "density", nint = 50, layout = c(2, 4))

histogram(~log2(FSC.H) | Days, gvhd10, xlab = "log Forward Scatter",
          type = "density", nint = 50, layout = c(2, 4),
          fill = "yellow", col = "goldenrod") + ypanel.density()

data(Chem97, package = "mlmRev")

qqmath(~ gcsescore | factor(score), data = Chem97, 
       f.value = ppoints(100))

qqmath(~ gcsescore | gender, Chem97, groups = score, aspect = "xy", 
       f.value = ppoints(100),
       ## legend.args = list(space = "right"),
       xlab = "Standard Normal Quantiles", 
       ylab = "Average GCSE Score")

Chem97.mod <- transform(Chem97, gcsescore.trans = gcsescore^2.34)

qqmath(~ gcsescore.trans | gender, Chem97.mod, groups = score,
       f.value = ppoints(100), aspect = "xy",
       ## legend.args = list(space = "right", title = "score"), 
       legend.args = list(title = "score"), 
       xlab = "Standard Normal Quantiles", 
       ylab = "Transformed GCSE Score")

## library("latticeExtra")

## ecdfplot(~ gcsescore | factor(score), data = Chem97, 
##          groups = gender, auto.key = list(columns = 2),
##          subset = gcsescore > 0, xlab = "Average GCSE Score")

## qqmath(~ gcsescore | factor(score), data = Chem97, groups = gender, 
##        ## auto.key = list(points = FALSE, lines = TRUE, columns = 2),
##        subset = gcsescore > 0, type = "l", distribution = qunif, 
##        ## prepanel = prepanel.qqmathline,
##        aspect = "xy",
##        xlab = "Standard Normal Quantiles", 
##        ylab = "Average GCSE Score")

## qq(gender ~ gcsescore | factor(score), Chem97, 
##    f.value = ppoints(100), aspect = 1)

## FIXME: wrong y-axis label. Need to generally fix labels when
## switch.axes = TRUE

bwplot(gcsescore ~ factor(score) | gender, data = Chem97, 
       xlab = "Average GCSE Score", switch.axes = TRUE)

bwplot(gcsescore^2.34 ~ gender | factor(score), data = Chem97, 
       varwidth = TRUE, layout = c(6, 1),
       ylab = "Transformed GCSE score")

bwplot(log(FSC.H) ~ Days, data = gvhd10, 
       xlab = "log(Forward Scatter)", ylab = "Days Past Transplant",
       switch.axes = TRUE)

## FIXME: write ypanel.violin()

## bwplot(log(FSC.H) ~ Days, gvhd10, 
##        panel = ypanel.violin, box.ratio = 3,
##        xlab = "log(Forward Scatter)", 
##        ylab = "Days Past Transplant",
##        switch.axes = TRUE)

stripplot(factor(mag) ~ depth, data = quakes)

stripplot(depth ~ factor(mag), data = quakes, 
          jitter.x = TRUE, alpha = 0.6,
          xlab = "Magnitude (Richter)", ylab = "Depth (km)")

stripplot(sqrt(abs(residuals(lm(yield~variety+year+site)))) ~ site, 
          data = barley, groups = year, jitter.x = TRUE,
          ## auto.key = list(points = TRUE, lines = TRUE, columns = 2),
          ## type = c("p", "a"), fun = median,
          ylab = expression(abs("Residual Barley Yield")^{1 / 2}))

## Examples from code/Chapter04.R
VADeaths
class(VADeaths)
## methods("dotplot")
## dotplot(VADeaths, groups = FALSE)
## dotplot(VADeaths, groups = FALSE, 
##         layout = c(1, 4), aspect = 0.7, 
##         origin = 0, type = c("p", "h"),
##         main = "Death Rates in Virginia - 1940", 
##         xlab = "Rate (per 1000)")
## dotplot(VADeaths, type = "o",
##         auto.key = list(lines = TRUE, space = "right"),
##         main = "Death Rates in Virginia - 1940",
##         xlab = "Rate (per 1000)")
## barchart(VADeaths, groups = FALSE,
##          layout = c(1, 4), aspect = 0.7, reference = FALSE, 
##          main = "Death Rates in Virginia - 1940",
##          xlab = "Rate (per 100)")
## data(postdoc, package = "latticeExtra")
## barchart(prop.table(postdoc, margin = 1), xlab = "Proportion",
##          auto.key = list(adj = 1))
## dotplot(prop.table(postdoc, margin = 1), groups = FALSE, 
##         xlab = "Proportion",
##         par.strip.text = list(abbreviate = TRUE, minlength = 10))
## dotplot(prop.table(postdoc, margin = 1), groups = FALSE, 
##         index.cond = function(x, y) median(x),
##         xlab = "Proportion", layout = c(1, 5), aspect = 0.6,
##         scales = list(y = list(relation = "free", rot = 0)),
##         prepanel = function(x, y) {
##             list(ylim = levels(reorder(y, x)))
##         },
##         panel = function(x, y, ...) {
##             panel.dotplot(x, reorder(y, x), ...)
##         })

data(Chem97, package = "mlmRev")
gcsescore.tab <- xtabs(~gcsescore + gender, Chem97)
gcsescore.df <- as.data.frame(gcsescore.tab)
gcsescore.df$gcsescore <- 
    as.numeric(as.character(gcsescore.df$gcsescore))
xyplot(Freq ~ gcsescore | gender, data = gcsescore.df, 
       type = "h", layout = c(1, 2), xlab = "Average GCSE Score")
score.tab <- xtabs(~score + gender, Chem97)
score.df <- as.data.frame(score.tab)
barchart(Freq ~ score | gender, data = score.df, origin = 0)

## Examples from code/Chapter05.R

xyplot(lat ~ long | cut(depth, 2), data = quakes)
## xyplot(lat ~ long | cut(depth, 3), data = quakes, 
##        aspect = "iso", pch = ".", cex = 2, type = c("p", "g"),
##        xlab = "Longitude", ylab = "Latitude", 
##        strip = strip.custom(strip.names = TRUE, var.name = "Depth"))

xyplot(lat ~ long, data = quakes, aspect = "iso",
       groups = cut(depth, breaks = quantile(depth, ppoints(4, 1))), 
       auto.key = list(columns = 3, title = "Depth"), 
       xlab = "Longitude", ylab = "Latitude")



### REST WILL NEED MORE WORK 

############################################################

## depth.col <- gray.colors(100)[cut(quakes$depth, 100, label = FALSE)]
## depth.ord <- rev(order(quakes$depth))

## xyplot(lat ~ long, data = quakes[depth.ord, ], 
##        aspect = "iso", type = c("p", "g"),
##        pch = 21, fill = depth.col[depth.ord], cex = 2,
##        xlab = "Longitude", ylab = "Latitude")

## quakes$Magnitude <- equal.count(quakes$mag, 4)
## summary(quakes$Magnitude)
## quakes$color <- depth.col
## quakes.ordered <- quakes[depth.ord, ]
## xyplot(lat ~ long | Magnitude, data = quakes.ordered,
##        aspect = "iso", fill.color = quakes.ordered$color, cex = 2,
##        panel = function(x, y, fill.color, ..., subscripts) {
##            fill <- fill.color[subscripts]
##            panel.grid(h = -1, v = -1)
##            panel.xyplot(x, y, pch = 21, fill = fill, ...)
##        },
##        xlab = "Longitude", ylab = "Latitude")
## depth.breaks <- do.breaks(range(quakes.ordered$depth), 50)
## quakes.ordered$color <- 
##     level.colors(quakes.ordered$depth, at = depth.breaks, 
##                  col.regions = gray.colors)
## xyplot(lat ~ long | Magnitude, data = quakes.ordered, 
##        aspect = "iso", groups = color, cex = 2,
##        panel = function(x, y, groups, ..., subscripts) {
##            fill <- groups[subscripts]
##            panel.grid(h = -1, v = -1)
##            panel.xyplot(x, y, pch = 21, fill = fill, ...)
##        },
##        legend = 
##        list(right = 
##             list(fun = draw.colorkey,
##                  args = list(key = list(col = gray.colors,
##                                         at = depth.breaks), 
##                              draw = FALSE))),
##        xlab = "Longitude", ylab = "Latitude")

## types.plain <- c("p", "l", "o", "r", "g", "s", "S", "h", "a", "smooth")
## types.horiz <- c("s", "S", "h", "a", "smooth")
## horiz <- rep(c(FALSE, TRUE), c(length(types.plain), length(types.horiz)))
## types <- c(types.plain, types.horiz)
## set.seed(2007041)
## x <- sample(seq(-10, 10, length = 15), 30, TRUE)
## y <- x + 0.25 * (x + 1)^2 + rnorm(length(x), sd = 5)
## xyplot(y ~ x | gl(1, length(types)),
##        xlab = "type", 
##        ylab = list(c("horizontal=TRUE", "horizontal=FALSE"), y = c(1/6, 4/6)),
##        as.table = TRUE, layout = c(5, 3),
##        between = list(y = c(0, 1)),
##        strip = function(...) {
##            require(grid)
##            panel.fill(trellis.par.get("strip.background")$col[1])
##            type <- types[panel.number()]
##            grid.text(lab = sprintf('"%s"', type), 
##                      x = 0.5, y = 0.5)
##            grid.rect()
##        },
##        scales = list(alternating = c(0, 2), tck = c(0, 0.7), draw = FALSE),
##        par.settings = 
##        list(layout.widths = list(strip.left = c(1, 0, 0, 0, 0))),
##        panel = function(...) {
##            type <- types[panel.number()]
##            horizontal <- horiz[panel.number()]
##            panel.xyplot(..., 
##                         type = type,
##                         horizontal = horizontal)
##        })[rep(1, length(types))]
## data(Earthquake, package = "MEMSS")
## xyplot(accel ~ distance, data = Earthquake,
##        panel = function(...) {
##            panel.grid(h = -1, v = -1)
##            panel.xyplot(...)
##            panel.loess(...)
##        }, 
##        xlab = "Distance From Epicenter (km)",
##        ylab = "Maximum Horizontal Acceleration (g)")
## xyplot(accel ~ distance, data = Earthquake,
##        type = c("g", "p", "smooth"),
##        scales = list(log = 2),
##        xlab = "Distance From Epicenter (km)",
##        ylab = "Maximum Horizontal Acceleration (g)")
## library("locfit")
## Earthquake$Magnitude <- 
##     equal.count(Earthquake$Richter, 3, overlap = 0.1)
## coef <- coef(lm(log2(accel) ~ log2(distance), data = Earthquake))
## xyplot(accel ~ distance | Magnitude, data = Earthquake,
##        scales = list(log = 2), col.line = "grey", lwd = 2,
##        panel = function(...) {
##            panel.abline(reg = coef)
##            panel.locfit(...)
##        },
##        xlab = "Distance From Epicenter (km)",
##        ylab = "Maximum Horizontal Acceleration (g)")
## data(SeatacWeather, package = "latticeExtra")
## xyplot(min.temp + max.temp + precip ~ day | month,
##        ylab = "Temperature and Rainfall", 
##        data = SeatacWeather, type = "l", lty = 1, col = "black")
## maxp <- max(SeatacWeather$precip, na.rm = TRUE)
## xyplot(min.temp + max.temp + I(80 * precip / maxp) ~ day | month,
##        data = SeatacWeather, lty = 1, col = "black",
##        ylab = "Temperature and Rainfall", 
##        type = c("l", "l", "h"), distribute.type = TRUE)
## update(trellis.last.object(),
##        ylab = "Temperature (Fahrenheit) \n and Rainfall (inches)",
##        panel = function(...) {
##            panel.xyplot(...)
##            if (panel.number() == 2) {
##                at <- pretty(c(0, maxp))
##                panel.axis("right", half = FALSE,
##                           at = at * 80 / maxp, labels = at)
##            }
##        })
## library("hexbin")
## data(gvhd10, package = "latticeExtra")
## xyplot(asinh(SSC.H) ~ asinh(FL2.H) | Days, gvhd10, aspect = 1, 
##        panel = panel.hexbinplot, .aspect.ratio = 1, trans = sqrt)
## splom(USArrests)
## splom(~USArrests[c(3, 1, 2, 4)] | state.region, 
##       pscales = 0, type = c("g", "p", "smooth"))
## splom(~data.frame(mpg, disp, hp, drat, wt, qsec),
##       data = mtcars, groups = cyl, pscales = 0, 
##       varnames = c("Miles\nper\ngallon", "Displacement\n(cu. in.)",
##                    "Gross\nhorsepower", "Rear\naxle\nratio", 
##                    "Weight", "1/4 mile\ntime"),
##       auto.key = list(columns = 3, title = "Number of Cylinders"))
## parallel(~mtcars[c(1, 3, 4, 5, 6, 7)] | factor(cyl), 
##          mtcars, groups = carb, 
##          key = simpleKey(levels(factor(mtcars$carb)), points = FALSE, 
##                          lines = TRUE, space = "top", columns = 3), 
##          layout = c(3, 1))
## data(gvhd10, package = "latticeExtra")
## parallel(~ asinh(gvhd10[c(3, 2, 4, 1, 5)]), data = gvhd10, 
##          subset = Days == "13", alpha = 0.01, lty = 1)


## ## Examples from code/Chapter06.R
## quakes$Magnitude <- equal.count(quakes$mag, 4)
## cloud(depth ~ lat * long | Magnitude, data = quakes, 
##       zlim = rev(range(quakes$depth)),
##       screen = list(z = 105, x = -70), panel.aspect = 0.75,
##       xlab = "Longitude", ylab = "Latitude", zlab = "Depth")
## cloud(depth ~ lat * long | Magnitude, data = quakes, 
##       zlim = rev(range(quakes$depth)), panel.aspect = 0.75,
##       screen = list(z = 80, x = -70), zoom = 0.7,
##       scales = list(z = list(arrows = FALSE, distance = 2)), 
##       xlab = "Longitude", ylab = "Latitude", 
##       zlab = list("Depth\n(km)", rot = 90))
## p <-
##     cloud(depth ~ long + lat, quakes, zlim = c(690, 30),
##           pch = ".", cex = 1.5, zoom = 1,
##           xlab = NULL, ylab = NULL, zlab = NULL,
##           par.settings = list(axis.line = list(col = "transparent")),
##           scales = list(draw = FALSE))
## npanel <- 4
## rotz <- seq(-30, 30, length = npanel)
## roty <- c(3, 0)
## update(p[rep(1, 2 * npanel)], 
##        layout = c(2, npanel),
##        panel = function(..., screen) {
##            crow <- current.row()
##            ccol <- current.column()
##            panel.cloud(..., screen = list(z = rotz[crow], 
##                                           x = -60,
##                                           y = roty[ccol]))
##        })
## state.info <-
##     data.frame(name = state.name,
##                long = state.center$x,
##                lat = state.center$y,
##                area = state.x77[, "Area"],
##                population = 1000 * state.x77[, "Population"])
## state.info$density <- with(state.info, population / area)
## cloud(density ~ long + lat, state.info,
##       subset = !(name %in% c("Alaska", "Hawaii")),
##       type = "h", lwd = 2, zlim = c(0, max(state.info$density)),
##       scales = list(arrows = FALSE))
## library("maps")
## state.map <- map("state", plot=FALSE, fill = FALSE)
## panel.3dmap <- function(..., rot.mat, distance, xlim, ylim, zlim,
##                         xlim.scaled, ylim.scaled, zlim.scaled)
## {
##     scaled.val <- function(x, original, scaled) {
##         scaled[1] + (x - original[1]) * diff(scaled) / diff(original)
##     }
##     m <- ltransform3dto3d(rbind(scaled.val(state.map$x, xlim, xlim.scaled),
##                                 scaled.val(state.map$y, ylim, ylim.scaled),
##                                 zlim.scaled[1]), 
##                           rot.mat, distance)
##     panel.lines(m[1,], m[2,], col = "grey76")
## }
## cloud(density ~ long + lat, state.info,
##       subset = !(name %in% c("Alaska", "Hawaii")),
##       panel.3d.cloud = function(...) {
##           panel.3dmap(...)
##           panel.3dscatter(...)
##       }, 
##       type = "h", scales = list(draw = FALSE), zoom = 1.1,
##       xlim = state.map$range[1:2], ylim = state.map$range[3:4],
##       xlab = NULL, ylab = NULL, zlab = NULL,
##       aspect = c(diff(state.map$range[3:4]) / diff(state.map$range[1:2]), 0.3),
##       panel.aspect = 0.75, lwd = 2, screen = list(z = 30, x = -60),
##       par.settings = list(axis.line = list(col = "transparent"),
##                           box.3d = list(col = "transparent", alpha = 0)))
## data(Cars93, package = "MASS")
## cor.Cars93 <- 
##     cor(Cars93[, !sapply(Cars93, is.factor)], use = "pair")
## data(Chem97, package = "mlmRev")
## Chem97$gcd <-
##     with(Chem97, 
##          cut(gcsescore,
##              breaks = quantile(gcsescore, ppoints(11, a = 1))))
## ChemTab <- xtabs(~ score + gcd + gender, Chem97)
## ChemTabDf <- as.data.frame.table(ChemTab)
## env <- environmental
## env$ozone <- env$ozone^(1/3)
## env$Radiation <- equal.count(env$radiation, 4)
## cloud(ozone ~ wind + temperature | Radiation, env)
## splom(env[1:4])
## fm1.env <- lm(ozone ~ radiation * temperature * wind, env)
## fm2.env <- 
##     loess(ozone ~ wind * temperature * radiation, env,
##           span = 0.75, degree = 1)
## fm3.env <- 
##     loess(ozone ~ wind * temperature * radiation, env,
##           parametric = c("radiation", "wind"), 
##           span = 0.75, degree = 2)
## library("locfit")
## fm4.env <- locfit(ozone ~ wind * temperature * radiation, env)
## w.mesh <- with(env, do.breaks(range(wind), 50))
## t.mesh <- with(env, do.breaks(range(temperature), 50))
## r.mesh <- with(env, do.breaks(range(radiation), 3))
## grid <- 
##     expand.grid(wind = w.mesh, 
##                 temperature = t.mesh,
##                 radiation = r.mesh)
## grid[["fit.linear"]] <- predict(fm1.env, newdata = grid)
## grid[["fit.loess.1"]] <- as.vector(predict(fm2.env, newdata = grid))
## grid[["fit.loess.2"]] <- as.vector(predict(fm3.env, newdata = grid))
## grid[["fit.locfit"]] <- predict(fm4.env, newdata = grid)
## wireframe(fit.linear + fit.loess.1 + fit.loess.2 + fit.locfit ~ 
##                                      wind * temperature | radiation, 
##           grid, outer = TRUE, shade = TRUE, zlab = "")
## levelplot(fit.linear + fit.loess.1 + fit.loess.2 + fit.locfit ~ 
##                                      wind * temperature | radiation, 
##           data = grid)
## contourplot(fit.locfit ~ wind * temperature | radiation, 
##             data = grid, aspect = 0.7, layout = c(1, 4),
##             cuts = 15, label.style = "align")
## levelplot(volcano)
## contourplot(volcano, cuts = 20, label = FALSE)
## wireframe(volcano, panel.aspect = 0.7, zoom = 1, lwd = 0.01)
## ## plot(levelplot(volcano), split = c(1, 1, 1, 3), more = TRUE)
## ## plot(contourplot(volcano, cuts = 20, label = FALSE), split = c(1, 2, 1, 3), more = TRUE)
## ## plot(wireframe(volcano, panel.aspect = 0.7, zoom = 1, lwd = 0.01), 
## ##      split = c(1, 3, 1, 3), more = FALSE)
## data(Chem97, package = "mlmRev")
## Chem97$gcd <-
##     with(Chem97, 
##          cut(gcsescore,
##              breaks = quantile(gcsescore, ppoints(11, a = 1))))
## ChemTab <- xtabs(~ score + gcd + gender, Chem97)
## ChemTabDf <- as.data.frame.table(ChemTab)
## data(Cars93, package = "MASS")
## cor.Cars93 <- cor(Cars93[, !sapply(Cars93, is.factor)], use = "pair")
## levelplot(cor.Cars93, 
##           scales = list(x = list(rot = 90)))
## ord <- order.dendrogram(as.dendrogram(hclust(dist(cor.Cars93))))
## levelplot(cor.Cars93[ord, ord], at = do.breaks(c(-1.01, 1.01), 20),
##           scales = list(x = list(rot = 90)))
## tick.at <- pretty(range(sqrt(ChemTabDf$Freq)))
## levelplot(sqrt(Freq) ~ score * gcd | gender, ChemTabDf, 
##           shrink = c(0.7, 1),
##           colorkey = 
##           list(labels = list(at = tick.at, labels = tick.at^2)),
##           aspect = "iso")
## cloud(Freq ~ score * gcd | gender, data = ChemTabDf, type = "h",
##       aspect = c(1.5, 0.75), panel.aspect = 0.75)
## library("latticeExtra")
## cloud(Freq ~ score * gcd | gender, data = ChemTabDf,
##       screen = list(z = -40, x = -25), zoom = 1.1,
##       col.facet = "grey", xbase = 0.6, ybase = 0.6,
##       par.settings = list(box.3d = list(col = "transparent")),
##       aspect = c(1.5, 0.75), panel.aspect = 0.75,
##       panel.3d.cloud = panel.3dbars)
## library("copula")
## grid <-
##     expand.grid(u = do.breaks(c(0.01, 0.99), 25),
##                 v = do.breaks(c(0.01, 0.99), 25))
## grid$frank  <- with(grid, dcopula(frankCopula(2),    cbind(u, v)))
## grid$gumbel <- with(grid, dcopula(gumbelCopula(1.2), cbind(u, v)))
## grid$normal <- with(grid, dcopula(normalCopula(.4),  cbind(u, v)))
## grid$t      <- with(grid, dcopula(tCopula(0.4),      cbind(u, v)))
## wireframe(frank + gumbel + normal + t ~ u * v, grid, outer = TRUE, 
##           zlab = "", screen = list(z = -30, x = -50), lwd = 0.01)
## wireframe(frank + gumbel + normal + t ~ u * v, grid, outer = TRUE, 
##           zlab = "", screen = list(z = -30, x = -50), 
##           scales = list(z = list(log = TRUE)), lwd = 0.01)
## kx <- function(u, v)
##     cos(u) * (r + cos(u/2) * sin(t*v) - sin(u/2) * sin(2*t*v))
## ky <- function(u, v) 
##     sin(u) * (r + cos(u/2) * sin(t*v) - sin(u/2) * sin(2*t*v))
## kz <- function(u, v) 
##     sin(u/2) * sin(t*v) + cos(u/2) * sin(t*v)
## n <- 50
## u <- seq(0.3, 1.25, length = n) * 2 * pi
## v <- seq(0, 1, length = n) * 2 * pi
## um <- matrix(u, length(u), length(u))
## vm <- matrix(v, length(v), length(v), byrow = TRUE)
## r <- 2
## t <- 1
## wireframe(kz(um, vm) ~ kx(um, vm) + ky(um, vm), shade = TRUE,
##           screen = list(z = 170, x = -60),
##           alpha = 0.75, panel.aspect = 0.6, aspect = c(1, 0.4))
## data(USAge.df, package = "latticeExtra")
## str(USAge.df)
## library("RColorBrewer")
## brewer.div <-
##     colorRampPalette(brewer.pal(11, "Spectral"),
##                      interpolate = "spline")
## levelplot(Population ~ Year * Age | Sex, data = USAge.df,
##           cuts = 199, col.regions = brewer.div(200),
##           aspect = "iso")


## ## Examples from code/Chapter07.R
## vad.plot <- 
##     dotplot(reorder(Var2, Freq) ~ Freq | Var1,
##             data = as.data.frame.table(VADeaths), 
##             origin = 0, type = c("p", "h"),
##             main = "Death Rates in Virginia - 1940", 
##             xlab = "Number of deaths per 100")
## vad.plot
## dot.line.settings <- trellis.par.get("dot.line")
## str(dot.line.settings)
## dot.line.settings$col <- "transparent"
## trellis.par.set("dot.line", dot.line.settings)
## plot.line.settings <- trellis.par.get("plot.line")
## str(plot.line.settings)
## plot.line.settings$lwd <- 2
## trellis.par.set("plot.line", plot.line.settings)
## vad.plot
## plot(trellis.last.object())
## panel.dotline <- 
##     function(x, y, 
##              col = dot.symbol$col, pch = dot.symbol$pch,
##              cex = dot.symbol$cex, alpha = dot.symbol$alpha,
##              col.line = plot.line$col, lty = plot.line$lty,
##              lwd = plot.line$lwd, alpha.line = plot.line$alpha,
##              ...)
## {
##     dot.symbol <- trellis.par.get("dot.symbol")
##     plot.line <- trellis.par.get("plot.line")
##     panel.segments(0, y, x, y, col = col.line, lty = lty, 
##                    lwd = lwd, alpha = alpha.line)
##     panel.points(x, y, col = col, pch = pch, cex = cex, alpha = alpha)
## }
## trellis.par.set(dot.line = dot.line.settings,
##                 plot.line = plot.line.settings)
## trellis.par.set(dot.line = list(col = "transparent"),
##                 plot.line = list(lwd = 2))
## trellis.par.set(list(dot.line = list(col = "transparent"),
##                      plot.line = list(lwd = 2)))
## update(vad.plot, 
##        par.settings = list(dot.line = list(col = "transparent"),
##                            plot.line = list(lwd = 2)))
## tp <- trellis.par.get()
## unusual <- 
##     c("grid.pars", "fontsize", "clip", 
##       "axis.components", 
##       "layout.heights", "layout.widths")
## for (u in unusual) tp[[u]] <- NULL
## names.tp <- lapply(tp, names)
## unames <- sort(unique(unlist(names.tp)))
## ans <- matrix(0, nrow = length(names.tp), ncol = length(unames))
## rownames(ans) <- names(names.tp)
## colnames(ans) <- unames
## for (i in seq(along = names.tp))
##     ans[i, ] <- as.numeric(unames %in% names.tp[[i]])
## ans <- ans[, order(-colSums(ans))]
## ans <- ans[order(rowSums(ans)), ]
## ans[ans == 0] <- NA
## levelplot(t(ans), colorkey = FALSE, 
##           scales = list(x = list(rot = 90)),
##           panel = function(x, y, z, ...) {
##               panel.abline(v = unique(as.numeric(x)), 
##                            h = unique(as.numeric(y)), 
##                            col = "darkgrey")
##               panel.xyplot(x, y, pch = 16 * z, ...)
##           },
##           xlab = "Graphical parameters", 
##           ylab = "Setting names")
## show.settings()
## ## Examples from code/Chapter08.R
## stripplot(depth ~ factor(mag), data = quakes, jitter.data = TRUE, 
##           scales = list(y = "free", rot = 0),
##           prepanel = function(x, y, ...) list(ylim = rev(range(y))),
##           xlab = "Magnitude (Richter scale)")
## data(biocAccess, package = "latticeExtra")
## xyplot(counts/1000 ~ time | equal.count(as.numeric(time), 
##                                         9, overlap = 0.1), 
##        biocAccess, type = "l", aspect = "xy", strip = FALSE,
##        ylab = "Numer of accesses (thousands)", xlab = "",
##        scales = list(x = list(relation = "sliced", axs = "i"), 
##                      y = list(alternating = FALSE)))
## data(Earthquake, package = "MEMSS")
## xyplot(accel ~ distance, data = Earthquake,
##        prepanel = prepanel.loess, aspect = "xy",
##        type = c("p", "g", "smooth"),
##        scales = list(log = 2),
##        xlab = "Distance From Epicenter (km)",
##        ylab = "Maximum Horizontal Acceleration (g)")
## yscale.components.log2 <- function(...) {
##     ans <- yscale.components.default(...)
##     ans$right <- ans$left
##     ans$left$labels$labels <- 
##         parse(text = ans$left$labels$labels)
##     ans$right$labels$labels <- 
##         MASS::fractions(2^(ans$right$labels$at))
##     ans
## }
## logTicks <- function (lim, loc = c(1, 5)) {
##     ii <- floor(log10(range(lim))) + c(-1, 2)
##     main <- 10^(ii[1]:ii[2])
##     r <- as.numeric(outer(loc, main, "*"))
##     r[lim[1] <= r & r <= lim[2]]
## }
## xscale.components.log2 <- function(lim, ...) {
##     ans <- xscale.components.default(lim = lim, ...)
##     tick.at <- logTicks(2^lim, loc = c(1, 3))
##     ans$bottom$ticks$at <- log(tick.at, 2)
##     ans$bottom$labels$at <- log(tick.at, 2)
##     ans$bottom$labels$labels <- as.character(tick.at)
##     ans
## }
## xyplot(accel ~ distance | cut(Richter, c(4.9, 5.5, 6.5, 7.8)),
##        data = Earthquake, type = c("p", "g"),
##        scales = list(log = 2, y = list(alternating = 3)),
##        xlab = "Distance From Epicenter (km)",
##        ylab = "Maximum Horizontal Acceleration (g)",
##        xscale.components = xscale.components.log2,
##        yscale.components = yscale.components.log2)
## xscale.components.log10 <- function(lim, ...) {
##     ans <- xscale.components.default(lim = lim, ...)
##     tick.at <- logTicks(10^lim, loc = 1:9)
##     tick.at.major <- logTicks(10^lim, loc = 1)
##     major <- tick.at %in% tick.at.major
##     ans$bottom$ticks$at <- log(tick.at, 10)
##     ans$bottom$ticks$tck <- ifelse(major, 1.5, 0.75)
##     ans$bottom$labels$at <- log(tick.at, 10)
##     ans$bottom$labels$labels <- as.character(tick.at)
##     ans$bottom$labels$labels[!major] <- ""
##     ans$bottom$labels$check.overlap <- FALSE
##     ans
## }
## xyplot(accel ~ distance, data = Earthquake, 
##        prepanel = prepanel.loess, aspect = "xy", 
##        type = c("p", "g"), scales = list(log = 10),
##        xlab = "Distance From Epicenter (km)",
##        ylab = "Maximum Horizontal Acceleration (g)",
##        xscale.components = xscale.components.log10)
## axis.CF <- function(side, ...) {
##     if (side == "right") {
##         F2C <- function(f) 5 * (f - 32) / 9 
##         C2F <- function(c) 32 + 9 * c / 5 
##         ylim <- current.panel.limits()$ylim
##         prettyF <- pretty(ylim)
##         prettyC <- pretty(F2C(ylim))
##         panel.axis(side = side, outside = TRUE, at = prettyF, 
##                    tck = 5, line.col = "grey65", text.col = "grey35")
##         panel.axis(side = side, outside = TRUE, at = C2F(prettyC), 
##                    labels = as.character(prettyC),
##                    tck = 1, line.col = "black", text.col = "black")
##     }
##     else axis.default(side = side, ...)
## }
## xyplot(nhtemp ~ time(nhtemp), aspect = "xy", type = "o",
##        scales = list(y = list(alternating = 2, tck = c(1, 5))),
##        axis = axis.CF, xlab = "Year", ylab = "Temperature", 
##        main = "Yearly temperature in New Haven, CT",
##        key = list(text = list(c("(Celcius)", "(Fahrenheit)"), 
##                   col = c("black", "grey35")), columns = 2))


## ## Examples from code/Chapter09.R
## data(Cars93, package = "MASS")
## table(Cars93$Cylinders)
## sup.sym <- Rows(trellis.par.get("superpose.symbol"), 1:5)
## str(sup.sym)
## xyplot(Price ~ EngineSize | reorder(AirBags, Price), data = Cars93, 
##        groups = Cylinders, subset = Cylinders != "rotary", 
##        scales = list(y = list(log = 2, tick.number = 3)), 
##        xlab = "Engine Size (litres)", 
##        ylab = "Average Price (1000 USD)",
##        key = list(text = list(levels(Cars93$Cylinders)[1:5]), 
##                   points = sup.sym, space = "right"))
## ## alternative, using auto.key
## xyplot(Price ~ EngineSize | reorder(AirBags, Price), data = Cars93, 
##        groups = Cylinders, subset = Cylinders != "rotary", 
##        scales = list(y = list(log = 2, tick.number = 3)), 
##        xlab = "Engine Size (litres)", 
##        ylab = "Average Price (1000 USD)",
##        auto.key = list(text = levels(Cars93$Cylinders)[1:5], 
##                        space = "right", points = TRUE))
## my.pch <- c(21:25, 20)
## my.fill <- c("transparent", "grey", "black")
## with(Cars93, 
##      xyplot(Price ~ EngineSize, 
##             scales = list(y = list(log = 2, tick.number = 3)),
##             panel = function(x, y, ..., subscripts) {
##                 pch <- my.pch[Cylinders[subscripts]]
##                 fill <- my.fill[AirBags[subscripts]]
##                 panel.xyplot(x, y, pch = pch, 
##                              fill = fill, col = "black")
##             },
##             key = list(space = "right", adj = 1,
##                        text = list(levels(Cylinders)), 
##                        points = list(pch = my.pch), 
##                        text = list(levels(AirBags)), 
##                        points = list(pch = 21, fill = my.fill),
##                        rep = FALSE)))

## ## ## color version, nit shown in book
## ## my.pch <- c(21:25, 20)
## ## my.fill <- c("orange", "skyblue", "lightgreen")

## ## col.compkey <- 
## ##     with(Cars93, 
## ##          xyplot(Price ~ EngineSize, 
## ##                 scales = list(y = list(log = 2, tick.number = 3)),
## ##                 panel = function(x, y, ..., subscripts) {
## ##                     pch <- my.pch[Cylinders[subscripts]]
## ##                     fill <- my.fill[AirBags[subscripts]]
## ##                     panel.xyplot(x, y, 
## ##                                  pch = pch, fill = fill, col = fill)
## ##                 },
## ##                 key = 
## ##                 list(space = "right", 
## ##                      text = list(levels(Cylinders)), 
## ##                      points = list(pch = my.pch), 
## ##                      text = list(levels(AirBags)), 
## ##                      points = list(pch = 16, col = my.fill),
## ##                      rep = FALSE)))


## hc1 <- hclust(dist(USArrests, method = "canberra"))
## hc1 <- as.dendrogram(hc1)
## ord.hc1 <- order.dendrogram(hc1)
## hc2 <- reorder(hc1, state.region[ord.hc1])
## ord.hc2 <- order.dendrogram(hc2)
## library(latticeExtra)
## region.colors <- trellis.par.get("superpose.polygon")$col
## levelplot(t(scale(USArrests))[, ord.hc2], 
##           scales = list(x = list(rot = 90)),
##           colorkey = FALSE,
##           legend =
##           list(right =
##                list(fun = dendrogramGrob,
##                     args =
##                     list(x = hc2, ord = ord.hc2,
##                          side = "right", size = 10, size.add = 0.5,
##                          add = list(rect =
##                            list(col = "transparent",
##                               fill = region.colors[state.region])),
##                          type = "rectangle"))))


## ## Examples from code/Chapter10.R
## boxcox.trans <- function(x, lambda) {
##     if (lambda == 0) log(x) else (x^lambda - 1) / lambda
## }
## Titanic1 <- as.data.frame(as.table(Titanic[, , "Adult" ,])) 
## Titanic1
## barchart(Class ~ Freq | Sex, Titanic1, 
##          groups = Survived, stack = TRUE, 
##          auto.key = list(title = "Survived", columns = 2))
## Titanic2 <- 
##     reshape(Titanic1, direction = "wide", v.names = "Freq", 
##             idvar = c("Class", "Sex"), timevar = "Survived")
## names(Titanic2) <- c("Class", "Sex", "Dead", "Alive")
## Titanic2
## barchart(Class ~ Dead + Alive | Sex, 
##          Titanic2, 
##          stack = TRUE, 
##          auto.key = list(columns = 2))
## plot(trellis.last.object())
## data(Gcsemv, package = "mlmRev")
## xyplot(written ~ course | gender, data = Gcsemv, 
##        type = c("g", "p", "smooth"),
##        xlab = "Coursework score", ylab = "Written exam score",
##        panel = function(x, y, ...) {
##            panel.xyplot(x, y, ...)
##            panel.rug(x = x[is.na(y)], y = y[is.na(x)])
##        })
## qqmath( ~ written + course, Gcsemv, type = c("p", "g"), 
##        outer = TRUE, groups = gender, auto.key = list(columns = 2),
##        f.value = ppoints(200), ylab = "Score")
## set.seed(20051028)
## x1 <- rexp(2000)
## x1 <- x1[x1 > 1]
## x2 <- rexp(1000)
## str(make.groups(x1, x2))
## qqmath(~ data, make.groups(x1, x2), groups = which, 
##        distribution = qexp, aspect = "iso", type = c('p', 'g'))
## str(beaver1)
## str(beaver2)
## beavers <- make.groups(beaver1, beaver2)
## str(beavers)
## beavers$hour <- 
##     with(beavers, time %/% 100 + 24*(day - 307) + (time %% 100)/60)
## xyplot(temp ~ hour | which, data = beavers, groups = activ, 
##        auto.key = list(text = c("inactive", "active"), columns = 2),
##        xlab = "Time (hours)", ylab = "Body Temperature (C)", 
##        scales = list(x = list(relation = "sliced")))
## data(USAge.df, package = "latticeExtra")
## head(USAge.df)
## xyplot(Population ~ Age | factor(Year), USAge.df, 
##        groups = Sex, type = c("l", "g"),
##        auto.key = list(points = FALSE, lines = TRUE, columns = 2),
##        aspect = "xy", ylab = "Population (millions)",
##        subset = Year %in% seq(1905, 1975, by = 10))
## xyplot(Population ~ Year | factor(Age), USAge.df, 
##        groups = Sex, type = "l", strip = FALSE, strip.left = TRUE, 
##        layout = c(1, 3), ylab = "Population (millions)", 
##        auto.key = list(lines = TRUE, points = FALSE, columns = 2),
##        subset = Age %in% c(0, 10, 20))
## xyplot(Population ~ Year | factor(Year - Age), USAge.df, 
##        groups = Sex, subset = (Year - Age) %in% 1894:1905,
##        type = c("g", "l"), ylab = "Population (millions)", 
##        auto.key = list(lines = TRUE, points = FALSE, columns = 2))
## xyplot(stations ~ mag, quakes, jitter.x = TRUE, 
##        type = c("p", "smooth"),
##        xlab = "Magnitude (Richter)", 
##        ylab = "Number of stations reporting") 
## quakes$Mag <- equal.count(quakes$mag, number = 10, overlap = 0.2)
## summary(quakes$Mag)
## as.character(levels(quakes$Mag))
## ps.mag <- plot(quakes$Mag, ylab = "Level",
##                xlab = "Magnitude (Richter)")
## bwp.quakes <- 
##     bwplot(stations ~ Mag, quakes, xlab = "Magnitude", 
##            ylab = "Number of stations reporting")
## plot(bwp.quakes, position = c(0, 0, 1, 0.65))
## plot(ps.mag, position = c(0, 0.65, 1, 1), newpage = FALSE)
## bwplot(sqrt(stations) ~ Mag, quakes, 
##        scales = 
##        list(x = list(limits = as.character(levels(quakes$Mag)), 
##                      rot = 60)),
##        xlab = "Magnitude (Richter)",
##        ylab = expression(sqrt("Number of stations")))
## qqmath(~ sqrt(stations) | Mag, quakes, 
##        type = c("p", "g"), pch = ".", cex = 3, 
##        prepanel = prepanel.qqmathline, aspect = "xy",
##        strip = strip.custom(strip.levels = TRUE, 
##                             strip.names = FALSE),
##        xlab = "Standard normal quantiles", 
##        ylab = expression(sqrt("Number of stations")))
## xyplot(sqrt(stations) ~ mag, quakes, cex = 0.6,
##        panel = panel.bwplot, horizontal = FALSE, box.ratio = 0.05, 
##        xlab = "Magnitude (Richter)", 
##        ylab = expression(sqrt("Number of stations")))
## state.density <-
##     data.frame(name = state.name,
##                area = state.x77[, "Area"],
##                population = state.x77[, "Population"],
##                region = state.region)
## state.density$density <- with(state.density, population / area)
## dotplot(reorder(name, density) ~ density, state.density,
##         xlab = "Population Density (thousands per square mile)")
## state.density$Density <-
##     shingle(state.density$density,
##             intervals = rbind(c(0, 0.2),
##                               c(0.2, 1)))
## dotplot(reorder(name, density) ~ density | Density, state.density,
##         strip = FALSE, layout = c(2, 1), levels.fos = 1:50,
##         scales = list(x = "free"), between = list(x = 0.5),
##         xlab = "Population Density (thousands per square mile)", 
##         par.settings = list(layout.widths = list(panel = c(2, 1))))
## cutAndStack <- 
##     function(x, number = 6, overlap = 0.1, type = 'l',
##              xlab = "Time", ylab = deparse(substitute(x)), ...) {
##     time <- if (is.ts(x)) time(x) else seq_along(x)
##     Time <- equal.count(as.numeric(time), 
##                         number = number, overlap = overlap)
##     xyplot(as.numeric(x) ~ time | Time,
##            type = type, xlab = xlab, ylab = ylab,
##            default.scales = list(x = list(relation = "free"),
##                                  y = list(relation = "free")),
##            ...)
## }
## cutAndStack(EuStockMarkets[, "DAX"], aspect = "xy",
##             scales = list(x = list(draw = FALSE), 
##                           y = list(rot = 0)))
## bdp1 <- 
## dotplot(as.character(variety) ~ yield | as.character(site), barley,
##         groups = year, layout = c(1, 6),
##         auto.key = list(space = "top", columns = 2),
##         ## strip = FALSE, strip.left = TRUE,
##         aspect = "fill")
## bdp2 <- 
## dotplot(variety ~ yield | site, barley,
##         groups = year, layout = c(1, 6),
##         auto.key = list(space = "top", columns = 2),
##         ## strip = FALSE, strip.left = TRUE)
##         aspect = "fill")
## plot(bdp1, split = c(1, 1, 2, 1))
## plot(bdp2, split = c(2, 1, 2, 1), newpage = FALSE)
## state.density <-
##     data.frame(name = state.name,
##                area = state.x77[, "Area"],
##                population = state.x77[, "Population"],
##                region = state.region)
## state.density$density <- with(state.density, population / area)
## dotplot(reorder(name, density) ~ 1000 * density, state.density, 
##         scales = list(x = list(log = 10)), 
##         xlab = "Density (per square mile)")
## state.density$region <- 
##     with(state.density, reorder(region, density, median))
## state.density$name <- 
##     with(state.density, 
##          reorder(reorder(name, density), as.numeric(region)))
## dotplot(name ~ 1000 * density | region, state.density, 
##         strip = FALSE, strip.left = TRUE, layout = c(1, 4),
##         scales = list(x = list(log = 10),
##                       y = list(relation = "free")),
##         xlab = "Density (per square mile)")
## library("latticeExtra")
## resizePanels()
## data(USCancerRates, package = "latticeExtra")
## xyplot(rate.male ~ rate.female | state, USCancerRates,
##        aspect = "iso", pch = ".", cex = 2,
##        index.cond = function(x, y) { median(y - x, na.rm = TRUE) },
##        scales = list(log = 2, at = c(75, 150, 300, 600)), 
##        panel = function(...) {
##            panel.grid(h = -1, v = -1)
##            panel.abline(0, 1)
##            panel.xyplot(...)
##        })
## strip.style4 <- function(..., style) {
##     strip.default(..., style = 4)
## }
## data(Chem97, package = "mlmRev")
## qqmath(~gcsescore | factor(score), Chem97, groups = gender, 
##        type = c("l", "g"),  aspect = "xy", 
##        auto.key = list(points = FALSE, lines = TRUE, columns = 2),
##        f.value = ppoints(100), strip = strip.style4,
##        xlab = "Standard normal quantiles", 
##        ylab = "Average GCSE score")
## qqmath(~gcsescore | factor(score), Chem97, groups = gender, 
##        type = c("l", "g"),  aspect = "xy", 
##        auto.key = list(points = FALSE, lines = TRUE, columns = 2),
##        f.value = ppoints(100), strip = strip.custom(style = 4),
##        xlab = "Standard normal quantiles", 
##        ylab = "Average GCSE score")
## strip.combined <- 
##     function(which.given, which.panel, factor.levels, ...) {
##     if (which.given == 1) {
##         panel.rect(0, 0, 1, 1, col = "grey90", border = 1)
##         panel.text(x = 0, y = 0.5, pos = 4, 
##                    lab = factor.levels[which.panel[which.given]])
##     }
##     if (which.given == 2) {
##         panel.text(x = 1, y = 0.5, pos = 2,
##                    lab = factor.levels[which.panel[which.given]])
##     }
## }
## qqmath(~ gcsescore | factor(score) + gender, Chem97, 
##        f.value = ppoints(100), type = c("l", "g"), aspect = "xy",
##        strip = strip.combined,
##        par.strip.text = list(lines = 0.5),
##        xlab = "Standard normal quantiles", 
##        ylab = "Average GCSE score")
## ## Examples from code/Chapter11.R
## methods(class = "trellis")
## methods(class = "shingle")
## methods(generic.function = "barchart")
## dp.uspe <- 
##     dotplot(t(USPersonalExpenditure), 
##             groups = FALSE, 
##             index.cond = function(x, y) median(x),
##             layout = c(1, 5), 
##             type = c("p", "h"),
##             xlab = "Expenditure (billion dollars)")
## dp.uspe.log <- 
##     dotplot(t(USPersonalExpenditure), 
##             groups = FALSE, 
##             index.cond = function(x, y) median(x),
##             layout = c(1, 5), 
##             scales = list(x = list(log = 2)),
##             xlab = "Expenditure (billion dollars)")
## plot(dp.uspe,     split = c(1, 1, 2, 1), more = TRUE)
## plot(dp.uspe.log, split = c(2, 1, 2, 1), more = FALSE)
## state <- data.frame(state.x77, state.region, state.name)
## state$state.name <- 
##     with(state, reorder(reorder(state.name, Frost), 
##                         as.numeric(state.region)))
## dpfrost <- 
##     dotplot(state.name ~ Frost | reorder(state.region, Frost),
##             data = state, layout = c(1, 4),
##             scales = list(y = list(relation = "free")))
## summary(dpfrost)
## plot(dpfrost, 
##      panel.height = list(x = c(16, 13, 9, 12), unit = "null"))
## update(trellis.last.object(), layout = c(1, 1))[2]
## npanel <- 12
## rot <- list(z = seq(0, 30, length = npanel), 
##             x = seq(0, -80, length = npanel))
## quakeLocs <-
##     cloud(depth ~ long + lat, quakes, pch = ".", cex = 1.5,
##           panel = function(..., screen) {
##               pn <- panel.number()
##               panel.cloud(..., screen = list(z = rot$z[pn], 
##                                              x = rot$x[pn]))
##           },
##           xlab = NULL, ylab = NULL, zlab = NULL, 
##           scales = list(draw = FALSE), zlim = c(690, 30), 
##           par.settings = list(axis.line = list(col="transparent")))
## quakeLocs[rep(1, npanel)]
## data(Chem97, package="mlmRev")
## ChemQQ <- 
##     qq(gender ~ gcsescore | factor(score), Chem97, 
##        f.value = ppoints(100), strip = strip.custom(style = 5))
## tmd(ChemQQ)
## library("latticeExtra")
## data(biocAccess)
## baxy <- xyplot(log10(counts) ~ hour | month + weekday, biocAccess,
##                type = c("p", "a"), as.table = TRUE,
##                pch = ".", cex = 2, col.line = "black")
## dimnames(baxy)$month
## dimnames(baxy)$month <- month.name[1:5]
## dimnames(baxy)
## useOuterStrips(baxy)


## ## Examples from code/Chapter12.R
## state <- data.frame(state.x77, state.region)
## trellis.vpname("xlab", prefix = "plot1")
## trellis.vpname("strip", column = 2, row = 2, prefix = "plot2")


## data(Chem97, package = "mlmRev")
## qqmath(~ gcsescore | factor(score), Chem97, groups = gender,
##        f.value = function(n) ppoints(100),
##        aspect = "xy", 
##        page = function(n) {
##            cat("Click on plot to place legend", fill = TRUE)
##            ll <- grid.locator(unit = "npc")
##            if (!is.null(ll))
##                draw.key(simpleKey(levels(factor(Chem97$gender))),
##                         vp = viewport(x = ll$x, y = ll$y),
##                         draw = TRUE)
##        })


## state <- data.frame(state.x77, state.region)
## xyplot(Murder ~ Life.Exp | state.region, data = state, 
##        layout = c(2, 2), type = c("p", "g"), subscripts = TRUE)
## while (!is.null(fp <- trellis.focus())) {
##     if (fp$col > 0 & fp$row > 0)
##         panel.identify(labels = rownames(state))
## }


## qqmath(~ (1000 * Population / Area), state, 
##        ylab = "Population Density (per square mile)",
##        xlab = "Standard Normal Quantiles",
##        scales = list(y = list(log = TRUE, at = 10^(0:3))))
## trellis.focus()
## do.call(panel.qqmathline, trellis.panelArgs())
## panel.identify.qqmath(labels = row.names(state))
## trellis.unfocus()


## env <- environmental
## env$ozone <- env$ozone^(1/3)
## splom(env, pscales = 0, col = "grey")
## trellis.focus("panel", 1, 1, highlight = FALSE)
## panel.link.splom(pch = 16, col = "black")
## trellis.unfocus()


## state$name <- with(state, 
##                    reorder(reorder(factor(rownames(state)), Frost), 
##                            as.numeric(state.region)))
## dotplot(name ~ Frost | reorder(state.region, Frost), data = state, 
##         layout = c(1, 4), scales = list(y = list(relation="free")))
## trellis.currentLayout()
## heights <- 
##     sapply(seq_len(nrow(trellis.currentLayout())),
##            function(i) {
##                trellis.focus("panel", column = 1, row = i, 
##                              highlight = FALSE)
##                h <- diff(current.panel.limits()$ylim)
##                trellis.unfocus()
##                h
##            })
## heights
## update(trellis.last.object(), 
##        par.settings = list(layout.heights = list(panel = heights)))


## ## Examples from code/Chapter13.R
## panel.hypotrochoid <- function(r, d, cycles = 10, density = 30)
## {
##     if (missing(r)) r <- runif(1, 0.25, 0.75)
##     if (missing(d)) d <- runif(1, 0.25 * r, r)
##     t <- 2 * pi * seq(0, cycles, by = 1/density)
##     x <- (1 - r) * cos(t) + d * cos((1 - r) * t / r)
##     y <- (1 - r) * sin(t) - d * sin((1 - r) * t / r)
##     panel.lines(x, y)
## }
## panel.hypocycloid <- function(x, y, cycles = x, density = 30) {
##     panel.hypotrochoid(r = x / y, d = x / y, 
##                        cycles = cycles, density = density)
## }
## prepanel.hypocycloid <- function(x, y) {
##     list(xlim = c(-1, 1), ylim = c(-1, 1))
## }
## grid <- data.frame(p = 11:30, q = 10)
## grid$k <- with(grid, factor(p / q))
## xyplot(p ~ q | k, grid, aspect = 1, scales = list(draw = FALSE),
##        prepanel = prepanel.hypocycloid, panel = panel.hypocycloid)
## p <- xyplot(c(-1, 1) ~ c(-1, 1), aspect = 1, cycles = 15,
##             scales = list(draw = FALSE), xlab = "", ylab = "",
##             panel = panel.hypotrochoid)
## set.seed(20070706)
## p[rep(1, 42)]
## library("logspline")
## prepanel.ls <- function(x, n = 50, ...) {
##     fit <- logspline(x)
##     xx <- do.breaks(range(x), n)
##     yy <- dlogspline(xx, fit)
##     list(ylim = c(0, max(yy)))
## }
## panel.ls <- function(x, n = 50, ...) {
##     fit <- logspline(x)
##     xx <- do.breaks(range(x), n)
##     yy <- dlogspline(xx, fit)
##     panel.lines(xx, yy, ...)
## }
## faithful$Eruptions <- equal.count(faithful$eruptions, 4)
## densityplot(~ waiting | Eruptions, data = faithful, 
##             prepanel = prepanel.ls, panel = panel.ls)
## panel.bwtufte <- function(x, y, coef = 1.5, ...) {
##     x <- as.numeric(x); y <- as.numeric(y)
##     ux <- sort(unique(x))
##     blist <- tapply(y, factor(x, levels = ux), boxplot.stats, 
##                     coef = coef, do.out = FALSE)
##     blist.stats <- t(sapply(blist, "[[", "stats"))
##     blist.out <- lapply(blist, "[[", "out")
##     panel.points(y = blist.stats[, 3], x = ux, pch = 16, ...)
##     panel.segments(x0 = rep(ux, 2),
##                    y0 = c(blist.stats[, 1], blist.stats[, 5]),
##                    x1 = rep(ux, 2),
##                    y1 = c(blist.stats[, 2], blist.stats[, 4]),
##                    ...)
## }
## data(Chem97, package = "mlmRev")
## bwplot(gcsescore^2.34 ~ gender | factor(score), Chem97, 
##        panel = panel.bwtufte, layout = c(6, 1),
##        ylab = "Transformed GCSE score")
## data(Cars93, package = "MASS")
## cor.Cars93 <- 
##     cor(Cars93[, !sapply(Cars93, is.factor)], use = "pair")
## ord <- order.dendrogram(as.dendrogram(hclust(dist(cor.Cars93))))
## panel.corrgram <- 
##     function(x, y, z, subscripts, at, 
##              level = 0.9, label = FALSE, ...) 
## {
##     require("ellipse", quietly = TRUE)
##     x <- as.numeric(x)[subscripts]
##     y <- as.numeric(y)[subscripts]
##     z <- as.numeric(z)[subscripts]
##     zcol <- level.colors(z, at = at, ...)
##     for (i in seq(along = z)) {
##         ell <- ellipse(z[i], level = level, npoints = 50,
##                        scale = c(.2, .2), centre = c(x[i], y[i]))
##         panel.polygon(ell, col = zcol[i], border = zcol[i], ...)
##     }
##     if (label) 
##         panel.text(x = x, y = y, lab = 100 * round(z, 2), cex = 0.8,
##                    col = ifelse(z < 0, "white", "black"))
## }    
## levelplot(cor.Cars93[ord, ord], at = do.breaks(c(-1.01, 1.01), 20), 
##           xlab = NULL, ylab = NULL, colorkey = list(space = "top"),
##           scales = list(x = list(rot = 90)),
##           panel = panel.corrgram, label = TRUE)
## panel.corrgram.2 <- 
##     function(x, y, z, subscripts, at = pretty(z), scale = 0.8, ...) 
## {
##     require("grid", quietly = TRUE)
##     x <- as.numeric(x)[subscripts]
##     y <- as.numeric(y)[subscripts]
##     z <- as.numeric(z)[subscripts]
##     zcol <- level.colors(z, at = at, ...)
##     for (i in seq(along = z))
##     {
##         lims <- range(0, z[i])
##         tval <- 2 * base::pi * 
##             seq(from = lims[1], to = lims[2], by = 0.01)
##         grid.polygon(x = x[i] + .5 * scale * c(0, sin(tval)),
##                      y = y[i] + .5 * scale * c(0, cos(tval)),
##                      default.units = "native", 
##                      gp = gpar(fill = zcol[i]))
##         grid.circle(x = x[i], y = y[i], r = .5 * scale, 
##                     default.units = "native")
##     }
## }
## levelplot(cor.Cars93[ord, ord], xlab = NULL, ylab = NULL,
##           at = do.breaks(c(-1.01, 1.01), 101),
##           panel = panel.corrgram.2, 
##           scales = list(x = list(rot = 90)),
##           colorkey = list(space = "top"),
##           col.regions = colorRampPalette(c("red", "white", "blue")))
## panel.3d.contour <- 
##     function(x, y, z, rot.mat, distance, 
##              nlevels = 20, zlim.scaled, ...)
## {
##     add.line <- trellis.par.get("add.line")
##     panel.3dwire(x, y, z, rot.mat, distance, 
##                  zlim.scaled = zlim.scaled, ...)
##     clines <- 
##         contourLines(x, y, matrix(z, nrow = length(x), byrow = TRUE),
##                      nlevels = nlevels)
##     for (ll in clines) {
##         m <- ltransform3dto3d(rbind(ll$x, ll$y, zlim.scaled[2]), 
##                               rot.mat, distance)
##         panel.lines(m[1,], m[2,], col = add.line$col,
##                     lty = add.line$lty, lwd = add.line$lwd)
##     }
## }
## wireframe(volcano, zlim = c(90, 250), nlevels = 10, 
##           aspect = c(61/87, .3), panel.aspect = 0.6,
##           panel.3d.wireframe = "panel.3d.contour", shade = TRUE,
##           screen = list(z = 20, x = -60))
## library("maps")
## county.map <- map("county", plot = FALSE, fill = TRUE)
## str(county.map)
## data(ancestry, package = "latticeExtra")
## ancestry <- subset(ancestry, !duplicated(county))
## rownames(ancestry) <- ancestry$county
## freq <- table(ancestry$top)
## keep <- names(freq)[freq > 10]
## ancestry$mode <-
##     with(ancestry,
##          factor(ifelse(top %in% keep, top, "Other")))
## modal.ancestry <- ancestry[county.map$names, "mode"]
## library("RColorBrewer")
## colors <- brewer.pal(n = nlevels(ancestry$mode), name = "Pastel1")
## xyplot(y ~ x, county.map, aspect = "iso", 
##        scales = list(draw = FALSE), xlab = "", ylab = "",
##        par.settings = list(axis.line = list(col = "transparent")),
##        col = colors[modal.ancestry], border = NA, 
##        panel = panel.polygon,
##        key =
##        list(text = list(levels(modal.ancestry), adj = 1),
##             rectangles = list(col = colors),
##             x = 1, y = 0, corner = c(1, 0)))
## rad <- function(x) { pi * x / 180 }
## county.map$xx <- with(county.map, cos(rad(x)) * cos(rad(y)))
## county.map$yy <- with(county.map, sin(rad(x)) * cos(rad(y)))
## county.map$zz <- with(county.map, sin(rad(y)))
## panel.3dpoly <- function (x, y, z, rot.mat = diag(4), distance, ...) 
## {
##     m <- ltransform3dto3d(rbind(x, y, z), rot.mat, distance)
##     panel.polygon(x = m[1, ], y = m[2, ], ...)
## }
## aspect <-
##     with(county.map,
##          c(diff(range(yy, na.rm = TRUE)), 
##            diff(range(zz, na.rm = TRUE))) /
##          diff(range(xx, na.rm = TRUE)))
## cloud(zz ~ xx * yy, county.map, par.box = list(col = "grey"),
##       aspect = aspect, panel.aspect = 0.6, lwd = 0.01,
##       panel.3d.cloud = panel.3dpoly, col = colors[modal.ancestry],
##       screen = list(z = 10, x = -30),
##       key = list(text = list(levels(modal.ancestry), adj = 1),
##                  rectangles = list(col = colors), 
##                  space = "top", columns = 4),
##       scales = list(draw = FALSE), zoom = 1.1, 
##       xlab = "", ylab = "", zlab = "")
## library("latticeExtra")
## library("mapproj")
## data(USCancerRates)
## rng <- with(USCancerRates, 
##             range(rate.male, rate.female, finite = TRUE))
## nbreaks <- 50
## breaks <- exp(do.breaks(log(rng), nbreaks))
## mapplot(rownames(USCancerRates) ~ rate.male + rate.female,
##         data = USCancerRates, breaks = breaks,
##         map = map("county", plot = FALSE, fill = TRUE, 
##                   projection = "tetra"),
##         scales = list(draw = FALSE), xlab = "", 
##         main = "Average yearly deaths due to cancer per 100000")
## ## Examples from code/Chapter14.R
## library("latticeExtra")
## xyplot(sunspot.year, aspect = "xy", 
##        strip = FALSE, strip.left = TRUE,
##        cut = list(number = 4, overlap = 0.05))
## data(biocAccess, package = "latticeExtra")
## ssd <- stl(ts(biocAccess$counts[1:(24 * 30 * 2)], frequency = 24), 
##            "periodic")
## xyplot(ssd, xlab = "Time (Days)")
## library("flowViz")
## data(GvHD, package = "flowCore")
## densityplot(Visit ~ `FSC-H` | Patient, data = GvHD)
## library("hexbin")
## data(NHANES)
## hexbinplot(Hemoglobin ~ TIBC | Sex, data = NHANES, aspect = 0.8)
## panel.piechart <-
##     function(x, y, labels = as.character(y),
##              edges = 200, radius = 0.8, clockwise = FALSE,
##              init.angle = if(clockwise) 90 else 0,
##              density = NULL, angle = 45, 
##              col = superpose.polygon$col,
##              border = superpose.polygon$border,
##              lty = superpose.polygon$lty, ...)
## {
##     stopifnot(require("gridBase"))
##     superpose.polygon <- trellis.par.get("superpose.polygon")
##     opar <- par(no.readonly = TRUE)
##     on.exit(par(opar))
##     if (panel.number() > 1) par(new = TRUE)
##     par(fig = gridFIG(), omi = c(0, 0, 0, 0), mai = c(0, 0, 0, 0))
##     pie(as.numeric(x), labels = labels, edges = edges, radius = radius,
##         clockwise = clockwise, init.angle = init.angle, angle = angle,
##         density = density, col = col, border  = border, lty = lty)
## }
## piechart <- function(x, data = NULL, panel = "panel.piechart", ...)
## {
##     ocall <- sys.call(sys.parent())
##     ocall[[1]] <- quote(piechart)
##     ccall <- match.call()
##     ccall$data <- data
##     ccall$panel <- panel
##     ccall$default.scales <- list(draw = FALSE)
##     ccall[[1]] <- quote(lattice::barchart)
##     ans <- eval.parent(ccall)
##     ans$call <- ocall
##     ans
## }
## par(new = TRUE)
## piechart(VADeaths, groups = FALSE, xlab = "")

## ## plot.new(); par(new = TRUE)
## ## plot(trellis.last.object(), newpage = FALSE)


