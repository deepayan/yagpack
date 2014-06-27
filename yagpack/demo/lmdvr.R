## NOT READY

library(RColorBrewer)
library(tessella)
library(yagpack) ## source.pkg("yagpack")
x11()
.yagpenv$backend <- graphics_primitives

## .yagpenv$theme <- yagp.theme("default")

## code from lattice book

## Chapter 1


data(Chem97, package = "mlmRev")
xtabs( ~ score, data = Chem97)

histogram(x = gcsescore, data = Chem97, margin.vars = ~factor(score))

densityplot(gcsescore, data = Chem97, margin.vars = ~factor(score),
            ref = TRUE)

###----------

densityplot(~ gcsescore, data = Chem97, groups = score,
            plot.points = FALSE, ref = TRUE,
            auto.key = list(columns = 3))

## tp1 <- histogram(~ gcsescore | factor(score), data = Chem97)
## tp2 <- 
##     densityplot(~ gcsescore, data = Chem97, groups = score,
##                 plot.points = FALSE,
##                 auto.key = list(space = "right", title = "score"))
## class(tp2)
## summary(tp1)
## plot(tp1, split = c(1, 1, 1, 2)) # FIXME: was plot instead of print
## plot(tp2, split = c(1, 2, 1, 2), newpage = FALSE)


## Chapter 2

data(Oats, package = "MEMSS")

tp1.oats <- xyplot(nitro, yield, 
    margin.vars = ~ Variety + Block, data = Oats, type = 'o')

## tp1.oats <- xyplot(nitro, yield,
##                    margin.vars = elist(Variety = Variety, Block = Block),
##                    data = Oats, type = 'o')

print(tp1.oats)
dim(tp1.oats)
dimnames(tp1.oats)

xtabs(~Variety + Block, data = Oats)

summary(tp1.oats)

## summary(tp1.oats[, 1])
## print(tp1.oats[, 1])

## update(tp1.oats, 
##        aspect="xy")

## update(tp1.oats, aspect = "xy",
##        layout = c(0, 18))

## update(tp1.oats, aspect = "xy", layout = c(0, 18), 
##        between = list(x = c(0, 0, 0.5), y = 0.5))

data(barley, package = "lattice")

dotplot(yield, variety, margin.vars = ~ site, data = barley, 
        layout = c(1, 6), aspect = 0.7,
        relation = list(y = "free"),
        groups = year) # , auto.key = list(space = 'right'))

## key.variety <- 
##     list(space = "right", text = list(levels(Oats$Variety)),
##          points = list(pch = 1:3, col = "black"))

xyplot(nitro, yield, margin.vars = ~ Block, data = Oats,
       aspect = "xy", type = "o", 
       groups = Variety,
       xlab = "Nitrogen concentration (cwt/acre)",
       ylab = "Yield (bushels/acre)", 
       main = "Yield of three varieties of oats",
       sub = "A 3 x 4 split plot experiment with 6 blocks")

barchart(Freq ~ Class, margin.vars = ~ Sex + Age,
         data = as.data.frame(Titanic), 
         groups = Survived, stack = FALSE)

barchart(Freq ~ Class, margin.vars = ~ Sex + Age,
         data = as.data.frame(Titanic), 
         groups = Survived, stack = TRUE, layout = c(4, 1),
         switch.axes = TRUE,
         auto.key = list(title = "Survived", columns = 2),
         relation = list(x = "free"))

## bc.titanic <- 
##     barchart(Class ~ Freq | Sex + Age, as.data.frame(Titanic), 
##              groups = Survived, stack = TRUE, layout = c(4, 1),
##              auto.key = list(title = "Survived", columns = 2),
##              scales = list(x = "free"))

## update(bc.titanic, 
##        panel = function(...) {
##            panel.grid(h = 0, v = -1)
##            panel.barchart(...)
##        })

## update(bc.titanic, 
##        panel = function(..., border) {
##            panel.barchart(..., border = "transparent")
##        })

## Chapter 3


densityplot(~ eruptions, data = faithful)

## FIXME
densityplot(~ eruptions, data = faithful, 
            kernel = "rect", bw = 11, plot.points = "rug",
            n = 200)

## library("flatticeExtra")
## data(gvhd10)
data(gvhd10, package = "latticeExtra")

densityplot(~log(FSC.H), margin.vars = ~ Days, data = gvhd10, 
            plot.points = FALSE, ref = TRUE,
            layout = c(2, 4))

histogram(~log2(FSC.H), margin.vars = ~ Days, gvhd10, xlab = "log Forward Scatter",
          type = "density", nint = 50, layout = c(2, 4))

data(Chem97, package = "mlmRev")

qqmath(~ gcsescore , margin.vars = ~  factor(score),
       data = Chem97, 
       f.value = ppoints(100)) # FIXME

## FIXME
qqmath(~ gcsescore , margin.vars = ~  gender, Chem97,
       groups = score, aspect = "xy", 
       f.value = ppoints(100), auto.key = list(space = "right"),
       xlab = "Standard Normal Quantiles", 
       ylab = "Average GCSE Score")

Chem97.mod <- transform(Chem97, gcsescore.trans = gcsescore^2.34)

qqmath(~ gcsescore.trans , margin.vars = ~  gender, Chem97.mod, groups = score,
       f.value = ppoints(100),
       ## aspect = "xy", # FIXME
       auto.key = list(space = "right", title = "score"), 
       xlab = "Standard Normal Quantiles", 
       ylab = "Transformed GCSE Score")

## library("latticeExtra")
## ecdfplot(~ gcsescore , margin.vars = ~  factor(score), data = Chem97, 
##          groups = gender, auto.key = list(columns = 2),
##          subset = gcsescore > 0, xlab = "Average GCSE Score")

qqmath(~ gcsescore , margin.vars = ~  factor(score),
       data = subset(Chem97, gcsescore > 0),
       groups = gender, 
       auto.key = list(points = FALSE, lines = TRUE, columns = 2),
       type = "l", distribution = qunif,
       ## aspect = "xy",
       xlab = "Standard Normal Quantiles", 
       ylab = "Average GCSE Score")

## qq(gender ~ gcsescore , margin.vars = ~  factor(score), Chem97, 
##    f.value = ppoints(100), aspect = 1)

bwplot(gcsescore ~ factor(score),
       margin.vars = ~  gender, data = Chem97,
       switch.axes = TRUE,
       xlab = "Average GCSE Score")

bwplot(gcsescore^2.34 ~ gender,
       margin.vars = ~ factor(score),
       data = Chem97, 
       varwidth = TRUE, layout = c(6, 1),
       ylab = "Transformed GCSE score")

bwplot(log(FSC.H) ~ Days, data = gvhd10,
       switch.axes = TRUE,
       xlab = "log(Forward Scatter)", ylab = "Days Past Transplant")

## bwplot(Days ~ log(FSC.H), gvhd10, 
##        panel = panel.violin, box.ratio = 3,
##        xlab = "log(Forward Scatter)", 
##        ylab = "Days Past Transplant")

stripplot <- xyplot

stripplot(factor(mag) ~ depth, data = quakes)

stripplot(depth ~ factor(mag), data = quakes, 
          jitter.data = TRUE, alpha = 0.6,
          xlab = "Magnitude (Richter)", ylab = "Depth (km)")

stripplot(sqrt(abs(residuals(lm(yield~variety+year+site)))) ~ site, 
          data = barley, groups = year, ## jitter.data = TRUE,
          ## auto.key = list(points = TRUE, lines = TRUE, columns = 2),
          ## type = c("p", "a"), fun = median,
          ylab = expression(abs("Residual Barley Yield")^{1 / 2}))


## Chapter 4

## VADeaths
## class(VADeaths)
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
xyplot(Freq ~ gcsescore , margin.vars = ~  gender, data = gcsescore.df, 
       type = "h", layout = c(1, 2), xlab = "Average GCSE Score")
score.tab <- xtabs(~score + gender, Chem97)
score.df <- as.data.frame(score.tab)
barchart(Freq ~ score , margin.vars = ~  gender,
         data = score.df, origin = 0)

## Chapter 5

xyplot(lat ~ long , margin.vars = ~  cut(depth, 2), data = quakes)

xyplot(lat ~ long , margin.vars = ~  cut(depth, 3), data = quakes, 
       aspect = "iso", pch = ".", cex = 2,
       type = "p", grid = TRUE,
       xlab = "Longitude", ylab = "Latitude")
       ## strip = strip.custom(strip.names = TRUE, var.name = "Depth"))

xyplot(lat ~ long, data = quakes, aspect = "iso",
       groups = cut(depth, breaks = quantile(depth, ppoints(4, 1))), 
       auto.key = list(columns = 3, title = "Depth"), 
       xlab = "Longitude", ylab = "Latitude")

depth.col <- gray.colors(100)[cut(quakes$depth, 100, label = FALSE)]

depth.ord <- rev(order(quakes$depth))

xyplot(lat ~ long, data = quakes[depth.ord, ], 
       aspect = "iso", type = "p", grid = TRUE,
       pch = 21, fill = depth.col[depth.ord], cex = 2,
       xlab = "Longitude", ylab = "Latitude")

## quakes$Magnitude <- equal.count(quakes$mag, 4)

## summary(quakes$Magnitude)
quakes$color <- depth.col
quakes.ordered <- quakes[depth.ord, ]

## xyplot(lat ~ long , margin.vars = ~  Magnitude, data = quakes.ordered,
##        aspect = "iso", fill.color = quakes.ordered$color, cex = 2,
##        panel = function(x, y, fill.color, ..., subscripts) {
##            fill <- fill.color[subscripts]
##            panel.grid(h = -1, v = -1)
##            panel.xyplot(x, y, pch = 21, fill = fill, ...)
##        },
##        xlab = "Longitude", ylab = "Latitude")

depth.breaks <- do.breaks(range(quakes.ordered$depth), 50)

quakes.ordered$color <- 
    level.colors(quakes.ordered$depth,
                 at = depth.breaks, 
                 colramp = gray.colors(100))

## xyplot(lat ~ long , margin.vars = ~  Magnitude, data = quakes.ordered, 
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
##                                         at = depth.breaks)))),
##        xlab = "Longitude", ylab = "Latitude")

## types.plain <- c("p", "l", "o", "r", "g", "s", "S", "h", "a", "smooth")
## types.horiz <- c("s", "S", "h", "a", "smooth")
## horiz <- rep(c(FALSE, TRUE), c(length(types.plain), length(types.horiz)))
## types <- c(types.plain, types.horiz)
## set.seed(2007041)
## x <- sample(seq(-10, 10, length = 15), 30, TRUE)
## y <- x + 0.25 * (x + 1)^2 + rnorm(length(x), sd = 5)

## xyplot(y ~ x , margin.vars = ~  gl(1, length(types)),
##        xlab = "type", 
##        ## FIXME ylab = list(c("horizontal=TRUE", "horizontal=FALSE"), y = c(1/6, 4/6)),
##        as.table = TRUE, layout = c(5, 3),
##        between = list(y = c(0, 1)),
##        strip = function(...) {
##            panel.fill(trellis.par.get("strip.background")$col[1])
##            type <- types[panel.number()]
##            panel.text(lab = sprintf('"%s"', type), 
##                       x = 0.5, y = 0.5)
##            panel.fill(border = "black")
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


data(Earthquake, package = "MEMSS")

xyplot(accel ~ distance, data = Earthquake,
       ## panel = function(...) {
       ##     panel.grid(h = -1, v = -1)
       ##     panel.xyplot(...)
       ##     panel.loess(...)
       ## },
       panel = ypanel.grid() + ypanel.xyplot() + ypanel.loess(),
       xlab = "Distance From Epicenter (km)",
       ylab = "Maximum Horizontal Acceleration (g)")

xyplot(accel ~ distance, data = Earthquake,
       ## type = c("g", "p", "smooth"),
       panel = ypanel.grid() + ypanel.xyplot() + ypanel.loess(),
       scales = list(log = 2),
       xlab = "Distance From Epicenter (km)",
       ylab = "Maximum Horizontal Acceleration (g)")

## library("locfit") # attaches lattice

Earthquake$Magnitude <- 
    equal.count(Earthquake$Richter, 3, overlap = 0.1)
coef <- coef(lm(log2(accel) ~ log2(distance), data = Earthquake))

xyplot(accel ~ distance , margin.vars = ~  Magnitude, data = Earthquake,
       scales = list(log = 2), col.line = "grey", lwd = 2,
       panel = function(...) {
           panel.abline(reg = coef)
           ## locfit::panel.locfit(...)
       },
       xlab = "Distance From Epicenter (km)",
       ylab = "Maximum Horizontal Acceleration (g)")

data(SeatacWeather, package = "latticeExtra")

xyplot(min.temp + max.temp + precip ~ day , margin.vars = ~  month,
       ylab = "Temperature and Rainfall", 
       data = SeatacWeather, type = "l", lty = 1, col = "black")

maxp <- max(SeatacWeather$precip, na.rm = TRUE)

xyplot(min.temp + max.temp + I(80 * precip / maxp) ~ day , margin.vars = ~  month,
       data = SeatacWeather, lty = 1, col = "black",
       ylab = "Temperature and Rainfall", 
       type = c("l", "l", "h"), distribute.type = TRUE)

update(trellis.last.object(),
       ylab = "Temperature (Fahrenheit) \n and Rainfall (inches)",
       panel = function(...) {
           panel.xyplot(...)
           if (panel.number() == 2) {
               at <- pretty(c(0, maxp))
               panel.axis("right", half = FALSE,
                          at = at * 80 / maxp, labels = at)
           }
       })

## library("hexbin")

data(gvhd10, package = "latticeExtra")

## xyplot(asinh(SSC.H) ~ asinh(FL2.H) , margin.vars = ~  Days, gvhd10, aspect = 1, 
##        panel = panel.hexbinplot, .aspect.ratio = 1, trans = sqrt)

splom(USArrests)

splom(~USArrests[c(3, 1, 2, 4)] , margin.vars = ~  state.region, 
      pscales = 0, type = c("g", "p", "smooth"))

splom(~data.frame(mpg, disp, hp, drat, wt, qsec),
      data = mtcars, groups = cyl, pscales = 0, 
      varnames = c("Miles\nper\ngallon", "Displacement\n(cu. in.)",
                   "Gross\nhorsepower", "Rear\naxle\nratio", 
                   "Weight", "1/4 mile\ntime"),
      auto.key = list(columns = 3, title = "Number of Cylinders"))

## ## FIXME: what's the problem?
## parallel(~mtcars[c(1, 3, 4, 5, 6, 7)] , margin.vars = ~  factor(cyl), 
##          mtcars, groups = carb, 
##          key = simpleKey(levels(factor(mtcars$carb)), points = FALSE, 
##                          lines = TRUE, space = "top", columns = 3), 
##          layout = c(3, 1))

data(gvhd10, package = "latticeExtra")

## parallel(~ asinh(gvhd10[c(3, 2, 4, 1, 5)]), data = gvhd10, 
##          subset = Days == "13", alpha = 0.01, lty = 1)



## Chapter 6


quakes$Magnitude <- equal.count(quakes$mag, 4)

cloud(depth ~ lat * long , margin.vars = ~  Magnitude, data = quakes, 
      zlim = rev(range(quakes$depth)),
      screen = list(z = 105, x = -70), panel.aspect = 0.75,
      xlab = "Longitude", ylab = "Latitude", zlab = "Depth")

cloud(depth ~ lat * long , margin.vars = ~  Magnitude, data = quakes, 
      zlim = rev(range(quakes$depth)), panel.aspect = 0.75,
      screen = list(z = 80, x = -70), zoom = 0.7,
      scales = list(z = list(arrows = FALSE, distance = 2)), 
      xlab = "Longitude", ylab = "Latitude", 
      zlab = list("Depth\n(km)", rot = 90))

p <-
    cloud(depth ~ long + lat, quakes, zlim = c(690, 30),
          pch = ".", cex = 1.5, zoom = 1,
          xlab = NULL, ylab = NULL, zlab = NULL,
          par.settings = list(axis.line = list(col = "transparent")),
          scales = list(draw = FALSE))

npanel <- 4
rotz <- seq(-30, 30, length = npanel)
roty <- c(3, 0)

update(p[rep(1, 2 * npanel)], 
       layout = c(2, npanel),
       panel = function(..., screen) {
           crow <- current.row()
           ccol <- current.column()
           panel.cloud(..., screen = list(z = rotz[crow], 
                                          x = -60,
                                          y = roty[ccol]))
       })

state.info <-
    data.frame(name = state.name,
               long = state.center$x,
               lat = state.center$y,
               area = state.x77[, "Area"],
               population = 1000 * state.x77[, "Population"])

state.info$density <- with(state.info, population / area)

cloud(density ~ long + lat, state.info,
      subset = !(name %in% c("Alaska", "Hawaii")),
      type = "h", lwd = 2, zlim = c(0, max(state.info$density)),
      scales = list(arrows = FALSE))

library("maps")

state.map <- map("state", plot=FALSE, fill = FALSE)

panel.3dmap <- function(..., rot.mat, distance, xlim, ylim, zlim,
                        xlim.scaled, ylim.scaled, zlim.scaled)
{
    scaled.val <- function(x, original, scaled) {
        scaled[1] + (x - original[1]) * diff(scaled) / diff(original)
    }
    m <- ltransform3dto3d(rbind(scaled.val(state.map$x, xlim, xlim.scaled),
                                scaled.val(state.map$y, ylim, ylim.scaled),
                                zlim.scaled[1]), 
                          rot.mat, distance)
    panel.lines(m[1,], m[2,], col = "grey76")
}

cloud(density ~ long + lat, state.info,
      subset = !(name %in% c("Alaska", "Hawaii")),
      panel.3d.cloud = function(...) {
          panel.3dmap(...)
          panel.3dscatter(...)
      }, 
      type = "h", scales = list(draw = FALSE), zoom = 1.1,
      xlim = state.map$range[1:2], ylim = state.map$range[3:4],
      xlab = NULL, ylab = NULL, zlab = NULL,
      aspect = c(diff(state.map$range[3:4]) / diff(state.map$range[1:2]), 0.3),
      panel.aspect = 0.75, lwd = 2, screen = list(z = 30, x = -60),
      par.settings = list(axis.line = list(col = "transparent"),
                          box.3d = list(col = "transparent", alpha = 0)))

data(Cars93, package = "MASS")

cor.Cars93 <- 
    cor(Cars93[, !sapply(Cars93, is.factor)], use = "pair")

data(Chem97, package = "mlmRev")

Chem97$gcd <-
    with(Chem97, 
         cut(gcsescore,
             breaks = quantile(gcsescore, ppoints(11, a = 1))))

ChemTab <- xtabs(~ score + gcd + gender, Chem97)

ChemTabDf <- as.data.frame.table(ChemTab)

env <- environmental
env$ozone <- env$ozone^(1/3)
env$Radiation <- equal.count(env$radiation, 4)

cloud(ozone ~ wind + temperature , margin.vars = ~  Radiation, env)

splom(env[1:4])

fm1.env <- lm(ozone ~ radiation * temperature * wind, env)

fm2.env <- 
    loess(ozone ~ wind * temperature * radiation, env,
          span = 0.75, degree = 1)

fm3.env <- 
    loess(ozone ~ wind * temperature * radiation, env,
          parametric = c("radiation", "wind"), 
          span = 0.75, degree = 2)

## library("locfit")

## fm4.env <- locfit::locfit(ozone ~ wind * temperature * radiation, env)

w.mesh <- with(env, do.breaks(range(wind), 50))
t.mesh <- with(env, do.breaks(range(temperature), 50))
r.mesh <- with(env, do.breaks(range(radiation), 3))

grid <- 
    expand.grid(wind = w.mesh, 
                temperature = t.mesh,
                radiation = r.mesh)
grid[["fit.linear"]] <- predict(fm1.env, newdata = grid)
grid[["fit.loess.1"]] <- as.vector(predict(fm2.env, newdata = grid))
grid[["fit.loess.2"]] <- as.vector(predict(fm3.env, newdata = grid))
## grid[["fit.locfit"]] <- predict(fm4.env, newdata = grid)

wireframe(fit.linear + fit.loess.1 + fit.loess.2 ~ # + fit.locfit ~ 
                                     wind * temperature , margin.vars = ~  radiation, 
          grid, outer = TRUE, shade = TRUE, zlab = "")

levelplot(fit.linear + fit.loess.1 + fit.loess.2 ~ # + fit.locfit ~ 
                                     wind * temperature , margin.vars = ~  radiation, 
          data = grid)

## contourplot(fit.locfit ~ wind * temperature , margin.vars = ~  radiation, 
##             data = grid, aspect = 0.7, layout = c(1, 4),
##             cuts = 15, label.style = "align")

levelplot(volcano)

contourplot(volcano, cuts = 20, label = FALSE)

wireframe(volcano, panel.aspect = 0.7, zoom = 1, lwd = 0.5)

## plot(levelplot(volcano), split = c(1, 1, 1, 3), more = TRUE)
## plot(contourplot(volcano, cuts = 20, label = FALSE), split = c(1, 2, 1, 3), more = TRUE)
## plot(wireframe(volcano, panel.aspect = 0.7, zoom = 1, lwd = 0.01), 
##      split = c(1, 3, 1, 3), more = FALSE)

data(Chem97, package = "mlmRev")

Chem97$gcd <-
    with(Chem97, 
         cut(gcsescore,
             breaks = quantile(gcsescore, ppoints(11, a = 1))))

ChemTab <- xtabs(~ score + gcd + gender, Chem97)
ChemTabDf <- as.data.frame.table(ChemTab)
data(Cars93, package = "MASS")
cor.Cars93 <- cor(Cars93[, !sapply(Cars93, is.factor)], use = "pair")

levelplot(cor.Cars93, 
          scales = list(x = list(rot = 90)))

ord <- order.dendrogram(as.dendrogram(hclust(dist(cor.Cars93))))

levelplot(cor.Cars93[ord, ord], at = do.breaks(c(-1.01, 1.01), 20),
          scales = list(x = list(rot = 90)))

tick.at <- pretty(range(sqrt(ChemTabDf$Freq)))

levelplot(sqrt(Freq) ~ score * gcd , margin.vars = ~  gender, ChemTabDf, 
          shrink = c(0.7, 1),
          colorkey = 
          list(labels = list(at = tick.at, labels = tick.at^2)),
          aspect = "iso")

cloud(Freq ~ score * gcd , margin.vars = ~  gender, data = ChemTabDf, type = "h",
      aspect = c(1.5, 0.75), panel.aspect = 0.75)

## library("latticeExtra")
panel.3dbars <- latticeExtra::panel.3dbars
environment(panel.3dbars) <- .GlobalEnv

cloud(Freq ~ score * gcd , margin.vars = ~  gender, data = ChemTabDf,
      screen = list(z = -40, x = -25), zoom = 1.1,
      col.facet = "grey", xbase = 0.6, ybase = 0.6,
      par.settings = list(box.3d = list(col = "transparent")),
      aspect = c(1.5, 0.75), panel.aspect = 0.75,
      panel.3d.cloud = panel.3dbars)

library("copula")

grid <-
    expand.grid(u = do.breaks(c(0.01, 0.99), 25),
                v = do.breaks(c(0.01, 0.99), 25))
grid$frank  <- with(grid, dcopula(frankCopula(2),    cbind(u, v)))
grid$gumbel <- with(grid, dcopula(gumbelCopula(1.2), cbind(u, v)))
grid$normal <- with(grid, dcopula(normalCopula(.4),  cbind(u, v)))
grid$t      <- with(grid, dcopula(tCopula(0.4),      cbind(u, v)))

wireframe(frank + gumbel + normal + t ~ u * v, grid, outer = TRUE, 
          zlab = "", screen = list(z = -30, x = -50), lwd = 0.5)

wireframe(frank + gumbel + normal + t ~ u * v, grid, outer = TRUE, 
          zlab = "", screen = list(z = -30, x = -50), 
          scales = list(z = list(log = TRUE)), lwd = 0.5)

kx <- function(u, v)
    cos(u) * (r + cos(u/2) * sin(t*v) - sin(u/2) * sin(2*t*v))
ky <- function(u, v) 
    sin(u) * (r + cos(u/2) * sin(t*v) - sin(u/2) * sin(2*t*v))
kz <- function(u, v) 
    sin(u/2) * sin(t*v) + cos(u/2) * sin(t*v)
n <- 50
u <- seq(0.3, 1.25, length = n) * 2 * pi
v <- seq(0, 1, length = n) * 2 * pi
um <- matrix(u, length(u), length(u))
vm <- matrix(v, length(v), length(v), byrow = TRUE)
r <- 2
t <- 1

wireframe(kz(um, vm) ~ kx(um, vm) + ky(um, vm), shade = TRUE,
          screen = list(z = 170, x = -60),
          alpha = 0.75, panel.aspect = 0.6, aspect = c(1, 0.4))

data(USAge.df, package = "latticeExtra")
str(USAge.df)

library("RColorBrewer")

brewer.div <-
    colorRampPalette(brewer.pal(11, "Spectral"),
                     interpolate = "spline")

levelplot(Population ~ Year * Age , margin.vars = ~  Sex, data = USAge.df,
          cuts = 199, col.regions = brewer.div(200),
          aspect = "iso")




## Chapter 7

vad.plot <- 
    dotplot(reorder(Var2, Freq) ~ Freq , margin.vars = ~  Var1,
            data = as.data.frame.table(VADeaths), 
            origin = 0, type = c("p", "h"),
            main = "Death Rates in Virginia - 1940", 
            xlab = "Number of deaths per 100")

vad.plot

dot.line.settings <- trellis.par.get("dot.line")
str(dot.line.settings)
dot.line.settings$col <- "transparent"
trellis.par.set("dot.line", dot.line.settings)
plot.line.settings <- trellis.par.get("plot.line")
str(plot.line.settings)
plot.line.settings$lwd <- 2
trellis.par.set("plot.line", plot.line.settings)
vad.plot

plot(trellis.last.object())

panel.dotline <- 
    function(x, y, 
             col = dot.symbol$col, pch = dot.symbol$pch,
             cex = dot.symbol$cex, alpha = dot.symbol$alpha,
             col.line = plot.line$col, lty = plot.line$lty,
             lwd = plot.line$lwd, alpha.line = plot.line$alpha,
             ...)
{
    dot.symbol <- trellis.par.get("dot.symbol")
    plot.line <- trellis.par.get("plot.line")
    panel.segments(0, y, x, y, col = col.line, lty = lty, 
                   lwd = lwd, alpha = alpha.line)
    panel.points(x, y, col = col, pch = pch, cex = cex, alpha = alpha)
}

trellis.par.set(dot.line = dot.line.settings,
                plot.line = plot.line.settings)
trellis.par.set(dot.line = list(col = "transparent"),
                plot.line = list(lwd = 2))
trellis.par.set(list(dot.line = list(col = "transparent"),
                     plot.line = list(lwd = 2)))

update(vad.plot, 
       par.settings = list(dot.line = list(col = "transparent"),
                           plot.line = list(lwd = 2)))

tp <- trellis.par.get()

unusual <- 
    c("grid.pars", "fontsize", "clip", 
      "axis.components", 
      "layout.heights", "layout.widths")

for (u in unusual) tp[[u]] <- NULL
names.tp <- lapply(tp, names)
unames <- sort(unique(unlist(names.tp)))
ans <- matrix(0, nrow = length(names.tp), ncol = length(unames))
rownames(ans) <- names(names.tp)
colnames(ans) <- unames

for (i in seq(along = names.tp))
    ans[i, ] <- as.numeric(unames %in% names.tp[[i]])

ans <- ans[, order(-colSums(ans))]
ans <- ans[order(rowSums(ans)), ]
ans[ans == 0] <- NA

levelplot(t(ans), colorkey = FALSE, 
          scales = list(x = list(rot = 90)),
          panel = function(x, y, z, ...) {
              panel.abline(v = unique(as.numeric(x)), 
                           h = unique(as.numeric(y)), 
                           col = "darkgrey")
              panel.xyplot(x, y, pch = 16 * z, ...)
          },
          xlab = "Graphical parameters", 
          ylab = "Setting names")

show.settings()




