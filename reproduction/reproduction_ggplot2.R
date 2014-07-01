library(RColorBrewer)
library(tessella)
library(yagpack) ## source.pkg("yagpack")
x11()
.yagpenv$backend <- graphics_primitives


##############
#  Diamonds  #
##############
data(diamonds, package = "ggplot2")

# raw data
p <- yplot(data = diamonds,
    panel.vars = elist(x = carat, y = price),
    panel = ypanel.xyplot(col="blue"),
    pch = 19, xlab = "carat", ylab = "price")
p

# loglog transformation
p <- yplot(data = diamonds,
    panel.vars = elist(x = log(carat), y = log(price)),
    panel = ypanel.xyplot(col="blue"),
    pch = 19, xlab = "log(carat)", ylab = "log(price)")
p

# carat versus x*y*z
p <- yplot(data = diamonds,
    panel.vars = elist(x = carat, y = x * y * z),
    panel = ypanel.xyplot(col="blue"),
    pch = 19, xlab = "carat", ylab = "x*y*z")
p

# colored by color
p <- yplot(data = diamonds,
    panel.vars = elist(x = carat, y = price, groups=color),
    panel = ypanel.xyplot(),
    pch = 19, xlab = "carat", ylab = "price")
p

# legend DOES NOT WORK
p <- yplot(data = diamonds,
    panel.vars = elist(x = carat, y = price, groups=color),
    panel = ypanel.xyplot(),
    pch = 19, xlab = "carat", ylab = "price", auto.key = list(space = 'right'))
p


# TODO aesthetics from main plot do not match aesthetics in legend
p <- xyplot(price ~ carat, data = diamonds, groups = color,
    pch = 19, xlab = "carat", ylab = "price", 
    auto.key = list(space = 'right'))
p



# TODO symbols for group instead of colors
p <- xyplot(price ~ carat, data = diamonds, groups = cut,
    pch = 19, xlab = "carat", ylab = "price", 
    auto.key = list(space = 'right'))
p



# alpha values (works!)
p <- xyplot(price ~ carat, data = diamonds, groups = cut,
    pch = 19, xlab = "carat", ylab = "price", alpha = 0.1,
    auto.key = list(space = 'right'))
p



# smoothed lines
p <- xyplot(price ~ carat, data = diamonds,
    pch = 19, xlab = "carat", ylab = "price", alpha = 0.1, lwd = 1,
    panel = ypanel.xyplot() + ypanel.loess(),
    auto.key = list(space = 'right'))
p
