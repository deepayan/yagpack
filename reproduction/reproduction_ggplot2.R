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
    panel = ypanel.xyplot() + ypanel.loess(span = 0.2),
    auto.key = list(space = 'right'))
p


# boxplots (fig 2.8b)
bwplot(price ~ color, data = diamonds)

# jittered points (fig 2.8) # TODO alpha bug
p <- xyplot(price/carat ~ color, data = diamonds,
    panel = ypanel.points(jitter.x=0.4, alpha = 0.5),
    pch = 19, xlab = "color", ylab = "price/carat")
p

p <- xyplot(price/carat ~ color, data = diamonds,
    panel = ypanel.xyplot() + ypanel.points(jitter.x=0.4),
    pch = 19, xlab = "color", ylab = "price/carat", alpha = 0.5)
p


# histogram
histogram(carat, data=diamonds, nint=30)

# stacked histogram
histogram(carat, data=diamonds, groups=color, nint=30)


# barplot (fig 2.13) # TODO do we have to xtabs it?
barchart(color, data=diamonds) # does not work

color.tab <- xtabs(~color, data=diamonds)
color.df <- as.data.frame(color.tab)
barchart(Freq ~ color, data = color.df, col = "orange", fill = "blue")



###############
#  Economics  #
###############
data(economics, package = "ggplot2")

head(economics)

p <- xyplot(uempmed ~ date, data = economics,
    panel = ypanel.xyplot() + ypanel.lines(),
    pch = 19, xlab = "date", ylab = "uempmed")
p





##############
#  Faceting  #
##############
histogram(carat, margin.vars = ~ color, 
    data = diamonds, 
    layout = c(1, 7), nint = 25)


histogram(carat, margin.vars = ~ color, 
    data = diamonds, 
    relation = list(y = "free"),
    layout = c(1, 7), nint = 25)



##########
#  Axes  #
##########
p <- yplot(data = diamonds,
    panel.vars = elist(x = price/carat, y = carat),
    panel = ypanel.xyplot(col="blue") + yagp.xaxis(lim=c(0, 20000), 
        side="bottom", at = c(5000, 10000), labels=c("Hey", "ya")),
    pch = 19, xlab = "Price ($/carat)", ylab = "Weight (carats)")
p
# TODO figure out how to make this non-terrible



###########
#  Color  #
###########
data(mpg, package="ggplot2")

p <- xyplot(hwy ~ displ, data = mpg, groups = factor(cyl),
    pch = 19, xlab = "displ", ylab = "hwy", 
    auto.key = list(space = 'right'))
p

p <- xyplot(hwy ~ displ, data = mpg, groups = factor(cyl),
    panel = ypanel.xyplot(col="blue") + ypanel.loess(), 
    pch = 19, xlab = "displ", ylab = "hwy", 
    auto.key = list(space = 'right'))
p

# TODO this should just not show the line instead of erroring out
p <- xyplot(hwy ~ displ, data = mpg, groups = factor(cyl),
    panel = ypanel.xyplot(col="blue") + ypanel.lm(), 
    pch = 19, xlab = "displ", ylab = "hwy", 
    auto.key = list(space = 'right'))
p


mpg <- mpg[-which(mpg$cyl == 5),]
p <- xyplot(hwy ~ displ, data = mpg, groups = factor(cyl),
    panel = ypanel.xyplot(col="blue") + ypanel.lm(), 
    pch = 19, xlab = "displ", ylab = "hwy", 
    auto.key = list(space = 'right'))
p



##########################
#  Faceting + Smoothing  #
########################## # TODO debug this example
p <- xyplot(hwy ~ displ, data = mpg, #groups = factor(cyl),
    margin.vars = ~ year,
    panel = ypanel.xyplot(col="blue") + ypanel.loess(), 
    pch = 19, xlab = "displ", ylab = "hwy", 
    auto.key = list(space = 'right'))
p


