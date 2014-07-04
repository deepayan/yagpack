library(RColorBrewer)
library(tessella)
library(yagpack) ## source.pkg("yagpack")
x11()
.yagpenv$backend <- graphics_primitives


##################
# Multiple Plots #
##################
data(diamonds, package = "ggplot2")
diamonds <- diamonds[1:1000,]

# raw data
p <- yplot(data = diamonds,
    panel.vars = elist(x = carat, y = price),
    panel = ypanel.xyplot(col="blue"),
    pch = 19, xlab = "carat", ylab = "price")
p

plot(p, position = list(x = 0, y = 0.6, w = 0.3, h = 0.4), new = TRUE)
plot(p, position = list(x = 0.3, y = 0.6, w = 0.7, h = 0.4), new = FALSE)
plot(p, position = list(x = 0, y = 0, w = 0.7, h = 0.6), new = FALSE)
plot(p, position = list(x = 0.7, y = 0, w = 0.3, h = 0.6), new = FALSE)


