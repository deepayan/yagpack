library(RColorBrewer)
library(tessella)
library(yagpack) ## source.pkg("yagpack")
x11()
.yagpenv$backend <- graphics_primitives



# Scatterplot
data(pressure)
p <- yplot(data = pressure,
    panel.vars = elist(x = temperature, y = pressure),
    panel = ypanel.xyplot(col="blue") + 
        ypanel.abline(h = 200, col = "orange", lwd = 2),
    pch = 19, xlab = "Temp (Celsius)", ylab = "Pressure (mm Hg)")
p

# TODO
text(150, 600, "Pressure (mm Hg) \nversus \nTemperature (Celsius)")




# TODO how to add text correctly



yplot(data = mtcars, panel.vars = elist(x = disp, y = mpg, groups = factor(gear)), 
    panel = (ypanel.grid(h = -1, v = -1) + 
          ylayer(mapping = map_points(mapcolor = TRUE), 
              render = render_xy(type = "S") + render_xy(type = "p"))))









# Histogram
x <- rnorm(100)
x <- as.data.frame(x)
histogram(x, data=x)
# TODO how to add density line to a plot



# Two-Line Scatter
