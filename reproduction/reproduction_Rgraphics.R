# TODO why can't you specify pch in ypanel.xyplot?
 

library(RColorBrewer)
library(tessella)
library(yagpack) ## source.pkg("yagpack")
x11()
.yagpenv$backend <- graphics_primitives



# Figure 1.1 Scatterplot
data(pressure)
p <- yplot(data = pressure,
    panel.vars = elist(x = temperature, y = pressure),
    panel = ypanel.xyplot(col="blue") + 
        ypanel.abline(h = 200, col = "orange", lwd = 2),
    pch = 19, xlab = "Temp (Celsius)", ylab = "Pressure (mm Hg)")
p


# text via a data frame
df2 <- data.frame(temperature = 150, pressure = 600)
p <- yplot(data = pressure,
    panel.vars = elist(x = temperature, y = pressure),
    panel = ypanel.xyplot(col="blue") + 
        ylayer(mapping = map_points(),
            render = render_text("test"),
            data = df2, 
            margin.vars = NULL),
    pch = 19, xlab = "Temp (Celcius)", ylab = "Pressure (mm Hg)")

# TODO
# text(150, 600, "Pressure (mm Hg) \nversus \nTemperature (Celsius)")


# directly
p <- yplot(data = pressure,
    panel.vars = elist(x = temperature, y = pressure),
    panel = ypanel.xyplot(col="blue") + 
        ylayer(mapping = map_null(),
            render = render_text(x = 150, y = 600, lab = "Pressure")),
    pch = 19, xlab = "Temp (Celcius)", ylab = "Pressure (mm Hg)")
p


# Figure 1.2 Histogram
pointsDf <- data.frame(x = rnorm(100))
densObj <- density(pointsDf$x, bw=0.5)
densDf <- data.frame(x = densObj$x, y = 67 * densObj$y)

histogram(x, data=pointsDf, freq=FALSE)

yplot(data=pointsDf, 
    panel.vars=elist(x=x),
    panel=ypanel.histogram())

# OK
yplot(data = pointsDf, 
    panel.vars = elist(x = x),
    panel = ypanel.histogram() + 
        ylayer(panel.vars = elist(x = x, y = y),
            mapping = map_points(),
            render = render_lines(lwd = 3, col = "blue"),
            data = densDf),
    main = "Histogram of Y", xlab = "Y", ylab = "Density"
)

# also works
yplot(data = pointsDf, 
    panel.vars = elist(x=x),
    panel = ypanel.histogram() + 
        ylayer(mapping = map_null(),
            render = render_lines(x = densObj$x, y = 67* densObj$y, lwd = 3, col = "blue")),
    main = "Histogram of Y", xlab = "Y", ylab = "Density"
)

# TODO how to appropriately scale the histogram, 
#      need for a freq argument in histogram similarly to hist(., freq=FALSE) in base graphics?
yplot(data=x, 
    panel.vars=elist(x=x),
    panel=ypanel.histogram() + ypanel.density(),
    main = "Histogram of Y", xlab = "Y", ylab = "Density"
)


# Figure 1.2 Two-Line Scatter
x1 <- 1:5; y1 <- 2*x1
y2 <- rep(5, 5)
data1 <- cbind.data.frame(x1, y1)
data2 <- cbind.data.frame(x1, y2)
data(pressure)
p <- yplot(data = data1,
    panel.vars = elist(x = x1, y = y1), #groups
    panel = ypanel.xyplot(col="blue"),
    xlab = "X", ylab = "Y", pch=19)
p


p <- yplot(data = data1,
    panel.vars = elist(x = x1, y = y1),
    panel = ypanel.xyplot(col="blue")
    + ylayer(mapping = map_points(x = x1, y = y2),
        render = render_points(pch = 15, col = "blue"),
        data = data2),
    xlab = "X", ylab = "Y", pch=19)
p


# TODO use theme for custom colors of lines and dots
data3 <- cbind.data.frame(x=c(x1, x1), y=c(y1, y2))
data3$groups <- rep(c(1, 2), each = 5)
p <- yplot(data = data3,
    panel.vars = elist(x=x, y=y, groups=groups, mapcolor=FALSE), #groups
    panel = ypanel.xyplot(),
    xlab = "X", ylab = "Y", pch=19, type="b", col=c("orange", "red"))
p


p <- yplot(data = data1,
    panel.vars = elist(x = x1, y = y1),
    panel = ypanel.xyplot(col="blue", pch=19),
    + ypanel.points(x = x1, y = y2, data = data2, col = "orange"),
    xlab = "X", ylab = "Y")
p



# Figure 1.4 barley example
# TODO huge space foreseen for legend
data(barley, package = "lattice")

dotplot(yield, variety, 
    margin.vars = ~ site, 
    data = barley, 
    layout = c(1, 6), 
    aspect = 0.7,
    relation = list(y = "free"),
    groups = year)



# Figure 2.2 regression diagnostics
data(cars)
model <- lm(log(dist) ~ log(speed), data=cars)
cars$res <- model$res
cars$fitted <- model$fitted
qqdata <- qqnorm(cars$res)
qqdata <- as.data.frame(qqdata)

p <- yplot(data = cars,
    panel.vars = elist(x = fitted, y = res),
    panel = ypanel.xyplot(col="red") + 
        ypanel.abline(h = 0, col = gray(0.5), lwd = 2, lty = 2),
    pch = 19, xlab = "Fitted Values", ylab = "Residuals")
p
# TODO make side-by-side plots using plot(., position = .)





