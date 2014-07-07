
library(RColorBrewer)
library(tessella)
library(imp)

source("utils.R")

library(qtutils)

## some device that supports the graphicsEvent API:
## x11(type = "Xlib") or windows() or quartz()? cairoDevice()?
QT(width = 12, height = 6, family = "Helvetica")

.impenv$backend <- graphics_primitives

psunspot <- 
    yplot(data = NULL,
          panel.vars = elist(x = time(sunspot.month), y = sunspot.month),
          panel = ypanel.lines(),
          ## xlim = range(time(sunspot.month)),
          ylab = "Number of sunspots")

psunspot
pan_zoom_ts(psunspot)


pstates <- 
    yplot(data = dstates,
          panel.vars = elist(x = Illiteracy, y = Murder),
          panel = ypanel.grid() + ypanel.xyplot())
pstates

pan_zoom_ts(pstates)


pquakes <- 
    yplot(data = do.call(data.frame, lapply(quakes, scale1)),
          panel.vars = elist(x = long, y = lat, z = -depth, size = mag),
          panel = ypanel.cloud.axes() + ypanel.cloud(screen = list()),
          distance = 0.2,
          xlim = c(-0.5, 0.5), ylim = c(-0.5, 0.5))

pquakes
rotate_cloud(pquakes)

## while (TRUE)
## {
##     p$shared.env$setup$rot.mat <-
##             ytransform3dMatrix(list(y = 17/10, x = 19/10), R.mat = p$shared.env$setup$rot.mat)
##     plot(p)
## }

