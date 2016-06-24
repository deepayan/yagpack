
library(RColorBrewer)
library(tessella)
library(imp)

library(RGtk2)
library(cairoDevice)

source("utils.R")

## This is meant to work with RGtk2().  Generally, we need something where
## two devices can be overlaid.

## demo(appWindow)
## demo(package="RGtk2") # to see the rest

## To draw R graphics inside an RGtk2 GUI:

## notebook works:

if (FALSE)
{
    win <- gtkWindow()
    lfixed <- gtkNotebook()

    dev1 <- gtkDrawingArea()
    dev2 <- gtkDrawingArea()
    win$add(lfixed)

    lfixed$appendPage(dev1)
    lfixed$appendPage(dev2)

    asCairoDevice(dev1)
    asCairoDevice(dev2)

    dev.set(2); plot(1:10)
    dev.set(3); plot(rnorm(100))
}



    
win <- gtkWindow()
lfixed <- gtkFixed()

dev1 <- gtkDrawingArea()
dev2 <- gtkDrawingArea()

dev1$setSizeRequest(1000, 800)
dev2$setSizeRequest(1000, 800)

win$add(lfixed)

lfixed$put(dev1, 1, 1)
lfixed$put(dev2, 1, 1)

asCairoDevice(dev1) #, width = 400, height = 400)
asCairoDevice(dev2)


dev.set(2); plot(1:10)
dev.set(3); plot(rnorm(100))

par("bg")








dev1 <- QT(width = 12, height = 6, family = "Helvetica")
dev2 <- QT(width = 12, height = 6, family = "Helvetica")
dev2$setStyleSheet("background: transparent")

.impenv$backend <- graphics_primitives

w <- Qt$QWidget()
l <- Qt$QStackedLayout(w)
l$setStackingMode(Qt$QStackedLayout$StackAll)
w$size <- qsize(500, 500)
l$addWidget(dev1)
l$addWidget(dev2)
w

l$setCurrentIndex(1) # dev2 on top
dev.set(2) # dev1 becomes active

piris <- 
    yplot(data = iris,
          margin.vars = elist(Species), layout = c(3,1),
          panel.vars = elist(x = cbind(Sepal.Length, Sepal.Width,
                                       Petal.Length, Petal.Width),
                             color = Species),
          panel = ypanel.parallel(),
          switch.axes = TRUE)

reorder_vars(piris, dev.main = 2, dev.feedback = 3)

identify_points_graphics(pstates,
                         dev.main = 2, dev.feedback = 3)

