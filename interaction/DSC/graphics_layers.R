
library(RColorBrewer)
library(tessella)
library(imp)
library(qtutils)

source("utils.R")

## This will only work with QT().  Generally, we need something where
## two devices can be overlaid.

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

