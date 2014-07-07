
library(RColorBrewer)
library(tessella)
library(imp)
library(qtbase)
library(quilt)
library(qtpaint)

source("utils.R")

scene <- qscene()
sview <-
    qplotView(scene = scene,
              rescale = "geometry",
              opengl = FALSE)

## sview$setRenderHint(Qt$QPainter$Antialiasing, TRUE)

sview$size <- qsize(600, 400)
sview
as.matrix(scene$sceneRect)
scene$setSceneRect(qrect(0, 0, sview$width, sview$height))
sview$setCursor(Qt$QCursor(Qt$Qt$CrossCursor))

identify_points_qtpaint(pstates, rscene = scene)



