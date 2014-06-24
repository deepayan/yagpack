
.yagpenv <- new.env(parent = emptyenv())


yagp.theme <- function(name = c("setting", "default", "lattice", "trellis", "bw"))
{
    name <- match.arg(name)
    switch(name,

           setting = {
               if (is.null(.yagpenv$theme)) yagp.custom.theme() else .yagpenv$theme
           },
           
           default = yagp.custom.theme(bg = "#eeeeee", reference = "white", axis = "#777777"),
           
           lattice = yagp.custom.theme(col = c("#0080ff", "#ff00ff", "darkgreen", "#ff0000", "orange", "#00ff00", "brown"),
                                         fill = c("#ccffff", "#ffccff", "#ccffcc", "#ffe5cc", "#cce6ff", "#ffffcc", "#ffcccc"),
                                         regions = function(...) rev(cm.colors(...)),
                                         strip = c("#ffe5cc", "#ccffcc", "#ccffff", "#cce6ff", "#ffccff", "#ffcccc", "#ffffcc"),
                                         reference = "#e6e6e6",
                                         bg = "transparent",
                                         fg = "black"),

           trellis = yagp.custom.theme(col = c("#00FFFF", "#FF00FF", "#00FF00", "#FF7F00", "#007EFF", "#FFFF00", "#FF0000"),
                                         fill = c("#CCFFFF", "#FFCCFF", "#CCFFCC", "#FFE5CC", "#CCE5FF", "#FFFFCC", "#FFCCCC"),
                                         regions = function(...) rev(cm.colors(...)),
                                         strip = c("#FFD18F", "#C8FFC8", "#C6FFFF", "#A9E2FF", "#FFC3FF", "#FF8C8A", "#FFFFC3"),
                                         reference = "#AAAAAA",
                                         bg = "#909090",
                                         fg = "black"),

           bw = yagp.custom.theme(col = c("#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000"),
                                    fill = c("#7B7B7B", "#D5D5D5", "#8B8B8B", "#C7C7C7", "#9A9A9A", "#B8B8B8", "#A9A9A9"),
                                    regions = grey(seq(0.3^2.2, 0.9^2.2, length = 100)^(1/2.2)),
                                    strip = c("#F2F2F2", "#F2F2F2", "#F2F2F2", "#F2F2F2", "#F2F2F2", "#F2F2F2", "#F2F2F2"),
                                    reference = "#AAAAAA",
                                    bg = "transparent",
                                    fg = "black"))
}



yagp.custom.theme <- 
    function(col = brewer.pal(n = 8, name = "Dark2"),
             fill = brewer.pal(n = 12, name = "Set3"),
             regions = brewer.pal(n = 11, name = "Spectral"),
             strip = brewer.pal(n = 8, name = "Pastel2")[c(7, 8, 5, 3, 2)],
             family = "Arial",
             pointsize = -1,
             reference = "#e8e8e8",
             bg = "transparent",
             fg = "black",
             axis = "black")
{
    if (!is.function(regions)) regions <- colorRampPalette(regions)
    theme <-
        list(default         = list(col = col, fill = "transparent", lty = 1, lwd = 1, cex = 1, pch = 1),
             polygon         = list(col = fg, fill = fill),
             reference       = list(col = reference, lty = 1, lwd = 1),
             regions         = list(col = regions(100)), # or just regions

             box.rectangle   = list(col = col, fill = "transparent"),
             box.umbrella    = list(col = col),

             dot.line        = list(col = reference),
             dot.symbol      = list(col = col, fill = col),

             background      = list(col = bg),

             add.line        = list(col = fg),
             add.text        = list(col = fg, family = family, pointsize = pointsize),
             axis            = list(col = axis, family = family, pointsize = pointsize),
             strip           = list(col = fg, fill = strip, family = family, pointsize = pointsize))
    theme
}
