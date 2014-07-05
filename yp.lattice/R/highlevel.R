


## high-level convenience functions

xyplot <- 
    function(x, y = NULL, data, enclos, groups = NULL, color = NULL, size = NULL,
             legend = auto.legend("p", panel.vars, data = data, enclos = enclos, more.args = legend.args),
             legend.args = list(),
             ...)
{
    panel <- ypanel.xyplot(...) 
    panel.vars <- panel.terms(list(x = substitute(x), y = substitute(y), groups = substitute(groups), color = substitute(color), size = substitute(size)))
    margin.vars <- attr(panel.vars, "margin")
    if (missing(data)) data <- parent.frame()
    if (missing(enclos)) enclos <- parent.frame()
    yplot(data = data, enclos = enclos, panel.vars = panel.vars, panel = panel, legend = legend, margin.vars = margin.vars, ...)
}

densityplot <- 
    function(x, data, enclos, weights = NULL, groups = NULL, 
             legend = auto.legend("l", panel.vars, data = data, enclos = enclos, more.args = legend.args),
             legend.args = list(),
             ref = TRUE, plot.points = "jitter", ...)
{
    panel <- ypanel.density(...)
    panel.vars <- panel.terms(list(x = substitute(x), weights = substitute(weights), groups = substitute(groups)))
    margin.vars <- attr(panel.vars, "margin")
    if (missing(data)) data <- parent.frame()
    if (missing(enclos)) enclos <- parent.frame()
    if (ref) panel <- ypanel.abline(h = 0, reference = TRUE) + panel
    if (isTRUE(plot.points)) panel <- panel + ypanel.points(y = 0, jitter.y = FALSE)
    else if (identical(plot.points, "jitter")) panel <- panel + ypanel.points(y = 0, jitter.y = TRUE)
    yplot(data = data, enclos = enclos, panel.vars = panel.vars, panel = panel, legend = legend, margin.vars = margin.vars, ...)
}

qqmath <- 
    function(x, data, enclos, groups = NULL, 
             legend = auto.legend("p", panel.vars, data = data, enclos = enclos, more.args = legend.args),
             legend.args = list(),
             ...)
{
    panel <- ypanel.qqmath(...)
    panel.vars <- panel.terms(list(x = substitute(x), groups = substitute(groups)))
    margin.vars <- attr(panel.vars, "margin")
    if (missing(data)) data <- parent.frame()
    if (missing(enclos)) enclos <- parent.frame()
    yplot(data = data, enclos = enclos, panel.vars = panel.vars, panel = panel, legend = legend, margin.vars = margin.vars, ...)
}

histogram <- 
    function(x, data, enclos, weights = NULL, groups = NULL,
             legend = auto.legend("r", panel.vars, data = data, enclos = enclos, more.args = legend.args),
             legend.args = list(),
             ...)
{
    panel <- ypanel.histogram(...) 
    panel.vars <- panel.terms(list(x = substitute(x), weights = substitute(weights), groups = substitute(groups)))
    margin.vars <- attr(panel.vars, "margin")
    if (missing(data)) data <- parent.frame()
    if (missing(enclos)) enclos <- parent.frame()
    yplot(data = data, enclos = enclos, panel.vars = panel.vars, panel = panel, legend = legend, margin.vars = margin.vars, ...)
}

stripplot <- dotplot <- xyplot


bwplot <- 
    function(x, y = NULL, data, enclos, groups = NULL, 
             legend = auto.legend("b", panel.vars, data = data, enclos = enclos, more.args = legend.args),
             legend.args = list(),
             ...)
{
    panel <- ypanel.boxplot(...)
    panel.vars <- panel.terms(list(x = substitute(x), y = substitute(y), groups = substitute(groups)))
    margin.vars <- attr(panel.vars, "margin")
    if (missing(data)) data <- parent.frame()
    if (missing(enclos)) enclos <- parent.frame()
    yplot(data = data, enclos = enclos, panel.vars = panel.vars, panel = panel, legend = legend, margin.vars = margin.vars, ...)
}

barchart <- 
    function(x, y = NULL, data, enclos, groups = NULL, 
             legend = auto.legend("r", panel.vars, data = data, enclos = enclos, more.args = legend.args),
             legend.args = list(),
             ...)
{
    panel = ypanel.barchart(...)
    panel.vars <- panel.terms(list(x = substitute(x), y = substitute(y), groups = substitute(groups)))
    margin.vars <- attr(panel.vars, "margin")
    if (missing(data)) data <- parent.frame()
    if (missing(enclos)) enclos <- parent.frame()
    yplot(data = data, enclos = enclos, panel.vars = panel.vars, panel = panel, legend = legend, margin.vars = margin.vars, ...)
}



## maps


## yagp.mapplot <- yplot_map <- 
##     function(x, y = NULL, data, enclos, 
##              panel = ypanel.mapplot, ...)
## {
##     panel.vars <- panel.terms(list(x = substitute(x), y = substitute(y)))
##     margin.vars <- attr(panel.vars, "margin")
##     if (missing(data)) data <- parent.frame()
##     if (missing(enclos)) enclos <- parent.frame()
##     yplot(data = data, enclos = enclos, panel.vars = panel.vars, panel = panel, margin.vars = margin.vars, ...)
## }
