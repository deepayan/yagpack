
## A central concept in our paradigm is the idea of evaluating an
## expression in a data source.  This would have been ideally done
## through eval(), but it is not generic. So, we define a new generic
## evaluate().

## We will need to use S4 if we want to dispatch on 2nd argument,
## needed if we retain order of arguments (or is it?).  On the other
## hand, dispatch on 'e' is not really needed.  Will decide later (and
## work around for now).

evaluate <- function(e, data, ...)
{
    UseMethod("evaluate")
}

## evaluate.default <- function(e, data, subset = TRUE, enclos = .GlobalEnv)
## {
##     if (inherits(data, "data.env"))
##         eval(e, data$data, enclos)[subset]
##     ## for data.frame, maybe subset first
##     else
##         eval(e, data, enclos)[subset]
## }

evaluate.default <- function(e, data, subset = TRUE, enclos = .GlobalEnv)
{
    if (inherits(data, "data.env")) data <- data$data
    eval(e, data[subset, , drop = FALSE], enclos)
}

## A simple data.frame wrapper that can be used as a persistent data
## source.  Additional components:
## 

## (1) a 'selection' indicator list (possibly multiple index vectors) 

## (2) a list of linked plots or other objects that need to be updated
##     on changes

## (3) logical flag: whether refreshes are needed.

data.env <-
    function(data)
{
    e <- new.env()
    e$data <- data
    e$selection <- list(TRUE)
    e$dirty <- FALSE
    e$links <- list()
    class(e) <- "data.env"
    e
}

as.data.frame.data.env <- function(x, ...) { return (x$data) }


## The print() method does not seem to work when auto-printing, not
## sure why (internal dispatch?).  Probably need S4 for proper
## environment inheritance.

## print.data.env <- function(x, ...)
## {
##     ls.str(x, all.names = TRUE)
## }


