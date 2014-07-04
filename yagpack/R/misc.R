


compute.position <-
    function(column, row,
             what = c("panel", "strip.top", "strip.left",
                      "xaxis.bottom", "xaxis.top",
                      "yaxis.left", "yaxis.right", "legend"),
             space = "top")
{
    what <- match.arg(what)

    ## counting rows from top to bottom
    positions <- list(panel = c(3, 3),
                      strip.top = c(3, 2),
                      strip.left = c(2, 3),
                      yaxis.left = c(1, 3),
                      yaxis.right = c(4, 3),
                      xaxis.bottom = c(3, 4),
                      xaxis.top = c(3, 1))
    
    k <- 10L
    c(column = (column-1L) * k, row = (row-1L) * k) + positions[[what]]
}


getLimits <- function(r)
{
    e <- as.matrix(r)
    list(xlim = e[, 1], ylim = e[, 2])
}



elist <- function(...)
{
    mcall <- match.call()
    lapply(as.list(mcall)[-1], as.expression)
}



## helper function to convert formula to expression-list

margin.terms <- function(x, sep = as.symbol("+"))
{
    if (!inherits(x, "formula")) stop("'x' must be a formula")
    if (length(x) != 2) stop("'x' must have length 2")
    x <- x[[2]]
    ans <- list()
    while ((length(x) == 3) && (x[[1]] == sep))
    {
        ans[[ length(ans)+1L ]] <- x[[3]]
        x <- x[[2]]
    }
    ans[[ length(ans)+1L ]] <- x
    lapply(rev(ans), as.expression)
}

## 'x' is a list of values returned by substitute.  We want to create
## an expression list from it, but allow the first term to be a
## formula (only when specified as such, as otherwise substitute won't
## know (we don't want to evaluate anything here))

panel.terms <- function(x, tilde = as.symbol("~"), bar = as.symbol("|"))
{
    ans <- lapply(x, as.expression)
    x <- x[[1]] ## overrides 'x' and possibly 'y' if this is a formula (has a ~)
    formula.terms <- list()
    if ((length(x) > 1) && (x[[1]] == tilde))
    {
        if (length(x) == 2)
            formula.terms$x <- as.expression(x[[2]])
        else
        {
            formula.terms$x <- as.expression(x[[3]])
            formula.terms$y <- as.expression(x[[2]])
        }
        ## If the formula had the form y ~ x | a, then formula.terms$x[[1]]
        ## is now x | a.  We want to check for this case and separate the
        ## LHS (as x) and RHS (as the margin variables).
        formx <- formula.terms$x[[1]]
        if ((length(formx) == 3) && (formx[[1]] == bar)) 
        {
            formula.terms$x <- as.expression(formx[[2]])
            margin <- formx[c(1, 3)]
            margin[[1]] <- tilde
            attr(ans, "margin") <- margin.terms(as.formula(margin))
        }
    }
    ans[names(formula.terms)] <- formula.terms
    ans
}


## constructors

yagp.relation <- function(x = "same", y = "same", z = "same",
                     margin = NULL)
{
    list(x = x, y = y, z = z, margin = margin)
}

yagp.alternating <- function(x = TRUE, y = TRUE)
{
    if (is.logical(x)) x <- if (x) c(1, 2) else 1
    if (is.logical(y)) y <- if (y) c(1, 2) else 1
    list(x = x, y = y)
}


