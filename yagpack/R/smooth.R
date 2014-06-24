
loess_constructor <-
    function(x, y, span = 2/3, degree = 1,
             family = c("symmetric", "gaussian"),
             evaluation = 50,
             horizontal = FALSE,
             ...)
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 2)
        return(list(x = numeric(0), y = numeric(0)))
    if (horizontal)
    {
        l <- loess.smooth(y[ok], x[ok], span = span, family = family,
                          degree = degree, evaluation = evaluation)
        tmp <- l$x
        l$x <- l$y
        l$y <- tmp
        l
    }
    else
    {
        loess.smooth(x[ok], y[ok], span = span, family = family,
                     degree = degree, evaluation = evaluation)
    }
}

lm_constructor <-
    function(x, y, degree = 1, evaluation = 50,
             horizontal = FALSE, ...)
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 2)
        return(list(x = numeric(0), y = numeric(0)))
    if (horizontal)
    {
        fm <- lm(x ~ poly(y, degree), subset = ok)
        l <- list(y = seq(from = min(y[ok]), to = max(y[ok]), length.out = evaluation))
        l$x <- predict(fm, newdata = l)
    }
    else
    {
        fm <- lm(y ~ poly(x, degree), subset = ok)
        l <- list(x = seq(from = min(x[ok]), to = max(x[ok]), length.out = evaluation))
        l$y <- predict(fm, newdata = l)
    }
    l
}



map_loess <-
    function(span = 2/3, degree = 1,
             family = c("symmetric", "gaussian"),
             evaluation = 50,
             horizontal = FALSE, ...)
{
    largs <- list(span = span, degree = degree, family = family,
                  evaluation = evaluation, horizontal = horizontal)
    f <- function(x, y, col = NULL, ..., groups = NULL, mapcolor = TRUE, limits = FALSE)
    {
        mapByGroups(x = x, y = y, constructor = loess_constructor, args = largs,
                    col = col, groups = groups, mapcolor = mapcolor,
                    limits = limits)
    }
    changeDefaults(f, ...)
}



map_lm <-
    function(degree = 1, 
             evaluation = if (degree == 1) 2 else 50,
             horizontal = FALSE, ...)
{
    largs <- list(degree = degree, evaluation = evaluation, horizontal = horizontal)
    f <- function(x, y, ..., col = NULL, groups = NULL, mapcolor = TRUE, limits = FALSE)
    {
        mapByGroups(x = x, y = y, constructor = lm_constructor, args = largs,
                    col = col, groups = groups, mapcolor = mapcolor,
                    limits = limits)
    }
    changeDefaults(f, ...)
}
