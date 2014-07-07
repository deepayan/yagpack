
## FIXME: make all this generic


compute_packet <- function(vars, levels, which)
{
    id <- !(do.call("pmax", lapply(vars, is.na)))
    stopifnot(any(id))
    for (i in seq_along(vars))
    {
        id <- id & (vars[[i]] == levels[[i]][which[i]])
    }
    which(id) ## add more structure to contain which
}


compute.packets <- function(margin.vars = NULL, data, enclos = .GlobalEnv)
    ## margin.vars = list(a = expression(a), b = expression(b))
    ## data = data.frame
{
    if (length(margin.vars) == 0)
    {
        ans <- array(list(TRUE), dim = c(1))
    }
    else 
    {
        mvars.list <-
            lapply(margin.vars, evaluate,
                   data = data, enclos = enclos)
        levels.list <- lapply(mvars.list, levels)
        nlevels.list <- lapply(levels.list, length)
        ans <-
            array(list(NA),
                  dim = unlist(nlevels.list),
                  dimnames = lapply(levels.list, as.character))
        all.combs <- as.matrix(do.call(expand.grid, lapply(nlevels.list, seq_len)))
        for (i in seq_len(nrow(all.combs)))
        {
            ans[[i]] <- compute_packet(mvars.list, levels.list, which = all.combs[i, ])
        }
    }
    ans
}

