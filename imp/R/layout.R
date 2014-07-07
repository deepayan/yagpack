
compute_layout_dim <-
    function(layout, nlevels, skip = FALSE)
{
    if (all(skip)) stop("skip cannot be all TRUE")
    number.of.cond <- length(nlevels)
    nplots <- prod(nlevels)
    
    if (!is.numeric(layout))
    {
        layout <- c(0,1,1)
        if (number.of.cond == 1) layout[2] <- nplots
        else
        {
            layout[1] <- nlevels[1]
            layout[2] <- nlevels[2]
        }
        skip <- rep(skip, length = max(layout[1] * layout[2], layout[2]))
        plots.per.page <- length(skip) - length(skip[skip])
        layout[3] <- ceiling(nplots/plots.per.page) # + 1
    }
    else if (length(layout) == 1)
        stop("layout must have at least 2 elements")
    else if (length(layout) == 2)
    {
        if(all(layout < 1))
            stop("at least one element of layout must be positive")
        else if (layout[2]==0) stop("inadmissible value of layout")
        
        skip <- rep(skip, length = max(layout[1] * layout[2], layout[2]))
        plots.per.page <- length(skip) - length(skip[skip])
        layout[3] <- ceiling(nplots / plots.per.page) # + 1 
    }
    else if (length(layout)==3)
    {
        if(layout[1] < 0 || layout[2] < 1 || layout[3] < 1)
            stop("invalid value for layout")
    }
    layout ## length 3 now

    if (layout[1] == 0)
    {
        ddim <- c(1, 1) ## par("din") 
        device.aspect <- ddim[2] / ddim[1]
        panel.aspect <- 1 ## panel.height[[1]] / panel.width[[1]]

        plots.per.page <- layout[2]
        m <- max (1, round(sqrt(layout[2] * device.aspect / panel.aspect)))
        n <- ceiling(plots.per.page/m)
        m <- ceiling(plots.per.page/n)
        layout[1] <- n
        layout[2] <- m
    }
    layout
}



compute.layout <-
    function(layout, nlevels, skip = FALSE)
{
    ldim <- compute_layout_dim(layout = layout, nlevels = nlevels, skip = skip)
    ans <- array(0L, dim = ldim)
    packet.order <- seq_len(min(prod(nlevels), length(ans)))
    panel.order <- which(rep(!skip, length.out = length(ans)))[packet.order]
    ans[panel.order] <- packet.order
    ans
}

