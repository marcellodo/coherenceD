hist.bks <- function(x, w = NULL, neff = NULL, robust=0, ...) {
    #
    # wtd.sd <- function(x, w){
    #     wm <- weighted.mean(x=x, w=w)
    #     sqrt(sum(w*(x-wm)^2)/wm)
    # }
    
    # manage input (weights)
    if (is.null(w)) {
        deff.uw <- 1
        w <- rep(1, length(x))
    }
    else {
        deff.uw <- length(w) * sum(w^2)/(sum(w)^2)
    }
    
    # effective sample size
    if (is.null(neff))  
        nn <- length(x)/deff.uw
    else nn <- neff
    cat("n and eff_n: ", c(length(x), nn), fill=T)
    
    # estimate quartiles
    qq <- wtd.qs(x = x, w = w, prb = c(0.25, 0.5, 0.75), ...)
    
    iqr.x <- unname(qq[3] - qq[1])
    bow <- unname(((qq[3] - qq[2]) - (qq[2] - qq[1]))/iqr.x) # Bowley skewness index
    
    
    # if(ties){
    #     tt <- data.frame(xtabs(ww~xx))
    #     xx <- as.numeric(as.character(tt[,1]))
    #     ww <- tt[,2]
    # }
    # deff.uw <- length(ww) * sum(ww^2)/(sum(ww)^2)
    # qq <- wtd.qs(x = xx, w = ww, prb = c(0.25, 0.5, 0.75))
    
    ###### estimate bin width
    wid <- 2 * iqr.x * 1/(nn^(1/3))
    cat("width: ", wid, fill=T)
    
    ###
    low <- min(x)
    up <- max(x)
    
    cat("min & max: ", c(low, up), fill=T)
    if(robust==1) {
        low <- max( low, qq[2] - 3 *(qq[2] - qq[1])/0.6745 )
        up <-  min( up, qq[2] + 3 *(qq[3] - qq[2])/0.6745 )
        cat("low & up with rob LS est.: ", c(low, up), fill=T)
    }
    if(robust==2) {
        low <- max(low, qq[1] - 1.5*2*(qq[2]-qq[1]) )
        up <-  min( up, qq[3] + 1.5*2*(qq[3]-qq[2]) ) 
        cat("low & up with Box: ", c(low, up), fill=T)
    }
    # cat("low & up bounds: ", c(low, up), fill=T)
    
    # estimate no. of bins 
    bb <- (up - low)/wid
    bins <- ceiling(bb)
    if(bins==1) bins <- 2
    
    # adjust for low and up
    span <- bins*wid
    dd <- span - (up-low)
    low <- low - dd/2
    up <- up + dd/2
    
    cat("mod low & up bounds: ", c(low, up), fill=T)
    #bins <- floor(bins)
    
    cat("bins: ", bins, fill=T)
    
    # breaks
    bk <- seq(from = low, to = up, by = wid)

    
    # summaries
    smr0 <- c(min=min(x), low.x=low, Q1=unname(qq[1]), median=unname(qq[2]),
              Q3=unname(qq[3]), up.x=up, max.x=max(x), IQR=iqr.x, Bow.skew=bow)
    
    smr1 <- c(width=wid, nbins= bins)
    
    list(summaries=smr0, hist.wb=smr1, breaks=bk)
}
