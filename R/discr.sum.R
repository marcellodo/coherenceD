discr.sum <- function(x, w, breaks, density=FALSE, bw=NULL, kernel=NULL){
    
    low <- min(breaks)
    up <- max(breaks)
    xx <- x
    # trimming
    if(min(x)<low) xx[x<low] <- low
    if(max(x)>up) xx[x>up] <- up
    cxx <- cut(x=xx, breaks=breaks, include.lowest=T)
    fq <- xtabs(w~cxx)
    ps <- prop.table(fq)
    
    df0 <- data.frame(fq)
    df0$relFreq <- c(ps)
    
    m <- length(breaks)
    low.b <- breaks[1:(m-1)] + 0.0000001   
    up.b <- breaks[2:m]
    midp <- (low.b+up.b)/2
    
    df0$low.b <- low.b
    df0$midpoint <- midp
    df0$up.b <- up.b
    
    out <- df0
    
    if(density){

        uni <- function(u) {(abs(u) <= 1) * 0.5} 
        tri <- function(u) {(abs(u) <= 1) * (1 - abs(u))} 
        epa <- function(u) {(abs(u) <= 1) * (3/4)*(1 - u^2) }
        biw <- function(u) {(abs(u) <= 1) * (15/16)*((1 - u^2)^2)}
        gauss <- function(u) {1/sqrt(2*pi) * exp(-(u^2)/2)}
        
        wdt <- breaks[2] - breaks[1]    
        if(is.null(bw)) bw <- wdt/1.25
        if(is.null(kernel)) kernel <- "gauss"
        
        sux <- sort(unique(x))
        dens <- numeric()
        for(j in 1:length(sux)){
            z <- (sux[j] - midp)/bw
            if(tolower(kernel) %in% c("uni","uniform") ) 
                dens[j] <- 1/bw * sum(ps * uni(z))
            if(tolower(kernel) %in% c("tri", "trinagular"))
               dens[j] <- 1/bw * sum(ps * tri(z))
            if(tolower(kernel) %in% c("epa", "epanec", "epanechnikow"))
                dens[j] <- 1/bw * sum(ps * epa(z))
            if(tolower(kernel) %in% c("bi", "biw", "biweight"))
                dens[j] <- 1/bw * sum(ps * biw(z))
            if(tolower(kernel) %in% c("gauss", "gaussian"))
                dens[j] <- 1/bw * sum(ps * gauss(z))
        }
        out <- list(binned.sum=df0, est.dens=data.frame(x=sux, dens))
    }
    out
}