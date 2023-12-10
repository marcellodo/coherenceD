smrs <- function (x, p=0.1, weights = NULL, ...) 
{
    # check
    if(p>=0.5) stop("p must be <0.5")
    
    # probs for quantiles 
    ps <- sort(c(p, 0.25, 0.5, 0.75, (1-p)))
    if(p==0.25) ps <- c(0.25, 0.5, 0.75)
    
    # data checks
    y <- x[!is.na(x)]
    if (!is.null(weights)) 
        ww <- weights[!is.na(x)]
    
    # estimate quantiles 
    if (is.null(weights)){
        qq <- quantile(x = y, probs = ps)
        mm <- mean(y)
        mdn <- unname(qq["50%"])
        iqr <- unname(qq["75%"] - qq["25%"])
    } 
    else {
        qq <- wtd.qs(x = y, w = ww, prb = ps, ...)
        mm <- weighted.mean(x = y, w = ww)
        mdn <- unname(qq["P50"])
        iqr <- unname(qq["P75"] - qq["P25"])
    }
    
    # quantile based location and scale
    
    
    if(p==0.25){ 
        shp <- 1
        sk <- (qq[1] - 2 * mdn + qq[3])/iqr
        out1 <- c(Min=min(y), Q1=unname(qq[1]), 
                  Median=mdn, Mean=mm,
                  Q3=unname(qq[3]), Max=max(y))
    }
    else {
        shp <- (qq[5] - qq[1])/iqr
        sk <- (qq[1] - 2 * mdn + qq[5])/(qq[5] - qq[1])
        out1 <- c(Min=min(y), P1=qq[1], Q1=unname(qq[2]), 
                  Median=mdn, Mean=mm,
                  Q3=unname(qq[4]), p2=qq[5], Max=max(y))
        names(out1)[2] <- paste0("P",as.character(p*100))
        names(out1)[7] <- paste0("P",as.character((1-p)*100))
    }
    
    list(summary=out1, 
         qq.based=c(p=p, IQR=iqr, shape=unname(shp), skewness=unname(sk))
    )

}
