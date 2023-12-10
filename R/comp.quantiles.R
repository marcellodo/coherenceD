comp.quantiles <- function (x1, x2, w1 = NULL, 
          w2 = NULL, pctp=c(0.25,0.50,0.75), 
          ref = FALSE, lambda1=NULL, ...) 
{

    #deff.uw <- length(ww) * sum(ww^2)/(sum(ww)^2)
    
    n1 <- length(x1)
    n2 <- length(x2)
    
    # deff.uw1 <- length(w1) * sum(w1^2)/(sum(w1)^2)
    # deff.uwB <- length(wB) * sum(wB^2)/(sum(wB)^2)
    
    # set probs according to sample size
    if(n1<20) stop("too few data in x1 to estimate quartiles")
    if(n2<20) stop("too few data in x2 to estimate quartiles")
    
    n <- min(n1, n2)
    
    if (n <= 80){
        warning("Having min(n1,n2)<=80 it is preferable to estimate just quartiles")
    } 
        
    if (n > 80 & n <= 180) 
        #pctp <- seq(from = 0.2, to = 0.8, by = 0.2) #P20,P40...
        warning("Having 80<min(n1,n2)<=180 it is preferable to estimate just quintiles")
    if (n > 180 & n<=400) 
        warning("Having 180<min(n1,n2)<=400 it is preferable to estimate just deciles")
        # pctp <- seq(from = 0.1, to = 0.9, by = 0.1) # deciles
    if (n > 400) 
        # pctp <- seq(from = 0.05, to = 0.95, by = 0.05) # P5,P10,P15,...
        warning("Having min(n1,n2)>400 it is possible to estimate P5,P10,P15,...")    
    
    # estimate quantiles
    if (!is.null(w1)) {
        qq.1 <- wtd.qs(x1, w = w1, prb = pctp, ...)
    }
    else qq.1 <- quantile(x = x1, probs = pctp)
    
    if (!is.null(w2)) {
        qq.2 <- wtd.qs(x2, w = w2, prb = pctp, ...)
    }
    else qq.2 <- quantile(x = x2, probs = pctp)
    # check ref
    qq.1 <- unname(qq.1)
    qq.2 <- unname(qq.2)
    if(ref){
        qq.ref <- qq.2
        out <- data.frame(Pct=paste0("P", pctp*100), 
                          qqs.1=qq.1, qqs.2=qq.2, qqs.ref=qq.ref, 
                          diff=qq.1-qq.ref, rel.diff=(qq.1-qq.ref)/qq.ref)
    }
    else{
       xx <- c(x1, x2)
       ww <- c(lambda1*w1, (1-lambda1)*w2)
       qq.ref <- wtd.qs(xx, w = ww, prb = pctp, ...)
       out <- data.frame(Pct=paste0("P", pctp*100), 
                         qqs.1=qq.1, qqs.2=qq.2, qqs.ref=qq.ref, 
                         diff.1=qq.1-qq.ref, rel.diff.1=(qq.1-qq.ref)/qq.ref,
                         diff.2=qq.2-qq.ref, rel.diff.2=(qq.2-qq.ref)/qq.ref)
       
    }
    # output
    out
}