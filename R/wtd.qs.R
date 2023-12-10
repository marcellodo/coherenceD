wtd.qs <- function(x, w, prb, method="interp", ties=FALSE){
    if(any(duplicated(prb))) stop("The prb argument included duplicated probs")
    prb <- sort(prb)
    prb0 <- prb
    if(prb[1]==0) prb <- prb[-1]
    if(prb[length(prb)]==1) prb <- prb[-length(prb)]
    if(!ties){
        ordx <- order(x, decreasing = FALSE)
        sx <- x[ordx]
        sw <- w[ordx]
        csw <- cumsum(sw)/sum(w)
        nq <- length(prb)
        qq <- numeric()
        for(k in 1:nq){
            q <- prb[k]
            if(method=="interp" | method=="interpolate"){
                j <- sum(csw <= q)
                qq[k] <- sx[j] +(q-csw[j])/(csw[j+1]-csw[j])*(sx[j+1]-sx[j])
            }
            if(method=="base" | method=="basic"){
                if(any(csw==q)){
                    pos <- which(csw==q)
                    qq[k] <- (1/2)*(sx[pos]+sx[pos+1])
                }
                else {
                    j <- sum(csw <= q)
                    qq[k] <- sx[j]
                }
            }
        }
    }  
    else{
        dd <- data.frame(xtabs(w~x))
         
        sx <- as.numeric(as.character(dd[,1]))
        sw <- dd[,"Freq"]
        csw <- cumsum(sw)/sum(w)
        nq <- length(prb)
        qq <- numeric()
        for(k in 1:nq){
            q <- prb[k]
            if(method=="interp" | method=="interpolate"){
                j <- sum(csw <= q)
                qq[k] <- sx[j] +(q-csw[j])/(csw[j+1]-csw[j])*(sx[j+1]-sx[j])
            }
            if(method=="base" | method=="basic"){
                if(any(csw==q)){
                    pos <- which(csw==q)
                    qq[k] <- (1/2)*(sx[pos]+sx[pos+1])
                }
                else {
                    j <- sum(csw <= q)
                    qq[k] <- sx[j]
                }
            }
        }
    }
    if(prb0[1]==0) qq <- c(min(x), qq)
    if(prb0[length(prb0)]==1) qq <- c(qq, max(x))
    names(qq) <- paste0("P",prb0*100)  
    qq
}