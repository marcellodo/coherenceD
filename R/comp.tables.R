comp.tables <- 
function (p1, p2, lambda1=NULL, ref = TRUE) 
{
    # check
    if (is.null(lambda1) && !ref) 
        stop("If p2 is not the reference distribution (ref=FALSE) please \n provide the lambda1 argument")
    if (sum(p1) > 1) 
        p1 <- prop.table(p1)
    if (sum(p2) > 1) 
        p2 <- prop.table(p2)
    #
    # calculate distance
    tvd <- 0.5 * sum(abs(p1 - p2))
    #ov <- 1 - tvd
    bhatt <- sum(sqrt(p1 * p2))
    #hell <- sqrt(1 - bhatt)
    dd <- c(tvd = tvd, overlap = 1-tvd, Bhatt = bhatt, Hell = sqrt(1 - bhatt))
    #
    #
    if (!ref) {
        p0 <- lambda1*p1 + (1-lambda1)*p2
        tvd1 <- 0.5 * sum(abs(p1 - p0))
        tvd1 <- 0.5 * sum(abs(p2 - p0))
        # ov1 <- 1 - tvd1
        # ov2 <- 1 - tvd2
        bhatt1 <- sum(sqrt(p1 * p0))
        bhatt2 <- sum(sqrt(p2 * p0))
        # hell1 <- sqrt(1 - bhatt1)
        # hell2 <- sqrt(1 - bhatt2)
        dd1 <- c(tvd = tvd1, overlap = 1-tvd1, Bhatt = bhatt1, Hell = sqrt(1 - bhatt1))
        dd2 <- c(tvd = tvd2, overlap = 1-tvd2, Bhatt = bhatt2, Hell = sqrt(1 - bhatt2))
        dd <- rbind(p1.to.ref=dd1,
                    p2.to.ref=dd2,
                    p1.to.p2=dd)
    }
    dd
}