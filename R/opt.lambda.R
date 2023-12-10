opt.lambda <- function(w1, w2){
  n1 <- length(w1)
  n2 <- length(w2)
  N1 <- sum(w1)
  N2 <- sum(w2)
  d1 <- 1 + (sd(w1)/mean(w1))^2
  d2 <- 1 + (sd(w2)/mean(w2))^2
  # 1st approx from knorn & graubard
  k1.1 <- (N1+N2)/(2*N1)*(1 - (d1/n1)/(d1/n1 + d2/n2))
  k2.1 <- (N1+N2)/(2*N2)*(1 - (d2/n2)/(d1/n1 + d2/n2))
  #
  # 2nd approx from knorn & graubard - A
  k1.2a <- (N1+N2)/(2*N1)*( n1/(n1+n2) )
  k2.2a <- (N1+N2)/(2*N2)*( n2/(n1+n2) )
  #
  # 2nd approx from knorn & graubard -B
  k1.2b <- (1 - (d1/n1)/(d1/n1 + d2/n2))
  k2.2b <- (1 - (d2/n2)/(d1/n1 + d2/n2))
  #
  # 3rd approx from knorn & graubard
  k1.3 <- ( n1/(n1+n2) )
  k2.3 <- ( n2/(n1+n2) )
  #
  #
  # O'Muir. & pedlow
  l1 <- (n1/d1) /(n1/d1 + n2/d2) 
  l2 <- (n2/d2) /(n1/d1 + n2/d2) 
  #
  #output
  smr.w <- rbind(n=c(n1,n2),
               N=c(N1,N2),
               Nc= mean(c(N1,N2))/c(N1,N2),
               mean.w=c(mean(w1),mean(w2)),
               sd.w=c(sd(w1),sd(w2)),
               CV.w=c(sd(w1)/mean(w1),sd(w2)/mean(w2)),
               deff.w=c(d1, d2)
  )
  colnames(smr.w) <- c("s1", "s2")
  kk <- rbind(kg1=c(k1.1, k2.1),
              kg2a=c(k1.2a, k2.2a),
              kg2b=c(k1.2b, k2.2b),
              kg3=c(k1.3, k2.3),
              omp=c(l1, l2))
  kk <- cbind(kk, rowSums(kk))
  colnames(kk) <- c("s1", "s2", "tot")
  list(summaries.w=smr.w, lambdas=kk)
  
}