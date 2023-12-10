setwd("D:/AAA-Lavoro/Progetti-GdL/New quality indicators/2023-12 uRos/R")
setwd("G:/AAA-Lavoro/Progetti-GdL/New quality indicators/2023-12 uRos/")


data(samp.A, package = "StatMatch")
data(samp.B, package = "StatMatch")

t.edu.A <- xtabs(ww~edu7, data=samp.A)
t.edu.B <- xtabs(ww~edu7, data=samp.B)
t.edu.B 
source("comp.tables.R")
comp.tables(p1 = t.edu.A, p2 = t.edu.B, 
            ref = TRUE) # t.edu.B is the reference one

source("opt.lambda.R")
opt.lambda(w1 = samp.A$ww, w2 = samp.B$ww)

source("wtd.qs.R")
source("smrs.R")

smrs(x=samp.A$n.income, weights = samp.A$ww, p = 0.10)

source("comp.quantiles.R")
comp.quantiles(x1 = samp.A$age, x2 = samp.B$age, w1 = samp.A$ww, w2 = samp.B$ww, pctp = seq(0.1,0.9,0.1), ref = TRUE)

source("hist.bks.R")
bk.0 <- hist.bks(x = samp.A$n.income, w = samp.A$ww, neff = NULL, robust = 0, nbins = "FD")
bk.0$hist.wb
bk.0 <- hist.bks(x = samp.A$n.income, w = samp.A$ww, neff = NULL, robust = 0, nbins = "kde1")
bk.0$hist.wb

bk.0 <- hist.bks(x = samp.A$n.income, w = samp.A$ww, neff = NULL, robust = 1, nbins = "FD")

source("discr.sum.R")
bk.0 <- hist.bks(x = samp.A$n.income, w = samp.A$ww, neff = NULL, robust = 1, nbins = "FD")
oo <- discr.sum(x=samp.A$n.income, w=samp.A$ww, breaks = bk.0$breaks, density = TRUE)
head(oo$binned.sum, 4)
head(oo$est.dens, 4)
