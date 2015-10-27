## Plotting the ratio of R using the step function assumptions and taking into account a margin change

resParams <- function(rec, ti, tr, th, thresh){
   frac <- rec / thresh[[1]]
    frac <- sapply(frac, function(x) if (x > 1) 1, else x
    frac
}
# Find the resilience of a single profile
res <- function(rec, ti, tr, th, thresh){
    rp <- resParams(rec, ti, tr, th, thresh)
    (ti + rp * tr + th)/(ti + tr + th)
}

resrat <- function(rec1, rec2,
                ti1, ti2, tr1, tr2, th1, th2, thresh){
    res1 <- res(rec1, ti1, tr1, th1, thresh)
    res2 <- res(rec2, ti2, tr2, th2, thresh)
    res1/res2
}

threshold <- data.frame((1:10)/10)
colnames(threshold) <- "list"


