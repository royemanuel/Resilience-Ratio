## Plotting the ratio of R using the step function assumptions and taking into account a margin change
library("ggplot2")
library("reshape2")
library("plyr")
library("dplyr")
resParams <- function(rec, hrz, ti, tr, th, thresh){
    frac <- data.frame(rec / thresh[[1]], hrz / thresh[[1]])
    colnames(frac) <- c("Rec", "Hrz")
    print("this is first")
    print(frac)
    frac[[1]] <- sapply(frac[[1]], function(x) if (x > 1) 1 else x)
    frac[[2]] <- sapply(frac[[2]], function(x) if (x > 1) 1 else x)
    print("after sapply")
    print(frac)
    frac
}
# Find the resilience of a single profile
res <- function(rec, hrz, ti, tr, th, thresh){
    rp <- resParams(rec, hrz, ti, tr, th, thresh)
    (ti + rp[[1]] * tr + rp[[2]] * th)/(ti + tr + th)
}

resrat <- function(rec1, rec2, hrz1, hrz2,
                ti1, ti2, tr1, tr2, th1, th2, thresh){
    res1 <- res(rec1, hrz1, ti1, tr1, th1, thresh)
    res2 <- res(rec2, hrz2, ti2, tr2, th2, thresh)
    Profile1 <- data.frame('Profile1', res1, thresh)
    Profile2  <- data.frame('Profile2', res2, thresh)
    resrat <- res1/res2
    Ratio <- data.frame('Ratio', resrat, thresh)
    varVal <- c("Profile", "Value", "Threshold")
    colnames(Profile1) <- varVal
    colnames(Profile2) <- varVal
    colnames(Ratio) <- varVal
    rbind(Profile1, Profile2, Ratio)
}
## buildProf makes a data frame that contains the information for the
## profiles so you can see what the performance profile looks like
## At this point, the values for before disturbance and after are
## hard coded into this
buildProf <- function(rec, hrz, ti, tr, th){
    Time <- c(0, ti, ti, ti+tr, ti+tr, ti+tr+th)
    Performance <- c(1, 1, rec,rec, hrz, hrz)
    profPlot <- data.frame(Time, Performance)
    colnames(profPlot) <- c("Time", "Performance")
    profPlot
}

threshold <- data.frame((1:10)/10)
colnames(threshold) <- "list"
resilience <- resrat(.5, .5, 1, .75,
            1, 1,
            1, 1,
            1, 1,
            threshold)
q <- ggplot(resilience, aes(x=Threshold, y=Value, group=Profile))
q <- q + geom_line(aes(color=Profile))

perfProfile1 <- buildProf(.5, .75, 1, 1, 1)
m <- ggplot(perfProfile1, aes(Time, Performance))
m <- m +geom_path() + ylim(0,2)
