## Plotting the ratio of R using the step function assumptions and taking into account a margin change
library("ggplot2")
library("reshape2")
library("plyr")
library("dplyr")
resParams <- function(rec, hrz, ti, tr, th, thresh){
    frac <- data.frame(rec / thresh[[1]], hrz / thresh[[1]])
    colnames(frac) <- c("Rec", "Hrz")
    frac[[1]] <- sapply(frac[[1]], function(x) if (x > 1) 1 else x)
    frac[[2]] <- sapply(frac[[2]], function(x) if (x > 1) 1 else x)
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
    rbind(Profile1, Profile2) ##, Ratio) Removing the Ratio form the plot
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

threshold <- data.frame((1:100)/100)
colnames(threshold) <- "list"
resilience <- resrat(.5, .1, ## After Failure Levels
                     .75, 1, ## After Recovery Level
                     1, 1,   ## Time of failure
                     1, .5,   ## Time duration of recovery
                     3, 3.5,   ## Time after recovery until planning horizon
                     threshold)
q <- ggplot(resilience, aes(x=Threshold, y=Value, group=Profile))
q <- q + geom_line(aes(linetype=Profile)) + theme_bw()



perfProfile1 <- buildProf(.5, .75, 1, 1, 3)
m <- ggplot(perfProfile1, aes(Time, Performance))
m <- m +geom_path() + ylim(0,2)

perfProfile2 <- buildProf(.1, 1, 1, .5, 3.5)
m <- ggplot(perfProfile1, aes(Time, Performance))
m <- m +geom_path() + ylim(0,2)



tidyProfiles <- function(profileList){
    tdy <- c()
    for (i in 1:length(profileList)){
        tdy <- rbind(tdy, cbind(paste(i), profileList[[i]]))
    }
    colnames(tdy) <-  c("Profile", "Time", "Performance")
    tdy
}

prof <- list(perfProfile1, perfProfile2)
tdProf <- tidyProfiles(prof)

n <- ggplot(tdProf, aes(x=Time, y=Performance, group=Profile))
n <- n + geom_line(aes(linetype=Profile)) + theme_bw()
