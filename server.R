## server.R for Resilience Ratio
library(shiny)
library(ggplot2)
##
## This is a calculation for two different resilience profiles over
## a performance threshold from 0 -> 1

## this will be a very simple model, where initial performance and
## post recovery performance are always above the margin.
## Plotting the ratio of R using the step function assumptions and taking into account a margin change
library("ggplot2")
resParams <- function(rec, ti, tr, th, thresh){
   frac <- rec / thresh[[1]]
    frac <- sapply(frac, function(x) if (x > 1) 1 else x)
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

threshold <- data.frame((1:1000)/1000)
colnames(threshold) <- "list"
buildProf <- function(rec, ti, tr, th){
    Time <- c(0, ti, ti, ti+tr, ti+tr, ti+tr+th)
    Performance <- c(1, 1, rec,rec,1, 1)
    profPlot <- data.frame(Time, Performance)
    colnames(profPlot) <- c("Time", "Performance")
    profPlot
}

axLim <- function(ti1, tr1, th1, ti2, tr2, th2){
    timeProf1 <- ti1 + tr1 + th1
    timeProf2 <- ti2 + tr2 + th2
    if (timeProf1 > timeProf2){
        timeProf1
    } else {
        timeProf2
    }
}


shinyServer(function(input, output){
    output$ResPlot <- renderPlot({
        resilience <- resrat(input$Q.r.1, input$Q.r.2,
                             input$t.i.1, input$t.i.2,
                             input$t.r.1, input$t.r.2,
                             input$t.h.1, input$t.h.2,
                             threshold)
        q <- ggplot(resilience, aes(x=Threshold, y=Value, group=Profile))
        q <- q + geom_line(aes(color=Profile))
        q <- q + geom_vline(xintercept = input$Threshold)
        print(q)
    })
    output$Profile1 <- renderPlot({
        xAxis <- axLim(input$t.i.1, input$t.r.1, input$t.h.1,
                       input$t.i.2, input$t.r.2, input$t.h.2)
        profile1 <- buildProf(input$Q.r.1, input$t.i.1,
                              input$t.r.1, input$t.h.1)
        m <- ggplot(profile1, aes(Time, Performance))
        m <- m +geom_path(color="red") + ylim(0, 1) + xlim(0, xAxis)
        m <- m + geom_hline(yintercept=input$Threshold)
        print(m)
    })
    output$Profile2 <- renderPlot({
        xAxis <- axLim(input$t.i.1, input$t.r.1, input$t.h.1,
                       input$t.i.2, input$t.r.2, input$t.h.2)
        profile2 <- buildProf(input$Q.r.2, input$t.i.2,
                              input$t.r.2, input$t.h.2)
        p <- ggplot(profile2, aes(Time, Performance))
        p <- p +geom_path(color="green4")  + ylim(0, 1) + xlim(0, xAxis)
        p <- p + geom_hline(yintercept=input$Threshold)
        print(p)
    })
    })
