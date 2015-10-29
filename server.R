## server.R for Resilience Ratio
library(shiny)
library(ggplot2)
                                        #
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

threshold <- data.frame((1:10)/10)
colnames(threshold) <- "list"

shinyServer(function(input, output){
    output$ResPlot <- renderPlot({
        resilience <- resrat(input$Q.r.1, input$Q.r.2,
                             input$t.i.1, input$t.i.2,
                             input$t.r.1, input$t.r.2,
                             input$t.h.1, input$t.h.2,
                             threshold)
        q <- ggplot(resilience, aes(x=Threshold, y=Value, group=Profile))
        q <- q + geom_line(aes(color=Profile))
        print(q)
    })
    })
