# server.R for Resilience Ratio
library(shiny)
library(ggplot2)
# This is a calculation for two different resilience profiles over
# a performance threshold from 0 -> 1

# this will be a very simple model, where initial performance and
# post recovery performance are always above the margin.
resParams <- function(rec, ti, tr, th, thresh){
    if (rec < thresh){
        rVal <- rec
    } else {
        rVal <- thresh
    }
    rVal / thresh
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

threshold <- (1:1000)/1000
shinyServer(function(input, output){
        output$Res1 <- renderPlot({
        p <- ggplot(data.frame(x=threshold), aes(x)) +
            stat_function(fun=function(x)res(rec=input$Q.r.1,
                              ti=input$t.i.1,
                              tr=input$t.r.1,
                              th=input$t.h.1,
                              thresh=x))
        #p <- p + scale_x_continuous("Functionality after Disturbance") +
            #scale_y_continuous("Resilience Factor")
        print(p)
    })
    output$Res2 <- renderPlot({
        n <- ggplot(data.frame(x=threshold), aes(x)) +
            stat_function(fun=function(x)res(#$init=input$Q.i.2,
                              rec=input$Q.r.2,
                              #horz=input$Q.h.2,
                              ti=input$t.i.2,
                              tr=input$t.r.2,
                              th=input$t.h.2,
                              thresh=x))
        #p <- p + scale_x_continuous("Functionality after Disturbance") +
            #scale_y_continuous("Resilience Factor")
        print(n)
    })
    output$Ratio.v.Threshold <- renderPlot({
        p <- ggplot(data.frame(x=threshold), aes(x)) +
            stat_function(fun=function(x)resrat(#init1=input$Q.i.1,
                              #init2=input$Q.i.2,
                              rec1=input$Q.r.1,
                              rec2=input$Q.r.2,
                              #horz1=input$Q.h.1,
                              #horz2=input$Q.h.2,
                              ti1=input$t.i.1,
                              ti2=input$t.i.2,
                              tr1=input$t.r.1,
                              tr2=input$t.r.2,
                              th1=input$t.h.1,
                              th2=input$t.h.2,
                              thresh=x))
        #p <- p + scale_x_continuous("Functionality after Disturbance") +
            #scale_y_continuous("Resilience Factor")
        print(p)
    })
    })
