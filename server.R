# server.R for Resilience Ratio
library(shiny)
library(ggplot2)
# This is a calculation for two different resilience profiles over
# a performance threshold from 0 -> 1
res1 <- function(Q.i.1, Q.r.1, Q.h.1, Q.i.2, Q.r.2, Q.h.2){
    
}
threshold <- 1:1000/1000
shinyServer(function(input, output){
    output$Ratio.v.Threshold <- renderPlot({
        p <- ggplot(data.frame(x=c(0, 1.5)), aes(x)) +
            stat_function(fun=function(x)rho(slackTime=input$slackTime,
                              tStar=input$tStar,
                              tF=input$tF,
                              a=input$a,
                              Fd=x,
                              Fi=input$Fi,
                              Fr=input$Fr))
        p <- p + scale_x_continuous("Functionality after Disturbance") +
            scale_y_continuous("Resilience Factor")
        print(p)
    })
    output$a.v.rho <- renderPlot({
        q <- ggplot(data.frame(x=c(0,1.5)), aes(x)) +
            stat_function(fun=function(x)rho(slackTime=input$slackTime,
                              tStar=input$tStar,
                              tF=input$tF,
                              a=x,
                              Fd=input$Fd,
                              Fi=input$Fi,
                              Fr=input$Fr))
        q <- q + scale_x_continuous("Degradation Factor") +
            scale_y_continuous("Rho")
        print(q)
    })
    }
            )
