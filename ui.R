                                        # ui.R
# Need to add the times too!
shinyUI(fluidPage(
    titlePanel("Resilience Ratio (Ayyub 2014)"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("Q.i.1",
                        "Performance Value of Profile 1 Prior to Disturbance",
                        min=0, max=1.5,
                        value=1.0, step=0.05),
            sliderInput("Q.r.1",
                        "Performance Value of Profile 1 Prior During Recovery",
                        min=0, max=1.5,
                        value=0, step=0.05),
            sliderInput("Q.h.1",
                        "Performance Value of Profile 1 After Recovery",
                        min=0, max=1.5,
                        value=0.5, step=0.05),
            sliderInput("Q.i.2",
                        "Performance Value of Profile 2 Prior to Disturbance",
                        min=0, max=1.5, value=1.0, step=0.05),
            sliderInput("Q.r.2",
                        "Performance Value of Profile 2 Prior During Recovery",
                        min=0, max=1.5,
                        value=0, step=0.05),
            sliderInput("Q.h.2",
                        "Performance Value of Profile 2 After Recovery",
                        min=0, max=1.5,
                        value=0.5, step=0.05)
        ),
        mainPanel(
            plotOutput("Ratio.v.Threshold")
        )
    )
))
