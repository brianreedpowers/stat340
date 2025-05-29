library(shiny)
library(tidyverse)
library(gridExtra)

range = 6
expd = 0.005

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Normal CDF, PDF, & inverse CDF"),

    # Sidebar 
    sidebarLayout(
      inputPanel(
        sliderInput("mu", label="Mean",min=-range,max=range,value=0,step=0.1),
        sliderInput("sigma2", label="Variance",min=.01,max=10,value=1,step=.01),
        sliderInput("x", label="x",min=-range,max=range,value=0,step=0.01)
      ),

      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("mainPlot")
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$mainPlot <- renderPlot({
    g1 <- ggplot()+stat_function(fun=~{dnorm(.x,input$mu,input$sigma2)},n=1001)+
      scale_x_continuous(expand=c(0,0),limits=c(-range,range)) + 
      scale_y_continuous(expand=c(expd,expd)) + 
      geom_point(aes(x=input$x,y=dnorm(input$x,input$mu,input$sigma2)),color="red",size=4) + 
      stat_function(fun=~{dnorm(.x,input$mu,input$sigma2)},n=1001,geom="area",xlim=c(-100,input$x),fill="red",alpha=.5)+
      labs(title="Probability density function (PDF)",x="x",y="Density")
    g2 <- ggplot()+geom_function(fun=~{pnorm(.x,input$mu,input$sigma2)},n=1001)+
      geom_point(aes(x=input$x,y=pnorm(input$x,input$mu,input$sigma2)),color="red",size=4) +
      scale_x_continuous(expand=c(expd,expd),limits=c(-range,range)) + scale_y_continuous(expand=c(expd,0)) + labs(title="Cumulative density function (CDF)",x="x",y="Probability")
    g3 <- ggplot()+geom_function(fun=~{qnorm(.x,input$mu,input$sigma2)},n=1001)+
      geom_point(aes(x=pnorm(input$x,input$mu,input$sigma2),y=input$x),color="red",size=4) +
      scale_x_continuous(expand=c(expd,expd),limits=c(0,1)) + scale_y_continuous(expand=c(expd,expd),limits=c(-range,range)) + labs(title="Inverse CDF",x="Probability",y="x")
          grid.arrange(g1,g2,g3,nrow=1)
    },height=300)
}

# Run the application 
shinyApp(ui = ui, server = server)
