library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("One Tail vs Two Tail Test Power: n=200, H_0: p=.5"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("p",
                        "True p:",
                        min = 0,
                        max = 1,
                        value = .5),
            sliderInput("alpha",
                        "α:",
                        min = 0.001,
                        max = .2,
                        value = .05),
            radioButtons( 
              inputId = "testType", 
              label = "Test Type", 
              choices = list( 
                "Lower Tail" = 1, 
                "Two Tail" = 2, 
                "Right Tail" = 3 
              ) 
            ), 
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        ),
        position = c("left", "right"),
        fluid = TRUE
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      RRl=NA; RRr=NA
      if(input$testType==1) RRl=qbinom(input$alpha,200,.5)/200
      if(input$testType==2) RRl=qbinom(input$alpha/2,200,.5)/200
      if(input$testType==2) RRr=qbinom(1-input$alpha/2,200,.5)/200
      if(input$testType==3) RRr=qbinom(1-input$alpha,200,.5)/200
      
      par(mfrow=c(2,1),
          mar = c(0,4,1,1) + 0.1)
      x <- seq(0,200); px <- dbinom(x,200,.5);
      plot(y=px, x=(x/200), type="s", col="gray", xaxt="n", ylab="probability")
      x <- seq(0,200); pxA <- dbinom(x,200,input$p);
      if(!is.na(RRl)) polygon(y=c(pxA[x/200<=RRl],0), x=c((x[x/200<=RRl]/200),RRl)+.0025, col="pink", border=NA)
      if(!is.na(RRr)) polygon(y=c(pxA[x/200>=RRr],0), x=c((x[x/200>=RRr]/200),RRr)+.0025, col="pink", border=NA)
      lines(y=pxA, x=(x/200), type="s")
      abline(v=c(RRl,RRr), col="red")
      text(x=.5, y=dbinom(100,200,.5), "p-hat (H0)", col="gray", pos=1)
      text(x=input$p, y=dbinom(100,200,.5)*.85, "p-hat (true)", col="black", pos=1)
      if(!is.na(RRl)) text(x=RRl, y=dbinom(100,200,.5)/3, pos=2, "Rejection Region")
      if(!is.na(RRr)) text(x=RRr, y=dbinom(100,200,.5)/3, pos=4, "Rejection Region")
      
      
      pAlt <- seq(0,1,by=.005)
      RR <- rep(0, length(pAlt))
      power <- 0
      if(input$testType<=2){
        RR <- pbinom(RRl*200, size=200,p=pAlt)
        power <- pbinom(RRl*200, size=200, p=input$p)
      } 
      if (input$testType>=2){
        RR <- RR + pbinom(RRr*200-1, size=200,p=pAlt, lower.tail=F) 
        power <- power + pbinom(RRr*200-1, size=200, p=input$p, lower.tail=F)
      }

    
      plot(x=pAlt, y=RR, type="l", ylab="rejection rate", xlab="true p", ylim=c(0,1))
      abline(h=input$alpha, col="red",lty=2)
      points(x=input$p, y=power)
      align <- 1
      if(power<.5) align <-3
      text(x=input$p, y=power, paste0("Power=",round(power,3)),pos=align)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
