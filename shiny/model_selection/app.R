library(shiny)

which.best <- function(x){
  if(criterion=="adjr2") return (which.max(x)) else return (which.min(x))
}

responseVar <- "mpg"
predictors <- names(mtcars)[-which(names(mtcars)==responseVar)]
nFormulas <- 2^length(predictors)

number2binary = function(number, noBits) {
  binary_vector = rev(as.numeric(intToBits(number)))
  if(missing(noBits)) {
    return(binary_vector)
  } else {
    binary_vector[-(1:(length(binary_vector) - noBits))]
  }
}

allModels <- data.frame(i=0:(nFormulas-1))
for(predictor in predictors){
  newdf <- data.frame(x=numeric(nFormulas)); names(newdf) <- predictor
  allModels <- cbind(allModels, newdf)
}
allModels$size <- 0
allModels$aic <- 0
allModels$bic <- 0
allModels$adjr2 <- 0
for(i in allModels$i){
  allModels[i+1, 2:11] <- number2binary(i, length(predictors))
  if(i==0){
    f <- reformulate("1", responseVar)
  } else {
    f <- reformulate(predictors[allModels[i+1,2:11]==1], responseVar)
  }
  allModels[i+1, "size"] <- sum(allModels[i+1, 2:11])+1
  fit <- lm(f, data=mtcars)
  allModels[i+1, "adjr2"] <- summary(fit)$adj.r.squared
  allModels[i+1, "aic"] <- AIC(fit)
  allModels[i+1, "bic"] <- -2*as.numeric(logLik(fit)) + log(nrow(mtcars))*(allModels[i+1, "size"]+1) 
}

edges <- data.frame(from=numeric(), to=numeric())
for(i in allModels[-nrow(allModels),"i"]){
  to <- i + 2^(which(rev(allModels[i,2:11]==0))-1)
  from <- rep(i, length(to))
  edges <- rbind(edges, data.frame(from,to))
}

criterion <- "adjr2"


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Model Selection Visualization"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput(
            "criterion",
            "Compare Models by",
            c("adjr2","aic","bic"),
            selected = "aic"),
          selectInput(
            "method",
            "Model Selection Method",
            c("best subset","forward stepwise","backward stepwise"),
            selected = "best subset"),
          p("In the mtcars dataset we consider linear models predicting ", strong("mpg"), " from the 10 other predictors."),
          p("The available predictors are ", strong("cyl, disp, hp, drat, wt, qsec, vs, am, gear,"),"and ",strong("carb"),"."),
          p("Model size can range from 1 (a model with only an intercept) to 11 (the full model).")
          
          ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("mainPlot")
        ),
        position = c("left", "right"),
        fluid = TRUE
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$mainPlot <- renderPlot({
      criterion <<- input$criterion
      allModels$compare <- allModels[,criterion]
      
      if(input$method=="best subset"){
        bestSubset <- numeric()
        for(size in unique(allModels$size)){
          bestSubset[size] <- allModels[allModels$size==size, "i"][which.best(allModels[allModels$size==size, criterion])]+1
        }
        plot(compare~size, data=allModels, main=paste("Best Subset by",criterion), ylab=criterion)
        points(compare~size, data=allModels[bestSubset,], col="magenta", pch=16)
        points(compare~size, data=allModels[bestSubset[which.best(allModels[bestSubset,criterion])],], col="magenta", cex=2, lwd=2)
      
      } else if(input$method=="forward stepwise"){
        bestForward <- numeric()
        for(size in unique(allModels$size)){
          if(size==1) bestForward[size] <- 1
          else {
            bestForward[size] <- allModels[edges[edges$from==bestForward[size-1],"to"] , "i"][which.best(allModels[edges[edges$from==bestForward[size-1],"to"], criterion])]+1
          }
        }
        plot(compare~size, data=allModels, main=paste("Forward Stepwise by",criterion), ylab=criterion)
        segments(allModels[edges$from,"size"],allModels[edges$from,criterion],allModels[edges$to,"size"],allModels[edges$to,criterion], col="gray")
        points(compare~size, data=allModels)
        edgesForward <- subset(edges, from %in% bestForward)
        segments(allModels[edgesForward$from,"size"],allModels[edgesForward$from,criterion],allModels[edgesForward$to,"size"],allModels[edgesForward$to,criterion], col="red")
        points(compare~size, data=allModels[bestForward,], col="red", pch=16)
        points(compare~size, data=allModels[bestForward[which.best(allModels[bestForward,criterion])],], col="red", cex=2, lwd=2)
        
      } else {
        bestBackward <- numeric(max(allModels$size))
        for(size in rev(unique(allModels$size))){
          if(size==11) {bestBackward[size] <- 1024}
          else {
            bestBackward[size] <- allModels[edges[edges$to==bestBackward[size+1],"from"] , "i"][which.best(allModels[edges[edges$to==bestBackward[size+1],"from"], criterion])]+1
          }
        }
        plot(compare~size, data=allModels, main=paste("Backward Stepwise by",criterion), ylab=criterion)
        segments(allModels[edges$from,"size"],allModels[edges$from,criterion],allModels[edges$to,"size"],allModels[edges$to,criterion], col="gray")
        points(compare~size, data=allModels)
        edgesBackward <- subset(edges, to %in% bestBackward)
        segments(allModels[edgesBackward$from,"size"],allModels[edgesBackward$from,criterion],allModels[edgesBackward$to,"size"],allModels[edgesBackward$to,criterion], col="blue")
        points(compare~size, data=allModels[bestBackward,], col="blue", pch=16)
        points(compare~size, data=allModels[bestBackward[which.best(allModels[bestBackward,criterion])],], col="blue", cex=2, lwd=2)
        
    }
            

  })
}

# Run the application 
shinyApp(ui = ui, server = server)
