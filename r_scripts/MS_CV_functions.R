require(leaps)
require(glmnet)
require(MASS)

#This takes an index (row index) and an object coming from regsubsets
#Also the string giving the response variable. It builds the formula
#And returns it as a formual type object
get_model_formula <- function(id, object, outcome){
  # get models data
  models <- summary(object)$which[id,-1]
  # Get model predictors
  predictors <- names(which(models == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}  

#This function takes in 
# a dataset containing all variable columns including the response variable.
# a string giving the response variable intended
# the maximum model size allowed. Currently buggy if this is too low
#It will return a list of models as **formulas** which you can do other stuff with.
bestSubsetSelection <- function(dataset, responseVar, maxModelSize){
  models <- regsubsets(reformulate(".",responseVar), data = dataset, nvmax = maxModelSize);
  modelList <- list("formula")
  nModels <- models$last-models$first+1
  for(i in 1:nModels){
    modelList[[i]] <- get_model_formula(i, models, responseVar)
  }
  return(modelList)  
}

forwardStepwiseSelection <- function(dataset, responseVar){
  predictors = c("1",names(dataset[,-which(names(dataset)==responseVar)]))
  used = 1
  M <- list() #empty list to hold all of the models, except M0
  
  #the null model and its RSS
  M0 <- lm(reformulate(predictors[used], responseVar), data=dataset)
  RSS <- sum(M0$residuals^2)
  
  #the list of formulas to return
  formulas <- list()
  
  for(model in 1:(length(predictors)-1)){
    RSS.best <- RSS
    for(try in predictors[-used]){
      fitModel <- lm(reformulate(c(predictors[used],try), responseVar), data=dataset)
      RSS.new <- sum(fitModel$residuals^2)
      if(RSS.new <= RSS.best){
        new.pred <- try
        RSS.best <- RSS.new
      }
    }
    formulas[[model]] <- reformulate(c(predictors[used],new.pred), responseVar)
    M[[model]] <- lm(formulas[[model]], data=dataset) 
    RSS <- sum(M[[model]]$residuals^2)
    print(paste("adding", new.pred, "; RSS = ", RSS))
    used <- c(used, which(predictors==new.pred))
  }
  return(formulas)  
}

backwardsStepwiseSelection <- function(dataset, responseVar){
  predictors = c("1",names(dataset[,-which(names(dataset)==responseVar)]))
  used = (1:ncol(dataset))[-which(names(dataset)==responseVar)]
  M <- list()
  Mfull <- lm(reformulate(predictors[c(1,used)], responseVar), data=dataset)
  RSS <- sum(Mfull$residuals^2)
  formulas <- list()
  formulas[[length(used)]] <- reformulate(predictors[used], responseVar)
  
  RSS.best <- RSS
  RSS.worst <- sum(lm(reformulate("1",response=responseVar), data=dataset)$residuals^2)
  print(paste("Full Model RSS: ", RSS))
  for(model in (length(used)-1):1){
    RSS.best <- RSS.worst
    for(try in used){
      modelFit <- lm(reformulate(predictors[used[-which(used==try)]], responseVar), data=dataset)
      RSS.new <- sum(modelFit$residuals^2)
      if(RSS.new <= RSS.best){
        new.pred <- try
        RSS.best <- RSS.new
      }
    }
    formulas[[model]] <- reformulate(predictors[used[-which(used==try)]], responseVar)
    M[[model]] <- lm(formulas[[model]], data=dataset) 
    RSS <- sum(M[[model]]$residuals^2)
    print(paste("removing", predictors[new.pred], "; RSS = ", RSS))
    used <- used[-which(used==new.pred)]
  }
  return(formulas)  
}

##----------------------------
## CROSS VALIDATION
##----------------------------

kfoldCV <- function(K, formulas, dataset, responseVar, reps=1){
  m <- length(formulas)
  
  #an empty data frame to store the results of each validation
  results <- data.frame(fold = rep(rep(1:K, each=m),times=reps),
                        model = rep(1:m, K*reps),
                        error = 0,
                        repl = rep(1:reps, each=m*K))    
  for(r in 1:reps){
    #idx is a shuffled vector of row numbers
    idx <- sample(1:nrow(dataset))
    #folds partitions the row indices
    folds <- split(idx, as.factor(1:K))
    for(k in 1:K){
      #split the data into training and testing sets
      training <- dataset[-folds[[k]],]
      testing <- dataset[folds[[k]],]
      #go through each model and estimate MSE
      for(f in 1:m){
        #fit the model to the training data
        fit <- lm(formula = formulas[[f]], data=training)
        #calculate the average squared error on the testing data
        results[results$fold == k & results$model == f & results$repl==r, "error"] <- mean((predict(fit, newdata=testing) - testing[,responseVar])^2)
      }
    }
  }
  #aggregate over each model & replicate, averaging the error
  aggregated <- aggregate(error~model, data=results, FUN="mean")
  #produces a simple line & dot plot
  plot(sqrt(error) ~ model, type="b", data=aggregated, ylab="RMSE")
  #  lines(error ~ model, data=aggregated)
  print(which(aggregated$error == min(aggregated$error)))
  print(formulas[[which(aggregated$error == min(aggregated$error))]])
  return(aggregated)
}

kfoldCV.ridge <- function(K, lambdas, dataset, responseVar, normalize=TRUE){
  m <- length(lambdas)
  
  #idx is a shuffled vector of row numbers
  idx <- sample(1:nrow(dataset))
  #folds partitions the row indices
  folds <- split(idx, as.factor(1:K))
  
  #an empty data frame to store the results of each validation
  results <- data.frame(fold = rep(1:K, rep(m,K)),
                        model = rep(1:m, K),
                        error = 0)    
  for(k in 1:K){
    #split the data into training and testing sets
    training <- dataset[-folds[[k]],]
    testing <- dataset[folds[[k]],]
    #go through each model and estimate MSE
    ridge_models <- lm.ridge(reformulate(".",responseVar), training, lambda=lambdas, normalize=normalize);
    
    Y <- testing[,c(responseVar)] 
    #    X <- cbind( 1, testing[,names(dataset) != responseVar])
    X <- model.matrix(lm(reformulate(".",responseVar), testing))
    for(f in 1:m){
      coeff <- coef(ridge_models)[f,]
      Y.hat <- as.numeric(coeff) %*% as.matrix(t(X))
      #calculate the average squared error on the testing data
      results[results$fold == k & results$model == f, "error"] <- mean((Y-Y.hat)^2)
    }
  }
  #aggregate over each model, averaging the error
  aggregated <- aggregate(error~model, data=results, FUN="mean")
  #produces a simple line & dot plot
  plot(sqrt(error) ~ sqrt(lambdas), type="b", data=aggregated, ylab="RMSE")
  #  lines(error ~ model, data=aggregated)

  print(paste0("Best Lambda: ", lambdas[[which(aggregated$error == min(aggregated$error))]] ))
  
  return(aggregated)
}

kfoldCV.LASSO <- function(K, lambdas, dataset, responseVar){
  m <- length(lambdas)
  
  #idx is a shuffled vector of row numbers
  idx <- sample(1:nrow(dataset))
  #folds partitions the row indices
  folds <- split(idx, as.factor(1:K))
  
  #an empty data frame to store the results of each validation
  results <- data.frame(fold = rep(1:K, rep(m,K)),
                        model = rep(1:m, K),
                        error = 0)    
  for(k in 1:K){
    #split the data into training and testing sets
    training <- dataset[-folds[[k]],]
    testing <- dataset[folds[[k]],]
    #go through each model and estimate MSE
    
    Y <- testing[,c(responseVar)] 
    X <- model.matrix(lm(reformulate(".",responseVar), testing))
    for(f in 1:m){
      mtc_lasso_lambda <- glmnet(training[,names(dataset) != responseVar], training[,c(responseVar)], alpha = 1, lambda=lambdas[f]);
      coeffs <- as.vector(coef(mtc_lasso_lambda))
      y.mtc.predict <- as.vector(coeffs %*% as.matrix(t(X)))
      
      results[results$fold == k & results$model == f, "error"] <- mean((y.mtc.predict-Y)^2)
    }
  }
  #aggregate over each model, averaging the error
  aggregated <- aggregate(error~model, data=results, FUN="mean")
  #produces a simple line & dot plot
  plot(error ~ lambdas, type="b", data=aggregated, ylab="MSE")
  #  print(which(aggregated$error == min(aggregated$error)))
  print(paste("best lambdas:", paste(lambdas[which(aggregated$error == min(aggregated$error))], collapse=",")))
  return(aggregated)
}

