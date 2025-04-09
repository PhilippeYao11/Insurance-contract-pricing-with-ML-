#----------------------------------------------------------------------#
#             Fonction pour les pourcentages dans camembert            #
#----------------------------------------------------------------------#

text_pie = function(vector,labels=c(),cex=1) {
  vector = vector/sum(vector)*2*pi
  temp = c()
  j = 0
  l = 0
  for (i in 1:length(vector)) {
    k = vector[i]/2        
    j =  j+l+k
    l = k
    text(cos(j)/2,sin(j)/2,labels[i],cex=cex)
  }
  vector = temp
}

#----------------------------------------------------------------------#
#                  Fonction de preparation des bases                   #
#----------------------------------------------------------------------#

mise_en_forme <- function(data, is.classif = T, is.train = T) {
  qualis <- c(1,2,3,4,5,7,10,11,12)
  quantis <- c(6,8,9,13)
  data[, qualis] <- lapply(data[, qualis], as.factor)
  data[, quantis] <- lapply(data[, quantis], as.numeric)
  
  if(length(levels(data$gender)) == 3){
    levels(data$gender) <- factor(c("Female","Male","Male"))
  }
  
  # base classification
  if(is.classif){
    #train 
    if(is.train){
      #Creation de la variable sinistre : 1 si y'a un sinistre 0 sinon
      data$sinistre <- as.factor(ifelse(data$claimValue > 0 , 1 , 0))
      data <- data[,-c(1,11,14)]
      return(data)
    }
    #test
    else{
      data <- data[,-c(1,11)]
      return(data)
    }
  }
  # base regression
  else{
    #train 
    if(is.train){
      data$claimValue <- as.numeric(data$claimValue)
      data <- data[data$claimValue > 0,-c(1,8,11)] # on elimine id, bonus et subregion et montants negatifs
      return(data)
    }
    #test
    else{
      data <- data[,-c(1,8,11)]
      return(data)
    }
  }
  
}

#----------------------------------------------------------------------#
#                    Fonction qui calcul des probas                    #
#----------------------------------------------------------------------#

probas <- function(train, test) {
  
  # parametrages
  labels <- train$sinistre
  new_tr <- model.matrix(~ . + 0, data = train[, -12])  
  new_ts <- model.matrix(~ . + 0, data = test)  
  labels <- as.numeric(labels) - 1
  train.matrix <- xgb.DMatrix(data = new_tr, label = labels) 
  
  # hyper Paramètres de xgboost
  xgb_params <- list(booster = "gbtree", objective = "binary:logistic",
                     eta = 0.01, gamma = 0, max_depth = 5, min_child_weight = 1, 
                     subsample = 1, colsample_bytree = 1,
                     lambda = 0, alpha = 0,
                     eval_metric = "error")
  
  # Modèle final
  final_model <- xgboost(data = train.matrix, nrounds = 646, params = xgb_params)
  
  # Prédiction
  
  test.matrix <- xgb.DMatrix(data = new_ts)
  predictions <- predict(final_model, test.matrix)
  return(predictions)
}

#----------------------------------------------------------------------#
#                Fonction qui calcul les couts moyens                  #
#----------------------------------------------------------------------#


cout_moyen <- function(Test.estime,Test){
  
  # parametrage
  test.matrix <- model.matrix(log(claimValue)~., data = Test.estime)[,-1]
  x.reg <- model.matrix(log(claimValue)~., data = Test.estime)[,-1]
  y.reg <- log(Test.estime$claimValue)
  
  # Ridge
  reg.ridge_inital <- glmnet(x.reg, y.reg, family = "gaussian", alpha=0)
  
  # meilleur lambda
  ridge.cv <- cv.glmnet(x = x.reg, y = y.reg, lambda = reg.ridge_inital$lambda, type.measure = "mse", nfolds = 70, alpha = 0)
  ridge.cv$lambda.min
  
  # model final
  reg.ridge_final <- glmnet(x = x.reg, y = y.reg, family = "gaussian", alpha = 0, 
                            lambda = ridge.cv$lambda.min)
  
  #predictions
  test.matrix <- model.matrix(log(claimValue)~., data = Test.estime)[,-1]
  averageCost <- predict.glmnet(reg.ridge_final,  newx = test.matrix)
  
  # cout moyen
  Test.estime$claimValue<-averageCost
  Test.estime <- transform(Test.estime, claimValue = exp(claimValue))
  Test.estime$id<-Test$id
  Test.estime <- Test.estime[, c("id", setdiff(names(Test.estime), "id"))]
  Test.estime <- Test.estime %>% rename("averageCost" = "claimValue")
  cout_moyen<-Test.estime[,c("id","averageCost")]
  
  return(cout_moyen)
}


#----------------------------------------------------------------------#
#            Fonction de mise en forme : exportation                   #
#----------------------------------------------------------------------#

exportation <- function(proba, coutmoyen, test){
  premium <- cbind(as.factor(test$id),proba,coutmoyen[,2])
  colnames(premium) <- c("id","probability","averageCost")
  as.data.frame(premium)
  write.csv(premium, file = "premium_12.csv", row.names = FALSE)
}
