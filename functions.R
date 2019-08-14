# setwd("~/Dropbox/Tjasulka/MAGISTERIJ/Bayes_2018/Trading_Bitcoin")

#comment: price between two seq days is always different (we always have increas/decrease in price)

funk_when_trade <- function(m){
  M2 <- apply(m,2,function(x){
    wh <- which(x!=0)
    cs2 <- x[wh]
    cs2[x[wh]==1 & lag(cs2,1)==1] <- 0
    cs2[x[wh]==-1 & lag(cs2,1)==-1] <- 0
    cs2[1] <- ifelse(cs2[1] == -1, 0, cs2[1]) #at the beginning we can only buy or wait
    x[wh] <- cs2
    x
  })
  return(M2)
}


func_TrVaTe <- function(modelTrValTe, #train, validation or test
                        model, #"ARMA_bayes", "ARMA_frekv", "LR_bayes", "LR_frekv", "RF_frekv"
                        d_range, #vector of window parameters
                        dd_range, #vector of train set lengths parameters (LR, RF)
                        ARMApq, #list of model names for ARMA ("AR_1","AR_2","MA_1","MA_2","ARMA_1_1","ARMA_1_2","ARMA_2_1")
                        gamma_1_range, #vector of trust parameters
                        seed = 123, #set.seed
                        n.iter=n.iter, n.warmup=n.warmup, delta=delta, n.chains=n.chains, #adapt for ARMA models (MA_2 + )
                        a0=a0, train.time0=train.time0, #from source file
                        val.time=val.time, pred.time=pred.time){ #from source file for prediction
  
  ##########################################################################
  ##example##
  # modelTrValTe = "train"
  # model <- "LR_frekv"
  # dd_range <-  c(50,80)
  # d_range <- c(30,60,90)
  # gamma_1_range <- c(.1,.2,.5)
  # i <- 1
  # seed <- 123
  # ARMApq <- list("AR_2")
  ##end example##
  ##########################################################################
  set.seed(seed)
  list.1 <- list()
  
  if(modelTrValTe == "validation"){
    a0 <- a0 + train.time0
    train.time0 <- val.time
  }else if(modelTrValTe == "test"){
    a0 <- a0 + train.time0 + val.time
    train.time0 <- pred.time
  }
  
  if(model %in% c("RF_frekv","LR_bayes","LR_frekv")){
    dd_range <- dd_range + 1 #train set of length dd_range, test set of length 1
    t0 <- a0 # time 0 
    t_max <- t0 + train.time0 #the same time for train the model for ARMA, LR, RF
    
    t_range <- c((t0+1):(t_max)) #train day times
    dd_vec <- rep(dd_range,each=length(d_range))
    d_vec <- rep(d_range,length(dd_range))
    
    time_min <- t0-max(dd_range)-max(d_range)+1
    xd <- price_all[time_min:t_max] #all data that we need; from time_min to time_max(=t_max)
    yd <- diff(xd, lag = 1)
    zd <- sign(yd)
    
    ######################################
    predicted.range.parameters <- c()
    for(i in 1:(length(dd_range)*length(d_range))){
      d.i <- d_vec[i]
      dd.i <- dd_vec[i]
      xd.i <- xd[(1+max(dd_range)-dd.i+max(d_range)-d.i):(length(xd))] #all data that we need; from time_min to time_max(=t_max)
      yd.i <- yd[(1+max(dd_range)-dd.i+max(d_range)-d.i):(length(yd))]
      zd.i <- zd[(1+max(dd_range)-dd.i+max(d_range)-d.i):(length(zd))]
      
      M.0 <- tail(rollapply(zd.i,width=d.i+1,FUN=identity),dd.i+train.time0-1)
      LastZ <- M.0[,d.i,drop=FALSE] # last change in price: 1st feature
      DistZ <- rowSums(M.0[,1:d.i,drop=FALSE]==-1)/d.i
      TrendZ.0 <- (M.0[,-ncol(M.0)] == M.0[,ncol(M.0)-1])
      TrendZ.0 <- TrendZ.0[,ncol(TrendZ.0):1]
      TrendZ <- apply(TrendZ.0, 1, function(x){
        out <- ifelse(sum(!x) == 0, d.i, which(x == FALSE)[1]-1)
        out
      })
      RespZ <- M.0[,d.i+1,drop=FALSE]
      M.features <- cbind(LastZ,DistZ,TrendZ,RespZ)
      data.frame.M.features <- data.frame(M.features)
      names(data.frame.M.features) <- c("LastZ","DistZ","TrendZ","RespZ")
      data.frame.M.features$LastZ <- factor(data.frame.M.features$LastZ, labels = c(0,1))
      data.frame.M.features$RespZ <- factor(data.frame.M.features$RespZ, labels = c(0,1))
      
      indexes <- rollapply(1:(train.time0 + dd.i - 1), width=dd.i, FUN=identity)
      if(model == "RF_frekv"){
        predicted <- apply(indexes, 1, function(x) { #vector of predictions
          
          # print(data.frame.M.features[x[1:(dd.i-1)],])
          RF <- randomForest(RespZ ~ .,
                             data = data.frame.M.features[x[1:(dd.i-1)],],
                             ntree = n.trees.rf)
          pred.rf <- predict(RF, data.frame.M.features[x[dd.i],-4], type = "prob")[2] #vector of probabilities for increase in price
          pred.rf
        })
      }else if(model == "LR_bayes"){
        model.stan <- model.stan.list[["LR"]]
        predicted0 <- apply(indexes, 1, function(x) { #stan output
          train.set <- data.frame.M.features[x[1:(dd.i-1)],]
          test.set <- data.frame.M.features[x[dd.i],-4,drop=FALSE]
          stan_data <- list(N = nrow(train.set),
                            x1 = c(0,1)[train.set[,"LastZ"]],
                            x2 = train.set[,"DistZ"],
                            x3 = train.set[,"TrendZ"],
                            z = c(0,1)[train.set[,"RespZ"]],
                            N_test = nrow(test.set),
                            x1_test = c(0,1)[test.set[,"LastZ"]],
                            x2_test = test.set[,"DistZ"],
                            x3_test = test.set[,"TrendZ"])
          y_fit <- sampling(model.stan,
                            data = stan_data,
                            iter = n.iter, warmup = n.warmup,
                            chains = n.chains,
                            seed = seed,
                            verbose = FALSE,
                            refresh = -1,
                            control = list(adapt_delta = delta))
          y_fit
          
        })
        predicted <- sapply(predicted0,function(x){ #vector of probabilities for increase in price
          summary(x,pars=c("z_test"))$summary[,"mean"]
        })
        list.1[[paste("TrainDays=",train.time0,"_dd=",dd.i,"_d=",d.i,sep="")]] <- predicted0
        
      }else if(model == "LR_frekv"){
        predicted <- apply(indexes, 1, function(x) { #vector of predictions; 
          #no regularization
          #TODO ridge regression
          LR <-  glm(RespZ ~ ., data = data.frame.M.features[x[1:(dd.i-1)],],
                     family=binomial(link="logit"))
          pred.lr <- predict(LR, data.frame.M.features[x[dd.i],,drop=FALSE],type="response", n.ahead=1)
          pred.lr
          
          # # glmnet package
          # data.frame.M.features$RespZ <- as.numeric(as.character(data.frame.M.features$RespZ))
          # 
          # x_train <- model.matrix(RespZ ~ ., data = data.frame.M.features[x[1:(dd.i-1)],])
          # y_train <- factor(data.frame.M.features[x[1:(dd.i-1)],"RespZ"],levels=c(0,1))
          # LR <- cv.glmnet(x_train,y_train,alpha=0,family="binomial",type.measure="mse")
          # lambda.1se <- LR$lambda.1se
          # 
          # x_test <- model.matrix(RespZ ~ ., data = data.frame.M.features[x[dd.i],,drop=FALSE])
          # pred.lr <- predict(LR, x_test, s=lambda.1se, type="response", n.ahead=1) #probability to go up
          # pred.lr
          
        })
      }
      
      predicted.range.parameters <- cbind(predicted.range.parameters, predicted)
      colnames(predicted.range.parameters)[i] <- paste(model,dd.i-1,d.i,sep="_")
    }
  }else if(model %in% c("ARMA_bayes","ARMA_frekv")){
    #1. napovedi
    pq_range <- unlist(ARMApq)
    
    t0 <- a0 # time 0 
    t_max <- t0 + train.time0 #the same time for train the model for ARMA, LR, RF
    
    t_range <- c((t0+1):(t_max)) #train day times
    
    pq_vec <- rep(pq_range,each=length(d_range))
    d_vec <- rep(d_range,length(pq_range))
    
    time_min <- t0-max(d_range)
    xd <- price_all[time_min:t_max] #all data that we need; from time_min to time_max(=t_max)
    yd <- diff(xd, lag = 1)
    zd <- sign(yd)
    
    predicted.range.parameters <- trust.range.parameters <- c()
    for(i in 1:(length(pq_range)*length(d_range))){
      d.i <- d_vec[i]
      pq.i <- pq_vec[i]
      
      xd.i <- xd[(1+max(d_range)-d.i):(length(xd))] #all data that we need; from time_min to time_max(=t_max)
      yd.i <- yd[(1+max(d_range)-d.i):(length(yd))]
      zd.i <- zd[(1+max(d_range)-d.i):(length(zd))]
      
      M.0 <- rollapply(yd.i,width=d.i+1,FUN=identity)
      M.0 <- M.0[,-ncol(M.0),drop=FALSE] #zadnjo želimo napovedati
      
      if(model == "ARMA_bayes"){
        model.stan <- model.stan.list[[pq.i]]
        predicted0 <- apply(M.0, 1, function(x) { #stan output
          stan_data <- list(y = x, T=length(x))
          y_fit <- sampling(model.stan,
                            data = stan_data,
                            iter = n.iter, warmup = n.warmup,
                            chains = n.chains,
                            seed = seed,
                            verbose = FALSE,
                            refresh = -1,
                            control = list(max_treedepth = 30,adapt_delta = delta))
          y_fit
          
        })
        predicted <- sapply(predicted0,function(x){
          summary(x,pars=c("predy"))$summary[,c("mean","sd")]
        })
        
        list.1[[paste("TrainDays=",train.time0,"_model=",pq.i,"_d=",d.i,sep="")]] <- predicted0
        
      }else if(model == "ARMA_frekv"){
        if(grepl("AR_",pq.i)){
          pp <- as.numeric(substr(pq.i,4,4))
          qq <- 0
        }else if(grepl("ARMA_",pq.i)){
          pp <- as.numeric(substr(pq.i,6,6))
          qq <- as.numeric(substr(pq.i,8,8))
        }else{
          pp <- 0
          qq <- as.numeric(substr(pq.i,4,4))
        }
        predicted <- apply(M.0, 1, function(x) { #stan output
          y_fit <- arima(x, c(pp, 0, qq)) # update the model each day
          pred <-  predict(y_fit, n.ahead=1)
          c("mean"=pred$pred[1],"sd"=pred$se[1])
        })
        
      }
      predicted.range.parameters <- cbind(predicted.range.parameters,predicted["mean",])      
      trust.range.parameters <- cbind(trust.range.parameters,predicted["sd",])      
      colnames(predicted.range.parameters)[i] <- paste(pq.i,d.i,sep="_")
      colnames(trust.range.parameters)[i] <- paste(pq.i,d.i,sep="_")
      
    }
  }
  
  x_i <- price_all[a0:(a0+train.time0-1)] #current prices
  real.prices <- price_all[(a0+1):(a0+train.time0)] #prices we want to predict
  
  if(model %in% c("RF_frekv","LR_bayes","LR_frekv")){
    #matrix of predictions (direction only)
    pred.sign <- ifelse(round(predicted.range.parameters,0)==0, -1, 1)  
    #matrix of trust in predictions
    pred.trust <- predicted.range.parameters
  }else{
    pred.sign <- ifelse(predicted.range.parameters>0, 1, -1)
    pred.trust <- abs(predicted.range.parameters/trust.range.parameters)
  }
  
  real.sign <- sign(real.prices-x_i)
  real.sign[real.sign==0] <- -1 
  real.sign <- factor(real.sign, levels=c(1,-1))
  confMat <- apply(pred.sign,2,function(x){
    confusionMatrix(factor(x,levels=c(1,-1)),real.sign)
  })
  
  if(model == "LR_bayes"){
    if(modelTrValTe=="train"){
      saveRDS(list.1,paste("./train_days/predicted_Models",model,
                           "_a0",a0,
                           "_trainTime",train.time0,
                           "_dRange",paste(d_range,collapse = "_"),
                           "_ddRange",paste(dd_range-1,collapse = "_"),
                           ".rds",sep=""))  
    }else if(modelTrValTe=="validation"){
      saveRDS(list.1,paste("./validation_days/predicted_Models",model,
                           "_a0",a0,
                           "_trainTime",train.time0,
                           "_dRange",paste(d_range,collapse = "_"),
                           "_ddRange",paste(dd_range-1,collapse = "_"),
                           ".rds",sep=""))  
    }else if(modelTrValTe=="test"){
      saveRDS(list.1,paste("./test_days/predicted_Models",model,
                           "_a0",a0,
                           "_trainTime",train.time0,
                           "_dRange",paste(d_range,collapse = "_"),
                           "_ddRange",paste(dd_range-1,collapse = "_"),
                           ".rds",sep=""))  
    }
    
  }else if(model == "ARMA_bayes"){ #save fit from stan
    if(modelTrValTe=="train"){
      saveRDS(list.1,paste("./train_days/predicted_Models",model,
                           "_a0",a0,
                           "_trainTime",train.time0,
                           "_dRange",paste(d_range,collapse = "_"),
                           "_ARMApq",paste(unlist(ARMApq),collapse = "_"),
                           ".rds",sep="")) 
    }else if(modelTrValTe=="validation"){
      saveRDS(list.1,paste("./validation_days/predicted_Models",model,
                           "_a0",a0,
                           "_trainTime",train.time0,
                           "_dRange",paste(d_range,collapse = "_"),
                           "_ARMApq",paste(unlist(ARMApq),collapse = "_"),
                           ".rds",sep="")) 
    }else if(modelTrValTe=="test"){
      saveRDS(list.1,paste("./test_days/predicted_Models",model,
                           "_a0",a0,
                           "_trainTime",train.time0,
                           "_dRange",paste(d_range,collapse = "_"),
                           "_ARMApq",paste(unlist(ARMApq),collapse = "_"),
                           ".rds",sep="")) 
    }
    
  }
  
  ###trading 
  M <- matrix(0,ncol=ncol(pred.trust),nrow=nrow(pred.trust))
  colnames(M) <- colnames(pred.trust)
  if(model %in% c("RF_frekv","LR_bayes","LR_frekv")){ 
    do.trade0 <- lapply(gamma_1_range,function(x){
      M[(pred.trust>=1-x) & (pred.sign == 1)] <- 1
      M[(pred.trust < x) & (pred.sign == -1)] <- -1
      # M
      funk_when_trade(M)
    })
  }else{ #ARMA; trade only when pred.trust >= gamma_1
    do.trade0 <- lapply(gamma_1_range,function(x){
      M[(pred.trust>x) & (pred.sign == 1)] <- 1
      M[(pred.trust>x) & (pred.sign == -1)] <- -1
      # M
      funk_when_trade(M)
    })
  }
  names(do.trade0) <- gamma_1_range
  
  if(modelTrValTe == "test"){
    confMat_gamma.list <- list()
    for(i in names(do.trade0)){
      do.trade0.x <- do.trade0[[i]]
      confMat_gamma <- apply(do.trade0.x,2,function(x){
        which.gamma <- which(do.trade0.x!=0)
        pred.sign.x <- do.trade0.x[which.gamma]
        real.sign.x <- real.sign[which.gamma]
        cM <- confusionMatrix(factor(pred.sign.x,levels=c(1,-1)),real.sign.x)
      })
      confMat_gamma.list[[i]] <- confMat_gamma
    }
  }
  
  # BTC price is always > 0
  # cumsum of trades needs to be in {0,1}
  list.budgets <- list()
  for(i in names(do.trade0)){ #how much money/btc value we have at each time
    do.trade1 <- do.trade0[[i]]
    money <- budget_start0 # our budget
    amount <- 0 # amount of BTC we own
    budgets_overall <- c()
    
    #if we would only like to predict for one day
    if(!is.matrix(do.trade1)){
      do.trade1 <- as.matrix(do.trade1)
    }
    
    matrix.budgets <- apply(do.trade1,2,function(x){
      for(n in 1:train.time0){
        if(x[n]==1){
          cat("buy, ")
          amount <- (money*.98)/x_i[n]
          money <- 0
          budgets_overall <- c(budgets_overall, x_i[n] * amount)
        }else if(x[n]==-1){
          cat("sell, ")
          money <- money + x_i[n]*.98*amount
          amount <- 0
          budgets_overall <- c(budgets_overall, money)
        }else if(x[n]==0 & amount>0){
          budgets_overall <- c(budgets_overall, x_i[n] * amount)
        }else if(x[n]==0 & amount==0){
          budgets_overall <- c(budgets_overall, money)
        }
      }
      budgets_overall
    })
    list.budgets[[i]] <- matrix.budgets
    
  }
  if(modelTrValTe == "test"){
    return(list("pred.trust"=pred.trust,"confMat"=confMat,
                "confMatgamma"=confMat_gamma.list,"list.budgets"=list.budgets,"trade.counts"=do.trade0))
  }else{
    return(list("pred.trust"=pred.trust,"confMat"=confMat,"list.budgets"=list.budgets,"trade.counts"=do.trade0))
  }
  
}

####################################################################
####################################################################
####################################################################
accuracy_function <- function(budgets=budgets1){
  accuracy.vec <- c()
  for(a in names(budgets$confMat)){
    accuracy <- budgets$confMat[[a]]$overall["Accuracy"]
    accuracy.vec <- rbind(accuracy.vec,accuracy)
  }
  rownames(accuracy.vec) <- names(budgets$confMat)
  return("bestAccuracy"=list(accuracy.vec[which(accuracy.vec == max(accuracy.vec)),,drop=FALSE],
         "allAccuracy"=accuracy.vec))
}

accuracy_function2 <- function(budgets=budgets1){
  accuracy.vec <- c()
  for(a in names(budgets$confMat)){
    accuracy <- budgets$confMat[[a]]$overall["Accuracy"]
    accuracy.vec <- rbind(accuracy.vec,accuracy)
  }
  rownames(accuracy.vec) <- names(budgets$confMat)
  return(round(accuracy.vec,4) %>% c())
}

accuracy_function3 <- function(budgets=budgets1,model.xx.gamma){
  accuracy.vec <- c()
  for(a in as.character(model.xx.gamma)){
    accuracy <- (budgets$confMatgamma[[a]])[[1]]$overall["Accuracy"]
    accuracy.vec <- rbind(accuracy.vec,accuracy)
  }
  rownames(accuracy.vec) <- as.character(model.xx.gamma)
  return(round(accuracy.vec,4) %>% c())
}

######

tradeCount_function <- function(budgets=budgets1){
  trade.count.list <- max.budgets.list <- list()
  for(b in names(budgets$list.budgets)){
    trade.count <-  colSums(budgets$trade.counts[[b]]!=0)
    trade.count.list[[b]] <- trade.count
    
    budgets.wh <- which(tail(budgets$list.budgets[[b]],1)==max(tail(budgets$list.budgets[[b]],1)))
    budgets.wh.names <- colnames(budgets$list.budgets[[b]])[budgets.wh]
    budgets.1 <- tail(budgets$list.budgets[[b]],1)[budgets.wh]
    max.budgets.list[[b]] <- cbind(budgets.wh.names,budgets.1)
  }
  return(list("tradeCount"=trade.count.list,"maxBudgets"=max.budgets.list))
}

tradeCount_function2 <- function(budgets=budgets1){
  trade.count.list <- c()
  for(b in names(budgets$list.budgets)){
    trade.count <-  colSums(budgets$trade.counts[[b]]!=0)
    trade.count.list <- c(trade.count.list,trade.count)
  }
  names(trade.count.list) <- names(budgets$list.budgets)
  return(trade.count.list)
}
####################################################################
####################################################################
#profits list function
profits_func0 <- function(test.data.xx){
  profits_list <- list()
  for(g in names(test.data.xx$list.budgets)){
    profits_list[[g]] <- cumsum(c(0,diff(test.data.xx$list.budgets[[g]],1)))
  }
  return(profits_list)
}

profits_func <- function(test.data.xx,names1){
  profits.data <- profits_func0(test.data.xx)
  if(names1 == "ARMA.bayes"){
    which.1 <- which(names(profits.data) %in% gamma1.ARMA_bayes)
  }else if(names1 == "LR.bayes"){
    which.1 <- which(names(profits.data) %in% gamma1.LR_bayes)
  }else if(names1 == "RF"){
    which.1 <- which(names(profits.data) %in% gamma1.RF_frekv)
  }else if(names1 == "ARMA.frekv"){
    which.1 <- which(names(profits.data) %in% gamma1.ARMA_frekv)
  }else if(names1 == "LR.frekv"){
    which.1 <- which(names(profits.data) %in% gamma1.LR_frekv)
  }
  profits.data <- profits.data[which.1] 
  profits_df <- data.frame(matrix(NA,nrow=pred.time,ncol=length(names(profits.data))))
  for(n in names(profits.data)){
    profits_df[,which(names(profits.data)==n)] <- profits.data[[n]]
  }
  names(profits_df) <- paste(names1," gamma*",1:length(which.1),sep="")
  profits_df$time <- ymd(ymd_hms(all_days[(a0+train.time0+val.time+1):(a0+train.time0+val.time+pred.time),]$Date))
  return(profits_df)
}

