# setwd("~/Dropbox/Tjasulka/MAGISTERIJ/Bayes_2018/Trading_Bitcoin")
# source("functions.R")
####################################
#summary of models (train set)
####################################

# model1 <- ...
# d_range1 <- d_range1.x
# dd_range1 <- dd_range1.x
# 
# if(model1 %in% c("ARMA_bayes","ARMA_frekv")){
#   budgets1 <- readRDS(paste("./train_days/output_budgets_Models",model1,
#                             "_a0",a0,"_trainTime",train.time0,
#                             "_dRange",paste(d_range1,collapse = "_"),
#                             ".rds",sep=""))
# }else{
#   budgets1 <- readRDS(paste("./train_days/output_budgets_Models",model1,
#                             "_a0",a0,"_trainTime",train.time0,
#                             "_dRange",paste(d_range1,collapse = "_"),
#                             "_ddRange",paste(dd_range1,collapse = "_"),
#                             ".rds",sep=""))
# }
# 
# 
# accuracy_function()
# tradeCount_function()

funk_run_testTime <- function(model1){
  ####################################
  #choose the best model parameters on train set
  ####################################
  if(model1 == "RF_frekv"){
    dd_range1 <- dd1.RF_frekv
    d_range1 <- d1.RF_frekv
    ARMApq1 <- NULL
  }else if(model1 == "LR_frekv"){
    dd_range1 <- dd1.LR_frekv
    d_range1 <- d1.LR_frekv
    ARMApq1 <- NULL
  }else if(model1 == "ARMA_frekv"){
    d_range1 <- d1.ARMA_frekv
    ARMApq1 <- list(pq1.ARMA_frekv)
    dd_range1 <- NULL
  }else if(model1 == "LR_bayes"){
    dd_range1 <- dd1.LR_bayes
    d_range1 <- d1.LR_bayes
    ARMApq1 <- NULL
  }else if(model1 == "ARMA_bayes"){
    d_range1 <- d1.ARMA_bayes
    ARMApq1 <- list(pq1.ARMA_bayes)
    dd_range1 <- NULL
  }
  gamma_1_range1 <- gamma_range1.x
  ####################################
  #trade
  ####################################
  s.time0 <- Sys.time()
  # profvis({
  budgets <- func_TrVaTe(modelTrValTe="test", 
                         model=model1, #"ARMA_bayes", "ARMA_frekv", "LR_bayes", "LR_frekv", "RF_frekv"
                         d_range=d_range1,
                         dd_range=dd_range1,
                         ARMApq=ARMApq1,
                         gamma_1_range=gamma_1_range1, #vector of trust parameters
                         seed = 123, #set.seed
                         n.iter=n.iter, n.warmup=n.warmup, delta=delta, n.chains=n.chains, #adapt for ARMA models (MA_2 + )
                         a0=a0, train.time0=train.time0,
                         val.time=val.time, pred.time=pred.time)
  # })
  
  s.time1 <- Sys.time()-s.time0; s.time1
  ####################################
  #save model output
  ####################################
  if(model1 %in% c("ARMA_bayes","ARMA_frekv")){
    saveRDS(budgets,paste("./test_days/output_budgets_Models",model1,
                          "_a0",a0 + train.time0 + val.time,
                          "_trainTime",pred.time,
                          "_dRange",paste(d_range1,collapse = "_"),
                          # "_ddRange",paste(dd_range1,collapse = "_"),
                          "_ARMApq",unlist(ARMApq1),
                          ".rds",sep=""))
  }else{
    saveRDS(budgets,paste("./test_days/output_budgets_Models",model1,
                          "_a0",a0 + train.time0 + val.time,
                          "_trainTime",pred.time,
                          "_dRange",paste(d_range1,collapse = "_"),
                          "_ddRange",paste(dd_range1,collapse = "_"),
                          # "_ARMApq",unlist(ARMApq1),
                          ".rds",sep=""))
  }
  # return(budgets)
  
}

