# setwd("~/Dropbox/Tjasulka/MAGISTERIJ/Bayes_2018/Trading_Bitcoin")
# source("functions.R")

################################################################
#trade on train set
################################################################
funk_run_trainTime <- function(model1){
  d_range1 <- d_range1.x
  dd_range1 <- dd_range1.x
  
  if(model1 == "ARMA_frekv"){
    ARMApq1 <- ARMApq.frekv.x
  }else if(model1 == "ARMA_bayes"){
    ARMApq1 <- ARMApq.bayes.x  
  }
  
  s.time0 <- Sys.time()
  # profvis({
  budgets <- func_TrVaTe(modelTrValTe="train", 
                         model=model1, #"ARMA_bayes", "ARMA_frekv", "LR_bayes", "LR_frekv", "RF_frekv"
                         d_range=d_range1, #vector of window parameters
                         dd_range=dd_range1, #vector of train set lengths parameters (LR, RF)
                         #MA2, ARMA11, ARMA12, ARMA21: težave s konvergenco
                         ARMApq=ARMApq1, #list of model names for ARMA ("AR_1","AR_2","MA_1","MA_2","ARMA_1_1","ARMA_1_2","ARMA_2_1")
                         gamma_1_range=gamma_range1.x, #vector of trust parameters
                         seed = 123, #set.seed
                         n.iter=n.iter, n.warmup=n.warmup, delta=delta, n.chains=n.chains,#adapt for ARMA models (MA_2 + )
                         a0=a0, train.time0=train.time0,
                         val.time=val.time, pred.time=pred.time) #for validation or for
  # })
  
  s.time1 <- Sys.time()-s.time0; s.time1
  ################################################################
  #save model output
  ################################################################
  if(model1 %in% c("ARMA_bayes","ARMA_frekv")){
    saveRDS(budgets,paste("./train_days/output_budgets_Models",model1,
                          "_a0",a0,
                          "_trainTime",train.time0,
                          "_dRange",paste(d_range1,collapse = "_"),
                          ".rds",sep=""))
  }else{
    saveRDS(budgets,paste("./train_days/output_budgets_Models",model1,
                          "_a0",a0,
                          "_trainTime",train.time0,
                          "_dRange",paste(d_range1,collapse = "_"),
                          "_ddRange",paste(dd_range1,collapse = "_"),
                          ".rds",sep=""))
  }
  # return(budgets)
  
}
