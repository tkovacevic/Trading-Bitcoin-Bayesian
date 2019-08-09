#source#
################################
#1) select parameters
# rm(list=ls())
setwd("~/Dropbox/Tjasulka/MAGISTERIJ/Bayes_2018/Trading_Bitcoin")
t0t0 <- "1.12.2014" #when we start to train our models
t1t1 <- "2.12.2014"
budget_start0 <- 5000

train.time0 <- 120 #for how many days we train our models (for d, dd and p, q parameters (and gamma parameter))
val.time <- 0 #validation set (we dont't use it)
pred.time <- 1315 #for how many days do we predict

train_end_time <- "31.3.2015"
zacetni_dan <- "1.4.2015"
koncni_dan <- "5.11.2018"
koncni_dan_xx <- "5.11.2018"
#################################
#Stan
#problems with divergence; use this for MA2, ARMA11, ARMA12, ARMA21
#only for ARMA models for experimenting with; for logistic regression we use defaults and 1 chain
n.iter <- 2000
n.warmup <- 1000
delta <- .8
n.chains <- 1
#################################
d_range1.x <- c(30,50,80,100)
dd_range1.x <- c(20,50,80,110)
gamma_range1.x <- seq(0.05,0.95,by=0.05)
ARMApq.bayes.x <- list("AR_1","AR_2",
                       "MA_1","MA_2",
                       "ARMA_1_1","ARMA_1_2",
                       "ARMA_2_1")
ARMApq.frekv.x <- list("MA_1")
models.x <- c("ARMA_bayes","LR_bayes",
            "ARMA_frekv","LR_frekv","RF_frekv")

n.trees.rf <- 500
################################
#2) find "optimal" parameters on train set
do_grid_search_ARMA_bayes <- FALSE
do_grid_search_LR_bayes <- FALSE
#############
do_grid_search_ARMA_frekv <- FALSE
do_grid_search_LR_frekv <- FALSE
do_grid_search_RF_frekv <- FALSE
###
gr.name <- c(do_grid_search_ARMA_bayes,do_grid_search_LR_bayes,
             do_grid_search_ARMA_frekv,do_grid_search_LR_frekv,do_grid_search_RF_frekv)
#############################
#3) run on data we want to predict
run_ARMA_bayes <- FALSE
run_LR_bayes <- FALSE
#############
run_ARMA_frekv <- FALSE
run_LR_frekv <- FALSE
run_RF_frekv <- FALSE
###
test.name <- c(run_ARMA_bayes,run_LR_bayes,
               run_ARMA_frekv,run_LR_frekv,run_RF_frekv)
#############
#4) plot linear regression on y data (bayesian)
run_linear_regression_y_bayes <- FALSE
################################
#5) packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(stats, ggplot2, scales, dplyr, grDevices,
               lubridate, caret, zoo, tidyr, xtable, # ridge, glmnet,#ridge, #todo
               psych, StanHeaders, gridExtra, randomForest,
               broom, rstan, profvis, RColorBrewer, bayesplot,
               truncnorm) #invgamma, 
rstan_options(auto_write = TRUE)
################################
#6) import data
all_days <- read.csv("./data/coindesk-bpi-USD-ohlc_data-2010-07-01_2018-11-05.csv")
price_all <- (all_days$High + all_days$Low)/2
a0 <- which(substr(as.character(all_days$Date),1,10) == dmy(t0t0)) #kje začnemo (prvi dan)
################################
#7) stan data
# model.stan.list <- list()
# model.stan.list[["AR_1"]] <- stan_model("AR_1.stan")
# model.stan.list[["AR_2"]] <- stan_model("AR_2.stan")
# model.stan.list[["MA_1"]] <- stan_model("MA_1.stan")
# model.stan.list[["MA_2"]] <- stan_model("MA_2.stan")
# model.stan.list[["ARMA_1_1"]] <- stan_model("ARMA_1_1.stan")
# model.stan.list[["ARMA_1_2"]] <- stan_model("ARMA_1_2.stan")
# model.stan.list[["ARMA_2_1"]] <- stan_model("ARMA_2_1.stan")
# model.stan.list[["LR"]] <- stan_model("logistic_regression.stan")
# model.stan.list[["linear_regression"]] <- stan_model("linear_regression.stan")
# saveRDS(model.stan.list,"ModelStanList.rds")
model.stan.list <- readRDS("ModelStanList.rds")
