library("GA")
library("xgboost")
library("rBayesianOptimization")
library("ROCit")
datatrain,feature=40,x_eta=7,x_lambda=7,x_max_depth=6,[30,45],x_iter=10,[100,250],upper_var,less_var,k_fold
solution_Byop_xgb=matrix(0,1,6)
solution_Byop_xgb=as.list(solution_Byop_xgb)
names(solution_Byop_xgb)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")

fopt_Byop_xgb=matrix(0,1,6)
fopt_Byop_xgb=as.list(fopt_Byop_xgb)
names(fopt_Byop_xgb)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")


result_Byop_xgb=matrix(0,1,6)
result_Byop_xgb=as.list(result_Byop_xgb)
names(result_Byop_xgb)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")



for(i in c("smote_700")){
  ff=function(x_eta,subsample,colsample_bytree,lambda1,lambda2,gamma ,x_max_depth,x_iter){fitness_xgb2(x_eta,subsample,colsample_bytree,lambda1,lambda2,gamma ,x_max_depth,x_iter,k_fold=5,datatrain =train_data_sample1[[i]])}
  Byop_xgb <- BayesianOptimization(FUN =ff,
                                   bounds = list(x_eta=c(0.005,0.6),
                                                 subsample=c(0.7,1),
                                                 colsample_bytree=c(0.7,1),
                                                 lambda1=c(0,60),
                                                 lambda2=c(0,60),
                                                 x_max_depth=c(5L,20L),
                                                 gamma=c(0.01,1),
                                                 x_iter=c(300L,1000L) 
                                   ),init_points = 10, n_iter = 30,acq = "ucb",verbose = TRUE)
  solution_Byop_xgb[[i]]=Byop_xgb $Best_Par
  fopt_Byop_xgb[[i]]=Byop_xgb $Best_Value
  result_Byop_xgb[[i]]=Byop_xgb$ History 
}

save(solution_Byop_xgb,fopt_Byop_xgb,result_Byop_xgb,file = "opt_parameter_xgb1")
load("opt_parameter_xgb1")

##light-----

solution_Byop_lgb=matrix(0,1,6)
solution_Byop_lgb=as.list(solution_Byop_lgb)
names(solution_Byop_lgb)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")

fopt_Byop_lgb=matrix(0,1,6)
fopt_Byop_lgb=as.list(fopt_Byop_lgb)
names(fopt_Byop_lgb)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")

result_Byop_lgb=matrix(0,1,6)
result_Byop_lgb=as.list(result_Byop_lgb)
names(result_Byop_lgb)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")
library("ROCit")

for(i in c("real_1000")){
  ff=function(learning_rate,num_iterations,max_depth, bagging_fraction,feature_fraction,lambda_l1, lambda_l2,num_leaves){fitness_lightgbm(datatrain=train_data_sample2[[i]] , learning_rate,num_iterations,max_depth, bagging_fraction,feature_fraction,lambda_l1, lambda_l2,num_leaves,k_fold=5)}
  Byop_lgb <- BayesianOptimization(FUN =ff,
                                   bounds = list(num_iterations=c(300L,900L),
                                                 max_depth=c(5L,30L),
                                                 bagging_fraction=c(0.7,0.9),
                                                 feature_fraction=c(0.7,0.9),
                                                 lambda_l1=c(0,60),
                                                 lambda_l2=c(0,60),
                                                 num_leaves = c(20L,100L),
                                                 learning_rate = c(0.005,0.6)
                                   ),init_points = 10, n_iter = 30,acq = "ucb",verbose = TRUE)
  solution_Byop_lgb[[i]]=Byop_lgb $Best_Par
  fopt_Byop_lgb[[i]]=Byop_lgb $Best_Value
  result_Byop_lgb[[i]]=Byop_lgb$ History 
}

save(  solution_Byop_lgb,fopt_Byop_lgb,result_Byop_lgb,file="opt_parameter_light")
load("opt_parameter_light")


##catboost----
solution_Byop_cat=matrix(0,1,6)
solution_Byop_cat=as.list(solution_Byop_cat)
names(solution_Byop_cat)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")

fopt_Byop_cat=matrix(0,1,6)
fopt_Byop_cat=as.list(fopt_Byop_cat)
names(fopt_Byop_cat)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")

result_Byop_cat=matrix(0,1,6)
result_Byop_cat=as.list(result_Byop_cat)
names(result_Byop_cat)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")

for(i in c("smote_700")){
  ff=function(iterations,depth,learning_rate,l2_leaf_reg,bagging_temperature,subsample,rsm){fitness_catboost(datatrain=train_data_sample2[[i]] ,iterations,depth,learning_rate,l2_leaf_reg,bagging_temperature,subsample,rsm,k_fold=2)}
  Byop_cat <- BayesianOptimization(FUN =ff,
                                   bounds = list(iterations=c(500L,900L),
                                                 depth=c(10L,15L),
                                                 bagging_temperature=c(0.7,0.9),
                                                 subsample=c(0.7,0.9),
                                                 l2_leaf_reg=c(0,20),
                                                 rsm=c(0.7,0.9),
                                                 learning_rate = c(0,0.5)
                                   ),init_points = 2, n_iter = 2,acq = "ucb",verbose = TRUE)
  solution_Byop_cat[[i]]=Byop_cat $Best_Par
  fopt_Byop_cat[[i]]=Byop_cat $Best_Value
  result_Byop_cat[[i]]=Byop_cat$ History 
}



