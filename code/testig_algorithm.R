##xgboost testing and info-----
#confusionMatrix,auc,fscore,accuracy,sensevity, specifity,precision(obj),without_pred, plot_auc....
confusionMatrix_xgb=matrix(0,1,6)
confusionMatrix_xgb=as.list(confusionMatrix_xgb)
names(confusionMatrix_xgb)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")

Auc_xgb=matrix(0,1,6)
Auc_xgb=as.list(Auc_xgb)
names(Auc_xgb)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")
7246+1261
Fscore_xgb=matrix(0,1,6)
Fscore_xgb=as.list(Fscore_xgb)
names(Fscore_xgb)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")

accuracy_xgb=matrix(0,1,6)
accuracy_xgb=as.list(accuracy_xgb)
names(accuracy_xgb)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")

sensitivity_xgb=matrix(0,1,6)
sensitivity_xgb=as.list(sensitivity_xgb)
names(sensitivity_xgb)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")


specifity_xgb=matrix(0,1,6)
specifity_xgb=as.list(specifity_xgb)
names(specifity_xgb)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")

precision_xgb=matrix(0,1,6)
precision_xgb=as.list(precision_xgb)
names(precision_xgb)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")

without_pred_xgb=matrix(0,1,6)
without_pred_xgb=as.list(without_pred_xgb)
names(without_pred_xgb)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")

plot_xgb=matrix(0,1,6)
plot_xgb=as.list(plot_xgb)
names(plot_xgb)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")

xgb1_xgb=matrix(0,1,6)
xgb1_xgb=as.list(xgb1_xgb)
names(xgb1_xgb)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")


#testing xgb
for(i in c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")){
test_xgb=fitness_test_xgb(datatrain=train_data_sample1[[i]],datatest=test_data_sample1[[i]], x_eta=solution_Byop_xgb[[i]][[1]],subsample=solution_Byop_xgb[[i]][[2]],colsample_bytree=solution_Byop_xgb[[i]][[3]],lambda1=solution_Byop_xgb[[i]][[4]],
                          lambda=solution_Byop_xgb[[i]][[5]],gamma=solution_Byop_xgb[[i]][[7]], max_depth=solution_Byop_xgb[[i]][[6]],x_iter=solution_Byop_xgb[[i]][[8]])
Auc_xgb[[i]]= test_xgb$Auc_xgb
Fscore_xgb[[i]]=test_xgb$fscore
accuracy_xgb[[i]]= test_xgb$accuracy
sensitivity_xgb[[i]]=test_xgb$sensitivity
specifity_xgb[[i]]=test_xgb$specifity
precision_xgb[[i]]= test_xgb$precision
without_pred_xgb[[i]]= test_xgb$without_pred_xgb
plot_xgb[[i]]=test_xgb$plot_info
xgb1_xgb[[i]]=test_xgb$xgb1
}

xgb.plot.importance(importance_matrix=xgb.importance(model=test_xgb$xgb1))

save(xgb1_xgb,confusionMatrix_xgb, Auc_xgb, Fscore_xgb, accuracy_xgb, sensitivity_xgb, specifity_xgb, precision_xgb, without_pred_xgb, plot_xgb,file="answer_xgb1")
load("answer_xgb1")



##light testing and info-----
#confusionMatrix,auc,fscore,accuracy,sensevity, specifity,precision(obj),without_pred, plot_auc....
confusionMatrix_lgb=matrix(0,1,6)
confusionMatrix_lgb=as.list(confusionMatrix_lgb)
names(confusionMatrix_lgb)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")

Auc_lgb=matrix(0,1,6)
Auc_lgb=as.list(Auc_lgb)
names(Auc_lgb)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")

Fscore_lgb=matrix(0,1,6)
Fscore_lgb=as.list(Fscore_lgb)
names(Fscore_lgb)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")

accuracy_lgb=matrix(0,1,6)
accuracy_lgb=as.list(accuracy_lgb)
names(accuracy_lgb)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")

sensitivity_lgb=matrix(0,1,6)
sensitivity_lgb=as.list(sensitivity_lgb)
names(sensitivity_lgb)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")


specifity_lgb=matrix(0,1,6)
specifity_lgb=as.list(specifity_lgb)
names(specifity_lgb)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")

precision_lgb=matrix(0,1,6)
precision_lgb=as.list(precision_lgb)
names(precision_lgb)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")

without_pred_lgb=matrix(0,1,6)
without_pred_lgb=as.list(without_pred_lgb)
names(without_pred_lgb)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")

plot_lgb=matrix(0,1,6)
plot_lgb=as.list(plot_lgb)
names(plot_lgb)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")


lgb1_lgb=matrix(0,1,6)
lgb1_lgb=as.list(lgb1_lgb)
names(lgb1_lgb)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")


#testing lgb...
for(i in c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")){
  test_lgb=fitness_test_lgb(datatrain=train_data_sample2[[i]],datatest=test_data_sample2[[i]],learning_rate=solution_Byop_lgb[[i]][[8]],num_iterations=solution_Byop_lgb[[i]][[1]],max_depth=solution_Byop_lgb[[i]][[2]], 
                            bagging_fraction=solution_Byop_lgb[[i]][[3]],feature_fraction=solution_Byop_lgb[[i]][[4]],
                            lambda_l1=solution_Byop_lgb[[i]][[5]], lambda_l2=solution_Byop_lgb[[i]][[6]],num_leaves=solution_Byop_lgb[[i]][[7]])
  confusionMatrix_lgb[[i]]=test_lgb$confmatrix
  Auc_lgb[[i]]= test_lgb$Auc_lgb
  Fscore_lgb[[i]]=test_lgb$fscore
  accuracy_lgb[[i]]= test_lgb$accuracy
  sensitivity_lgb[[i]]=test_lgb$sensitivity
  specifity_lgb[[i]]=test_lgb$specifity
  precision_lgb[[i]]= test_lgb$precision
  without_pred_lgb[[i]]= test_lgb$without_pred_xgb
  plot_lgb[[i]]=test_lgb$plot_info
  lgb1_lgb[[i]]=test_lgb$lgb_1
}
save(lgb1_lgb,confusionMatrix_lgb, Auc_lgb, Fscore_lgb, accuracy_lgb, sensitivity_lgb, specifity_lgb, precision_lgb,  without_pred_lgb, plot_lgb,file="answer_lgb2")
load("answer_lgb2")
