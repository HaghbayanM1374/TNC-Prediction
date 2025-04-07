setwd("C:/Users/asus/Downloads")
load('answer_xgb1')
load('dataprd')
load('answer_xgb')
load('answer_lgb')
load('answer_lgb2')
load('opt_parameter_xgb')
load('opt_parameter_xgb1')
load('opt_parameter_light')
library("pROC")
library("GA")
library("xgboost")
library("rBayesianOptimization")
library("ROCit")

gg_importance_xgb=matrix(0,1,6)
gg_importance_xgb=as.list(gg_importance_xgb)
names(gg_importance_xgb)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")

for(i in  c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")){
  kkk=as.data.frame(xgb.importance(model=xgb1_xgb[[1]]))[1:15,]
  kkk[which(kkk$Feature=="Mather_age"),1]="Maternal age"
  kkk[which(kkk$Feature=="Father_age"),1]="Paternal age"
  kkk[which(kkk$Feature=="Pregnancy_week"),1]="Weeks of gestation "
  if(i=="real_700"){
    y="700"
    x="XGBoost method AND cut-off"
  }else if(i=="smote_700") {
    y="700"
    x="XGBoost-smote_nc method AND cut-off"
  }else if(i=="real_1000") {
    y="1000"
    x="XGBoost method AND cut-off"
  }else if(i=="smote_1000") {
    y="1000"
    x="XGBoost-smote_nc method AND cut-off"
  }else if(i=="real_1500") {
    y="1500"
    x="XGBoost method AND cut-off"
  }else{
    y="1500"
    x="XGBoost-smote_nc method AND cut-off"
  }
  gg_importance_xgb[[i]]=ggplot(kkk, aes(reorder(Feature,Gain), Gain))+geom_col(color="blue",fill=rgb(0.2,0.8,0.8,0.1))+
    coord_flip()+
    xlab("Features")+
    ylab("  ")+
    
    theme(plot.title = element_text(hjust = 0.1, color = "dark blue", size = 6, face = "bold.italic"),
          axis.line.y = element_line(size = 0.6, color = "black"),
          axis.line.x = element_line(size = 0.6, color = "black"),
          axis.text.y = element_text(size = 6),
          panel.background = element_rect(fill = "white"))
  
  
}
save(gg_importance_xgb,file = "xgb_imporance_tnc")
load("xgb_imporance_tnc")
library("ggpubr")
ggarrange(gg_importance_xgb[[1]],gg_importance_xgb[[2]],gg_importance_xgb[[3]]
          , gg_importance_xgb[[4]] ,gg_importance_xgb[[5]],gg_importance_xgb[[6]],
          labels = c("A", "B", "C", "D" , "E", "F"),
          ncol = 2, nrow = 3)        

gg_importance_lgb=matrix(0,1,6)
gg_importance_lgb=as.list(gg_importance_lgb)
names(gg_importance_lgb)=c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")

for(i in  c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")){
  kkk=as.data.frame(lgb1_lgb[[i]])
  kkk[which(kkk$Feature=="number_of_childeren"),1]="birth_order"
  kkk[which(kkk$Feature=="Mather_age"),1]="Maternal age"
  kkk[which(kkk$Feature=="Father_age"),1]="Paternal age"
  kkk[which(kkk$Feature=="Pregnancy_week"),1]="Weeks of gestation "
  if(i=="real_700"){
    y="700"
    x="LightGBM method AND cut-off"
  }else if(i=="smote_700") {
    y="700"
    x="LightGBM-smote_nc method AND cut-off"
  }else if(i=="real_1000") {
    y="1000"
    x="LightGBM method AND cut-off"
  }else if(i=="smote_1000") {
    y="1000"
    x="LightGBM-smote_nc method AND cut-off"
  }else if(i=="real_1500") {
    y="1500"
    x="LightGBM method AND cut-off"
  }else{
    y="1500"
    x="LightGBM-smote_nc method AND cut-off"
  }
  gg_importance_lgb[[i]]=ggplot(kkk, aes(reorder(Feature,Gain), Gain))+geom_col(color="green",fill=rgb(0.2,0.8,0.8,0.1))+
    coord_flip()+
    xlab("Features")+
    ylab("  ")+
    
    theme(plot.title = element_text(hjust = 0.1, color = "dark blue", size = 5, face = "italic"),
          axis.line.y = element_line(size = 0.8, color = "black"),
          axis.line.x = element_line(size = 0.6, color = "black"),
          axis.text.y = element_text(size = 8),
          panel.background = element_rect(fill = "white"))
  
  
}
library("ggpubr")
ggarrange(gg_importance_lgb[[1]],gg_importance_lgb[[2]],gg_importance_lgb[[3]]
          , gg_importance_lgb[[4]] ,gg_importance_lgb[[5]],gg_importance_lgb[[6]],
          labels = c("A", "B", "C", "D" , "E", "F"),
          ncol = 2, nrow = 3)        

plot_700=data.frame(Method=c("XGBoost","XGBoost_Smote","LightGbm","LightGBM_Smote"),
                    AUC=c(Auc_xgb[[1]],Auc_xgb[[2]],Auc_lgb[[1]],Auc_lgb[[2]]),
                    accuracy=c(accuracy_xgb[[1]],accuracy_xgb[[2]],accuracy_lgb[[1]],accuracy_lgb[[2]]),
                    sensitivity=c(sensitivity_xgb[[1]],sensitivity_xgb[[2]],sensitivity_lgb[[1]],sensitivity_lgb[[2]]),
                    specifity=c(specifity_xgb[[1]],specifity_xgb[[2]],specifity_lgb[[1]],specifity_lgb[[2]]),
                    F_score=c(Fscore_xgb[[1]],Fscore_xgb[[2]],Fscore_lgb[[1]],Fscore_lgb[[2]]) ,
                    precision=c(precision_xgb[[1]],precision_xgb[[2]],precision_lgb[[1]],precision_lgb[[2]]))

plot_1000=data.frame(Method=c("XGBoost","XGBoost_Smote","LightGbm","LightGBM_Smote"),
                    AUC=c(Auc_xgb[[3]],Auc_xgb[[4]],Auc_lgb[[3]],Auc_lgb[[4]]),
                    accuracy=c(accuracy_xgb[[3]],accuracy_xgb[[4]],accuracy_lgb[[3]],accuracy_lgb[[4]]),
                    sensitivity=c(sensitivity_xgb[[3]],sensitivity_xgb[[4]],sensitivity_lgb[[3]],sensitivity_lgb[[4]]),
                    specifity=c(specifity_xgb[[3]],specifity_xgb[[4]],specifity_lgb[[3]],specifity_lgb[[4]]),
                    F_score=c(Fscore_xgb[[3]],Fscore_xgb[[4]],Fscore_lgb[[3]],Fscore_lgb[[4]]) ,
                    precision=c(precision_xgb[[3]],precision_xgb[[4]],precision_lgb[[3]],precision_lgb[[4]]))

plot_1500=data.frame(Method=c("XGBoost","XGBoost_Smote","LightGbm","LightGBM_Smote"),
                    AUC=c(Auc_xgb[[5]],Auc_xgb[[6]],Auc_lgb[[5]],Auc_lgb[[6]]),
                    accuracy=c(accuracy_xgb[[5]],accuracy_xgb[[6]],accuracy_lgb[[5]],accuracy_lgb[[6]]),
                    sensitivity=c(sensitivity_xgb[[5]],sensitivity_xgb[[6]],sensitivity_lgb[[5]],sensitivity_lgb[[6]]),
                    specifity=c(specifity_xgb[[5]],specifity_xgb[[6]],specifity_lgb[[5]],specifity_lgb[[6]]),
                    F_score=c(Fscore_xgb[[5]],Fscore_xgb[[6]],Fscore_lgb[[5]],Fscore_lgb[[6]]) ,
                    precision=c(precision_xgb[[5]],precision_xgb[[6]],precision_lgb[[5]],precision_lgb[[6]]))
save(plot_1500,plot_1000,plot_700,file="plot_table_cutoff")
load("plot_table_cutoff")
write.csv(plot_1500,"C:/Users/Asus/Desktop/plot_1500.csv")
write.csv(plot_1000,"C:/Users/Asus/Desktop/plot_1000.csv")
write.csv(plot_700,"C:/Users/Asus/Desktop/plot_700.csv")
##plot performance700------
gg_performance_700=matrix(0,1,6)
gg_performance_700=as.list(gg_performance_700)
names(gg_performance_700)=c( "AUC",  "accuracy", "sensitivity", "specifity",   "F_score", "precision")
gg_performance_700=alist(AUC=,  accuracy=, sensitivity=, specifity=,   F_score=, precision=)

for(i in  c( "AUC",  "accuracy", "sensitivity", "specifity",   "F_score", "precision")){
  #x=i
  #y="for cut-off 700"
  if(i=="AUC"){
  gg_performance_700[[i]]=(ggplot(plot_700, aes(reorder(Method,AUC),AUC,fill=Method))+geom_col()+
                             scale_fill_manual( values = c("XGBoost" = "red3", "XGBoost_Smote" = "red", "LightGbm" = "magenta3","LightGBM_Smote"="magenta"))+                        
     xlab("Methods")  +ylab(i) +
    theme(plot.title = element_text(hjust = 0.1, color = "dark blue", size = 8, face = "bold.italic"),
          axis.line.y = element_line(size = 0.6, color = "black"),
          axis.line.x = element_line(size = 0.1, color = "black"),
          axis.text.x = element_text(size = 4),
          panel.background = element_rect(fill = "white"))
     )}else if(i=="accuracy"){
    gg_performance_700[[i]]=(ggplot(plot_700, aes(reorder(Method,accuracy),accuracy,fill=Method))+geom_col()+
                               scale_fill_manual( values = c("XGBoost" = "red3", "XGBoost_Smote" = "red", "LightGbm" = "magenta3","LightGBM_Smote"="magenta"))+
                               xlab("Methods")  +ylab(i) + 
                               theme(plot.title = element_text(hjust = 0.1, color = "dark blue", size = 8, face = "bold.italic"),
                                     axis.line.y = element_line(size = 0.6, color = "black"),
                                     axis.line.x = element_line(size = 0.1, color = "black"),
                                     axis.text.x = element_text(size =4),
                                     panel.background = element_rect(fill = "white")))
     }else if(i=="sensitivity"){
       gg_performance_700[[i]]=(ggplot(plot_700, aes(reorder(Method,sensitivity),sensitivity,fill=Method))+geom_col()+
                                  scale_fill_manual( values = c("XGBoost" = "red3", "XGBoost_Smote" = "red", "LightGbm" = "magenta3","LightGBM_Smote"="magenta"))+
                                  xlab("Methods")  +ylab(i) + 
                                  theme(plot.title = element_text(hjust = 0.1, color = "dark blue", size = 8, face = "bold.italic"),
                                        axis.line.y = element_line(size = 0.6, color = "black"),
                                        axis.line.x = element_line(size = 0.1, color = "black"),
                                        axis.text.x = element_text(size = 4),
                                        panel.background = element_rect(fill = "white")))
     }else if(i=="specifity"){
       gg_performance_700[[i]]=(ggplot(plot_700, aes(reorder(Method,specifity),specifity,fill=Method))+geom_col()+
                                  scale_fill_manual( values = c("XGBoost" = "red3", "XGBoost_Smote" = "red", "LightGbm" = "magenta3","LightGBM_Smote"="magenta"))+
                                  xlab("Methods")  +ylab("specificity") + 
                                  theme(plot.title = element_text(hjust = 0.1, color = "dark blue", size = 8, face = "bold.italic"),
                                        axis.line.y = element_line(size = 0.6, color = "black"),
                                        axis.line.x = element_line(size = 0.1, color = "black"),
                                        axis.text.x = element_text(size = 4),
                                        panel.background = element_rect(fill = "white")))
     }else if(i=="F_score"){
       gg_performance_700[[i]]=(ggplot(plot_700, aes(reorder(Method,F_score),F_score,fill=Method))+geom_col()+
                                  scale_fill_manual( values = c("XGBoost" = "red3", "XGBoost_Smote" = "red", "LightGbm" = "magenta3","LightGBM_Smote"="magenta"))+
                                  xlab("Methods")  +ylab(i) + 
                                  theme(plot.title = element_text(hjust = 0.1, color = "dark blue", size = 8, face = "bold.italic"),
                                        axis.line.y = element_line(size = 0.6, color = "black"),
                                        axis.line.x = element_line(size = 0.1, color = "black"),
                                        axis.text.x = element_text(size = 4),
                                        panel.background = element_rect(fill = "white")))
     }else{
       gg_performance_700[[i]]=(ggplot(plot_700, aes(reorder(Method,precision),precision,fill=Method))+geom_col()+
                                  scale_fill_manual( values = c("XGBoost" = "red3", "XGBoost_Smote" = "red", "LightGbm" = "magenta3","LightGBM_Smote"="magenta"))+
                                  xlab("Methods")  +ylab(i) + 
                                  theme(plot.title = element_text(hjust = 0.1, color = "dark blue", size = 8, face = "bold.italic"),
                                        axis.line.y = element_line(size = 0.6, color = "black"),
                                        axis.line.x = element_line(size = 0.1, color = "black"),
                                        axis.text.x = element_text(size = 4),
                                        panel.background = element_rect(fill = "white")))
     }
  
  
}
ggarrange(gg_performance_700[[1]],gg_performance_700[[2]],gg_performance_700[[3]]
          , gg_performance_700[[4]] ,gg_performance_700[[5]],gg_performance_700[[6]],
          labels = c("A", "B", "C", "D" , "E", "F"),
          ncol = 2, nrow = 3)
##plot performance1500------
gg_performance_1500=matrix(0,1,6)
gg_performance_1500=as.list(gg_performance_1500)
names(gg_performance_1500)=c( "AUC",  "accuracy", "sensitivity", "specifity",   "F_score", "precision")
gg_performance_1500=alist(AUC=,  accuracy=, sensitivity=, specifity=,   F_score=, precision=)

for(i in  c( "AUC",  "accuracy", "sensitivity", "specifity",   "F_score", "precision")){
  #x=i
  #y="for cut-off 1500"
  if(i=="AUC"){
    gg_performance_1500[[i]]=(ggplot(plot_1500, aes(reorder(Method,AUC),AUC,fill=Method))+geom_col()+
                               scale_fill_manual( values = c("XGBoost" = "blue", "XGBoost_Smote" = "lightblue", "LightGbm" = "green","LightGBM_Smote"="lightgreen"))+                        
                               xlab("Methods")  +ylab(i) + 
                               theme(plot.title = element_text(hjust = 0.1, color = "dark blue", size = 8, face = "bold.italic"),
                                     axis.line.y = element_line(size = 0.6, color = "black"),
                                     axis.line.x = element_line(size = 0.1, color = "black"),
                                     axis.text.x = element_text(size = 4),
                                     panel.background = element_rect(fill = "white"))
    )}else if(i=="accuracy"){
      gg_performance_1500[[i]]=(ggplot(plot_1500, aes(reorder(Method,accuracy),accuracy,fill=Method))+geom_col()+
                                 scale_fill_manual( values = c("XGBoost" = "blue", "XGBoost_Smote" = "lightblue", "LightGbm" = "green","LightGBM_Smote"="lightgreen"))+
                                 xlab("Methods")  +ylab(i) + 
                                 theme(plot.title = element_text(hjust = 0.1, color = "dark blue", size = 8, face = "bold.italic"),
                                       axis.line.y = element_line(size = 0.6, color = "black"),
                                       axis.line.x = element_line(size = 0.1, color = "black"),
                                       axis.text.x = element_text(size =4),
                                       panel.background = element_rect(fill = "white")))
    }else if(i=="sensitivity"){
      gg_performance_1500[[i]]=(ggplot(plot_1500, aes(reorder(Method,sensitivity),sensitivity,fill=Method))+geom_col()+
                                 scale_fill_manual( values = c("XGBoost" = "blue", "XGBoost_Smote" = "lightblue", "LightGbm" = "green","LightGBM_Smote"="lightgreen"))+
                                 xlab("Methods")  +ylab(i) + 
                                 theme(plot.title = element_text(hjust = 0.1, color = "dark blue", size = 8, face = "bold.italic"),
                                       axis.line.y = element_line(size = 0.6, color = "black"),
                                       axis.line.x = element_line(size = 0.1, color = "black"),
                                       axis.text.x = element_text(size = 4),
                                       panel.background = element_rect(fill = "white")))
    }else if(i=="specifity"){
      gg_performance_1500[[i]]=(ggplot(plot_1500, aes(reorder(Method,specifity),specifity,fill=Method))+geom_col()+
                                 scale_fill_manual( values = c("XGBoost" = "blue", "XGBoost_Smote" = "lightblue", "LightGbm" = "green","LightGBM_Smote"="lightgreen"))+
                                 xlab("Methods")  +ylab("specificity") + 
                                 theme(plot.title = element_text(hjust = 0.1, color = "dark blue", size = 8, face = "bold.italic"),
                                       axis.line.y = element_line(size = 0.6, color = "black"),
                                       axis.line.x = element_line(size = 0.1, color = "black"),
                                       axis.text.x = element_text(size = 4),
                                       panel.background = element_rect(fill = "white")))
    }else if(i=="F_score"){
      gg_performance_1500[[i]]=(ggplot(plot_1500, aes(reorder(Method,F_score),F_score,fill=Method))+geom_col()+
                                 scale_fill_manual( values = c("XGBoost" = "blue", "XGBoost_Smote" = "lightblue", "LightGbm" = "green","LightGBM_Smote"="lightgreen"))+
                                 xlab("Methods")  +ylab(i) + 
                                 theme(plot.title = element_text(hjust = 0.1, color = "dark blue", size = 8, face = "bold.italic"),
                                       axis.line.y = element_line(size = 0.6, color = "black"),
                                       axis.line.x = element_line(size = 0.1, color = "black"),
                                       axis.text.x = element_text(size = 4),
                                       panel.background = element_rect(fill = "white")))
    }else{
      gg_performance_1500[[i]]=(ggplot(plot_1500, aes(reorder(Method,precision),precision,fill=Method))+geom_col()+
                                 scale_fill_manual( values = c("XGBoost" = "blue", "XGBoost_Smote" = "lightblue", "LightGbm" = "green","LightGBM_Smote"="lightgreen"))+
                                 xlab("Methods")  +ylab(i) + 
                                 theme(plot.title = element_text(hjust = 0.1, color = "dark blue", size = 8, face = "bold.italic"),
                                       axis.line.y = element_line(size = 0.6, color = "black"),
                                       axis.line.x = element_line(size = 0.1, color = "black"),
                                       axis.text.x = element_text(size = 4),
                                       panel.background = element_rect(fill = "white")))
    }
  
  
}
ggarrange(gg_performance_1500[[1]],gg_performance_1500[[2]],gg_performance_1500[[3]]
          , gg_performance_1500[[4]] ,gg_performance_1500[[5]],gg_performance_1500[[6]],
          labels = c("A", "B", "C", "D" , "E", "F"),
          ncol = 2, nrow = 3)



##plot performance1000------
gg_performance_1000=matrix(0,1,6)
gg_performance_1000=as.list(gg_performance_1000)
names(gg_performance_1000)=c( "AUC",  "accuracy", "sensitivity", "specifity",   "F_score", "precision")
gg_performance_1000=alist(AUC=,  accuracy=, sensitivity=, specifity=,   F_score=, precision=)

for(i in  c( "AUC",  "accuracy", "sensitivity", "specifity",   "F_score", "precision")){
  #x=i
  #y="for cut-off 1000"
  if(i=="AUC"){
    gg_performance_1000[[i]]=(ggplot(plot_1000, aes(reorder(Method,AUC),AUC,fill=Method))+geom_col()+
                                scale_fill_manual( values = c("XGBoost" = "orange3", "XGBoost_Smote" = "orange", "LightGbm" = "yellow3","LightGBM_Smote"="yellow"))+                        
                                xlab("Methods")  +ylab(i) + 
                                theme(plot.title = element_text(hjust = 0.1, color = "dark blue", size = 8, face = "bold.italic"),
                                      axis.line.y = element_line(size = 0.6, color = "black"),
                                      axis.line.x = element_line(size = 0.1, color = "black"),
                                      axis.text.x = element_text(size = 4),
                                      panel.background = element_rect(fill = "white"))
    )}else if(i=="accuracy"){
      gg_performance_1000[[i]]=(ggplot(plot_1000, aes(reorder(Method,accuracy),accuracy,fill=Method))+geom_col()+
                                  scale_fill_manual( values = c("XGBoost" = "orange3", "XGBoost_Smote" = "orange", "LightGbm" = "yellow3","LightGBM_Smote"="yellow"))+
                                  xlab("Methods")  +ylab(i) + 
                                  theme(plot.title = element_text(hjust = 0.1, color = "dark blue", size = 8, face = "bold.italic"),
                                        axis.line.y = element_line(size = 0.6, color = "black"),
                                        axis.line.x = element_line(size = 0.1, color = "black"),
                                        axis.text.x = element_text(size =4),
                                        panel.background = element_rect(fill = "white")))
    }else if(i=="sensitivity"){
      gg_performance_1000[[i]]=(ggplot(plot_1000, aes(reorder(Method,sensitivity),sensitivity,fill=Method))+geom_col()+
                                  scale_fill_manual( values = c("XGBoost" = "orange3", "XGBoost_Smote" = "orange", "LightGbm" = "yellow3","LightGBM_Smote"="yellow"))+
                                  xlab("Methods")  +ylab(i) + 
                                  theme(plot.title = element_text(hjust = 0.1, color = "dark blue", size = 8, face = "bold.italic"),
                                        axis.line.y = element_line(size = 0.6, color = "black"),
                                        axis.line.x = element_line(size = 0.1, color = "black"),
                                        axis.text.x = element_text(size = 4),
                                        panel.background = element_rect(fill = "white")))
    }else if(i=="specifity"){
      gg_performance_1000[[i]]=(ggplot(plot_1000, aes(reorder(Method,specifity),specifity,fill=Method))+geom_col()+
                                  scale_fill_manual( values = c("XGBoost" = "orange3", "XGBoost_Smote" = "orange", "LightGbm" = "yellow3","LightGBM_Smote"="yellow"))+
                                  xlab("Methods")  +ylab("specificity") + 
                                  theme(plot.title = element_text(hjust = 0.1, color = "dark blue", size = 8, face = "bold.italic"),
                                        axis.line.y = element_line(size = 0.6, color = "black"),
                                        axis.line.x = element_line(size = 0.1, color = "black"),
                                        axis.text.x = element_text(size = 4),
                                        panel.background = element_rect(fill = "white")))
    }else if(i=="F_score"){
      gg_performance_1000[[i]]=(ggplot(plot_1000, aes(reorder(Method,F_score),F_score,fill=Method))+geom_col()+
                                  scale_fill_manual( values = c("XGBoost" = "orange3", "XGBoost_Smote" = "orange", "LightGbm" = "yellow3","LightGBM_Smote"="yellow"))+
                                  xlab("Methods")  +ylab(i) + 
                                  theme(plot.title = element_text(hjust = 0.1, color = "dark blue", size = 8, face = "bold.italic"),
                                        axis.line.y = element_line(size = 0.6, color = "black"),
                                        axis.line.x = element_line(size = 0.1, color = "black"),
                                        axis.text.x = element_text(size = 4),
                                        panel.background = element_rect(fill = "white")))
    }else{
      gg_performance_1000[[i]]=(ggplot(plot_1000, aes(reorder(Method,precision),precision,fill=Method))+geom_col()+
                                  scale_fill_manual( values = c("XGBoost" = "orange3", "XGBoost_Smote" = "orange", "LightGbm" = "yellow3","LightGBM_Smote"="yellow"))+
                                  xlab("Methods")  +ylab(i) + 
                                  theme(plot.title = element_text(hjust = 0.1, color = "dark blue", size = 8, face = "bold.italic"),
                                        axis.line.y = element_line(size = 0.6, color = "black"),
                                        axis.line.x = element_line(size = 0.1, color = "black"),
                                        axis.text.x = element_text(size = 4),
                                        panel.background = element_rect(fill = "white")))
    }
  
  
}
ggarrange(gg_performance_1000[[1]],gg_performance_1000[[2]],gg_performance_1000[[3]]
          , gg_performance_1000[[4]] ,gg_performance_1000[[5]],gg_performance_1000[[6]],
          labels = c("A", "B", "C", "D" , "E", "F"),
          ncol = 2, nrow = 3)



##AUC_plot700----

par(mar=c(4,4,4,4),mfrow=(c(2,2)))
plot(plot_xgb[[1]],legendpos="bottom", YIndex = F,values = T,legend = F,col=c("red3","red4"))
title(main=paste("A","XGBoost",sep = "          "), font.main= 1, col.main= "darkblue")
plot(plot_xgb[[2]],legendpos="bottom",YIndex = FALSE,values = T,legend = F,col=c("red","red4"))
title(main=paste("B","XGBoost-smote_nc",sep = "       "), font.main= 1, col.main= "darkblue")
plot(plot_lgb[[1]],legendpos="bottom",YIndex = FALSE,values = T,legend = F,col=c("magenta3","red4"))
title(main=paste("C","LightGBM",sep = "          "), font.main= 1, col.main= "darkblue")
plot(plot_lgb[[2]],legendpos="bottom",YIndex = FALSE,values = T,legend = F,col=c("magenta","red4"))
title(main=paste("D","LightGBM-smote_nc",sep = "       "), font.main= 1, col.main= "darkblue")

##AUC_plot1000----
par(mar=c(4,4,4,4),mfrow=(c(2,2)))
M1=plot(plot_xgb[[3]],legendpos="bottom",YIndex = FALSE,values = T,legend = F,col=c("orange3","red4"))
title(main=paste("A","XGBoost",sep = "          "), font.main= 1, col.main= "darkblue")
plot(plot_xgb[[4]],legendpos="bottom",YIndex = FALSE,values = T,legend = F,col=c("orange","red4"))
title(main=paste("B","XGBoost-smote_nc",sep = "       "), font.main= 1, col.main= "darkblue")
plot(plot_lgb[[3]],legendpos="bottom",YIndex = FALSE,values = T,legend = F,col=c("Yellow3","red4"))
title(main=paste("C","LightGBM",sep = "          "), font.main= 1, col.main= "darkblue")
plot(plot_lgb[[4]],legendpos="bottom",YIndex = FALSE,values = T,legend = F,col=c("Yellow","red4"))
title(main=paste("D","LightGBM-smote_nc",sep = "       "), font.main= 1, col.main= "darkblue")

##AUC_plot1500----
par(mar=c(4,4,4,4),mfrow=(c(2,2)))
plot(plot_xgb[[5]],legendpos="bottom",YIndex = FALSE,values = T,legend = F,col=c("blue","red4"))
title(main=paste("A","XGBoost",sep = "          "), font.main= 1, col.main= "darkblue")
plot(plot_xgb[[6]],legendpos="bottom",YIndex = FALSE,values = T,legend = F,col=c("lightblue","red4"))
title(main=paste("B","XGBoost-smote_nc",sep = "       "), font.main= 1, col.main= "darkblue")
plot(plot_lgb[[5]],legendpos="bottom",YIndex = FALSE,values = T,legend = F,col=c("green","red4"))
title(main=paste("C","LightGBM",sep = "       "), font.main= 1, col.main= "darkblue")
plot(plot_lgb[[6]],legendpos="bottom",YIndex = FALSE,values = T,legend = F,col=c("lightgreen","red4"))
title(main=paste("D","LightGBM-smote_nc",sep = "          "), font.main= 1, col.main= "darkblue")
