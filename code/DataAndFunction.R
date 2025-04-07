library("glmnet")         #Lasso and Elastic-Net Regularized GLM
library("rpart")          #Classification and Regression Trees 
library("rpart.plot")     #Plot Decision Tree
library("randomForest")   #Random Forests for Classification and Regression 
library("readxl")
library("janitor")
library("xgboost")
library("dplyr")
library("ggplot2")
library("smotefamily")
library("lightgbm")
#library("mltools")
#library("data.table")
data <- read_excel("F:/project 1/TNC_b.xlsx")

data$Date_pregn <- as.Date(data$Date_pregn, tz="UTC")
data$date_employment <- as.Date(data$date_employment, tz="UTC")
data$duration=difftime(data$Date_pregn,data$date_employment,units = "days")/365
#data$experience=if_else(data$duration>=4,1,if_else(data$duration<4 & data$duration>=1,2,3))
data$experience=as.numeric(data$duration)
##felan 8 related to bloody
data=data[,c(-5,-4,-9,-11)]
data=data[,c(1:6,8,7)]
#"experience"
factor_TNC <- c("state","number_of_childeren","abortion")
data[, factor_TNC] <- lapply(data[, factor_TNC], factor)
#"number_of_childeren"
##cleaning
summary(data)
head(data)
keep <- !apply(is.na(data) , 1, any)
data=data[keep,]
##tnccutofff classify
data$class1500=lapply(data$CellCount,function(x){if(x>=1500) data$class1500=1 else data$class1500=0})
data$class1500=as.numeric(data$class1500)
data$class1500=as.factor(data$class1500)
data$class1000=lapply(data$CellCount,function(x){if(x>=1000) data$class1000=1 else data$class1000=0})
data$class1000=as.numeric(data$class1000)
data$class1000=as.factor(data$class1000)
data$class700=lapply(data$CellCount,function(x){if(x>=700) data$class700=1 else data$class700=0})
data$class700=as.numeric(data$class700)
data$class700=as.factor(data$class700)

data<-data[,-8] ##delete cellcount


model_city=model.matrix(~data$state)
model_city=as.data.frame(model_city[,-1])
city=c("Alborz" , "ARDEBIL","AZARBAYEJAN_GH","AZARBAYEJAN_SH", "BOOSHEHR","CHAHR MAHAL","Esfehan","FARS", "GHAZVIN","GHOM","GILAN" ,"GOLESTAN", "HAMEDAN"               
       ,"HORMOZGAN","ILAM"                  
       , "KERMAN" ,"KERMANSHAH"            
       , "KHORASAN_GO" , "KHORASAN_RAZAVI"       
       ,"KHORASAN_SH","KHOZESTAN"             
       ,"KOHGELOYE","KORDESTAN"             
       ,"LORESTAN","MARKAZI"               
       , "MAZANDARAN","SEMNAN"                
       ,"SISTAN_VA_BALOOCHESTAN" ,"TEHRAN"                
       , "YAZD", "ZANJAN")



count_prob=matrix(0,length(city),8)
colnames(count_prob)=c("ratio","no.city","low700","up700","low1000","up1000","low1500","up1500")
rownames(count_prob)=(city)
for(i in city){
  count_prob[i,1]=nrow(data[data$state==i,1])/nrow(data)
  count_prob[i,2]=nrow(data[which(data$state==i ),])
  count_prob[i,3]=nrow(data[which(data$class700==0 & data$state==i ),])
  count_prob[i,4]=nrow(data[which(data$class700==1 & data$state==i ),])
  count_prob[i,5]=nrow(data[which(data$class1000==0 & data$state==i ),])
  count_prob[i,6]=nrow(data[which(data$class1000==1 & data$state==i ),])
  count_prob[i,7]=nrow(data[which(data$class1500==0 & data$state==i ),])
  count_prob[i,8]=nrow(data[which(data$class1500==1 & data$state==i ),])
  
}
before_number_data=nrow(data)
city_with_lowdata=rownames(count_prob[count_prob[,"no.city"]>800,])
data=data[which(data$state %in% city_with_lowdata),]
after_number_data=nrow(data)
###train sample

train_data_sample=as.data.frame(matrix(0,1,length(data)))
colnames( train_data_sample)=names(data)
train_data_sample=train_data_sample[-1,]

test_data_sample=as.data.frame(matrix(0,1,length(data)))
colnames( test_data_sample)=names(data)
test_data_sample=test_data_sample[-1,]

city=city_with_lowdata
for(i in city){
  set.seed(1234)
  total=which(data$state==i)
  case_train=sample(which(data$state==i),0.8*length(total))
  train_data_sample=rbind( train_data_sample,data[case_train,])
  test_data_sample=rbind( test_data_sample,data[setdiff(total,case_train),])
}
test_data_sample$state=factor(test_data_sample$state)

counts1=table(train_data_sample$class700)
counts2=table(train_data_sample$class1000)
counts3=table(train_data_sample$class1500)
par(mar=c(2,2,2,2),mfrow=(c(2,2)))
barplot(counts1,col=c("red","darkblue"),main="A                                                     ",xlab = "Class",names.arg = c("<0.7",">0.7"))
barplot(counts2,col=c("red","darkblue"),main="B                                                     ",xlab = "Class",names.arg = c("<1",">1"))
barplot(counts3,col=c("red","darkblue"),main="C                                                     ",xlab = "Class",names.arg = c("<1.5",">1.5"))

#save(train_data_sample,test_data_sample,count_prob,file="dataprd")
load(file="dataprd")
write.csv(train_data_sample,"C:/Users/Asus/Desktop/train_data.csv")

train_real_700=read_excel("F:/project 1//project 1/dataSmote.xlsx",sheet = 1)
train_real_1000=read_excel("F:/project 1//project 1/dataSmote.xlsx",sheet = 2)
train_real_1500=read_excel("F:/project 1/dataSmote.xlsx",sheet = 3)
train_smote_700=read_excel("F:/project 1/dataSmote.xlsx",sheet = 4)
train_smote_1000=read_excel("F:/project 1/dataSmote.xlsx",sheet = 5)
train_smote_1500=read_excel("F:/project 1/dataSmote.xlsx",sheet = 6)

train_real_700[, factor_TNC] <- sapply(train_real_700[, factor_TNC], factor)
train_real_1000[, factor_TNC] <- sapply(train_real_1000[, factor_TNC], factor)
train_real_1500[, factor_TNC] <- sapply(train_real_1500[, factor_TNC], factor)
train_smote_700[, factor_TNC] <- sapply(train_smote_700[, factor_TNC], factor)
train_smote_1000[, factor_TNC] <- sapply(train_smote_1000[, factor_TNC], factor)
train_smote_1500[, factor_TNC] <- sapply(train_smote_1500[, factor_TNC], factor)

train_data_sample1=alist(real_700= ,smote_700= ,real_1000= ,smote_1000= ,real_1500= ,smote_1500= )
train_data_sample1[[1]]=train_real_700
train_data_sample1[[2]]=train_smote_700
train_data_sample1[[3]]=train_real_1000
train_data_sample1[[4]]=train_smote_1000
train_data_sample1[[5]]=train_real_1500
train_data_sample1[[6]]=train_smote_1500

train_data_sample2=alist(real_700= ,smote_700= ,real_1000= ,smote_1000= ,real_1500= ,smote_1500= )
train_data_sample2[[1]]=data.matrix(as.data.frame(train_real_700))
train_data_sample2[[2]]=data.matrix(as.data.frame(train_smote_700))
train_data_sample2[[3]]=data.matrix(as.data.frame(train_real_1000))
train_data_sample2[[4]]=data.matrix(as.data.frame(train_smote_1000))
train_data_sample2[[5]]=data.matrix(as.data.frame(train_real_1500))
train_data_sample2[[6]]=data.matrix(as.data.frame(train_smote_1500))




test_data_sample1=alist(real_700= ,smote_700= ,real_1000= ,smote_1000= ,real_1500= ,smote_1500= )

test_data_sample1[[1]]=test_data_sample[,c(1:7,10)]
test_data_sample1[[2]]=test_data_sample[,c(1:7,10)]
test_data_sample1[[3]]=test_data_sample[,c(1:7,9)]
test_data_sample1[[4]]=test_data_sample[,c(1:7,9)]
test_data_sample1[[5]]=test_data_sample[,c(1:7,8)]
test_data_sample1[[6]]=test_data_sample[,c(1:7,8)]

test_data_sample2=alist(real_700= ,smote_700= ,real_1000= ,smote_1000= ,real_1500= ,smote_1500= )

test_data_sample2[[1]]=data.matrix(as.data.frame(test_data_sample[,c(1:7,10)]))
test_data_sample2[[2]]=data.matrix(as.data.frame(test_data_sample[,c(1:7,10)]))
test_data_sample2[[3]]=data.matrix(as.data.frame(test_data_sample[,c(1:7,9)]))
test_data_sample2[[4]]=data.matrix(as.data.frame(test_data_sample[,c(1:7,9)]))
test_data_sample2[[5]]=data.matrix(as.data.frame(test_data_sample[,c(1:7,8)]))
test_data_sample2[[6]]=data.matrix(as.data.frame(test_data_sample[,c(1:7,8)]))

test_data_sample2[[1]][,8]=ifelse(test_data_sample2[[1]][,8]>1,1,0)
test_data_sample2[[2]][,8]=ifelse(test_data_sample2[[2]][,8]>1,1,0)
test_data_sample2[[3]][,8]=ifelse(test_data_sample2[[3]][,8]>1,1,0)
test_data_sample2[[4]][,8]=ifelse(test_data_sample2[[4]][,8]>1,1,0)
test_data_sample2[[5]][,8]=ifelse(test_data_sample2[[5]][,8]>1,1,0)
test_data_sample2[[6]][,8]=ifelse(test_data_sample2[[6]][,8]>1,1,0)
#####
g="real_700"
for(g in c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")){
model_city1=model.matrix(~ 0+train_data_sample1[[as.character(g)]]$state)  
colnames(model_city1)=c(city_with_lowdata)
num_child=model.matrix(~ 0+train_data_sample1[[as.character(g)]]$number_of_childeren) 
colnames(num_child)=c("birth_order0","birth_order1","birth_order2","birth_order3","birth_order4","birth_order5")
train_data_sample1[[as.character(g)]]=cbind(model_city1, train_data_sample1[[as.character(g)]])
train_data_sample1[[as.character(g)]]=select(train_data_sample1[[as.character(g)]],-state)
train_data_sample1[[as.character(g)]]=cbind(num_child, train_data_sample1[[as.character(g)]])
train_data_sample1[[as.character(g)]]=select(train_data_sample1[[as.character(g)]],-number_of_childeren)

for(g in c("real_700","smote_700","real_1000","smote_1000","real_1500", "smote_1500")){


colnames(model_city2)=c(city_with_lowdata)
num_child2=model.matrix(~ 0+test_data_sample1[[as.character(g)]]$number_of_childeren) 
colnames(num_child2)=c("birth_order0","birth_order1","birth_order2","birth_order3","birth_order4","birth_order5")

test_data_sample1[[as.character(g)]]=cbind(model_city2, test_data_sample1[[as.character(g)]])
test_data_sample1[[as.character(g)]]=select(test_data_sample1[[as.character(g)]],-state)
test_data_sample1[[as.character(g)]]=cbind(num_child2, test_data_sample1[[as.character(g)]])
test_data_sample1[[as.character(g)]]=select(test_data_sample1[[as.character(g)]],-number_of_childeren)

}



avalute_fscore<-function(datatest_y,prediction_model){
  prediction_model=factor(prediction_model,levels = c(0,1))
  confm_svm <- as.matrix(table(actual = datatest_y, prediction = prediction_model))
  f_score<-(confm_svm[2,2])/(confm_svm[2,2]+confm_svm[1,2])
  return(f_score)
}

avalute_fitness<-function(mean_f_score,num_feathures,num_total){
  FITNESS<-(-mean_f_score)
  return(FITNESS)
  
  
}



##xg-----
fitness_xgb=function(datatrain,x,upper_var,less_var,k_fold){
  feature=rep(1,41)
  x_eta= x[1:7] 
  x_lambda=x[8:14]
  x_max_depth=x[15:20]
  x_iter= x[21:29]
  x_eta_trans=binary2decimal(x_eta)/100
  x_lambda_trans=binary2decimal(x_lambda)/100
  x_max_depth_trans=binary2decimal(x_max_depth)
  x_iter_trans=binary2decimal(x_iter)
  
  data_sample_train_chr<-model.matrix(datatrain[,ncol(datatrain)]~ +.,datatrain)[,-1]
  data_sample_train_chr<-data_sample_train_chr[,-ncol(data_sample_train_chr)]
  data_sample_train_y<-datatrain[,ncol(datatrain)]
  fold <- sample(1 : k_fold, nrow(datatrain), rep = TRUE)
  ftr=ifelse(feature<1,-1,1)
  col_ch<-c(1:ncol(data_sample_train_chr))
  col_ch<-col_ch[which(ftr>0)]
  if (x_eta_trans>upper_var[1]){
    x_eta_trans=upper_var[1]
  } else if(x_eta_trans<less_var[1]) {
    x_eta_trans=less_var[1] 
  }  
  if(x_lambda_trans>upper_var[2]) {
    x_lambda_trans=upper_var[2]
  } else if (x_lambda_trans<less_var[2]) {
    x_lambda_trans=less_var[2]
  } 
  if(x_max_depth_trans>upper_var[3]){
    x_max_depth_trans=upper_var[3]    
  } else if(x_max_depth_trans<less_var[3]){
    x_max_depth_trans=less_var[3]
  }
  if(x_iter_trans>upper_var[4]) {
    x_iter_trans=upper_var[4]
  } else if(x_iter_trans<less_var[4])  {
    x_iter_trans=less_var[4]
  }    
  
  matrix_f_sc<-matrix(0,1,k_fold)
  for(i in 1:k_fold){
    data_sample_train_chr_new<-data_sample_train_chr[which(fold!=i),col_ch]
    data_sample_train_y_new<-data_sample_train_y[which(fold!=i)]
    set.seed(123)
    
    xgb_1 <- xgboost(data =  data_sample_train_chr_new, 
                     label = data_sample_train_y_new,
                     eta = x_eta_trans,                       #learning rate
                     lambda = x_lambda_trans,                      #regularization term
                     max_depth = x_max_depth_trans,                   #tree depth 
                     nrounds = x_iter_trans,                   #max number of boosting iterations
                     objective = "binary:hinge" ,        #for classification models
                     verbose = 1                          #silent
    ) 
    data_sample_test_chr_new<-data_sample_train_chr[which(fold==i),col_ch]
    data_sample_test_y_new<-data_sample_train_y[which(fold==i)]
    predict_y<-predict(xgb_1,data_sample_test_chr_new)
    f_sc<-avalute_fscore(data_sample_test_y_new,predict_y)
    matrix_f_sc[i]<- f_sc
  }
  mean_f_sc=mean(matrix_f_sc) 
  avalute_fitness(mean_f_sc,ncol(data_sample_train_chr_new),ncol(data_sample_train_chr))  
}

##xgb_bssa----
fitness_xgb_bssa=function(datatrain,feature,x_eta,x_lambda2,x_lambda2,x_max_depth,x_iter,upper_var,less_var,k_fold){
  x_eta_trans=x_eta
  x_lambda_trans=x_lambda
  x_max_depth_trans=binary2decimal(x_max_depth)
  x_iter_trans=binary2decimal(x_iter)
  
  data_sample_train_chr<-model.matrix(datatrain[,ncol(datatrain)]~ +.,datatrain)[,-1]
  data_sample_train_chr<-data_sample_train_chr[,-ncol(data_sample_train_chr)]
  data_sample_train_y<-datatrain[,ncol(datatrain)]
  fold <- sample(1 : k_fold, nrow(datatrain), rep = TRUE)
  ftr=ifelse(feature<1,-1,1)
  col_ch<-c(1:ncol(data_sample_train_chr))
  col_ch<-col_ch[which(ftr>0)]
  if (x_eta_trans>upper_var[1]){
    x_eta_trans=upper_var[1]
  } else if(x_eta_trans<less_var[1]) {
    x_eta_trans=less_var[1] 
  }  
  if(x_lambda_trans>upper_var[2]) {
    x_lambda_trans=upper_var[2]
  } else if (x_lambda_trans<less_var[2]) {
    x_lambda_trans=less_var[2]
  } 
  if(x_max_depth_trans>upper_var[3]){
    x_max_depth_trans=upper_var[3]    
  } else if(x_max_depth_trans<less_var[3]){
    x_max_depth_trans=less_var[3]
  }
  if(x_iter_trans>upper_var[4]) {
    x_iter_trans=upper_var[4]
  } else if(x_iter_trans<less_var[4])  {
    x_iter_trans=less_var[4]
  }    
  
  matrix_f_sc<-matrix(0,1,k_fold)
  for(i in 1:k_fold){
    data_sample_train_chr_new<-data_sample_train_chr[which(fold!=i),col_ch]
    data_sample_train_y_new<-data_sample_train_y[which(fold!=i)]
    set.seed(123)
    
    xgb_1 <- xgboost(data =  data_sample_train_chr_new, 
                     label = data_sample_train_y_new,
                     eta = x_eta_trans,                       #learning rate
                     lambda = x_lambda_trans,                      #regularization term
                     max_depth = x_max_depth_trans,                   #tree depth 
                     nrounds = x_iter_trans,                   #max number of boosting iterations
                     objective = "binary:hinge" ,        #for classification models
                     verbose = 0                          #silent
    ) 
    data_sample_test_chr_new<-data_sample_train_chr[which(fold==i),col_ch]
    data_sample_test_y_new<-data_sample_train_y[which(fold==i)]
    predict_y<-predict(xgb_1,data_sample_test_chr_new)
    f_sc<-avalute_fscore(data_sample_test_y_new,predict_y)
    matrix_f_sc[i]<- f_sc
  }
  mean_f_sc=mean(matrix_f_sc) 
  avalute_fitness(mean_f_sc,ncol(data_sample_train_chr_new),ncol(data_sample_train_chr))  
}


#xgb_basian optimizan-----
fitness_xgb2=function(datatrain, x_eta,subsample,colsample_bytree,lambda1,lambda2,gamma ,x_max_depth,x_iter,k_fold){
  feature=rep(1,28)
  
  x_eta_trans= x_eta
  x_lambda_trans=lambda2
  x_max_depth_trans=x_max_depth
  x_iter_trans=x_iter
  
  data_sample_train_chr<-as.matrix(apply(datatrain,2,as.numeric))
  data_sample_train_chr<-data_sample_train_chr[,-ncol(data_sample_train_chr)]
  data_sample_train_y<-datatrain[,ncol(datatrain)]
  fold <- sample(1 : k_fold, nrow(datatrain), rep = TRUE)
  ftr=ifelse(feature<1,-1,1)
  col_ch<-c(1:ncol(data_sample_train_chr))
  col_ch<-col_ch[which(ftr>0)]
  
  
  matrix_f_sc<-matrix(0,1,k_fold)
  for(i in 1:k_fold){
    data_sample_train_chr_new<-data_sample_train_chr[which(fold!=i),col_ch]
    data_sample_train_y_new<-data_sample_train_y[which(fold!=i)]
    set.seed(123)
    
    xgb_1 <- xgboost(data =  data_sample_train_chr_new, 
                     label = data_sample_train_y_new,
                     colsample_bytree=colsample_bytree,
                     eta = x_eta_trans,                       #learning rate
                     lambda = x_lambda_trans,                      #regularization term
                     alpha=lambda1,
                     max_depth = x_max_depth_trans,                   #tree depth 
                     nrounds = x_iter_trans,                   #max number of boosting iterations
                     subsample=subsample,
                     gamma=gamma,
                     objective = "binary:logistic" ,        #for classification models
                     verbose = 1                          #silent
    ) 
    data_sample_test_chr_new<-data_sample_train_chr[which(fold==i),col_ch]
    data_sample_test_y_new<-data_sample_train_y[which(fold==i)]
    predict_y<-predict(xgb_1,data_sample_test_chr_new)
    #f_sc<-avalute_fscore(data_sample_test_y_new,predict_y)
    f_sc=rocit(score = predict_y, class = data_sample_test_y_new)$AUC
    matrix_f_sc[i]<- f_sc
  }
  mean_f_sc=mean(matrix_f_sc) 
  k=avalute_fitness(mean_f_sc,ncol(data_sample_train_chr_new),ncol(data_sample_train_chr)) 
  list(Score =-k , Pred = 0)
}
#lightboosting_basian optimazation-----
fitness_lightgbm=function(datatrain,learning_rate,num_iterations,max_depth, bagging_fraction,feature_fraction,lambda_l1, lambda_l2,num_leaves,k_fold){
  feature=rep(1,7)
  
  learning_rate_trans = learning_rate
  num_iterations_trans=num_iterations
  max_depth_trans=max_depth
  bagging_fraction_trans=bagging_fraction
  feature_fraction_trans= feature_fraction
  lambda_l1_trans=lambda_l1
  lambda_l2_trans= lambda_l2
  num_leaves_trans = num_leaves
  
  data_sample_train_chr<-(datatrain)
  
  data_sample_train_chr<-data_sample_train_chr[,-ncol(data_sample_train_chr)]
  data_sample_train_y<-datatrain[,ncol(datatrain)]
  fold <- sample(1 : k_fold, nrow(datatrain), rep = TRUE)
  ftr=ifelse(feature<1,-1,1)
  col_ch<-c(1:ncol(data_sample_train_chr))
  col_ch<-col_ch[which(ftr>0)]
  
  
  matrix_f_sc<-matrix(0,1,k_fold)
  for(i in 1:k_fold){
    data_sample_train_chr_new<-data_sample_train_chr[which(fold!=i),col_ch]
    data_sample_train_y_new<-data_sample_train_y[which(fold!=i)]
    set.seed(123)
    train_params <- list(
      learning_rate = learning_rate_trans,
      boosting= "goss",
      num_iterations=num_iterations_trans,
      max_depth=max_depth_trans,
      bagging_fraction=bagging_fraction_trans,
      feature_fraction=feature_fraction_trans,
      lambda_l1=lambda_l1_trans,
      lambda_l2=lambda_l2_trans,
      num_leaves = num_leaves_trans,
      objective = "binary"
    )
    dtrain=lgb.Dataset(data_sample_train_chr_new,label=data_sample_train_y_new, categorical_feature=c(1L,2L,3L))
    lightgbm_1 <- lgb.train(data =   dtrain, 
                        params = train_params
                        
    )
    ##
    
    data_sample_test_chr_new<-data_sample_train_chr[which(fold==i),col_ch]
    data_sample_test_y_new<-data_sample_train_y[which(fold==i)]
    predict_y<-predict(lightgbm_1 ,data_sample_test_chr_new)
    #
    #ruc=rocit(score = predict_y, class = data_sample_test_y_new)
    #cutt=plot(ruc ,legendpos="bottom",values = T,legend = F)
    #predict_y<-if_else((predict_y>=as.numeric(cutt[[6]][4])),1,0)
    #predict_y=factor(predict_y,levels = c(0,1))
    #f_sc<-avalute_fscore(data_sample_test_y_new,predict_y)
    f_sc=rocit(score = predict_y, class = data_sample_test_y_new)$AUC
    matrix_f_sc[i]<- f_sc
   
  }
  mean_f_sc=mean(matrix_f_sc) 
  k=avalute_fitness(mean_f_sc,ncol(data_sample_train_chr_new),ncol(data_sample_train_chr)) 
  list(Score =-k , Pred = 0)
}

##xgb_test----
fitness_test_xgb=function(datatrain,datatest, x_eta,subsample,colsample_bytree,lambda1,lambda,gamma, max_depth,x_iter){
 
  
  data_sample_train_chr<-as.matrix(apply(datatrain,2,as.numeric))
  data_sample_train_chr<-data_sample_train_chr[,-ncol(data_sample_train_chr)]
  data_sample_train_y<-datatrain[,ncol(datatrain)]
  
  data_sample_test_chr<-as.matrix(apply(datatest,2,as.numeric))
  data_sample_test_chr<-data_sample_test_chr[,-ncol(data_sample_test_chr)]
  data_sample_test_y<-datatest[,ncol(datatest)]
    set.seed(123)
    xgb_1 <- xgboost(data =  data_sample_train_chr, 
                     label = data_sample_train_y,
                     colsample_bytree=colsample_bytree,
                     eta = x_eta,                       #learning rate
                     lambda =lambda,                      #regularization term
                     alpha=lambda1,
                     max_depth = max_depth,                   #tree depth 
                     nrounds = x_iter,                   #max number of boosting iterations
                     subsample=subsample,
                     gamma=gamma,
                     objective = "binary:logistic" ,        #for classification models
                     verbose = 1                          #silent
    ) 
    xgb1=xgb_1
    predict_y<-predict(xgb_1,data_sample_test_chr)
    ruc=rocit(score = predict_y, class = data_sample_test_y)
    cutt=plot(ruc ,legendpos="bottom",values = T,legend = F)
    predict_y<-if_else((predict_y>=as.numeric(cutt[[6]][4])),1,0)
    predict_y=factor(predict_y,levels = c(0,1))
    confmatrix<- as.matrix(table(actual =data_sample_test_y , prediction = predict_y))
    plot_info=ruc
    Auc_xgb=ruc$AUC
    fscore=(2*confmatrix[2,2])/(2*confmatrix[2,2]+confmatrix[1,2]+confmatrix[2,1])
    accuracy=(confmatrix[2,2]+confmatrix[1,1])/sum(confmatrix)
    sensitivity=confmatrix[2,2]/(confmatrix[2,2]+confmatrix[2,1])
    specifity=confmatrix[1,1]/(confmatrix[1,1]+confmatrix[1,2])
    precision=confmatrix[2,2]/(confmatrix[2,2]+confmatrix[1,2])
    without_pred_xgb = (confmatrix[2,2]+confmatrix[2,1])/sum(confmatrix)
      
    result=list(xgb1=xgb1,confmatrix=confmatrix,plot_info=plot_info,Auc_xgb=Auc_xgb,fscore=fscore,
                accuracy=accuracy,sensitivity=sensitivity, specifity=specifity,precision=precision,without_pred_xgb = without_pred_xgb )
}
##lgb_test----
fitness_test_lgb=function(datatrain,datatest,learning_rate,num_iterations,max_depth, bagging_fraction,feature_fraction,lambda_l1, lambda_l2,num_leaves ){
  
  
  data_sample_train_chr<-datatrain
  data_sample_train_chr<-data_sample_train_chr[,-ncol(data_sample_train_chr)]
  data_sample_train_y<-datatrain[,ncol(datatrain)]
  
  data_sample_test_chr<-datatest
  data_sample_test_chr<-data_sample_test_chr[,-ncol(data_sample_test_chr)]
  data_sample_test_y<-datatest[,ncol(datatest)]
  set.seed(123)
  train_params <- list(
    learning_rate = learning_rate,
    boosting= "goss",
    num_iterations=num_iterations,
    max_depth=max_depth,
    bagging_fraction=bagging_fraction,
    feature_fraction=feature_fraction,
    lambda_l1=lambda_l1,
    lambda_l2=lambda_l2,
    num_leaves = num_leaves,
    objective = "binary"
  )
  dtrain=lgb.Dataset(data_sample_train_chr,label=data_sample_train_y, categorical_feature=c(1L,2L,3L))
  lightgbm_1 <- lgb.train(data =   dtrain, 
                          params = train_params,
                          verbose = 1
                          
  )
  lgb_1=lgb.importance(model=lightgbm_1)
  predict_y<-predict(lightgbm_1 ,data_sample_test_chr)
  ruc=rocit(score = predict_y, class = data_sample_test_y)
  cutt=plot(ruc ,legendpos="bottom",values = T,legend = F)
  predict_y<-if_else((predict_y>=as.numeric(cutt[[6]][4])),1,0)
  predict_y=factor(predict_y,levels = c(0,1))
  confmatrix<- as.matrix(table(actual =data_sample_test_y , prediction = predict_y))
  plot_info=ruc
  Auc_lgb=ruc$AUC
  fscore=(2*confmatrix[2,2])/(2*confmatrix[2,2]+confmatrix[1,2]+confmatrix[2,1])
  accuracy=(confmatrix[2,2]+confmatrix[1,1])/sum(confmatrix)
  sensitivity=confmatrix[2,2]/(confmatrix[2,2]+confmatrix[2,1])
  specifity=confmatrix[1,1]/(confmatrix[1,1]+confmatrix[1,2])
  precision=confmatrix[2,2]/(confmatrix[2,2]+confmatrix[1,2])
  without_pred_lgb = (confmatrix[2,2]+confmatrix[2,1])/sum(confmatrix)
  
  result=list(lgb_1=lgb_1,confmatrix=confmatrix,plot_info=plot_info,Auc_lgb=Auc_lgb,fscore=fscore,
              accuracy=accuracy,sensitivity=sensitivity, specifity=specifity,precision=precision,without_pred_xgb = without_pred_lgb )
}

