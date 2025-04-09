# Description
---
## Topic: *Increasing the Efficacy of Umbilical Cord Blood Banking Using Machine Learning Algorithms: A Case Study from Royan Cord Blood Bank*
### Summary
Cord blood is the blood that is obtained after the birth of a baby. Cord blood is rich in stem cells, which are used to treat a variety of diseases, including cancers and immune disorders. These treatments' effectiveness depends on the quantity of total nucleated cells (TNCs) in cord blood units (CBUs). Both public and private cord blood banks store these CBUs. Public banks rely on government funding for the cost of testing, storing, and maintaining CBUs. In addition, the quantity of TNCs in each CBU remains uncertain until the TNC test is conducted. This study aims to utilize ensemble learning algorithms to aid public banks in identifying and collecting potentially valuable CBUs prior to TNC testing in order to save the cost of TNC testing on CBUs that are not valuable. This study has three main contributions: Firstly, it demonstrates that the XGBoost and LightGBM algorithms can identify CBUs with TNC of more than 0.7×10^9,1×10^9, and 1.5×10^9; Secondly, the study combines the smote_NC method with Xgboost and LightGBM algorithms and evaluates each algorithm in identifying high TNC samples. Lastly, this article considers the effect of the phlebotomist experience on identifying high TNC samples, a variable overlooked in other studies.
### Software and packages 
This paper uses XGBoost, LightGBM, and rBayesianOptimization packages to run XGBoost, LightGBM, and Bayesian optimization algorithms in R software with version 4.1.1 R. In this research, the imbalanced-Learn library in Python software version 3.7.3 is used to implement the SMOTE_NC technique.
### Description of code files
**DataAndFunction.R**
In this file, the data format and missing values were fixed, and it was identified which part of the data should be assigned to the train and test data. Furthermore, functions like XGBoost and LightGBM are defined to be used for other code files.
**smote TNC.html**
In this file, when the threshold for TNC is identified as 0.7×10^9 or 1.5×10^9, we face an imbalance data. So, SMOTE_NC was applied to solve this issue.
**xgb_light_opt.R**
In this file, the parameters of XGBoost and LightGBM were tuned by the Bayesian optimization method. 
**testig_algorithm.R**
In this file, the performance of algorithms was evaluated by test data.
**plot.R**
In this file, the performance of algorithms has been shown in some pictures.
## Road map
![Road map]([image.jpg](https://github.com/HaghbayanM1374/TNC-Prediction/blob/master/picture/process.jpg))
