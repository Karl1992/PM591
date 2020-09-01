setwd("D:/study/MasterinUSC/PM591/final project")
NIS <- read.csv("NIS2012-200K.csv")
nis <- NIS

## Feature Selection

nis <- nis[,c("AGE", "FEMALE", "RACE", "HOSPBRTH", "PL_NCHS2006", "ZIPINC_QRTL", "AMONTH", "AWEEKEND", "ELECTIVE", "LOS", "TRAN_IN", "CM_AIDS", "CM_ALCOHOL", "CM_ANEMDEF", "CM_ARTH", "CM_BLDLOSS",	"CM_CHF",	"CM_CHRNLUNG", "CM_COAG", "CM_DEPRESS", "CM_DM",	"CM_DMCX", "CM_DRUG", "CM_HTN_C",	"CM_HYPOTHY",	"CM_LIVER", "CM_LYMPH",	"CM_LYTES", "CM_METS", "CM_NEURO", "CM_OBESE", "CM_PARA", "CM_PERIVASC", "CM_PSYCH", "CM_PULMCIRC",	"CM_RENLFAIL", "CM_TUMOR", "CM_ULCER", "CM_VALVE",	"CM_WGHTLOSS", "NEOMAT", "APRDRG_Risk_Mortality", "APRDRG_Severity", "MDC", "NDX", "ORPROC", "NCHRONIC", "NECODE", "HCUP_ED", "NPR", "HOSP_DIVISION", "NIS_STRATUM", "PAY1", "TOTCHG","DIED")]

#transfer data into right forms
#tranfer numeric variables into factor variables
for(i in 1:55){
  nis[,i]=as.factor(nis[,i])
}
#transfer into numeirc(6 continous variables)
nis$AGE=as.numeric(as.character(nis$AGE))
nis$LOS=as.numeric(as.character(nis$LOS))
nis$NDX=as.numeric(as.character(nis$NDX))
nis$NCHRONIC=as.numeric(as.character(nis$NCHRONIC))
nis$NECODE=as.numeric(as.character(nis$NECODE))
nis$NPR=as.numeric(as.character(nis$NPR))
nis$TOTCHG=as.numeric(as.character(nis$TOTCHG))
summary(nis)


#delete missing data
nis=nis[complete.cases(nis$AGE),]
nis=nis[complete.cases(nis$PL_NCHS2006),]
nis=nis[complete.cases(nis$AMONTH),]
nis=nis[complete.cases(nis$AWEEKEND),]
nis=nis[complete.cases(nis$TRAN_IN),]
nis=nis[complete.cases(nis$LOS),]
nis=nis[complete.cases(nis$TOTCHG),]
summary(nis)


#variable recoding
nis$HOSP_CONTROL <- numeric()
nis$HOSP_LOCTEACH <- numeric()
nis$HOSP_BEDSIZE <- numeric()
for (i in 1:nrow(nis)) {
  inter=strsplit(as.character(nis$NIS_STRATUM[i]),split = "")[[1]]
  nis$HOSP_CONTROL[i]=as.numeric(inter[2])
  nis$HOSP_LOCTEACH[i]=as.numeric(inter[3])
  nis$HOSP_BEDSIZE[i]=as.numeric(inter[4])
}
nis=nis[,-which(colnames(nis)=="NIS_STRATUM")]
nis$HOSP_CONTROL=as.factor(nis$HOSP_CONTROL)
nis$HOSP_LOCTEACH=as.factor(nis$HOSP_LOCTEACH)
nis$HOSP_BEDSIZE=as.factor(nis$HOSP_BEDSIZE)
summary(nis)


#rename and relabel
#delete meaningless data
#female
nis=nis[-which(nis$FEMALE==""),]
nis$FEMALE=as.numeric(nis$FEMALE)
nis$FEMALE=factor(nis$FEMALE,labels=c("0","1","2"))
#race
nis=nis[-which(nis$RACE==""),]
nis=nis[-which(nis$RACE=="A"),]
nis$RACE=as.numeric(nis$RACE)
nis$RACE=factor(nis$RACE,labels=c("1","2","3","4","5","6"))
#ZIPINC_QRTL
nis=nis[-which(nis$ZIPINC_QRTL==""),]
nis$ZIPINC_QRTL=as.numeric(nis$ZIPINC_QRTL)
nis$ZIPINC_QRTL=factor(nis$ZIPINC_QRTL,labels=c("1","2","3","4"))
#ELECTIVE
nis=nis[-which(nis$ELECTIVE==""),]
nis=nis[-which(nis$ELECTIVE=="A"),]
nis$ELECTIVE=as.numeric(nis$ELECTIVE)
nis$ELECTIVE=factor(nis$ELECTIVE,labels=c("0","1"))
#DIED
nis=nis[-which(nis$DIED==""),]
nis=nis[-which(nis$DIED=="A"),]
nis$DIED=as.numeric(nis$DIED)
nis$DIED=factor(nis$DIED,labels=c("0","1"))
#PAY1
nis=nis[-which(nis$PAY1==""),]
nis=nis[-which(nis$PAY1=="A"),]
nis$PAY1=as.numeric(nis$PAY1)
nis$PAY1=factor(nis$PAY1,labels=c("1","2","3","4","5","6"))
summary(nis)
nrow(nis)


#save this dataset
write.csv(nis,file="~/desktop/pm591/project/nis_new.csv",col.names=T,row.names=T)
#copy this dataset
nisold=nis
nrow(nisold)
summary(nisold)


#check distribution of independent variables
summary(nisold[nisold$DIED==1,])
summary(nisold[nisold$DIED==0,])
str(nisold)



#machine learning method trainning:ridge/lasso/svm/rf/lda/knn
m=nrow(nisold)

#sample
set.seed(2019)
nisnew=nisold

#specify task
library(mlr)
death_tsk=makeClassifTask(id="death",data=nisnew,target="DIED")

#split
set.seed(2019)
split_desc=makeResampleDesc(method='Holdout',stratify=TRUE,split=0.7)
split=makeResampleInstance(split_desc,task=death_tsk)
trainset=split$train.inds[[1]]
testset=split$test.inds[[1]]


####ridge
#specify ridge learner
ridge_lnr=makeLearner("classif.cvglmnet", 
                      fix.factors.prediction=TRUE,
                      predict.type="prob",
                      alpha=0,type.measure='auc')
#ridge model
#tuning parameter: 10cv by default
set.seed(2019)
death_ridge=train(learner=ridge_lnr,task=death_tsk,subset=trainset)

#plot the tuning lambda and auc
plot(death_ridge$learner.model$cvm,death_ridge$learner.model$lambda)

#extracts the optimal lambda and auc
lambda.min_ridge=death_ridge$learner.model$lambda.min
lambda.min_ridge #0.005491684
auc_ridge=max(death_ridge$learner.model$cvm)
auc_ridge #0.9442709 




####lasso
#specify lasso learner
lasso_lnr=makeLearner("classif.cvglmnet", 
                      fix.factors.prediction=TRUE,
                      predict.type="prob",
                      alpha=1,type.measure='auc')
#lasso model
#tuning parameter: 10cv by default
set.seed(2019)
death_lasso=train(learner=lasso_lnr,task=death_tsk,subset=trainset)

#plot the tuning lambda and auc
plot(death_lasso$learner.model$lambda,death_lasso$learner.model$cvm)

#extracts the optimal lambda
lambda.min_lasso=death_lasso$learner.model$lambda.min 
lambda.min_lasso #8.154976e-05
auc_lasso=max(death_lasso$learner.model$cvm)
auc_lasso #0.9450915




####forward logistic regression
#specify logistic regression model
logreg_lnr=makeLearner("classif.logreg",
                       fix.factors.prediction=TRUE,
                       predict.type="prob")
#creates dummies for the categorical variables
log_tsk=createDummyFeatures(death_tsk,method="reference")  
#specify a resampling strategy
set.seed(2019)
rdesc=makeResampleDesc("CV",iters=10L)
#specify a trainning subtask
log_tsk_train=subsetTask(log_tsk,subset=trainset)
#specify forward method
ctrl_forward=makeFeatSelControlSequential(method='sfs')
#selection features using forward method
psa_forwd=selectFeatures(learner=logreg_lnr,task=log_tsk_train, 
                         resampling=rdesc,measures=auc, 
                         control=ctrl_forward,show.info=TRUE)
#features selected
psa_forwd$x
#cross-validated AUC
cv_auc_forward=psa_forwd$y
cv_auc_forward 



#### SVM
#specify SVM learner
rbfsvm.lrn=makeLearner("classif.ksvm",
                       par.vals=list(kernel="vanilladot"),predict.type="prob")
#tuning parameter
cv10_stratified=makeResampleDesc("CV",iters=5L,stratify=TRUE) 
ctrl=makeTuneControlIrace(maxExperiments=200L)  
ps=makeParamSet(
  makeDiscreteParam("C",values=seq(1e-6,4,length=5))
)
#train SVM
set.seed(2019)
nis_svm_tune=tuneParams(rbfsvm.lrn,subsetTask(death_tsk,trainset),cv10_stratified,
                        measures=list(auc,mmce),ps,control=ctrl,show.info=TRUE)
nis_svm_tune$x
nis_svm_tune




####random forest
library(randomForest)
library(pROC)
#random forest trainning
death_RF=randomForest(DIED~.,data=nisnew[trainset,],
                      mtry=sqrt(60),
                      ntree=500,
                      strata=nisnew$DIED[trainset,],
                      samplesize=as.vector(table(nisnew$DIED[trainset])))
death_RF
#train random forest
head(death_RF$votes)
varImpPlot(death_RF,cex.lab=1.5,cex.axis=2,cex=1.3,n.var=20,main="",pch=16,col='red4')
roc_train=roc(nisnew[trainset,]$DIED,death_RF$votes[,1])
#performace of random forest
auc(roc_train) #0.9481
ci.auc(roc_train) #0.9438-0.9525
mmce_RF=measureMMCE(nisnew[trainset,]$DIED,death_RF$predicted)
mmce_RF #0.01605139
detach(pROC)



#model selection
###ridge
#extracts the optimal lambda and auc
lambda.min_ridge=death_ridge$learner.model$lambda.min
lambda.min_ridge #0.005491684
auc_ridge=max(death_ridge$learner.model$cvm)
auc_ridge #0.9442709 

###lasso
#extracts the optimal lambda
lambda.min_lasso=death_lasso$learner.model$lambda.min 
lambda.min_lasso #8.154976e-05
auc_lasso=max(death_lasso$learner.model$cvm)
auc_lasso #0.9450915

###forward logistic regression
#features selected
psa_forwd$x
#cross-validated AUC
cv_auc_forward=psa_forwd$y
cv_auc_forward 

###SVM
nis_svm_tune$x
nis_svm_tune

###Random Forest
#performace of random forest
auc(roc_train) #0.9481
ci.auc(roc_train) #0.9438-0.9525
mmce_RF=measureMMCE(nisnew[trainset,]$DIED,death_RF$predicted)
mmce_RF #0.01605139




#prediction performance
