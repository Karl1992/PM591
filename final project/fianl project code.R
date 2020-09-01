#cleaning data
#DXCCSn is set to invalid (.A), if the diagnosis code (DXn) is invalid by the HCUP criteria (EDX02).
#DXCCSn is set to inconsistent (.C), if the diagnosis code (DXn) is inconsistent with age (EAGE04 and EAGE05) or sex of the patient (EDX03).

#delete missing data and transfer data into original form
setwd("~/desktop/pm591/project")
nis_clean=read.csv("nis_clean.csv")
nis_clean=nis_clean[,-1]
changing=c("APRDRG_Risk_Mortality", "APRDRG_Severity", "CM_AIDS", "CM_ALCOHOL", "CM_ANEMDEF", "CM_ARTH", "CM_BLDLOSS CM_CHF", "CM_CHRNLUNG", "CM_COAG", "CM_DEPRESS", "CM_DM", "CM_DMCX", "CM_DRUG", "CM_HTN_C", "CM_HYPOTHY", "CM_LIVER", "CM_LYMPH", "CM_LYTES", "CM_METS", "CM_NEURO", "CM_OBESE", "CM_DRUG", "CM_HTN_C", "CM_HYPOTHY", "CM_LIVER", "CM_LYMPH", "CM_LYTES", "CM_METS", "CM_NEURO", "CM_PARA", "CM_PERIVASC", "CM_PSYCH", "CM_PULMCIRC", "CM_RENLFAIL", "CM_TUMOR", "CM_ULCER", "CM_VALVE", "CM_WGHTLOSS", "AWEEKEND", "DIED", "AMONTH", "DISPUNIFORM", "DQTR", "DRG","DRG24", "DRG_NoPOA" ,"DRGVER", "HCUP_ED", "HOSPBRTH", "HOSP_DIVISION", "MDC", "MDC24", "MDC_NoPOA", "TRAN_IN", "TRAN_OUT", "RACE", "PL_NCHS2006", "ORPROC", "NEOMAT", "NIS_STRATUM", "CM_CHF", "CM_BLDLOSS","PR")
for (i in changing) {
  nis_clean[,colnames(nis_clean) == i] <- as.factor(nis_clean[,colnames(nis_clean) == i])
}
nis_clean=nis_clean[-which(nis_clean$ELECTIVE=="A"),]
ELECTIVE=as.numeric(nis_clean$ELECTIVE)
ELECTIVE=factor(ELECTIVE,labels=c("0","1","blank"))
summary(ELECTIVE)
nis_clean$ELECTIVE=ELECTIVE

#variables selection and recoding
nis_clean_new <- nis_clean[,c("AGE", "FEMALE", "RACE", "HOSPBRTH", "PL_NCHS2006", "ZIPINC_QRTL", "AMONTH", "AWEEKEND", "DQTR", "ELECTIVE", "LOS", "TRAN_IN", "TRAN_OUT", "CM_AIDS", "CM_ALCOHOL", "CM_ANEMDEF", "CM_ARTH", "CM_BLDLOSS",	"CM_CHF",	"CM_CHRNLUNG", "CM_COAG", "CM_DEPRESS", "CM_DM",	"CM_DMCX", "CM_DRUG", "CM_HTN_C",	"CM_HYPOTHY",	"CM_LIVER", "CM_LYMPH",	"CM_LYTES", "CM_METS", "CM_NEURO", "CM_OBESE", "CM_PARA", "CM_PERIVASC", "CM_PSYCH", "CM_PULMCIRC",	"CM_RENLFAIL", "CM_TUMOR", "CM_ULCER", "CM_VALVE",	"CM_WGHTLOSS", "NEOMAT", "DISPUNIFORM", "APRDRG_Risk_Mortality", "APRDRG_Severity", "MDC", "NDX", "ORPROC", "NCHRONIC", "NECODE", "HCUP_ED", "NPR", "HOSP_DIVISION", "NIS_STRATUM", "PAY1", "TOTCHG","DIED")]

HOSP_CONTROL <- numeric()
HOSP_LOCTEACH <- numeric()
HOSP_BEDSIZE <- numeric()
for (i in 1:length(nis_clean_new$NIS_STRATUM)) {
  inter <- strsplit(as.character(nis_clean_new$NIS_STRATUM[i]), split = "")[[1]]
  HOSP_CONTROL[i] <- as.numeric(inter[2])
  HOSP_LOCTEACH[i] <- as.numeric(inter[3])
  HOSP_BEDSIZE[i] <- as.numeric(inter[4])
}
nis_clean_new <- nis_clean_new[,-which(colnames(nis_clean_new) == "NIS_STRATUM")]
HOSP_CONTROL <- as.factor(HOSP_CONTROL)
HOSP_LOCTEACH <- as.factor(HOSP_LOCTEACH)
HOSP_BEDSIZE <- as.factor(HOSP_BEDSIZE)
nis_clean_new <- cbind(nis_clean_new, HOSP_CONTROL, HOSP_LOCTEACH, HOSP_BEDSIZE)



#machine learning
n=nrow(nis_clean)
nis=nis_clean_new

#specify task
library(mlr)
death_tsk=makeClassifTask(id="death",data=nis,target="DIED")

#split
set.seed(20190421)
split_desc = makeResampleDesc(method='Holdout', stratify = TRUE, split=0.7)
split = makeResampleInstance(split_desc,task=death_tsk)
trainset = split$train.inds[[1]]
testset = split$test.inds[[1]]



#ridge
library(mlr)
#specify ridge learner
ridge_lnr=makeLearner("classif.cvglmnet", 
                      fix.factors.prediction = TRUE,
                      predict.type = "prob",
                      alpha=0, type.measure='auc')
#ridge model
#specify ridge model
death_ridge = train(learner = ridge_lnr, task = death_tsk, subset=trainset)

#extracts the optimal lambda
lambda.min_ridge = death_ridge$learner.model$lambda.min 

#creates a new learner where lambda is fixed at the optimal value
ridge_lambda.min_lnr = makeLearner("classif.glmnet", lambda=lambda.min_ridge,
                                   fix.factors.prediction = TRUE,
                                   predict.type = "prob", alpha=0) 

#trains the ridge model with the optimal lambda on train data set
death_ridge_final = train(learner = ridge_lambda.min_lnr, task = death_tsk, subset=trainset) 

#list performance
performance(pred=death_ridge_final,measures=auc)
performance(pred=death_ridge_final,measures=mmce)




#lasso
#specify lasso learner
lasso_lnr=makeLearner("classif.cvglmnet", 
                      fix.factors.prediction = TRUE,
                      predict.type = "prob",
                      alpha=1, type.measure='auc')
#lasso model
#specify lasso model
death_lasso = train(learner = lasso_lnr, task = death_tsk, subset=trainset)

#extracts the optimal lambda
lambda.min_lasso = death_lasso$learner.model$lambda.min 

#creates a new learner where lambda is fixed at the optimal value
lasso_lambda.min_lnr = makeLearner("classif.glmnet", lambda=lambda.min_lasso,
                                   fix.factors.prediction = TRUE,
                                   predict.type = "prob", alpha=0) 

#trains the lasso model with the optimal lambda on train data set
death_lasso_final = train(learner = lasso_lambda.min_lnr, task = death_tsk, subset=trainset) 

#list performance
performance(pred=death_lasso_final,measures=auc)
performance(pred=death_lasso_final,measures=mmce)


## SVM
#SVM learner
svm.lrn = makeLearner("classif.ksvm", predict.type = "prob")

#tuning parameter
cv5_stratified = makeResampleDesc("CV", iters=5, stratify = TRUE) 
ctrl = makeTuneControlIrace(maxExperiments = 200L)  

ps_extended = makeParamSet(
  makeDiscreteParam("kernel", values = c("vanilladot", "polydot", "rbfdot")),
  makeDiscreteParam("C", values = seq(1e-6, 5, length=10) ),
  makeDiscreteParam("sigma", values = c(1e-3, 1e-2, 1e-1), requires = quote(kernel == "rbfdot") ), 
  makeIntegerParam("degree", lower = 1L, upper = 5L, requires = quote(kernel == "polydot") )
)

#train SVM
set.seed(20190421)
#detach("package:pROC", unload=TRUE)
nis_svm_tune = tuneParams(svm.lrn, subsetTask(death_tsk, trainset), cv5_stratified, measures=list(auc,mmce), ps_extended, control = ctrl, show.info = FALSE)
nis_svm_tune$x
nis_svm_tune




# random forest
set.seed(20190421)
library(randomForest)
library(pROC)
#random forest learner
rf = randomForest(DIED ~ . , data = nis, mtry = sqrt(60))
rf
# OOB estimate of  error rate: 0.5%

#train random forest
head(rf$votes)
varImpPlot(rf, cex.lab=1.5, cex.axis=2, cex=1.3, n.var=20, main="", pch=16, col='red4')
roc_train = roc(nis$DIED, rf$votes[,1])

#performace of random forest
auc(roc_train); ci.auc(roc_train)
mmce_rf <- measureMMCE(nis$DIED, rf$predicted);mmce_rf



#performance of all machine learning method
#ridge
performance(pred=death_ridge_final,measures=auc)
performance(pred=death_ridge_final,measures=mmce)
#lasso
performance(pred=death_lasso_final,measures=auc)
performance(pred=death_lasso_final,measures=mmce)
#SVM
nis_svm_tune
#random forest 
auc(roc_train)
mmce_rf

#model choose


#prediction performance
