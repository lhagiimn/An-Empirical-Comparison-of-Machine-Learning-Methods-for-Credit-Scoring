if(! "randomForest" %in% installed.packages()) install.packages("randomForest", depend = TRUE)
library(randomForest)
if(! "e1071" %in% installed.packages()) install.packages("e1071", depend = TRUE)
library(e1071)
if(! "ROCR" %in% installed.packages()) install.packages("ROCR", depend = TRUE)
library(ROCR)
if(! "earth" %in% installed.packages()) install.packages("earth", depend = TRUE)
library(earth)
if(! "xgboost" %in% installed.packages()) install.packages("xgboost", depend = TRUE)
library(xgboost)
if(! "dummies" %in% installed.packages()) install.packages("dummies", depend = TRUE)
library(dummies)
if(! "keras" %in% installed.packages()) install.packages("keras", depend = TRUE)
library(keras)
if(! "hmeasure" %in% installed.packages()) install.packages("hmeasure", depend = TRUE)
library('hmeasure')

load("training.rda")
load("test.rda")


train=list()
row_names=rownames(training)
for (i in 1:10) {
		sample = sample(row_names, size=floor(nrow(training)/10), replace = FALSE)
		train[[i]]  <- training[-match(sample, rownames(training)), ]
}


test=test[,match(names(training), names(test))]


###############         Logistic regression         #########################
	
	
logit_all=list()

marsModelAll=list()

for (i in 1:10) {
logit_all[[i]] <- glm(LATE60 ~ ., family=binomial(link="logit"), data = train[[i]])
	
###############         MARS regression         #########################
marsModelAll[[i]] <- earth(LATE60 ~ ., data=train[[i]], glm=list(family=binomial))	
	
}	



### model loading ##################

load('rf_model.rda')
load('svm_model.rda')
load('MLP_all.rda')
load('DMLP_all.rda')
load('MDMLP_all.rda')
load('XGBoost_model.rda')
load('SCF_result_relu.rda')



######################## CALCULATE MEASUREMENTS 

print_result = function(model=NULL, fit=NULL, test) {
library('hmeasure')
library(ROCR)
result=matrix(6, 1, 6)
	if(is.null(model)) {
		fit <- fit
	}else{
		fit <- predict(model, type="response", newdata=test)
	}
	
	result[1,] = as.matrix(HMeasure(test$LATE60, fit, threshold=0.061918)$metrics[c('H','AUC','TP', 'FP', 'TN', 'FN')])
	return(result)
}


print_resultXG = function(model=NULL, fit=NULL, test) {
library('hmeasure')
library(ROCR)

testy=as.matrix(test$LATE60)
testx=as.matrix(test[,-1])

result=matrix(6, 1, 6)
	if(is.null(model)) {
		fit <- fit
	}else{
		fit <- predict(model, newdata=testx)
	}
	
	result[1,] = as.matrix(HMeasure(test$LATE60, fit, threshold=0.061918)$metrics[c('H','AUC','TP', 'FP', 'TN', 'FN')])
	
	return(result)
}

################## subset has 94 features
result_all=matrix(rep(0, 66), 11, 6)
colnames(result_all)=c('H','AUC','TP', 'FP', 'TN', 'FN')
rownames(result_all)=c("Logistic", "MARS", "SVR", "RF", "XGBoost", "MLP", "Deeper MLP", "Deep MLP",  
"MLP with softmax", "Deeper MLP with softmax", "Deep MLP with softmax")


for (i in 1:10) {
	result_all[1,] = result_all[1,] + print_result(model = logit_all[[i]], test = test)
	result_all[2,] = result_all[2,] + print_result(model = marsModelAll[[i]], test = test)
	result_all[3,] = result_all[3,] + print_result(model = svm_model[[i]], test = test)
	result_all[4,] = result_all[4,] + print_result(model = rf_model[[i]], test = test)
	result_all[5,] = result_all[5,] + print_resultXG(model = XGBoost_model$model[[i]], test = test)
	result_all[6,] = result_all[6,] +print_result(fit =  MLP_all[[2]][[i]], test = test)
	result_all[7,] = result_all[7,] +print_result(fit =  DMLP_all[[2]][[i]], test = test)
	result_all[8,] = result_all[8,] +print_result(fit =  MDMLP_all[[2]][[i]], test = test)
	result_all[9,] = result_all[9,] +print_result(fit =  SCF_result_relu[[1]][[i]][[1]][,1], test = test)
	result_all[10,] = result_all[10,] +print_result(fit =  SCF_result_relu[[2]][[i]][[1]][,1], test = test)
	result_all[11,] = result_all[11,] +print_result(fit =  SCF_result_relu[[3]][[i]][[1]][,1], test = test)
}

result_all[1,] = result_all[1,]/10
result_all[2,] = result_all[2,]/10
result_all[3,] = result_all[3,]/10
result_all[4,] = result_all[4,]/10
result_all[5,] = result_all[5,]/10
result_all[6,] = result_all[6,]/10
result_all[7,] = result_all[7,]/10
result_all[8,] = result_all[8,]/10
result_all[9,] = result_all[9,]/10
result_all[10,] = result_all[10,]/10
result_all[11,] = result_all[11,]/10

result_all



######################## ROC curve

ROC = function(model=NULL, fit=NULL, test=test) {
library(ROCR)
	if(is.null(model)) {
		fit <- fit
	}else{
		fit <- predict(model, type="response", newdata=test)
	}
	
	return(fit)
}

ROCXG = function(model=NULL, fit=NULL, test=test) {
library(ROCR)
testy=as.matrix(test$LATE60)
testx=as.matrix(test[,-1])

	if(is.null(model)) {
		fit <- fit
	}else{
		fit <- predict(model, newdata=testx)
	}
	
	return(fit)
}


################## ROC curve for subset has 94 features
testy_all=list()
fit_log_all=list()
fit_mars_all=list()
fit_svm_all = list()
fit_rf_all = list()
fit_xgboost_all = list()
fit_MLP_all = list()
fit_dMLP_all = list()
fit_mdMLP_all = list()
fit_MLP_all_relu = list()
fit_dMLP_all_relu = list()
fit_mdMLP_all_relu = list()

for (i in 1:10) {
	testy_all[[i]]= test$LATE60
	fit_log_all[[i]]= ROC(model = logit_all[[i]], test = test)
	fit_mars_all[[i]]= ROC(model = marsModelAll[[i]], test = test)
	fit_svm_all[[i]] = ROC(model = svm_model[[i]], test = test)
	fit_rf_all[[i]] = fit =  ROC(model = rf_model[[i]], test = test)
	fit_xgboost_all[[i]] = ROCXG(model=XGBoost_model$model[[i]], test = test)
	fit_MLP_all[[i]] = ROC(fit =  MLP_all[[2]][[i]], test = test)
	fit_dMLP_all[[i]] =  ROC(fit =  DMLP_all[[2]][[i]], test = test)
	fit_mdMLP_all[[i]] = ROC(fit =  MDMLP_all[[2]][[i]], test = test)
	fit_MLP_all_relu[[i]] = ROC(fit =  SCF_result_relu[[1]][[i]][[1]][,1], test = test)
	fit_dMLP_all_relu[[i]] =  ROC(fit =  SCF_result_relu[[2]][[i]][[1]][,1], test = test)
	fit_mdMLP_all_relu[[i]] = ROC(fit =  SCF_result_relu[[3]][[i]][[1]][,1], test = test)
}

pred_log_all <- prediction(fit_log_all, testy_all)
pref_log_all=performance(pred_log_all, 'tpr', 'fpr')

pred_mars_all <- prediction(fit_mars_all, testy_all)
pref_mars_all=performance(pred_mars_all, 'tpr', 'fpr')

pred_svm_all <- prediction(fit_svm_all, testy_all)
pref_svm_all=performance(pred_svm_all, 'tpr', 'fpr')

pred_rf_all <- prediction(fit_rf_all, testy_all)
pref_rf_all=performance(pred_rf_all, 'tpr', 'fpr')

pred_xgboost_all <- prediction(fit_xgboost_all, testy_all)
pref_xgboost_all=performance(pred_xgboost_all, 'tpr', 'fpr')

pred_MLP_all <- prediction(fit_MLP_all, testy_all)
pref_MLP_all=performance(pred_MLP_all, 'tpr', 'fpr')

pred_dMLP_all <- prediction(fit_dMLP_all, testy_all)
pref_dMLP_all=performance(pred_dMLP_all, 'tpr', 'fpr')

pred_mdMLP_all <- prediction(fit_mdMLP_all, testy_all)
pref_mdMLP_all=performance(pred_mdMLP_all, 'tpr', 'fpr')

pred_MLP_all_relu <- prediction(fit_MLP_all_relu, testy_all)
pref_MLP_all_relu=performance(pred_MLP_all_relu, 'tpr', 'fpr')

pred_dMLP_all_relu <- prediction(fit_dMLP_all_relu, testy_all)
pref_dMLP_all_relu=performance(pred_dMLP_all_relu, 'tpr', 'fpr')

pred_mdMLP_all_relu <- prediction(fit_mdMLP_all_relu, testy_all)
pref_mdMLP_all_relu=performance(pred_mdMLP_all_relu, 'tpr', 'fpr')



jpeg(paste('ROC curve for SCF subset', '.jpg', sep=""), width = 7, height = 7, units = 'in', res = 1080)
	plot(pref_log_all, lty=1, col=0,main="ROC curve for SCF subset")
	plot(pref_log_all, avg="vertical", lty = 1, lwd=1.8, col=2,add=TRUE)
	plot(pref_mars_all, avg="vertical", lty=1, lwd=1.8, col=3,add=TRUE)
	plot(pref_svm_all, avg="vertical", lty=2, lwd=2, col='sienna',add=TRUE)
	plot(pref_rf_all, avg="vertical", lty=3, lwd=2.2, col=5,add=TRUE)
	plot(pref_xgboost_all, avg="vertical", lty=3, lwd=2.2, col=6,add=TRUE)
	plot(pref_MLP_all, avg="vertical", lty=4, lwd=2, col=1,add=TRUE)
	plot(pref_dMLP_all, avg="vertical", lty=4, lwd=2, col='firebrick',add=TRUE)
	plot(pref_mdMLP_all, avg="vertical", lty=4, lwd=2, col=4,add=TRUE)
	plot(pref_MLP_all_relu, avg="vertical", lty=4, lwd=2, col=7,add=TRUE)
	plot(pref_dMLP_all_relu, avg="vertical", lty=4, lwd=2, col=8,add=TRUE)
	plot(pref_mdMLP_all_relu, avg="vertical", lty=4, lwd=2, col=9,add=TRUE)
	legend(0.5,0.5, c('Logistic','MARS', 'SVR', "Random Forest", 
	"XGBoost", "MLP with sigmoid", "Deep MLP with sigmoid", "Deeper MLP with sigmoid", 
	"MLP with softmax", "Deeper MLP with softmax", "Deep MLP with softmax"),
	col=c(2, 3, 'sienna', 5, 6, 1, 'firebrick', 4, 7,8, 9),lty=c(1, 1, 2, 3, 3, 4, 4, 4, 4, 4, 4), lwd=2)
dev.off()



###################### Multi-Class credit scoring result

intoMultipleClasses =  function(model=NULL, fit_test=NULL, fit_train=NULL, test=test, train=train) {
		if(is.null(model)) {
			fit_train <- fit_test
			fit_test <- fit_test
		}else{
			fit_train <- predict(model, type='response', newdata=test)
			fit_test <- predict(model, type='response', newdata=test)
			
		}
	w1=quantile(fit_train, 0.98)
	w2=quantile(fit_train, 0.93)
	w3=quantile(fit_train, 0.85)
	w4=quantile(fit_train, 0.73)
	w5=quantile(fit_train, 0.58)
	w6=quantile(fit_train, 0.40)
	w7=quantile(fit_train, 0.13)
	prediction <- ifelse(fit_test >= w1, "C8", 
		ifelse(fit_test >= w2, "C7",
		ifelse(fit_test >= w3, "C6",
		ifelse(fit_test >= w4, "C5",
		ifelse(fit_test >= w5, "C4",
		ifelse(fit_test >= w6, "C3", 
		ifelse(fit_test >= w7, "C2","C1")))))))
		PD=matrix(32, 8, 4)
		colnames(PD)=c("Good borrowers", "Bad borrowers", "Deliquency rates", "Share")
		rownames(PD)=c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8")
		for(i in 1:8) {
			PD[i,1]=length(test$LATE60[which(prediction==paste0("C", i))])
			PD[i,2]=sum(test$LATE60[which(prediction==paste0("C", i))])
			
			if(PD[i,1]==0) {
				PD[i,3]=0
			}else {
				PD[i,3]=PD[i,2]/(PD[i,1])
			}
			PD[i,4]=length(test$LATE60[which(prediction==paste0("C", i))])/length(test$LATE60)
		}
		
		ECL = cumsum(PD[,4] * PD[,3])
		return(ECL)
	}
	
intoMultipleClassesXG =  function(model=NULL, fit_test=NULL, fit_train=NULL, test=NULL, train=NULL) {

trainy=as.matrix(train$LATE60)
trainx=as.matrix(train[,-1])
testy=as.matrix(test$LATE60)
testx=as.matrix(test[,-1])

		if(is.null(model)) {
			fit_train <- fit_test
			fit_test <- fit_test
		}else{
			fit_train <- predict(model, newdata=testx)
			fit_test <- predict(model, newdata=testx)
		}
	w1=quantile(fit_train, 0.98)
	w2=quantile(fit_train, 0.93)
	w3=quantile(fit_train, 0.85)
	w4=quantile(fit_train, 0.73)
	w5=quantile(fit_train, 0.58)
	w6=quantile(fit_train, 0.40)
	w7=quantile(fit_train, 0.13)
	prediction <- ifelse(fit_test >= w1, "C8", 
		ifelse(fit_test >= w2, "C7",
		ifelse(fit_test >= w3, "C6",
		ifelse(fit_test >= w4, "C5",
		ifelse(fit_test >= w5, "C4",
		ifelse(fit_test >= w6, "C3", 
		ifelse(fit_test >= w7, "C2","C1")))))))
		PD=matrix(32, 8, 4)
		colnames(PD)=c("Good borrowers", "Bad borrowers", "Deliquency rates", "Share")
		rownames(PD)=c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8")
		for(i in 1:8) {
			PD[i,1]=length(test$LATE60[which(prediction==paste0("C", i))])
			PD[i,2]=sum(test$LATE60[which(prediction==paste0("C", i))])
			
			if(PD[i,1]==0) {
				PD[i,3]=0
			}else {
				PD[i,3]=PD[i,2]/(PD[i,1])
			}
			PD[i,4]=length(test$LATE60[which(prediction==paste0("C", i))])/length(test$LATE60)
		}
		
		ECL = cumsum(PD[,4] * PD[,3])
		return(ECL)
	}

##################### Filter subset
pd = c(0.001, 0.004, 0.011, 0.025, 0.044, 0.062, 0.076, 0.085)

ECL_all=matrix(960, 120, 8)
rownames(ECL_all)=c(rep("Logistic", 10), rep("MARS", 10), rep("SVR", 10), rep("RF", 10),
 rep("XGBoost", 10), rep("MLP", 10), rep("Deep MLP", 10), rep("Deeper MLP", 10), 
 rep("MLP with softmax", 10), rep("Deep MLP with softmax", 10), 
 rep("Deeper MLP with softmax", 10), rep("FICO", 10))
colnames(ECL_all) =c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8")


for (i in 1:10) {
sample_n=sample(which(test$LATE60==0), size=2077, replace=FALSE)
sample_p= which(test$LATE60==1)
test1=test[c(sample_n,sample_p),]
ECL_all[i,] = intoMultipleClasses(model = logit_all[[i]], test = test1, train = train[[i]])
ECL_all[i+10, ] = intoMultipleClasses(model = marsModelAll[[i]], test = test1, train = train[[i]])
ECL_all[i+20, ] = intoMultipleClasses(model = svm_model[[i]], test = test1, train = train[[i]])
ECL_all[i+30, ] = intoMultipleClasses(model = rf_model[[i]], test = test1, train = train[[i]])
ECL_all[i+40, ] = intoMultipleClassesXG(model =  XGBoost_model$model[[i]], test = test1, train = train[[i]])
ECL_all[i+50, ] = intoMultipleClasses(fit_test =  MLP_all[[2]][[i]][c(sample_n,sample_p),], 
fit_train =  MLP_all[[3]][[i]], test = test1, train=training)
ECL_all[i+60, ] = intoMultipleClasses(fit_test =  DMLP_all[[2]][[i]][c(sample_n,sample_p),], 
fit_train =  DMLP_all[[3]][[i]], test = test1, train=training)
ECL_all[i+70, ] = intoMultipleClasses(fit_test =  MDMLP_all[[2]][[i]][c(sample_n,sample_p),], 
fit_train =  MDMLP_all[[3]][[i]], test = test1, train=training)
ECL_all[i+80, ] = intoMultipleClasses(fit_test =  SCF_result_relu[[1]][[i]][[1]][,1][c(sample_n,sample_p)], 
fit_train =  SCF_result_relu[[1]][[i]][[2]][,1], test = test1, train=training)
ECL_all[i+90, ] = intoMultipleClasses(fit_test =  SCF_result_relu[[2]][[i]][[1]][,1][c(sample_n,sample_p)], 
fit_train =  SCF_result_relu[[2]][[i]][[2]][,1], test = test1, train=training)
ECL_all[i+100, ] = intoMultipleClasses(fit_test =  SCF_result_relu[[3]][[i]][[1]][,1][c(sample_n,sample_p)], 
fit_train =  SCF_result_relu[[3]][[i]][[2]][,1], test = test1, train=training)
ECL_all[i+110, ] = pd
}

ACL_result = matrix(96, 12, 8)
rownames(ACL_result)=c("Logistic","MARS", "SVR", "RF",
 "XGBoost","MLP","Deep MLP","Deeper MLP", "MLP with softmax", 
 "Deeper MLP with softmax", "Deep MLP with softmax", "FICO")
colnames(ACL_result) =c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8")

for (i in 1:12) {
	ACL_result[i, ] = colMeans(ECL_all[(10*i-9):(10*i), ])
}

jpeg(paste('Expected credit loss for SCF subset', '.jpg', sep=""), width = 8, height = 6, units = 'in', res = 300)
plot(colMeans(ECL_all[1:10, ]), type='l', ylab='Cum ACL', xlab='Credit classes',
		xaxt='n', cex.axis=0.8, col=2,  lty = 2, lwd=2)
axis(1, at = c(1:8), labels=c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8"))
lines(colMeans(ECL_all[11:20, ]), col=3, lty = 2, lwd=2)
lines(colMeans(ECL_all[21:30, ]), col='sienna',  lty = 3, lwd=2)
lines(colMeans(ECL_all[31:40, ]), col=5,  lty = 4, lwd=2)
lines(colMeans(ECL_all[41:50, ]), col=6,  lty = 4, lwd=2)
lines(colMeans(ECL_all[51:60, ]), col=7,  lty = 5, lwd=2)
lines(colMeans(ECL_all[61:70, ]), col=8,  lty = 5, lwd=2)
lines(colMeans(ECL_all[71:80, ]), col=4,  lty = 5, lwd=2)
lines(pd, col=1, lwd=2.5)
legend(1, 0.084, c("Logistic", "MARS", "SVR","RF",
 "XGBoost", "MLP", "Deep MLP","Deeper MLP","MLP with softmax", 
 "Deeper MLP with softmax", "Deep MLP with softmax", "FICO"), col=c(2, 3, 'sienna', 5, 6, 7, 8, 4, 1, 9, 10, 11), 
	lty=c(2, 2, 3, 4, 4, 5, 5, 5, 6, 6, 6, 1), lwd=c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2.5))
dev.off()


