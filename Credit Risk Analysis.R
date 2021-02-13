CR <- read.csv(file.choose() , header = T)
CR$loan_status=as.factor(CR$loan_status)
CR$person_home_ownership=as.factor(CR$person_home_ownership)
CR$loan_intent=as.factor(CR$loan_intent)
CR$loan_grade=as.factor(CR$loan_grade)
CR$cb_person_default_on_file=as.factor(CR$cb_person_default_on_file)
str(CR)

library(caret)
set.seed(123)
training.samples <- createDataPartition(CR$loan_status, p = 0.7, list = FALSE)
train.data <- CR[training.samples, ]
test.data <- CR[-training.samples, ]
library(ISLR)
logreg <- glm(loan_status ~ ., data = train.data, family = "binomial")
summary(logreg)
logreg1 <- glm(loan_status ~ . - person_age - loan_percent_income - cb_person_default_on_file 
               - cb_person_cred_hist_length, data = train.data, family = "binomial")
summary(logreg1)
anova(logreg1, test="Chisq")
#significant variables improve our model
# transformed_estimates <- cbind(Estimates=round(coef(logreg1),5),OR=round(exp(coef(logreg1)),5))
# transformed_estimates

library(dplyr)
test.data$predicted.risk=predict(logreg1,newdata=test.data,type="response")
table(test.data$loan_status, as.numeric(test.data$predicted.risk >= 0.5))
Overall_Accuracy_train = (6483 + 841) / nrow(test.data)
#0.8487
Sensitivity_train = 841 / (1019+841) 
#0.4365
Specificity_train = 6483/ (6483 + 841)  
#0.8886

#Baseline model
table(test.data$loan_status)
Baseline_accuracy=6729/(6729+1860)
#0.78344
#The baseline accuracy is 0.78344. Hence, we see that the Revised Model beats the baseline model comfortably.

library(ROCR)
pred = prediction(test.data$predicted.risk, test.data$loan_status)
as.numeric(performance(pred, "auc")@y.values)
# Make predictions on training set
predictTrain = predict(logreg1, type="response")
# Prediction function
ROCRpred = prediction(predictTrain, train.data$loan_status)
# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")
# Plot ROC curve
plot(ROCRperf)
# Add colors
plot(ROCRperf, colorize=TRUE)
# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(1,0,by=-0.5), text.adj=c(-0.2,1.7))

####Interpretation
Overall_Accuracy = (1142 + 6115) / nrow(test.data)
#0.8449
Sensitivity = 1142 / (1142 + 718) 
#0.6139
Specificity = 6115/ (614 + 6115)  
#0.908
#Testing the new threshold on the test data set and call it vector t1
t1 = table(test.data$loan_status, as.numeric(test.data$predicted.risk >= 0.35))
t1
# At a cutoff of 0.35, the sensitivity sees a drastic improvement from (calulate it)% in the original model to (calulate it)%. 
# And the Overall Accuracy of the model is also not compromised much as it is still 
# considerably better at 84% than the baseline accuracy of 78%.
