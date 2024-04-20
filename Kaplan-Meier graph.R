library(survival)
library(dplyr)
library(glmnet)
attach(turbofan_df)

fit.surv <- survfit(Surv(max_cycle, status) ~ 1)
plot(fit.surv, xlab = "Cycles", ylab = "Estimated Probability for Survival", 
     main='Kaplan-Meier Survival Curve')

summary(fit.surv, times =150)

print(fit.surv)

### Operational Setting 3
fit.op <- survfit(Surv(max_cycle, status) ~ operational_setting_3)
plot(fit.op, xlab = "Cycles", ylab = "Estimated Probability for Survival", 
     col = c (2 ,4), main='Kaplan-Meier Survival Curve Stratified with 
     operational_setting_3')
legend_labels <- levels(operational_setting_1)

legend("topright", levels(operational_setting_3), col = c(2,4), lty = 1)   

logrank.test <- survdiff(Surv(max_cycle, status) ~ operational_setting_3)
logrank.test

coxph(Surv(max_cycle, status) ~ operational_setting_3)

"""
The test suggests the there is significant difference between the survival curves

"""
fit.sm <- survfit(Surv(max_cycle, status) ~ sensor_measurement_16)
plot(fit.sm, xlab = "Cycles", ylab = "Estimated Probability for Survival", 
     col = c(2,4), main='Kaplan-Meier Survival Curve Stratified with 
     sensor_measurement_16')

legend("topright", levels(sensor_measurement_16), col = c(2,4), lty = 1)   

logrank.test <- survdiff(Surv(max_cycle, status) ~ sensor_measurement_16)
logrank.test

coxph(Surv(max_cycle, status) ~ sensor_measurement_16)
###############################################################################

fit.sm <- survfit(Surv(max_cycle, status) ~ sensor_measurement_19)
plot(fit.sm, xlab = "Cycles", ylab = "Estimated Probability for Survival", 
     col = c(2,4), main='Kaplan-Meier Survival Curve 
     Stratified with sensor_measurement_19')

legend("topright", levels(sensor_measurement_19), col = c(2,4), lty = 1)   

logrank.test <- survdiff(Surv(max_cycle, status) ~ sensor_measurement_19)
logrank.test

coxph(Surv(max_cycle, status) ~ sensor_measurement_19)

#levels(sensor_measurement_16)

model <- coxph(Surv(max_cycle, status) ~ ., data = turbofan_df)
summary(model)
############### Basic GLMNET
p <- ncol(turbofan_df) - 2
p

data <- as.matrix(turbofan_df[, 3:p])
show_fit=glmnet(data,Surv(max_cycle, status),standardize=TRUE,
                lambda=seq(0,0.25,.001),alpha=1,family = "cox")
print(show_fit)

plot(show_fit,label=TRUE)
plot(show_fit,xvar = "lambda",label=TRUE) 

################## Partial Devience
surv_model <- cv.glmnet(data,Surv(max_cycle, status), family = "cox", 
                        type.measure = "deviance",alpha=1,nfolds = 10)
plot(surv_model)
print(surv_model)


lambda_opt <- surv_model$lambda.1se

# Fit the model using the optimal lambda value
fit <- glmnet(data,Surv(max_cycle, status), family = "cox", 
              type.measure = "deviance",alpha=1,lambda = lambda_opt)
coef(fit)

predictions <- predict(fit, newx = data, type = "response")


c_index <- Cindex(predictions,Surv(max_cycle, status))
c_index

################## Cindex
surv_model_C = cv.glmnet(data,Surv(max_cycle, status), 
                         family = "cox", type.measure = "C", 
                         alpha=1,nfolds = 10)
plot(surv_model_C)
print(surv_model_C)


lambda_opt <- surv_model_C$lambda.1se

# Fit the model using the optimal lambda value
fit_C <- glmnet(data,Surv(max_cycle, status), 
                family = "cox", 
                type.measure = "C",alpha=1,lambda = lambda_opt)
coef(fit_C)

# Make predictions
predictions <- predict(fit_C, newx = data, type = "response")
# Evaluate the model performance using concordance index (c-index)
c_index <- Cindex(predictions,Surv(max_cycle, status))
c_index
