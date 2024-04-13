library(survival)
library(dplyr)
library(glmnet)
attach(turbofan_df)

fit.surv <- survfit(Surv(max_cycle, status) ~ 1)
plot(fit.surv, xlab = "Cycles", ylab = "Estimated Probability for Survival")

summary(fit.surv, times =150)

print(fit.surv)

### Operational Setting 3
fit.op <- survfit(Surv(max_cycle, status) ~ operational_setting_3)
plot(fit.op, xlab = "Cycles", ylab = "Estimated Probability for Survival", 
     col = c (2 ,4))
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
     col = c(2,4))

legend("topright", levels(sensor_measurement_16), col = c(2,4), lty = 1)   

logrank.test <- survdiff(Surv(max_cycle, status) ~ sensor_measurement_16)
logrank.test


###############################################################################

fit.sm <- survfit(Surv(max_cycle, status) ~ sensor_measurement_19)
plot(fit.sm, xlab = "Cycles", ylab = "Estimated Probability for Survival", 
     col = c(2,4))

legend("topright", levels(sensor_measurement_19), col = c(2,4), lty = 1)   

logrank.test <- survdiff(Surv(max_cycle, status) ~ sensor_measurement_19)
logrank.test

coxph(Surv(max_cycle, status) ~ sensor_measurement_19)

#levels(sensor_measurement_16)

model <- coxph(Surv(max_cycle, status) ~ ., data = turbofan_df)
summary(model)


show_fit=glmnet(turbofan_df,Surv(max_cycle, status),standardize=TRUE,
                lambda=seq(0,0.5,.01),alpha=1,family = "cox", maxit = 5000000)
print(show_fit)

p <- ncol(turbofan_df) - 2

data <- as.matrix(turbofan_df[, 3:p])

surv_model <- cv.glmnet(data,Surv(max_cycle,status), family = "cox", 
                        type.measure = "deviance",alpha=1,nfolds = 10)

plot(show_fit,label=TRUE)
plot(show_fit,xvar = "lambda",label=TRUE)

print(surv_model)


lambda_opt <- surv_model$lambda.1se

fit <- glmnet(data,Surv(max_cycle,status), family = "cox", 
              type.measure = "deviance",alpha=1,lambda = lambda_opt)

coef(fit)

predictions <- predict(fit, newx = data, type = "response")

c_index <- Cindex(predictions,Surv(max_cycle, status))
print(c_index)

surv_model_C = cv.glmnet(data,Surv(max_cycle,status), family = "cox", 
                         type.measure = "C", alpha=1,nfolds = 10)


plot(surv_model_C)
print(surv_model_C)



lambda_opt <- surv_model_C$lambda.1se

fit_C <- glmnet(data,Surv(max_cycle,status), family = "cox", 
                type.measure = "C",alpha=1,lambda = lambda_opt)
coef(fit_C)


predictions <- predict(fit_C, newx = data, type = "response")

c_index <- Cindex(predictions,Surv(max_cycle, status))
c_index







names(which(model$coefficients[,4]))
print(model$coefficients)

model$coefficients[,4]


a <- summary(model)$coefficients
a[,5]
c <- names(which(a[,5] < 0.05))

b <- rownames(a)[a[,5] < 0.05 ]

print(a[b, , drop=FALSE])


coxph(Surv(max_cycle, status) ~ operational_setting_1 + operational_setting_2 +
        operational_setting_3 + sensor_measurement_01 + sensor_measurement_05 +
        sensor_measurement_06 + sens, data = turbofan_df )
