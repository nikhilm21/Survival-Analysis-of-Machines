library("survival")

attach(turbofan_df)

fit.surv <- survfit(Surv(max_cycle, status) ~ 1)
plot(fit.surv, xlab = "Cycles", ylab = "Estimated Probability for Survival")


### Operational Setting 3
fit.op <- survfit(Surv(max_cycle, status) ~ operational_setting_3)
plot(fit.op, xlab = "Cycles", ylab = "Estimated Probability for Survival", 
     col = c (2 ,4))
legend_labels <- levels(operational_setting_1)

legend("topright", levels(operational_setting_3), col = c(2,4), lty = 1)   

logrank.test <- survdiff(Surv(max_cycle, status) ~ operational_setting_3)
logrank.test

"""
The test suggests the there is significant difference between the survival curves

"""
fit.sm <- survfit(Surv(max_cycle, status) ~ sensor_measurement_16)
plot(fit.sm, xlab = "Cycles", ylab = "Estimated Probability for Survival", 
     col = c(2,4))

legend("topright", levels(sensor_measurement_16), col = c(2,4), lty = 1)   

logrank.test <- survdiff(Surv(max_cycle, status) ~ sensor_measurement_16)
logrank.test


levels(sensor_measurement_16)
###############################################################################

fit.cox <- coxph(Surv(max_cycle, status) ~ ., data = turbofan_df)
summary(fit.cox)
