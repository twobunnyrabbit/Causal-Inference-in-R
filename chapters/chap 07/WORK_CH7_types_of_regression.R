library(ggplot2)
# Simulated data
set.seed(123)
house_data <- data.frame(size = runif(100, 1000, 5000), 
                         price = runif(100, 200000, 500000))
# Linear regression model
model <- lm(price ~ size, data = house_data)
summary(model)
ggplot(house_data, aes(x=size, y=price)) + 
  geom_point() +
  geom_smooth(method="lm", col="blue")

set.seed(123)
exam_data <- data.frame(study_hours = runif(100, 0, 20),
                        passed = rbinom(100, 1, prob=0.5))

# Logistic regression model
model <- glm(passed ~ study_hours, data=exam_data, family="binomial")

# Summary of the model
summary(model)

# Predicting probabilities
exam_data$predicted_probability <- predict(model, type = "response")


library(MASS)
set.seed(123)
ad_data <- data.frame(ad_spend = runif(100, 100, 1000),
                      sign_ups = rpois(100, lambda=20))
# Poisson regression model
model <- glm(sign_ups ~ ad_spend, data=ad_data, family="poisson")
summary(model)


library(survival)
set.seed(123)
maintenance_data <- data.frame(maintenance_freq = runif(100, 1, 12),
                               failure_time = runif(100, 0, 24),
                               event = rbinom(100, 1, 0.5))
# Cox proportional hazards model
surv_obj <- Surv(time = maintenance_data$failure_time, event = maintenance_data$event)
model <- coxph(surv_obj ~ maintenance_freq, data = maintenance_data)
summary(model)
