set.seed(123) # Ensure reproducibility
n <- 200 # Number of observations per group

# Generating data for Group A
sales_A <- data.frame(
  strategy = rep('A', n),
  salesperson_experience = rnorm(n, 5, 2), # Avg 5 years experience, SD 2
  customer_age = sample(25:65, n, replace = TRUE),
  customer_income = rnorm(n, 60000, 10000), # Avg income 60k, SD 10k
  financing = sample(c('yes', 'no'), n, replace = TRUE, prob = c(0.7, 0.3)),
  cars_sold = rpois(n, 3) # Avg 3 cars sold, poisson distribution
)

# Generating data for Group B
sales_B <- data.frame(
  strategy = rep('B', n),
  salesperson_experience = rnorm(n, 5, 2), # Same distribution for fairness
  customer_age = sample(25:65, n, replace = TRUE),
  customer_income = rnorm(n, 60000, 10000),
  financing = sample(c('yes', 'no'), n, replace = TRUE, prob = c(0.7, 0.3)),
  cars_sold = rpois(n, lambda = 4) # Hypothesizing an improvement with strategy B
)

# Combining both datasets
sales_data <- rbind(sales_A, sales_B)

## STEP2
library(ggplot2)

# Plotting distribution of cars sold by strategy
ggplot(sales_data, aes(x = strategy, y = cars_sold)) +
  geom_boxplot() +
  labs(title = "Distribution of Cars Sold by Strategy", y = "Cars Sold", x = "Sales Strategy")


## STEP 3
t_test_result <- t.test(cars_sold ~ strategy, data = sales_data)
print(t_test_result)

## STEP 4: MULTIVARIATE ANA

# Converting 'financing' to numeric for regression analysis
sales_data$financing_numeric <- ifelse(sales_data$financing == 'yes', 1, 0)

lm_result <- lm(cars_sold ~ strategy + salesperson_experience + customer_age + customer_income + financing_numeric, data = sales_data)
summary(lm_result)

## STEP 6 
# Checking for normality
shapiro.test(sales_data$cars_sold[sales_data$strategy == 'A'])
shapiro.test(sales_data$cars_sold[sales_data$strategy == 'B'])

# Checking for equality of variances
var.test(cars_sold ~ strategy, data = sales_data)



## STEP 7
install.packages("effsize")

library(effsize)
effect_size <- cohen.d(sales_data$cars_sold ~ sales_data$strategy, pooled = TRUE)
print(effect_size)


## STEP 8
install.packages("pwr")

library(pwr)
pwr_result <- pwr.t.test(d = effect_size$estimate,
                         sig.level = 0.05,
                         power = 0.8,
                         alternative = 'two.sided')
print(pwr_result)


## STEP 9
# Stratified analysis example by financing option
lm_finance_yes <- lm(cars_sold ~ strategy, data = sales_data, subset = (financing == 'yes'))
lm_finance_no <- lm(cars_sold ~ strategy, data = sales_data, subset = (financing == 'no'))
summary(lm_finance_yes)
summary(lm_finance_no)

p.adjust(c(lm_finance_yes$coefficients[2], lm_finance_no$coefficients[2]), method = 'bonferroni')




##STEP 10
# Interaction effect of strategy and salesperson experience
interaction_model <- lm(cars_sold ~ strategy * salesperson_experience, data = sales_data)
summary(interaction_model)

library(ggplot2)
ggplot(sales_data, aes(x = salesperson_experience, y = cars_sold, color = strategy)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = "Interaction Effect of Strategy and Salesperson Experience on Cars Sold",
       x = "Salesperson Experience (years)",
       y = "Cars Sold")


