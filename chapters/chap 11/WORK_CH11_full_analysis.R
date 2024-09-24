install.packages("mediation")
install.packages("lavaan")
library(mediation)
library(lavaan)

# PREP DATA
set.seed(123) # For reproducibility
n <- 1000 # Number of observations

# Creating synthetic data
data <- data.frame(
  Gender = factor(sample(c("Male", "Female"), n, replace = TRUE)),
  AgeGroup = factor(sample(c("Under 30", "30-60", "Over 60"), n, replace = TRUE, prob = c(0.2, 0.5, 0.3))),
  IncomeLevel = sample(20000:100000, n, replace = TRUE),
  PolicyDuration = sample(1:30, n, replace = TRUE),
  NumberofClaims = rpois(n, lambda = 2),
  CustomerSatisfaction = sample(1:10, n, replace = TRUE),
  Termination = factor(sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.3, 0.7)))
)

# Adding binary column for Termination
data$TerminationBinary <- ifelse(data$Termination == "Yes", 1, 0)

# Converting unordered factors to numeric
data$GenderNumeric <- as.numeric(data$Gender)
data$AgeGroupNumeric <- as.numeric(data$AgeGroup)

# CONDUCT MEDIATION ANALYSIS
str(data)
summary(data)
table(data$Gender, data$Termination)

# Mediator model
med.model <- lm(CustomerSatisfaction ~ NumberofClaims + GenderNumeric + AgeGroupNumeric + IncomeLevel + PolicyDuration, data = data)

# Outcome model
out.model <- glm(TerminationBinary ~ CustomerSatisfaction + NumberofClaims + GenderNumeric + AgeGroupNumeric + IncomeLevel + PolicyDuration, family = "binomial", data = data)

med.out <- mediate(med.model, out.model, treat = "NumberofClaims", mediator = "CustomerSatisfaction", robustSE = TRUE, sims = 500)
summary(med.out)

# ADVANCED MEDIATION MODELS
# Setting up the model with multiple mediators
model <- '
# Mediation paths
CustomerSatisfaction ~ b1*NumberofClaims
IncomeLevel ~ b2*NumberofClaims
TerminationBinary ~ c1*CustomerSatisfaction + c2*IncomeLevel + c3*NumberofClaims
# Indirect effects
CustomerSatisfactionMediation := b1 * c1
IncomeLevelMediation := b2 * c2
# Total effect
TotalEffect := b1 * c1 + b2 * c2 + c3
'

fit <- sem(model, data = data, missing = "ML", estimator = "MLR", fixed.x = FALSE)
summary(fit, standardized = TRUE, fit.measures = TRUE)

# MEDIATION IN THE PRESENCE OF MODERATION
model_modmed <- '
# Moderated mediation paths
CustomerSatisfaction ~ b1*NumberofClaims + b3*GenderNumeric + b4*NumberofClaims:GenderNumeric
IncomeLevel ~ b2*NumberofClaims + b5*GenderNumeric + b6*NumberofClaims:GenderNumeric
TerminationBinary ~ c1*CustomerSatisfaction + c2*IncomeLevel + c3*NumberofClaims + c4*GenderNumeric
'

fit_modmed <- sem(model_modmed, data = data, missing = "ML", estimator = "MLR", fixed.x = FALSE)
summary(fit_modmed, standardized = TRUE, fit.measures = TRUE)

# LONGITUDINAL MEDIATION ANALYSIS
# Extending the dataset for a longitudinal perspective
data$Year <- rep(1:5, each = n/5)

# Building a simple longitudinal mediation model for demonstration
# Assuming CustomerSatisfaction as the mediator for Year 1 impact on TerminationBinary in Year 5
model_long <- '
TerminationBinary ~ a*CustomerSatisfaction + b*NumberofClaims + c*Year
CustomerSatisfaction ~ d*NumberofClaims + e*Year
'

fit_long <- growth(model_long, data = data, estimator = "MLR")
summary(fit_long, standardized = TRUE, fit.measures = TRUE)