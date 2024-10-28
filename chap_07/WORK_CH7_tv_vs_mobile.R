set.seed(123) # Ensure reproducibility 

n <- 500 

# Generating synthetic data with a clearer linear relationship 

data <- data.frame( 
  
  mobile_hours = runif(n, 1, 5), # Mobile screen time in hours 
  
  tv_hours = runif(n, 1, 5), # TV screen time in hours 
  
  physical_activity = runif(n, 0, 10), # Physical activity in hours per week 
  
  sleep_quality = sample(1:10, n, replace = TRUE), # Sleep quality on a scale of 1 to 10 
  
  SES = sample(c("low", "medium", "high"), n, replace = TRUE), # Socioeconomic status 
  
  age = sample(12:18, n, replace = TRUE) # Age in years 
  
) 

# Convert SES to a numeric scale for simplicity 

data$SES_numeric <- as.numeric(factor(data$SES, levels = c("low", "medium", "high"))) 

# Adjusting academic performance to ensure a linear relationship 

data$GPA <- with(data, 
                 
                 3 +  
                   
                   -0.2 * mobile_hours + # Negative impact from mobile screen time 
                   
                   -0.15 * tv_hours + # Negative impact from TV screen time 
                   
                   0.05 * physical_activity + # Positive impact from physical activity 
                   
                   0.1 * sleep_quality / 10 + # Positive impact from sleep quality 
                   
                   0.1 * SES_numeric + # Positive impact from higher SES 
                   
                   -0.02 * age + # Slight negative impact from older age 
                   
                   rnorm(n, mean = 0, sd = 0.25) # Random noise 
                 
) 


library(ggplot2) 

# Visualizing GPA against mobile hours with SES as color 

ggplot(data, aes(x = mobile_hours, y = GPA, color = SES)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() + 
  labs(title = "GPA vs. Mobile Screen Time by SES", 
       x = "Mobile Screen Time (hours)", 
       y = "GPA") +
  theme(
    text = element_text(size = 14),  # Increase base text size
    plot.title = element_text(size = 14, face = "bold"),  # Larger, bold title
    axis.title = element_text(size = 16),  # Larger axis titles
    axis.text = element_text(size = 12),  # Larger axis text
    legend.title = element_text(size = 14),  # Larger legend title
    legend.text = element_text(size = 12)  # Larger legend text
  )

# Visualizing GPA against TV hours with SES as color 

ggplot(data, aes(x = tv_hours, y = GPA, color = SES)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() + 
  labs(title = "GPA vs. TV Screen Time by SES", 
       x = "TV Screen Time (hours)", 
       y = "GPA") +
  theme(
    text = element_text(size = 14),  # Increase base text size
    plot.title = element_text(size = 14, face = "bold"),  # Larger, bold title
    axis.title = element_text(size = 16),  # Larger axis titles
    axis.text = element_text(size = 12),  # Larger axis text
    legend.title = element_text(size = 14),  # Larger legend title
    legend.text = element_text(size = 12)  # Larger legend text
  )


## explore the data
library(ggplot2)

# Plotting GPA against mobile and TV hours
ggplot(data, aes(x = mobile_hours, y = GPA)) +
  geom_point(aes(color = SES), alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~SES) +
  theme_minimal() +
  labs(title = "Impact of Mobile Hours on GPA by Socioeconomic Status",
       x = "Mobile Screen Time (hours)",
       y = "GPA")

ggplot(data, aes(x = tv_hours, y = GPA)) +
  geom_point(aes(color = SES), alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~SES) +
  theme_minimal() +
  labs(title = "Impact of TV Hours on GPA by Socioeconomic Status",
       x = "TV Screen Time (hours)",
       y = "GPA")


# Fitting the linear model
model <- lm(GPA ~ mobile_hours + tv_hours + physical_activity + sleep_quality + SES + age + mobile_hours:tv_hours, data = data)
summary(model)


ggplot(data, aes(x = mobile_hours + tv_hours, y = GPA)) +
  geom_point(aes(color = SES), alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "green") +
  theme_minimal() +
  labs(title = "Model Fit: Impact of Total Screen Time on GPA",
       x = "Total Screen Time (hours)",
       y = "GPA")

