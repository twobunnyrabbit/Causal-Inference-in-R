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
       y = "GPA") 

# Visualizing GPA against TV hours with SES as color 

ggplot(data, aes(x = tv_hours, y = GPA, color = SES)) + 
  
  geom_point(alpha = 0.5) + 
  
  geom_smooth(method = "lm", se = FALSE) + 
  
  theme_minimal() + 
  
  labs(title = "GPA vs. TV Screen Time by SES", x = "TV Screen Time (hours)", y = "GPA") 