n <- 1000 

data <- data.frame( 
  
  working_hours = runif(n, 6, 12), # Total working hours per day 
  
  commute_mode = factor(sample(c("Car", "Public Transport", "Bike", "Walk"), size = n, replace = TRUE)), # Mode of commute 
  
  traffic_level = runif(n, 0, 10), # Traffic level on a scale from 0 (no traffic) to 10 (heavy traffic) 
  
  work_flexibility = runif(n, 0, 1), # Flexibility in work schedule (0 = no flexibility, 1 = high flexibility) 
  
  stress_level = rep(0, n) # Placeholder for stress level 
  
) 


data$stress_level <- with(data, 2 + (0.5 * working_hours) + (traffic_level^2)/10 - (work_flexibility * 2) + 
                            
                            ifelse(commute_mode == "Car", traffic_level * 0.4, ifelse(commute_mode == "Public Transport", traffic_level * 0.2, 0)) + 
                            
                            rnorm(n, mean = 0, sd = 2)) 



# Visualize the non-linear relationship between traffic level and stress level 

library(ggplot2) 

ggplot(data, aes(x = traffic_level, y = stress_level, color = commute_mode)) +  
  
  geom_point(alpha = 0.5) + 
  
  geom_smooth(method = "loess", formula = y ~ x, size = 1) + 
  
  labs(title = "Impact of Traffic Level on Stress Level by Commute Mode", 
       
       x = "Traffic Level", 
       
       y = "Stress Level") + 
  
  theme_minimal() 