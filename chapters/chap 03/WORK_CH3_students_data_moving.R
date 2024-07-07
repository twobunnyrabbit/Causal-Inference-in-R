install.packages("MatchIt")
install.packages("ggplot2")

# get current work directory
getwd()
#set work directory
setwd("your working dir path string")

# Simulating a dataset
set.seed(123)  # Setting a seed for reproducibility
foldername = "./CHAP3/data/"
students <- read.table(file = paste(foldername, "student_data.csv"), sep = ",", header = TRUE)

# convert variables to factor
students$noise_level_pre <- as.factor(students$noise_level_pre)
students$noise_level_post <- as.factor(students$noise_level_post)
students$part_time_job <- as.factor(students$part_time_job)
students$grade_improvement <- students$post_move_grade - students$pre_move_grade  # Calculating grade improvement


# Get a summary of the data
summary(students)

# View the structure
str(students)

# Check for NA values
sum(is.na(students))

# exploratory data analysis
# visualize
# Basic exploratory data analysis
summary(students)  # Getting a summary of the dataset
plot(students$pre_move_grade, students$post_move_grade, 
     main="Pre vs. Post Move Grades", 
     xlab="Pre Move Grades", ylab="Post Move Grades")  # Scatterplot of grades before and after moving

plot(students$noise_level_pre, students$pre_move_grade, 
     main="Pre Noise Level vs. Pre Move Grades", 
     xlab="Pre Noise Level ", ylab="Pre Move Grades")  


plot(students$noise_level_post, students$post_move_grade, 
     main="Post Noise Level vs. Post Move Grades", 
     xlab="Post Noise Level ", ylab="Post Move Grades")  

library(ggplot2)

# Histogram for pre-move grades
ggplot(students, aes(x = pre_move_grade)) + geom_histogram(binwidth = 1, fill = "blue", color = "black")

# Histogram for post-move grades
ggplot(students, aes(x = post_move_grade)) + geom_histogram(binwidth = 1, fill = "green", color = "black")

# Scatter plot for study hours vs grade improvement
ggplot(students, aes(x = study_hours, y = grade_improvement)) + geom_point() + geom_smooth(method = "lm")

# Boxplot for noise level pre-move vs grade improvement
ggplot(students, aes(x = noise_level_pre, y = grade_improvement, fill = noise_level_pre)) + geom_boxplot()

# Boxplot for noise level post-move vs grade improvement
ggplot(students, aes(x = noise_level_post, y = grade_improvement, fill = noise_level_post)) + geom_boxplot()


# Correlation Analysis

# Load the corrplot package
install.packages("corrplot")
library(corrplot)

# Calculate correlations
correlations <- cor(students[, sapply(students, is.numeric)])

# Plot the correlations
corrplot(correlations, method = "circle")

#. Saving Your Plots
g <- ggplot(students, aes(x = study_hours, y = grade_improvement)) + geom_point()
ggsave("study_hours_vs_grade_improvement.png", plot = g)



# simple causal technique
# compare means
# T-test to compare means
t_out=t.test(students$pre_move_grade, students$post_move_grade, paired=TRUE)
t_out

# Regression analysis
lm_model <- lm(grade_improvement ~ noise_level_pre + noise_level_post + study_hours + part_time_job + family_income, data=students)
regression_data = summary(lm_model)  # Summarizing the linear model
regression_data

model_summary <- capture.output(regression_data)
writeLines(model_summary,paste(foldername,"regression_data1.txt"))
residuals_output <- capture.output(residuals(lm_model))
writeLines(residuals_output,paste(foldername,"regression_data_resid1.txt"))



# Propensity score matching - requires the 'MatchIt' package
library(MatchIt)
m.out <- matchit(noise_level_post ~ study_hours + part_time_job + family_income, method = "nearest", data = students)
matched_data <- match.data(m.out)  # Getting the matched dataset
# save data
write.table(matched_data, file = paste(foldername,"matched_data1.txt"), row.names = FALSE, sep = "\t")



## visualize the results
# Visualization - requires the 'ggplot2' package
library(ggplot2)
ggplot(students, aes(x=noise_level_pre, y=grade_improvement, color=noise_level_post)) + 
  geom_boxplot() +
  labs(title="Grade Improvement vs. Noise Level", x="Pre-Move Noise Level", y="Grade Improvement")




