#extracting the data
install.packages('httr')
library(httr)
url <- "https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv"
response <- GET(url)
if (status_code(response) == 200)
study_hours <- content(response, 'text')  
std_hrs <- read.csv(text = study_hours)

#Simple Linear Regression
#scores as dependent variable (y-axis)
#no. of study hours as independent variable (x-axis)
regr <- lm(formula = Scores ~ Hours, data = std_hrs)
summary(regr)

#to assess the normality of the residuals
resid(regr)
plot(resid(regr))
plot(density(resid(regr)))
qqnorm(resid(regr))
qqline(resid(regr))
#hence, the given data follows a normal distribution

#to predict scores for a student that studies
#for 9.25 hrs a day
new_score <- data.frame(Hours = 9.25)
pred_score <- predict(regr, new_score)
print(pred_score)

#to evaluate the model
plot(regr)
mse <- mean(residuals(regr)^2)
rmse <- sqrt(mse)
