
##Justin Barreras
##Logistic Regression on Titanic Dataset
df <- read.csv("titanic_project.csv")
train <- df[1:900,]
test <- df[901:1046,]
start1 <- Sys.time()
glm1 <- glm(survived~pclass, data = train, family = "binomial")
end1 <- Sys.time()
t1 <- predict(glm1, newdata = test, type = "response")
print("These are the coefficients")
glm1$coefficients
t1 <- ifelse(t1 > 0.5, 1,0)
##predicting on the test data
conf_matr <- table(t1, test$survived)
##computing the accuracy
acc <- mean(t1==test$survived)
print(paste("The model has shown to only have this accuracy: " ,acc))
library(caret)
print(paste("The model has this True Positive rate/sensitivity: ", sensitivity(conf_matr)))
print(paste("However the model has this true negative rate/specificity: ", specificity(conf_matr)))
tx <- (end1-start1)*60
print(paste("THe total amount of time (in seconds) it took the ML algorithm is :", tx))