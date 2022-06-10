df <- read.csv("titanic_project.csv")
df$pclass <- factor(df$pclass)
df$survived <- factor(df$survived)
df$sex <- factor(df$sex, levels=c("female", "male"))
train <- df[1:900,]
test <- df[901:1046,]

# perform Naive Bayes
library(e1071)
nb1 <- naiveBayes(survived~pclass+age+sex, data=train)
pred <- predict(nb1, newdata=test[,-2], type="raw")
# look at first 5 (actual: 0 1 1 1 0)
pred[1:5,]

apriori <- c(
  nrow(df[df$survived=="0",])/nrow(df),
  nrow(df[df$survived=="1",])/nrow(df)
)
print("Prior probability, survived=no, survived=yes:")
apriori