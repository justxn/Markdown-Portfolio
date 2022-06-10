
library(e1071)
library(caret)
df <- read.csv("titanic_project.csv")
train <- df[1:900,]
test <- df[901:1046,]
start2 <- Sys.time()
##train model
nb1 <- naiveBayes(as.factor(survived)~as.factor(pclass)+sex+age, data = train)
##output model
nb1
##test model
t2 <- predict(nb1, newdata = test, type = "class")
end2 <- Sys.time()
##output metrics
cm2 <- table(t2, test$survived)
cm2
print(paste("We have an accuracy of : ", mean(t2==test$survived)))
print(paste("The model has this True Positive rate/sensitivity: ", sensitivity(cm2)))
print(paste("The model also has this true negative rate/specificity: ", specificity(cm2)))
print(paste("The total amount of time(in minutes) Naive Bayes took was :",end2-start2))


##ANALYZING THE TITANIC DATA

hist(df$age)
hist(df$pclass)

ggplot(df, aes(x = survived, fill=pclass)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', 
            aes(label=stat(count)), 
            position = position_dodge(width=1), 
            vjust=-0.5)+
  theme_classic()
ggplot(df, aes(x = age)) +
  geom_density(fill='coral')