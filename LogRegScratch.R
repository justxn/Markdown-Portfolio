df <- read.csv("titanic_project.csv")
train <- df[1:900,]
test <- df[901:1046,]

sigmoid <- function(z){
  1.0 / (1+exp(-z))
}
# set up weight vector, label vector, and data matrix
weights <- c(1, 1)
data_matrix <- cbind(rep(1, nrow(train)), train$pclass)
labels <- as.integer(train$survived) 

learning_rate <- 0.001

  
  prob_vector <- data_matrix %*% weights
  error <- labels - sigmoid(prob_vector)
  weights <- weights + learning_rate * t(data_matrix) %*% error

weights

