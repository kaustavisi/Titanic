library(pacman)
pacman::p_load(rpart, rpart.plot, rattle)

train <- read.csv("train.csv")
orig <- train
## test <- read.csv("test.csv")


## Simulation to validate default CART
test.tree <- function(ntest = 100, ...)
{
    test.index <- (sample(1:nrow(orig), size = 1) +
                    (1:ntest)) %% nrow(orig)
    test <- orig[test.index, -2]
    train <- orig[-test.index, ]
    true <- orig[test.index, 2]    
    fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Fare +
                     Parch + Embarked,
                 data = train,
                 method = "class")
    ## rpart.plot(fit)
    pred <- predict(fit, test, type = "class")    
    1 - sum(abs(as.numeric(as.vector(pred)) - true)) / length(pred)
}

hist(replicate(100, test.tree())) 

