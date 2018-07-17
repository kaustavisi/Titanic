train <- read.csv("train.csv")
orig <- train
## Crude imputation of Age
orig$Age[is.na(orig$Age)] = mean(orig$Age, na.rm = TRUE)


## test <- read.csv("test.csv")

test.glm <- function(ntest = 100,
                     formula = "Survived ~ Pclass + Sex + Age + SibSp",
                     ...)
{
    test.index <- (sample(1:nrow(orig), size = 1) +
                    (1:ntest)) %% nrow(orig)
    test <- orig[test.index, -2]
    train <- orig[-test.index, ]
    true <- orig[test.index, 2]    
    fit <- glm(formula, family = binomial, data = train)
    pred <- predict(fit, test, type = "response")    
    1 - sum(abs(as.numeric(as.vector(pred)) - true)) / length(pred)
}

hist(replicate(100, test.glm()))
