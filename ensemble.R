library(pacman)
pacman::p_load(rpart, rpart.plot)

## Missing value imputation is not needed
model.CART <- function(train, test,
                       formula = "Survived ~ Pclass + Sex + Age +
                           SibSp + Fare + Parch + Embarked")
{
    fm <- rpart(formula, data = train, method = "class")
    as.numeric(as.vector(predict(fm, test, type = "class")))
}


## Before using this, one may like to impute the missing value. If
## missing values are not imputed, while ensembling, user needs to be
## careful on scoring (remove NA and put less count while aggregating)
model.glm <- function(train, test,
                      formula = "Survived ~ Pclass + Sex + Age +
                           SibSp")
{
    fm <- glm(formula, family = binomial, data = train)
    as.numeric(as.vector(predict(fm, test, type = "response")))
}


## Need imputed data for proper functioning. 
model.qda <- function(train, test,
                       formula = "Survived ~ Pclass + Sex + Age +
                           SibSp + Fare + Parch")
{
    fm <- qda(as.formula(formula), data = train)
    as.numeric(as.vector(predict(fm, test)$class))
}

## Need imputed data for proper functioning.
model.lda <- function(train, test,
                       formula = "Survived ~ Pclass + Sex + Age +
                           SibSp + Fare + Parch")
{
    fm <- lda(as.formula(formula), data = train)
    as.numeric(as.vector(predict(fm, test)$class))
}

