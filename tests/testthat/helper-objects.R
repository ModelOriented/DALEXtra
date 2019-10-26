library("randomForest")
library("xgboost")
library("DALEX")

HR_glm_model <- glm(status == "fired" ~ ., data = HR, family = "binomial")
explainer_glm <- explain(HR_glm_model, data = HR,  y = HR$status == "fired", verbose = FALSE)

HR_rf_model <- randomForest(status ~ ., data = HR, ntree = 100)
explainer_HR_rf  <- explain(HR_rf_model, data = HR, y = HR$status, verbose = FALSE)

loss_cross_entropy <- function (observed, predicted, p_min = 0.0001) {
  p <- sapply(seq_along(observed), function(i) predicted[i, observed[i]])
  sum(-log(pmax(p, p_min)))
}


# Random Forest
# Example of a model built using a data frame
titanic_small <- na.omit(titanic[1:1000,])
rf_model <- randomForest(survived == "yes" ~ gender + age + class + embarked +
                           fare + sibsp + parch,  data = titanic_small)

explainer_rf <- explain(rf_model, data = titanic_small,
                        y = titanic_small$survived == "yes", label = "RF", verbose = FALSE)

# xgboost (using matrix object)
# Example of a model that relies on a numeric matrix

titanic_small_mat <- as.matrix(titanic_small[,c(2,6,7,8)])
titanic_small_survived <- ifelse(titanic_small$survived == "yes", 1, 0)

xgb_model <- xgboost(data = titanic_small_mat, label = titanic_small_survived,
                     nrounds = 2, verbose = FALSE)

explainer_xgb <- explain(xgb_model,
                         data=titanic_small_mat,
                         y = titanic_small_survived, label="xgboost", verbose = FALSE)

# helper objects for aspect_importance tests
# titanic
titanic_data <- titanic_imputed
titanic_data$country <- NULL

titanic_glm_model <- glm(survived == "yes" ~ class+gender+age+sibsp+parch+fare+embarked,
                         titanic_data, family = "binomial")

titanic_new_observation <- data.frame(
  class = factor("1st", levels = c("1st", "2nd", "3rd", "deck crew",
                                   "engineering crew", "restaurant staff",
                                   "victualling crew")),
  gender = factor("male", levels = c("female", "male")),
  age = 8,
  sibsp = 0,
  parch = 0,
  fare = 72,
  embarked = factor("Southampton", levels = c("Belfast","Cherbourg",
                                              "Queenstown","Southampton"))
)

titanic_aspects <- list(wealth = c("class", "fare"),
                        family = c("gender", "sibsp", "parch"),
                        age = "age",
                        embarked = "embarked")


# apartments

apartments_lm_model <- lm(m2.price ~ ., data = apartments)

apartments_aspects <- list(space = c("surface", "no.rooms"),
                           construction.year = "construction.year",
                           floor = "floor",
                           district = "district")

apartments_new_observation <- apartments_test[2,-1]

apartments_num <- apartments[,unlist(lapply(apartments, is.numeric))]

apartments_num_lm_model <- lm(m2.price ~ ., data = apartments_num)

apartments_num_new_observation <- apartments_num[2,-1]

apartments_num_mod <- apartments_num[,-1]


