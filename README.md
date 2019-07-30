# DALEXtra

[![Build Status](https://travis-ci.org/ModelOriented/DALEXtra.svg?branch=master)](https://travis-ci.org/ModelOriented/DALEXtra)
[![Coverage
Status](https://img.shields.io/codecov/c/github/ModelOriented/DALEXtra/master.svg)](https://codecov.io/github/ModelOriented/DALEXtra?branch=master)

An extension for DALEX package

# Installation and configuration

Install `DALEXtra` and `DALEX` packages

```
devtools::install_github("ModelOriented/DALEXtra")
install.packages("DALEX")
```

Install `reticulate` package

```
install.packages("reticulate")
```

https://modeloriented.github.io/DALEXtra/

# Examples
```
    # Explainer build (Keep in mind that 18th column is target)
    titanic_test <- read.csv(system.file("extdata", "titanic_test.csv", package = "DALEXtra"))
    # Keep in mind that when pickle is being built and loaded,
    # not only Python version but libraries versions has to match aswell
    explainer <- explain_scikitlearn(system.file("extdata", "scikitlearn.pkl", package = "DALEXtra"),
    condaenv = "myenv", data = titanic_test[,1:17], y = titanic_test$survived)
    print(model_performance(explainer))

    # Predictions with newdata
    explainer$model$predict_function(explainer$model, titanic_test[1:10,1:17])
```
