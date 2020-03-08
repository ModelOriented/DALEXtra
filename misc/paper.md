---
title: 'DALEXtra: Cross language model comparison'
authors:
- affiliation: 1
  name: Szymon Maksymiuk
  orcid: 0000-0002-3120-1601
- affiliation: 1
  name: Przemyslaw Biecek
  orcid: 0000-0001-8423-1823
date: "28 February 2020"
bibliography: paper.bib
tags:
- R and Python integration
- Champion-Challenger analysis
- explainable artificial intelligence
- predictive modeling
- interpretable machine learning
affiliations:
- index: 1
  name: Faculty of Mathematics and Information Science, Warsaw University of Technology
---

# Introduction

We have came to the point where machine learning predictive models usage is wider than even researchers of the past
could expect. Business, health care and many others relay to some extend on constantly changing models. Unfortunately constatantly new appliances and need of improvement lead to more and more complicated balck-boxes. That is why we seek for tools dedicated to Explainable Artifital Inteligence (XAI) that will help us to understand predictions. Good examples of such software are `DALEX` [@DALEX] R package or `lime` [@lime] and `shap` [@NIPS2017_7062] Python libraries. 

Growing popularity of machine learning, has sped up software development in that area. Developers took as their objective to make training models faster and smoother. R libraries `mlr`[@mlr] and `caret`[@caret], Python `scikit-learn`[@sklearn], or Java `h2o`[@h2o] made today's researchers life easier than ever before. Unfortuantely such a fast grow in many directions, that were determinated by variety of used programming language, made it hard to compare two deeply models that was build using different enviroments.

Comaprison of Machine Learing models is tedious and not always well defined task. It is becasue there are many ways where such anlysis could go. Comaprison of profiles of vriables in our model (`ingredients`[@ingredients]), relative importance of that features `lime` [@lime] or analysis of residuals or measurements functions (`auditor`[@auditor]).
All of mentioned above technics are legitimate approach that can, but not always will, let us to determinate which models is the best.

The `DALEXtra` serves as an extension for `DALEX`[@DALEX] R package. One of it applications is to provide dedicated API that wrapps models created using various Machine Learning libraries in order to explain them using DrWhy.ai[@drwhy] family. That is necessery to perform Champion-Challenger anlysis that will be covered in future paragraphs.

# Funnel Plot

Funnel plot is a new way to present various metrics scores of predictive model. It stands as as a contrast to global approach where we calculate measurements and plot them together. Instead of that, we do calculate metrics on subsets of our dataset, that are determinated by variable distribution. Every numerics variable is being divided by number of bins, stated by the user, according to empirical distribution. For categorical vlaues, each level frequent than cutoff will determinate different subset. Calculations can be made with any type of measurement function with a property that lower score indicates better performance (such as MSE or 1-AUC).

## Code example

```
 library("mlr")
 library("DALEXtra")
 task <- mlr::makeRegrTask(
   id = "R",
   data = apartments,
   target = "m2.price"
 )
 learner_lm <- mlr::makeLearner("regr.lm")
 model_lm <- mlr::train(learner_lm, task)
 explainer_lm <- explain_mlr(model_lm, apartmentsTest, apartmentsTest$m2.price, label = "LM")
 learner_rf <- mlr::makeLearner("regr.randomForest" )
 model_rf <- mlr::train(learner_rf, task)
 explainer_rf <- explain_mlr(model_rf, apartmentsTest, apartmentsTest$m2.price, label = "RF")
 funnel_plot_data <- funnel_measure(explainer_lm, explainer_gbm,
                             nbins = 5, measure_function = DALEX::loss_root_mean_square)
 plot(funnel_plot_data)

```

Fig1 [description]

# Champion-challenger

Champion-challenger analysis is prediction oritented way to presnted how given model (Champion) behaves in comparison to other (possibly many Challengers). DALEXtra package allows user to create a report in an automatics way using three different already implemented sections and one deafult.

  - **Funnel Plot** described in the previous paragraph.

  - **Overall comaprison** section that aims to visualize global behaviour of our model taking into consideration whole
  dataset. In fact it consists out of two sections
     - **Radar Plot** Plots scores scaled to [0,1] compartment using radar type gemoetry. It represents global approach
     when we are taking into consideration scores over whole datasets.
     - **Accordance Plot** for each observation it plots Champion response at OX axis and challengers responses at OY
     axis possibly using colours to distinguish models
     
  - **Training-test comparison** plot presents relation between score that models achieved using test dataset and
  training datset. Champion and Challengers are distinguished using different colours. It helps to prevent over-fitting
  
  - **Default section** besides implemented sections we can pase any other object on which `plot` method can be used. 
  (eg. `iBreakDown`[@iBreakDown]. It will be included in the report as independent section. 


# Integration

# Conclusions

# Acknowledgments

Work on this package was financially supported by the ‘NCN Opus grant 2016/21/B/ST6/02176’.

# References
