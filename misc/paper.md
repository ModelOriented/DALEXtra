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
output: pdf_document
---

# Introduction

We have come to the point where machine learning predictive models usage is wider than even researchers of the past could expect. Business [@Business], health care and many others relay to some extend on constantly changing models. Unfortunately constantly new appliances and need for improvement lead to more and more complicated black-boxes. That is why we seek for tools dedicated to Explainable Artificial Intelligence (XAI) that will help us to understand predictions. Good examples of such software are `DALEX` [@DALEX] R package or `lime` [@lime] and `shap` [@NIPS2017_7062] Python libraries. 

The growing popularity of machine learning has sped up software development in that area. Developers took as their objective to make training models faster and smoother. R libraries `mlr`[@mlr] and `caret`[@caret], Python `scikit-learn`[@sklearn], or Java `h2o`[@h2o] made today's machine learning experts life easier than ever before. It is no longer hard to quickly make a decent model, even without sophisticated knowledge about machine learning. What is challenging nowadays is to understand what stands behind the model's decisions. This is exactly is the motivation for development of Explainable Artificial Intelligence (XAI) tool, like `DAELX`[@DALEX].

Comparison of Machine Learning models is tedious and not always a well-defined task. It is because there are many ways where such analysis could go. On top of that, fast development conducted in many directions, that were determined by variety of used programming language, made it hard to compare the behavior of models that was build using different environments. We can of course compare measurements but it is next to impossible to compare explanations. Therefore we need a unified way to analyze profiles of variables in our model (`ingredients`[@ingredients]), the relative importance of features `lime` [@lime] or residuals of model (`auditor`[@auditor]). All of mentioned above technics are a legitimate approach that can, but not always will, let us determine which model is the best.

The `DALEXtra` serves as an extension for `DALEX`[@DALEX] R package. One of its applications is to provide dedicated API that wraps models created using various Machine Learning libraries in order to explain them using DrWhy.ai[@drwhy] family. That is necessary to perform Champion-Challenger analysis that will be covered in future paragraphs.

# Funnel Plot

The Funnel Plot is a new way to present various metrics scores of predictive model. It stands as a contrast to a global approach where we calculate measurements and plot them together. Instead of that, we do calculate metrics on subsets of our dataset, that are determined by variable distribution. Every numerics variable is being divided into bins, which number is stated by the user, according to empirical distribution. For categorical values, each level more frequent than cutoff will determine a different subset. Calculations can be made with any type of measurement function with a property that lower score indicates better performance (such as MSE or 1-AUC).

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
 funnel_plot_data <- funnel_measure(explainer_lm, explainer_rf,
                             nbins = 5, measure_function = DALEX::loss_root_mean_square)
 plot(funnel_plot_data)

```

![Funnel Plot. For every variable, dot on the right side of the violet line means
that Linear Model (Champion model) is better for given subset. Dot on the left means that Random Forest model (Challenger model) is better. Values one the edges of the plot indicates subset that caused maximum deviation in measure difference for each side.](funnel_plot_example.png)

# Champion-challenger

Champion-challenger analysis is a prediction oriented way to present how a given model (Champion) behaves in comparison to other (possibly many Challengers). DALEXtra package allows users to create a report in an automatics way using three different already implemented sections and one default.

  - **Funnel Plot** described in the previous paragraph.

  - **Overall comparison** section that aims to visualize the global behavior of our model taking into consideration whole dataset. In fact, it consists of two sections
     - **Radar Plot** Plots scores scaled to [0,1] compartment using radar type geometry. It represents a global approach where we are taking into consideration scores over the whole datasets. [@auditor]
     - **Accordance Plot** for each observation it plots Champion response at OX axis and challengers responses at OY
     axis possibly using colors to distinguish models
     
  - **Training-test comparison** plot presents the relation between a score that models achieved using test dataset and training dataset. Champion and Challengers are distinguished using different colors. It helps to prevent over-fitting
  
  - **Default section** besides implemented sections we can pass any other object on which `plot` method can be used. 
  (eg. `iBreakDown`[@iBreakDown]. It will be included in the report as an independent section. 
  
# Integration

# Conclusions

# Acknowledgments

Work on this package was financially supported by the ‘NCN Opus grant 2016/21/B/ST6/02176’.

# References
