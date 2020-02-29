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

# The `DALEXtra` package

# Funnel Plot

# Champion-challenger

# Integrations

# Conclusions

# Acknowledgments

Work on this package was financially supported by the ‘NCN Opus grant 2016/21/B/ST6/02176’.

# References
